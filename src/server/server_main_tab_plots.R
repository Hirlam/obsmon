#########
# Plots #
#########
# Start with the plotly tab disabled, so users don't see two "Plot" tabs
shinyjs::hide(selector="#mainArea li a[data-value=plotlyTab]")

currentPlotPid <- reactiveVal(-1)
plotStartedNotifId <- reactiveVal(-1)
plotInterrupted <- reactiveVal()
onclick("cancelPlot", {
  showNotification("Cancelling plot", type="warning", duration=1)
  plotInterrupted(TRUE)
  tools::pskill(currentPlotPid(), tools::SIGINT)
})

readyPlot <- reactiveVal(NULL)
observeEvent(input$doPlot, {
  # Make sure a plot cannot be requested if another is being produced.
  # Although the plot button is hidden when the plot is being prepared,
  # such an action may sometimes not be quick enough (e.g., when rendering
  # is ongoing).
  if(currentPlotPid()>-1) {
    showNotification(
      "Another plot is being produced. Please wait.",
      type="warning", duration=1
    )
  }
  req(currentPlotPid()==-1)

  # This erases any plot currently on display. Useful to avoid confusion if
  # producing the plot fails for whatever reason.
  readyPlot(NULL)

  db <- req(activeDb())
  plotter <- plotTypesFlat[[req(input$plottype)]]
  plotRequest <- list()
  plotRequest$expName <- req(input$experiment)
  plotRequest$dbType <- db$dbType
  plotRequest$criteria <- plotsBuildCriteria(input)
  if(requireSingleStation()) {
    validStation <- length(plotRequest$criteria$station)==1
    if(!validStation) {
      showNotification(
        "This plot requires choosing one station!",
        type="error", duration=2
      )
    }
    req(validStation)
  }

  ##########################################################
  # All checks performed: We can now proceed with the plot #
  ##########################################################
  # Prevent another plot from being requested
  disableShinyInputs(input, except="^multiPlots*")
  shinyjs::hide("doPlot")

  # Offer possibility to cancel plot
  plotInterrupted(FALSE)
  shinyjs::enable("cancelPlot")
  shinyjs::show("cancelPlot")

  # The plot tab does not keep the spinner running if the plot
  # is NULL, but the plotly tab does. Using this to keep the
  # spinner running while the plot is being prepared.
  shinyjs::show(selector="#mainArea li a[data-value=plotlyTab]")
  if(input$mainArea %in% c("plotTab", "plotlyTab")) {
    updateTabsetPanel(session, "mainArea", "plotlyTab")
  }
  shinyjs::hide(selector="#mainArea li a[data-value=plotTab]")

  plotStartedNotifId(showNotification(
    "Gathering data for plot...", type="message", duration=NULL
  ))

  # Using "sink" to suppress two annoying blank lines that are sent
  # to the stdout whenever a plot is cancelled. If more than these
  # empty lines are produced as output, then it will be shown upon
  # completion of the assync task (be it successfull or not)
  tmpStdOut <- vector('character')
  tmpStdOutCon <- textConnection('tmpStdOut', 'wr', local=TRUE)
  tempSinkMsgs <- !isTRUE(
    trimws(toupper(obsmonConfig$general$logLevel))=="DEBUG"
  )
  if(tempSinkMsgs) sink(tmpStdOutCon, type="message")

  # Prepare plot assyncronously
  newFutPlot <- futureCall(
    FUN=preparePlots,
    args=list(plotter=plotter, plotRequest=plotRequest, db=db)
  )
  currentPlotPid(newFutPlot$job$pid)

  # Cancel sink, so error/warning messages can be printed again
  if(tempSinkMsgs) sink(type="message")

  then(newFutPlot,
    onFulfilled=function(value) {
      # Enable/disable, show/hide appropriate inputs
      # (i) Maps tab
      if(is.null(value$obmap)) {
        if(input$mainArea=="mapTab") {
          updateTabsetPanel(session, "mainArea", "plotTab")
        }
        js$disableTab("mapTab")
      } else {
        js$enableTab("mapTab")
      }
      # (ii) Interactive or regular plot tabs
      interactive <- plotCanBeMadeInteractive(value$obplot)
      shinyjs::toggle(
        condition=interactive, selector="#mainArea li a[data-value=plotlyTab]"
      )
      shinyjs::toggle(
        condition=!interactive, selector="#mainArea li a[data-value=plotTab]"
      )
      if(input$mainArea %in% c("plotTab", "plotlyTab")) {
        if(interactive) {
          updateTabsetPanel(session, "mainArea", "plotlyTab")
        } else {
          updateTabsetPanel(session, "mainArea", "plotTab")
        }
      }

      # Update readyPlot reactive
      readyPlot(value)
    },
    onRejected=function(e) {
      if(!plotInterrupted()) {
        showNotification("Could not produce plot", duration=1, type="error")
        flog.error(e)
      }
      # The plot tab does not keep the spinner running if the plot
      # is NULL, but the plotly tab does. Using this to remove the
      # spinner is the plot fails for whatever reason.
      shinyjs::show(selector="#mainArea li a[data-value=plotTab]")
      if(input$mainArea %in% c("plotTab", "plotlyTab")) {
        updateTabsetPanel(session, "mainArea", "plotTab")
      }
      shinyjs::hide(selector="#mainArea li a[data-value=plotlyTab]")

      readyPlot(NULL)
    }
  )
  plotCleanup <- finally(newFutPlot, function() {
    currentPlotPid(-1)
    removeNotification(plotStartedNotifId())
    shinyjs::hide("cancelPlot")
    shinyjs::show("doPlot")
    enableShinyInputs(input, except="^multiPlots*")
    # Printing stdout produced during assync plot, if any
    if(isTRUE(trimws(tmpStdOut)!="")) cat(paste0(tmpStdOut, "\n"))
    close(tmpStdOutCon)
  })
  catch(plotCleanup, function(e) {
    # This prevents printing the annoying "Unhandled promise error" msg when
    # plots are cancelled
    if(!plotInterrupted()) flog.error(e)
    NULL
  })
  # This NULL is necessary in order to avoid the future from blocking
  NULL
})

# Finally, producing the output
# Rendering UI slots for the outputs dynamically
output$plotContainer <- renderUI(plotOutputInsideFluidRow("plot"))
output$plotlyContainer <- renderUI(plotlyOutputInsideFluidRow("plotly"))
output$mapAndMapTitleContainer <- renderUI(
  mapAndMapTitleOutput("map", "mapTitle")
)
output$queryAndTableContainer <- renderUI(
  queryUsedAndDataTableOutput("queryUsed", "dataTable")
)

# Rendering plot/map/dataTable
# (i) Rendering plots
# (i.i) Interactive plot, if plot supports it
output$plotly <- renderPlotly({
  req(plotCanBeMadeInteractive(readyPlot()$obplot))
  notifId <- showNotification(
    "Rendering plot...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  myPlot <- addTitleToPlot(readyPlot()$obplot, readyPlot()$title)
  # Convert ggplot object to plotly and customise
  myPlot <- ggplotly(req(myPlot), tooltip = c("x","y")) %>%
    config(
      displaylogo=FALSE, collaborate=FALSE, cloud=FALSE,
      scrollZoom=TRUE
    )

  myPlot
})
# (i.ii) Non-interactive plot, if plot does not support interactivity
output$plot <- renderPlot({
  req(!plotCanBeMadeInteractive(readyPlot()$obplot))
  if(!is.null(readyPlot()$obplot)) {
    notifId <- showNotification(
      "Rendering plot...", duration=NULL, type="message"
    )
    on.exit(removeNotification(notifId))
  }
  # Add title to plot
  myPlot <- tryCatch(
    grid.arrange(readyPlot()$obplot, top=textGrob(readyPlot()$title)),
    error=function(e) {
      if(!is.null(readyPlot()$obplot)) {
        flog.error("Problems setting plot title: %s", e)
      }
      readyPlot()$obplot
    }
  )
  addTitleToPlot(readyPlot()$obplot, readyPlot()$title)
},
  res=96, pointsize=18
)

# (ii) Rendering dataTables
output$dataTable <- renderDataTable({
    if(!is.null(readyPlot()$plotData)) {
      notifId <- showNotification(
        "Rendering data table...", duration=NULL, type="message"
      )
      on.exit(removeNotification(notifId))
    }
    tryCatch(
      readyPlot()$plotData,
      error=function(e) NULL
    )
  },
  options=list(pageLength=100)
)
output$queryUsed <- renderText(
  tryCatch(
    readyPlot()$queryUsed,
    error=function(e) NULL
  )
)
output$dataTableDownloadAsTxt <- downloadHandler(
  filename = function() "plot_data.txt",
  content = function(file) {
    dataInfo <- plotExportedDataInfo(readyPlot())
    write.table(readyPlot()$plotData, file, sep="\t", row.names=FALSE)
    write(paste0("\n", dataInfo), file, append=TRUE)
  }
)
output$dataTableDownloadAsCsv <- downloadHandler(
  filename = function() "plot_data.csv",
  content = function(file) {
    dataInfo <- plotExportedDataInfo(readyPlot())
    write.csv(readyPlot()$plotData, file, row.names=FALSE)
    write(paste0("\n", dataInfo), file, append=TRUE)
  }
)

# (iii) Rendering maps
output$map <- renderLeaflet({
  notifId <- showNotification(
    "Rendering map...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  tryCatch(
    readyPlot()$obmap,
    error=function(e) NULL
  )
})
output$mapTitle <- renderText(
  tryCatch(
    readyPlot()$title,
    error=function(e) NULL
  )
)
