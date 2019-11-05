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
  killProcessTree(currentPlotPid(), warnFail=TRUE)
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

  # Prepare plot asyncronously
  newFutPlotAndOutput <- futureCall(
    FUN=preparePlotsCapturingOutput,
    args=list(plotter=plotter, plotRequest=plotRequest, db=db)
  )
  plotPID <- newFutPlotAndOutput$job$pid
  currentPlotPid(plotPID)
  session$onSessionEnded(function() {
    flog.debug(
      "Session finished: Making sure plot task with PID=%s is killed",
      plotPID
    )
    killProcessTree(plotPID)
  })

  then(newFutPlotAndOutput,
    onFulfilled=function(value) {
      # Enable/disable, show/hide appropriate inputs
      # (i) Maps tab
      if(is.null(value$plots$obmap)) {
        if(input$mainArea=="mapTab") {
          updateTabsetPanel(session, "mainArea", "plotTab")
        }
        js$disableTab("mapTab")
      } else {
        js$enableTab("mapTab")
      }
      # (ii) Interactive or regular plot tabs
      interactive <- plotIsPlotly(value$plots$obplot)
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
      readyPlot(value$plots)
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
  plotCleanup <- finally(newFutPlotAndOutput, function() {
    currentPlotPid(-1)
    removeNotification(plotStartedNotifId())
    shinyjs::hide("cancelPlot")
    shinyjs::show("doPlot")
    enableShinyInputs(input, except="^multiPlots*")
    # Force-kill forked processes
    killProcessTree(plotPID)
    # Printing output produced during async plot, if any
    resolvedValue <- value(newFutPlotAndOutput)
    producedOutput <- resolvedValue$output
    if(length(producedOutput)>0) message(paste0(producedOutput, "\n"))
  })
  catch(plotCleanup, function(e) {
    # This prevents printing the annoying "Unhandled promise error" msg when
    # plots are cancelled
    if(!plotInterrupted()) flog.error(e)
    NULL
  })
  # This NULL is necessary in order to prevent the future from blocking
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
  myPlot <- readyPlot()$obplot
  req(plotIsPlotly(myPlot))
  notifId <- showNotification(
    "Rendering plot...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  # Convert ggplot object to plotly and customise
  # See <https://plotly-r.com/control-modebar.html>
  myPlot <- req(myPlot) %>%
    config(
      # See all config options at
      # <https://github.com/plotly/plotly.js/blob/master/src/plot_api/plot_config.js>
      displaylogo=FALSE, cloud=FALSE,
      scrollZoom=TRUE,
      toImageButtonOptions = list(
        filename="obsmon_plot",
        format="png",
        height=plotlySaveAsFigDimensions$height,
        width=plotlySaveAsFigDimensions$width
      ),
      # Decide what users can or cannot edit
      editable=TRUE,
      edits=list(
        titleText=FALSE,
        shapePosition=FALSE
      )
    )
  myPlot <- addTitleToPlot(myPlot, readyPlot()$title)
  myPlot
})
# (i.ii) Non-interactive plot, if plot does not support interactivity
output$plot <- renderPlot({
  req(!plotIsPlotly(readyPlot()$obplot))
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
