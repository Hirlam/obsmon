#########
# Plots #
#########
# Start with the plotly tab disabled, so users don't see two "Plot" tabs
shinyjs::hide(selector="#mainArea li a[data-value=plotlyTab]")

currentPlotPid <- reactiveVal(-1)
plotStartedNotifId <- reactiveVal(-1)
plotInterrupted <- reactiveVal()
observeEvent(input$cancelPlot, {
  showNotification("Cancelling plot", type="warning", duration=1)
  plotInterrupted(TRUE)
  killProcessTree(currentPlotPid(), warnFail=TRUE)
}, priority=2000, ignoreInit=TRUE)

# Management of plot progress bar
plotProgressFile <- reactiveVal(NULL)
plotProgressStatus <- reactiveVal(function() NULL)
plotProgressBar <- reactiveVal(NULL)
observeEvent(plotProgressFile(), {
  plotProgressStatus(reactiveFileReader(
    500, session, isolate(plotProgressFile()), readPlotProgressFile
  ))
})
observeEvent(plotProgressStatus()(), {
  pProgress <- plotProgressStatus()()
  req(isTRUE(pProgress$total>0), isTRUE(pProgress$current<=pProgress$total))
  removeNotification(plotStartedNotifId())
  progress <- plotProgressBar()
  if(is.null(progress)) {
    progress <- shiny::Progress$new(max=pProgress$total)
    plotProgressBar(progress)
  }
  # Subtract 1 from barValue as progress is updated when the process begins
  barValue <- pProgress$current-1
  progress$set(
    value=barValue,
    message=sprintf(
      "Plot: Querying %s data files: %.0f%%",
      pProgress$total,  100.0*(barValue/pProgress$total)
    )
  )
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

  if(activePlotType()$requiresSingleStation) {
    validStation <- length(input$station)==1
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

  plotStartedNotifId(showNotification(
    "Processing plot request...", type="message", duration=NULL
  ))

  # Trigger creation of progress bar
  plotProgressFile(tempfile(pattern="plotProgress"))

  # The plot tab does not keep the spinner running if the plot
  # is NULL, but the plotly tab does. Using this to keep the
  # spinner running while the plot is being prepared.
  shinyjs::show(selector="#mainArea li a[data-value=plotlyTab]")
  if(input$mainArea %in% c("plotTab", "plotlyTab")) {
    updateTabsetPanel(session, "mainArea", "plotlyTab")
  }
  shinyjs::hide(selector="#mainArea li a[data-value=plotTab]")

  newPlot <- obsmonPlot(
    parentType=activePlotType(),
    db=req(activeDb()),
    paramsAsInUiInput=reactiveValuesToList(input)
  )

  # Fetch data asyncronously
  asyncFetchDataOutput <- futureCall(
    FUN=function(...) {
      capture.output(newPlot$fetchRawData(...), type="message")
    },
    args=list(progressFile=plotProgressFile())
  )
  plotPID <- asyncFetchDataOutput$job$pid
  currentPlotPid(plotPID)
  session$onSessionEnded(function() {
    flog.debug(
      "Session finished: Making sure plot task with PID=%s is killed",
      plotPID
    )
    killProcessTree(plotPID)
  })

  then(asyncFetchDataOutput,
    onFulfilled=function(value) {
      # TODO: Move obmap and obplot assignments to where they are being
      # rendered
      obmap <- newPlot$generateLeafletMap()
      obplot <- newPlot$generate()
      # Enable/disable, show/hide appropriate inputs
      # (i) Maps tab
      if(is.null(obmap)) {
        if(input$mainArea=="mapTab") {
          updateTabsetPanel(session, "mainArea", "plotTab")
        }
        hideTab("mainArea", "mapTab")
      } else {
        showTab("mainArea", "mapTab")
      }
      # (ii) Interactive or regular plot tabs
      interactive <- "plotly" %in% class(obplot)
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
      readyPlot(newPlot)
    },
    onRejected=function(e) {
      if(!plotInterrupted()) {
        showNotification("Could not produce plot", duration=1, type="error")
        flog.error(e)
      }
      # The plot tab does not keep the spinner running if the plot
      # is NULL, but the plotly tab does. Using this to remove the
      # spinner if the plot fails for whatever reason.
      shinyjs::show(selector="#mainArea li a[data-value=plotTab]")
      if(input$mainArea %in% c("plotTab", "plotlyTab")) {
        updateTabsetPanel(session, "mainArea", "plotTab")
      }
      shinyjs::hide(selector="#mainArea li a[data-value=plotlyTab]")

      readyPlot(NULL)
    }
  )
  plotCleanup <- finally(asyncFetchDataOutput, function() {
    currentPlotPid(-1)
    # Reset items related to plot progress bar
    removeNotification(plotStartedNotifId())
    unlink(plotProgressFile())
    if(!is.null(plotProgressBar())) plotProgressBar()$close()
    plotProgressFile(NULL)
    plotProgressBar(NULL)
    # Hide/show and disable/enable relevant inputs
    shinyjs::hide("cancelPlot")
    shinyjs::show("doPlot")
    enableShinyInputs(input, except="^multiPlots*")
    # Force-kill forked processes
    killProcessTree(plotPID)
    # Printing output produced during async plot, if any
    producedOutput <- value(asyncFetchDataOutput)
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
}, priority=2000)

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
hideTab("mainArea", "mapTab")

# Rendering plot/map/dataTable
# (i) Rendering plots
# (i.i) Interactive plot, if plot is a plotly object
output$plotly <- renderPlotly({
  req(readyPlot())
  obplot <- readyPlot()$generate()
  req("plotly" %in% class(obplot))
  notifId <- showNotification(
    "Rendering plot...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  obplot
})
# (i.ii) Non-interactive plot, if plot is not a plotly object
output$plot <- renderPlot({
  req(readyPlot())
  obplot <- readyPlot()$generate()
  if(is.null(obplot)) return(NULL)
  req(!("plotly" %in% class(obplot)))

  notifId <- showNotification(
    "Rendering plot...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))

  obplot
},
  res=96, pointsize=18
)

# (ii) Rendering dataTables
output$dataTable <- renderDataTable({
    if(!is.null(readyPlot()$data)) {
      notifId <- showNotification(
        "Rendering data table...", duration=NULL, type="message"
      )
      on.exit(removeNotification(notifId))
    }
    tryCatch(
      readyPlot()$data,
      error=function(e) NULL
    )
  },
  options=list(pageLength=100)
)
output$queryUsed <- renderText(
  tryCatch(
    readyPlot()$sqliteQuery,
    error=function(e) NULL
  )
)
output$dataTableDownloadAsTxt <- downloadHandler(
  filename = function() "plot_data.txt",
  content = function(file) req(obsmonPlotObj())$exportData(file, format="txt")
)
output$dataTableDownloadAsCsv <- downloadHandler(
  filename = function() "plot_data.csv",
  content = function(file) req(obsmonPlotObj())$exportData(file, format="csv")
)

# (iii) Rendering maps
output$map <- renderLeaflet({
  notifId <- showNotification(
    "Rendering map...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  tryCatch(
    readyPlot()$generateLeafletMap(),
    error=function(e) NULL
  )
})
output$mapTitle <- renderText(
  tryCatch(
    readyPlot()$title,
    error=function(e) NULL
  )
)
