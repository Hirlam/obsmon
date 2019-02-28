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
  # Although the plot button is hidden when the multiPlot is being prepared,
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

  ###############################################################
  # All checks performed: We can now proceed with the multiPlot #
  ###############################################################
  # Prevent another plot from being requested
  disableShinyInputs(input)
  shinyjs::hide("doPlot")

  # Offer possibility to cancel plot
  plotInterrupted(FALSE)
  shinyjs::enable("cancelPlot")
  shinyjs::show("cancelPlot")

  plotStartedNotifId(showNotification(
    "Gathering data for plot...", type="message", duration=NULL
  ))

  # Using "sink" to suppress some annoying stuff futureCall sends to stderr.
  # Any eventual error messages wull be catched using "then" or "catch". The
  # aforementioned annoying stuff is:
  # 1. "Warning in serialize(what, NULL, xdr = FALSE) :
  #    'package:DBI' may not be available when loading",
  #     For this, suppressWarnings would work
  # 2. Two blank lines printed to stderr whenever a plot is cancelled
  #    suppressWarnings did not work for this for whatever reason
  futureCallStderrFilePath <- tempfile()
  tmpFile <- file(futureCallStderrFilePath, open="wt")
  sink(tmpFile, type="message")

  # Prepare plot assyncronously
  newFutPlot <- futureCall(
    FUN=preparePlots,
    args=list(plotter=plotter, plotRequest=plotRequest, db=db)
  )
  currentPlotPid(newFutPlot$job$pid)

  # Cancel sink, so error/warning messages can be printed again
  sink(type="message")

  then(newFutPlot,
    onFulfilled=function(value) {
      if(is.null(value$obmap)) {
        if(input$mainArea=="mapTab") {
          updateTabsetPanel(session, "mainArea", "plotTab")
        }
        js$disableTab("mapTab")
      } else {
        js$enableTab("mapTab")
      }
      readyPlot(value)
    },
    onRejected=function(e) {
      if(!plotInterrupted()) {
        showNotification("Could not produce plot", duration=1, type="error")
        flog.error(e)
      }
      readyPlot(NULL)
    }
  )
  plotCleanup <- finally(newFutPlot, function() {
    currentPlotPid(-1)
    removeNotification(plotStartedNotifId())
    shinyjs::hide("cancelPlot")
    shinyjs::show("doPlot")
    enableShinyInputs(input)
    # Cleaning temp file used for futureCall stderr
    close(tmpFile)
    unlink(futureCallStderrFilePath)
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
observeEvent(readyPlot(), {
  interactive <- plotCanBeMadeInteractive(readyPlot()$obplot)
  # Enable/disable, show/hide appropriate inputs
  shinyjs::toggle(
    condition=interactive, selector="#mainArea li a[data-value=plotlyTab]"
  )
  shinyjs::toggle(
    condition=!interactive, selector="#mainArea li a[data-value=plotTab]"
  )
  if(interactive && input$mainArea=="plotTab") {
    updateTabsetPanel(session, "mainArea", "plotlyTab")
  }
  if(!interactive && input$mainArea=="plotlyTab") {
    updateTabsetPanel(session, "mainArea", "plotTab")
  }
},
  ignoreNULL=FALSE
)

# (i) Rendering plots
# (i.i) Interactive plot, if plot supports it
output$plotly <- renderPlotly({
  req(plotCanBeMadeInteractive(readyPlot()$obplot))
  notifId <- showNotification(
    "Rendering interactive plot...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  # Add title to plot
  myPlot <- tryCatch(
    readyPlot()$obplot + ggtitle(readyPlot()$title),
    error=function(e) {
      flog.error("Problems setting plot title: %s", e)
      readyPlot()$obplot
    }
  )
  # Convert ggplot object to plotly and customise
  myPlot <- ggplotly(req(myPlot), tooltip = c("x","y","colour")) %>%
    config(
      displaylogo=FALSE, collaborate=FALSE, cloud=FALSE,
      scrollZoom=TRUE
    ) %>%
    layout(title=readyPlot()$title, legend=list(orientation="v"))

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
  myPlot
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
