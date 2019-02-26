#########
# Plots #
#########
currentPlotPid <- reactiveVal(-1)
plotStartedNotifId <- reactiveVal(-1)
plotInterrupted <- reactiveVal()
onclick("cancelPlot", {
  showNotification("Cancelling plot", type="warning", duration=1)
  plotInterrupted(TRUE)
  tools::pskill(currentPlotPid(), tools::SIGINT)
})

readyPlot <- reactiveVal()
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
      readyPlot(value)
      if(is.null(value$obmap)) {
        if(input$mainArea=="mapTab") {
          updateTabsetPanel(session, "mainArea", "plotTab")
        }
        js$disableTab("mapTab")
      } else {
        js$enableTab("mapTab")
      }
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
output$mapAndMapTitleContainer <- renderUI(
  mapAndMapTitleOutput("map", "mapTitle")
)
output$queryAndTableContainer <- renderUI(
  queryUsedAndDataTableOutput("queryUsed", "dataTable")
)


# Rendering plot/map/dataTable

# (i) Rendering plots
# Code for zoomable plot adapted from
# https://shiny.rstudio.com/gallery/plot-interaction-zoom.html
plotRanges <- reactiveValues(x=NULL, y=NULL)
# Reset range upon generation of new plot
observeEvent({readyPlot()}, {
  plotRanges$x <- NULL
  plotRanges$y <- NULL
})
# When a double-click happens, check if there's a brush on the plot.
# If so, zoom to the brush bounds; if not, reset the zoom.
observeEvent(input$plot_dblclick, {
  brush <- input$plot_brush
  if(is.null(brush)) {
    plotRanges$x <- NULL
    plotRanges$y <- NULL
  } else {
    plotRanges$x <- c(brush$xmin, brush$xmax)
    plotRanges$y <- c(brush$ymin, brush$ymax)
  }
})

output$plot <- renderPlot({
  if(!is.null(readyPlot()$obplot)) {
    notifId <- showNotification(
      "Rendering plot...", duration=NULL, type="message"
    )
    on.exit(removeNotification(notifId))
  }
  # Add title to plot
  myPlot <- tryCatch({
      if(is.ggplot(readyPlot()$obplot)) {
        readyPlot()$obplot + ggtitle(readyPlot()$title)
      } else {
        grid.arrange(readyPlot()$obplot, top=textGrob(readyPlot()$title))
      }
    },
    error=function(e) {
      if(!is.null(readyPlot()$obplot)) {
        flog.error("Problems setting plot title: %s", e)
      }
      readyPlot()$obplot
    }
  )
  # Re-render plot when user zooms in
  # At the moment, zoomming in is only supported for simple ggplot objects
  # using cartesian coordinates
  allowZoom <- isTRUE(attr(myPlot, "allowZoom"))
  if(allowZoom && !is.null(c(plotRanges$x, plotRanges$y))) {
    zoomTransform <- tryCatch({
      if("CoordFlip" %in% class(myPlot$coordinates)) {
        coord_flip(xlim=plotRanges$y, ylim=plotRanges$x, expand=FALSE)
      } else {
        coord_cartesian(xlim=plotRanges$x, ylim=plotRanges$y, expand=FALSE)
      }
      },
      error=function(e) {
        if(!is.null(myPlot)) flog.error("Problems zooming in: %s", e)
        NULL
      }
    )
    if(!is.null(zoomTransform)) myPlot <- myPlot + zoomTransform
  }
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
