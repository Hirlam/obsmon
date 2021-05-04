#########
# Plots #
#########
# Start with the plotTab disabled, so users don't see two "Plot" tabs
shinyjs::hide(selector="#mainArea li a[data-value=plotTab]")
# Also start with the mapTab disabled. Will be enabled if needed.
shinyjs::hide(selector="#mainArea li a[data-value=mapTab]")

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

obsmonPlotObj <- reactiveVal()
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

  # Erase any plot currently on display
  obsmonPlotObj(NULL)

  if(activePlotType()$requiresSingleStation && length(input$station) !=1) {
    showNotification(
      "This plot requires choosing one station!",
      type="error", duration=2
    )
    return(NULL)
  }

  ##########################################################
  # All checks performed: We can now proceed with the plot #
  ##########################################################
  # Prevent another plot from being requested
  disableShinyInputs(input, except=c("^multiPlots*", "^cancelPlot$"))
  shinyjs::hide("doPlot")

  # Offer possibility to cancel plot
  plotInterrupted(FALSE)
  shinyjs::show("cancelPlot")

  plotStartedNotifId(showNotification(
    "Processing plot request...", type="message", duration=NULL
  ))

  # Trigger creation of progress bar
  plotProgressFile(tempfile(pattern="plotProgress"))

  # Fetch raw data asyncronously so app doesn't freeze
  asyncNewPlotAndOutput <- futureCall(
    FUN=function(parentType, db, paramsAsInUiInput, ...) {
      output <- capture.output({
        newPlot <- obsmonPlotClass$new(
          parentType=parentType,
          db=db,
          paramsAsInUiInput=paramsAsInUiInput
        )
        newPlot$fetchRawData(...)
      })
      return(list(newPlot=newPlot, output=output))
    },
    args=list(
      parentType=activePlotType(),
      db=req(activeDb()),
      paramsAsInUiInput=reactiveValuesToList(input),
      progressFile=plotProgressFile()
    )
  )
  currentPlotPid(asyncNewPlotAndOutput$job$pid)

  then(asyncNewPlotAndOutput,
    onFulfilled=function(value) {
      obsmonPlotObj(value$newPlot)
    },
    onRejected=function(e) {
      if(!plotInterrupted()) {
        showNotification("Could not fetch plot data", duration=1, type="error")
        flog.error(e)
      }
    }
  )
  plotCleanup <- finally(asyncNewPlotAndOutput, function() {
    # Force-kill eventual zombie forked processes and reset pid
    killProcessTree(currentPlotPid())
    currentPlotPid(-1)

    # Reset items related to plot progress bar
    removeNotification(plotStartedNotifId())
    unlink(plotProgressFile())
    plotProgressFile(NULL)
    if(!is.null(plotProgressBar())) {
      plotProgressBar()$close()
      plotProgressBar(NULL)
    }

    # Hide/show and disable/enable relevant inputs
    shinyjs::hide("cancelPlot")
    shinyjs::show("doPlot")
    enableShinyInputs(input, except="^multiPlots*")

    # Printing output produced during async plot, if any
    producedOutput <- value(asyncNewPlotAndOutput)$output
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
chart <- reactive({
  if (is.null(obsmonPlotObj())) return(NULL)
  notifId <- showNotification(
    "Producing plot...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  obsmonPlotObj()$chart
})
leafletMap <- reactive({
  if (is.null(obsmonPlotObj())) return(NULL)
  notifId <- showNotification(
    "Producing leaflet map...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  obsmonPlotObj()$leafletMap
})

# Enable/disable, show/hide appropriate inputs
observe({
  req(obsmonPlotObj())
  # (i) Maps tab
  if(is.null(leafletMap())) {
    if(input$mainArea=="mapTab") {
      updateTabsetPanel(session, "mainArea", "plotlyTab")
    }
    shinyjs::hide(selector="#mainArea li a[data-value=mapTab]")
  } else {
    shinyjs::show(selector="#mainArea li a[data-value=mapTab]")
  }

  # (ii) Interactive or regular plot tabs
  interactive <- "plotly" %in% class(chart())
  shinyjs::toggle(
    condition=interactive, selector="#mainArea li a[data-value=plotlyTab]"
  )
  shinyjs::toggle(
    condition=!interactive, selector="#mainArea li a[data-value=plotTab]"
  )

  if(interactive && input$mainArea=="plotTab") {
    updateTabsetPanel(session, "mainArea", "plotlyTab")
  } else if(!interactive && input$mainArea=="plotlyTab") {
    updateTabsetPanel(session, "mainArea", "plotTab")
  }
})

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
# (i.i) Interactive plot, if plot is a plotly object
output$plotly <- renderPlotly({
  if(is.null(obsmonPlotObj())) return(NULL)
  req("plotly" %in% class(chart()))
  notifId <- showNotification(
    "Rendering plot...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  chart()
})
# (i.ii) Non-interactive plot, if plot is not a plotly object
output$plot <- renderPlot({
  if(is.null(obsmonPlotObj())) return(NULL)
  req(!("plotly" %in% class(chart())))
  notifId <- showNotification(
    "Rendering plot...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  chart()
},
  res=96, pointsize=18
)

# (ii) Rendering dataTables
output$dataTable <- renderDataTable({
  if(is.null(obsmonPlotObj())) return(NULL)
  notifId <- showNotification(
    "Rendering data table...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  obsmonPlotObj()$dataWithUnits
},
  options=list(scrollX=TRUE, scrollY="300px")
)
output$queryUsed <- renderText(obsmonPlotObj()$sqliteQuery)

output$dataTableDownloadAsTxt <- downloadHandler(
  filename = function() "plot_data.txt",
  content = function(file) req(obsmonPlotObj())$exportData(file, format="txt")
)
output$dataTableDownloadAsCsv <- downloadHandler(
  filename = function() "plot_data.csv",
  content = function(file) req(obsmonPlotObj())$exportData(file, format="csv")
)

# (iii) Rendering leaflet maps
output$map <- renderLeaflet({
  if(is.null(leafletMap())) return(NULL)
  notifId <- showNotification(
    "Rendering map...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  leafletMap()
})
output$mapTitle <- renderText(obsmonPlotObj()$title)

# Interactively update colorbar range in charts where this applies
output$mainAreaPlotEditingOptions <- renderUI({
  # TODO: Come up with a way to prevent users from selecting a range
  #       that leaves data out
  chart <- req(obsmonPlotObj()$chart)
  cmin <- Inf
  cmax <- -Inf
  for (dataProperty in chart$x$data) {
    cmin <- min(cmin, dataProperty$marker$cmin)
    cmax <- max(cmin, dataProperty$marker$cmax)
  }
  req(all(is.finite(c(cmin, cmax))))

  colorMapsDf <- RColorBrewer::brewer.pal.info
  colorMapChoices <- list()
  for(categ in unique(colorMapsDf$category)) {
    colorMapChoices[[categ]] <- rownames(subset(colorMapsDf, category==categ))
  }

  tags$div(
    pickerInput(
      "mainTabPlotColorscaleColorMap",
      label="Color Map",
      choices=colorMapChoices,
      multiple=TRUE,
      options=list(
        `max-options`=1,
        `none-selected-text`="Select color map",
        `live-search`=TRUE
      )
    ),
    numericRangeInput(
      "mainTabPlotColorscaleRange",
      label="Color Scale Range",
      value=as.numeric(format(c(cmin, cmax), digits=3))
    )
  )
})

observeEvent(input$mainTabPlotColorscaleRange,{
  cmin <- input$mainTabPlotColorscaleRange[1]
  cmax <- input$mainTabPlotColorscaleRange[2]

  plotlyProxy(outputId="plotly", session) %>%
    plotlyProxyInvoke(
      method="update",
      list(
        marker.cmin=cmin,
        marker.cmax=cmax
      )
    )
})

observeEvent(input$mainTabPlotColorscaleColorMap,{
  colorScaleName <- input$mainTabPlotColorscaleColorMap

  pallete <- brewer.pal(brewer.pal.info[colorScaleName,]$maxcolors, colorScaleName)
  pallete <- colorRampPalette(pallete)(25)
  palleteRgba <- sapply(pallete, plotly::toRGB, USE.NAMES=FALSE)

  colorMap <- t(mapply(c,
    seq(0, 1, length.out=length(palleteRgba)),
    palleteRgba
  ))

  plotlyProxy(outputId="plotly", session) %>%
    plotlyProxyInvoke(
      method="restyle",
      list(marker.colorscale=list(colorMap))
    )
})
