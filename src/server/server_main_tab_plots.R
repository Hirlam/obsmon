#########
# Plots #
#########
appendTab(inputId="mainAreaTabsetPanel", nonInteractivePlotTabPanel("plot"))
appendTab(inputId="mainAreaTabsetPanel", interactivePlotTabPanel("plotly"))
hideTab("mainAreaTabsetPanel", "plotlyTab") # Show only 1 plot tab at a time
appendTab(inputId="mainAreaTabsetPanel", leafletMapTabPanel())
hideTab("mainAreaTabsetPanel", "mapTab") # Show mapTab only when applicable
appendTab(inputId="mainAreaTabsetPanel", queryAndDataTabPanel())

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
chart <- reactiveVal()
leafletMap <- reactiveVal()
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
  chart(NULL)
  leafletMap(NULL)

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
        # Trigger data fetching & eventual post-processing
        # Calling fetchRawData is needed in order to pass the
        # progressFile arg. Then newPlot$data just triggers the
        # post-processing of the fetched data.
        newPlot$fetchRawData(...)
        invisible(newPlot$data)
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

# Modify the plot data, without performing a new query, if
# units change
updatePlotAfterUnitsChange <- reactive({
  variableUnits()
  levelsUnits()
}) %>% debounce(1000)
observeEvent(updatePlotAfterUnitsChange(), {
  req(obsmonPlotObj())
  newObsmonPlotObj <- obsmonPlotObj()
  obsmonPlotObj(NULL)

  newObsmonPlotObj$paramsAsInUiInput$levelsUnits <- levelsUnits()
  newObsmonPlotObj$paramsAsInUiInput$variableUnits <- variableUnits()
  obsmonPlotObj(newObsmonPlotObj)
}, ignoreNULL=FALSE)

# Modify the plot, without performing a new query, if
# user asks for levels to be grouped into standard ones
observeEvent(input$groupLevelsIntoStandardSwitch, {
  req(obsmonPlotObj())
  newObsmonPlotObj <- obsmonPlotObj()
  obsmonPlotObj(NULL)

  newObsmonPlotObj$paramsAsInUiInput$groupLevelsIntoStandardSwitch <-
    input$groupLevelsIntoStandardSwitch
  obsmonPlotObj(newObsmonPlotObj)
})

# Finally, producing the output
# Long computations may be required when producing the charts/leaflet plots.
# We'll produce these in an async manner before rendering, to keep the UI
# responsive.
observeEvent(obsmonPlotObj(), {
  req(obsmonPlotObj())

  chart(NULL)
  notifId <- showNotification(
    "Producing plot...", duration=NULL, type="message"
  )

  futureChart <- future(obsmonPlotObj()$chart, seed=TRUE)
  then(futureChart,
    onFulfilled=function(value) {
      chart(value)
    },
    onRejected=function(e) {
      showNotification("Could not create plot chart", duration=1, type="error")
      flog.error(e)
      chart(NULL)
    }
  )
  futureChartCleanup <- finally(futureChart, function() {
    removeNotification(notifId)
  })

  # This NULL is necessary in order to prevent the future from blocking
  NULL
})

observeEvent(obsmonPlotObj(), {
  req(obsmonPlotObj())

  leafletMap(NULL)
  shouldProduceLeafletMap <- (
    "maps" %in% tolower(obsmonPlotObj()$parentType$category) ||
    class(obsmonPlotObj()$parentType$leafletPlottingFunction) != "uninitializedField"
  )
  req(shouldProduceLeafletMap)

  notifId <- showNotification(
    "Producing leaflet map...", duration=NULL, type="message"
  )

  futureLeafletMap <- future(obsmonPlotObj()$leafletMap, seed=TRUE)
  then(futureLeafletMap,
    onFulfilled=function(value) {
      leafletMap(value)
    },
    onRejected=function(e) {
      showNotification("Could not create leaflet map", duration=1, type="error")
      flog.error(e)
      leafletMap(NULL)
    }
  )
  futureLeafletMapCleanup <- finally(futureLeafletMap, function() {
    removeNotification(notifId)
  })

  # This NULL is necessary in order to prevent the future from blocking
  NULL
})

# Enable/disable, show/hide appropriate inputs
observe({
  req(obsmonPlotObj())
  # (i) Maps tab
  if(is.null(leafletMap())) {
    if(isTRUE(input$mainAreaTabsetPanel=="mapTab")) {
      updateTabsetPanel(session, "mainAreaTabsetPanel", "plotlyTab")
    }
    hideTab("mainAreaTabsetPanel", "mapTab")
  } else {
    showTab("mainAreaTabsetPanel", "mapTab")
  }

  # (ii) Interactive or regular plot tabs
  interactive <- isTRUE("plotly" %in% class(chart()))
  if(interactive) {
    hideTab("mainAreaTabsetPanel", "plotTab")
    showTab("mainAreaTabsetPanel", "plotlyTab")
  } else {
    hideTab("mainAreaTabsetPanel", "plotlyTab")
    showTab("mainAreaTabsetPanel", "plotTab")
  }

  if(isTRUE(interactive && input$mainAreaTabsetPanel=="plotTab")) {
    updateTabsetPanel(session, "mainAreaTabsetPanel", "plotlyTab")
  } else if(!interactive && input$mainAreaTabsetPanel=="plotlyTab") {
    updateTabsetPanel(session, "mainAreaTabsetPanel", "plotTab")
  }
})

# Rendering plot/map/dataTable
# (i) Rendering plots
# (i.i) Interactive plot, if plot is a plotly object
output$plotly <- renderPlotly({
  req("plotly" %in% class(req(chart())))
  notifId <- showNotification(
    "Rendering plot...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  chart()
})
# (i.ii) Non-interactive plot, if plot is not a plotly object
output$plot <- renderPlot({
  req(!("plotly" %in% class(req(chart()))))
  notifId <- showNotification(
    "Rendering plot...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  chart()
},
  res=96, pointsize=18
)

# (ii) Rendering dataTables
output$rawDataTable <- renderDataTable({
  req(obsmonPlotObj())
  notifId <- showNotification(
    "Rendering data table...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  obsmonPlotObj()$rawData
},
  options=list(scrollX=TRUE, scrollY="300px")
)
output$queryUsed <- renderText(obsmonPlotObj()$sqliteQuery)

output$rawDataTableDownloadAsTxt <- downloadHandler(
  filename = function() "raw_data.txt",
  content = function(file) req(obsmonPlotObj())$exportData(file, format="txt", raw=TRUE)
)
output$rawDataTableDownloadAsCsv <- downloadHandler(
  filename = function() "raw_data.csv",
  content = function(file) req(obsmonPlotObj())$exportData(file, format="csv", raw=TRUE)
)

output$plotDataTable <- renderDataTable({
  req(obsmonPlotObj())
  notifId <- showNotification(
    "Rendering data table...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  obsmonPlotObj()$dataWithUnits
},
  options=list(scrollX=TRUE, scrollY="300px")
)
output$plotDataTableDownloadAsTxt <- downloadHandler(
  filename = function() "plot_data.txt",
  content = function(file) req(obsmonPlotObj())$exportData(file, format="txt")
)
output$plotDataTableDownloadAsCsv <- downloadHandler(
  filename = function() "plot_data.csv",
  content = function(file) req(obsmonPlotObj())$exportData(file, format="csv")
)

# (iii) Rendering leaflet maps
output$map <- renderLeaflet({
  req(leafletMap())
  notifId <- showNotification(
    "Rendering map...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  leafletMap()
})
output$mapTitle <- renderText(obsmonPlotObj()$title)

# Interactively update colorbar range in charts where this applies
initialColorbarRange <- reactive({
  cmin <- Inf
  cmax <- -Inf
  for (dataProperty in req(chart()$x$data)) {
    cmin <- min(cmin, dataProperty$marker$cmin)
    cmax <- max(cmin, dataProperty$marker$cmax)
  }
  return(c(floor(cmin), ceiling(cmax)))
})

colorscaleInputsJustInitialised <- reactiveVal()
output$plotlyPlotEditingOptions <- renderUI({
  req(all(is.finite(initialColorbarRange())))

  colorMapsDf <- RColorBrewer::brewer.pal.info
  colorMapChoices <- list()
  for(categ in unique(colorMapsDf$category)) {
    colorMapChoices[[categ]] <- rownames(subset(colorMapsDf, category==categ))
  }

  initialColormap <- .getSuitableColorScale(obsmonPlotObj()$data)
  initialColormapName <- intersect(initialColormap$name, unlist(colorMapChoices))

  colorscaleInputsJustInitialised(TRUE)
  shinyBS::bsCollapse(shinyBS::bsCollapsePanel(
    title=shiny::icon("cog"),
    value="Colour Options",
    pickerInput(
      "mainTabPlotColorscaleColorMap",
      label="Colour Map",
      choices=colorMapChoices,
      selected=initialColormapName,
      multiple=TRUE,
      options=list(
        `max-options`=1,
        `none-selected-text`="Select colour map",
        `live-search`=TRUE
      )
    ),
    numericRangeInput(
      "mainTabPlotColorscaleRange",
      label="Colour Scale Range",
      value=initialColorbarRange()
    )
  ))
})

triggerColorscaleUpdate <- eventReactive({
  input$mainTabPlotColorscaleColorMap
  input$mainTabPlotColorscaleRange
}, {
  if(isTRUE(colorscaleInputsJustInitialised())) {
    colorscaleInputsJustInitialised(FALSE)
    return(NULL)
  }
  return(TRUE)
})

newColorscale <- eventReactive(triggerColorscaleUpdate(), {
  colorScaleName <- req(input$mainTabPlotColorscaleColorMap)
  palette <- brewer.pal(brewer.pal.info[colorScaleName,]$maxcolors, colorScaleName)
  return(list(
    name=colorScaleName,
    palette=colorRampPalette(palette)(25),
    domain=req(input$mainTabPlotColorscaleRange)
  ))
})

observeEvent(newColorscale(), {
  palette <- newColorscale()$palette
  colorMap <- t(mapply(c, seq(0, 1, length.out=length(palette)), palette))
  plotlyProxy(outputId="plotly", session) %>%
    plotlyProxyInvoke(
      method="update",
      list(
        marker.cmin=req(newColorscale()$domain[1]),
        marker.cmax=req(newColorscale()$domain[2])
      )
    ) %>%
    plotlyProxyInvoke(
      method="restyle",
      list(marker.colorscale=list(colorMap))
    )
})

observeEvent(newColorscale(), {
  req("grid_i" %in% colnames(obsmonPlotObj()$data))

  dataPal <- colorNumeric(palette=newColorscale()$palette, domain=newColorscale()$domain)
  indices <- list()
  values <- list()
  for(iData in seq_along(chart()$x$data)) {
    dataEntry <- tryCatch(chart()$x$data[[iData]], error=function(e) NULL)
    if(!isTRUE(dataEntry$type == "scattergeo")) next
    value <- as.numeric(dataEntry$customdata[[1]])
    if(length(value) == 0) next
    values <- append(values, dataPal(value))
    indices <- append(indices, iData-1)
  }

  plotlyProxy(outputId="plotly", session) %>%
    plotlyProxyInvoke(
      method="restyle",
      list(fillcolor=values),
      indices
    )
})
