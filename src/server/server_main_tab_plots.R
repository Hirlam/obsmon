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
output$rawDataTable <- renderDataTable({
  if(is.null(obsmonPlotObj())) return(NULL)
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
  if(is.null(obsmonPlotObj())) return(NULL)
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
  if(is.null(leafletMap())) return(NULL)
  notifId <- showNotification(
    "Rendering map...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  leafletMap()
})
output$mapTitle <- renderText(obsmonPlotObj()$title)

# Interactively update colorbar range in charts where this applies
initialColorbarRange <- reactive({
  if(length(obsmonPlotObj()$userDataColormap)>0) {
    return(obsmonPlotObj()$userDataColormap$domain)
  }

  cmin <- Inf
  cmax <- -Inf
  for (dataProperty in obsmonPlotObj()$chart$x$data) {
    cmin <- min(cmin, dataProperty$marker$cmin)
    cmax <- max(cmin, dataProperty$marker$cmax)
  }
  return(as.numeric(format(c(cmin, cmax), digits=3)))
})

output$plotlyPlotEditingOptions <- renderUI({
  req(all(is.finite(initialColorbarRange())))

  colorMapsDf <- RColorBrewer::brewer.pal.info
  colorMapChoices <- list()
  for(categ in unique(colorMapsDf$category)) {
    colorMapChoices[[categ]] <- rownames(subset(colorMapsDf, category==categ))
  }

  shinyBS::bsCollapse(shinyBS::bsCollapsePanel(
    title=shiny::icon("cog"),
    value="Colour Options",
    pickerInput(
      "mainTabPlotColorscaleColorMap",
      label="Colour Map",
      choices=colorMapChoices,
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

observe({
  isolate(req(!("grid_i" %in% colnames(obsmonPlotObj()$data))))
  plotlyProxy(outputId="plotly", session) %>%
    plotlyProxyInvoke(
      method="update",
      list(
        marker.cmin=req(input$mainTabPlotColorscaleRange[1]),
        marker.cmax=req(input$mainTabPlotColorscaleRange[2])
      )
    )
})

observeEvent(input$mainTabPlotColorscaleColorMap, {
  req(!("grid_i" %in% colnames(obsmonPlotObj()$data)))
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

observeEvent(input$mainTabPlotColorscaleColorMap, {
  req("grid_i" %in% colnames(obsmonPlotObj()$data))
  colorScaleName <- input$mainTabPlotColorscaleColorMap

  pallete <- brewer.pal(brewer.pal.info[colorScaleName,]$maxcolors, colorScaleName)
  pallete <- colorRampPalette(pallete)(25)
  palleteRgba <- sapply(pallete, plotly::toRGB, USE.NAMES=FALSE)

  newColormap <- list(
    name=colorScaleName,
    palette=pallete,
    domain=input$mainTabPlotColorscaleRange
  )

  newObsmonPlotObj <- obsmonPlotObj()
  newObsmonPlotObj$userDataColormap <- newColormap
  obsmonPlotObj(NULL)
  obsmonPlotObj(newObsmonPlotObj)
})


griddedMapColorbarRange <- eventReactive(input$mainTabPlotColorscaleRange, {
  req("grid_i" %in% colnames(obsmonPlotObj()$data))
  req(all(is.finite(initialColorbarRange())))
  req(!isTRUE(all.equal(input$mainTabPlotColorscaleRange, obsmonPlotObj()$userDataColormap$domain)))
  req(!isTRUE(all.equal(input$mainTabPlotColorscaleRange, initialColorbarRange())))
  return(input$mainTabPlotColorscaleRange)
}) %>% debounce(500)
observeEvent(griddedMapColorbarRange(), {
  colorScaleName <- input$mainTabPlotColorscaleColorMap
  if(length(colorScaleName)==0) {
    newColormap <- obsmonPlotObj()$userDataColormap
    if(length(newColormap)==0) {
      newColormap <- .getSuitableColorScale(obsmonPlotObj()$data)
    }
  } else {
    pallete <- brewer.pal(brewer.pal.info[colorScaleName,]$maxcolors, colorScaleName)
    pallete <- colorRampPalette(pallete)(25)
    palleteRgba <- sapply(pallete, plotly::toRGB, USE.NAMES=FALSE)

    newColormap <- list(name=colorScaleName, palette=pallete)
  }
  newColormap$domain <- griddedMapColorbarRange()

  newObsmonPlotObj <- obsmonPlotObj()
  newObsmonPlotObj$userDataColormap <- newColormap
  obsmonPlotObj(NULL)
  obsmonPlotObj(newObsmonPlotObj)
})
