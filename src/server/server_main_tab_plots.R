#########
# Plots #
#########
appendTab(inputId="mainAreaTabsetPanel", nonInteractivePlotTabPanel("plot"))
appendTab(inputId="mainAreaTabsetPanel", interactivePlotTabPanel("plotly"))
hideTab("mainAreaTabsetPanel", "plotlyTab") # Show only 1 plot tab at a time
appendTab(inputId="mainAreaTabsetPanel", leafletMapTabPanel())
hideTab("mainAreaTabsetPanel", "mapTab") # Show mapTab only when applicable
appendTab(inputId="mainAreaTabsetPanel", queryAndDataTabPanel())

##################################
# Logic for "Cancel Plot" button #
##################################
currentPlotPid <- reactiveVal(-1)
observeEvent(currentPlotPid(), {
  # Prevent another plot from being requested which another is
  # on course, and offer possibility to cancel plot
  thereIsAPlotInPreparation <- isTRUE(currentPlotPid() >= 0)

  shinyjs::toggle("doPlot", condition=!thereIsAPlotInPreparation)
  shinyjs::toggle("cancelPlot", condition=thereIsAPlotInPreparation)

  if(thereIsAPlotInPreparation) {
    disableShinyInputs(input, except=c("^multiPlots*", "^cancelPlot$"))
    plotInterrupted(FALSE)
  } else {
    enableShinyInputs(input, except="^multiPlots*")
  }
})

plotInterrupted <- reactiveVal()
observeEvent(input$cancelPlot, {
  showNotification("Cancelling plot", type="warning", duration=1)
  plotInterrupted(TRUE)
  killProcessTree(currentPlotPid(), warnFail=TRUE)
}, priority=2000, ignoreInit=TRUE)


###################################################
# Management of "fetching plot data" progress bar #
###################################################
plotProgressFile <- reactiveVal(NULL)
plotProgressStatus <- reactiveVal(function() NULL)
plotProgressBar <- reactiveVal(NULL)
observeEvent(plotProgressFile(), {
  plotProgressStatus(reactiveFileReader(
    500, session, isolate(plotProgressFile()), readPlotProgressFile
  ))
})
plotStartedNotifId <- reactiveVal(-1)
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


###############################################################################
# Fetch plot data when user clicks in "Plot". Then update the obsmonPlotObj   #
# (using replaceObsmonPlotObj) to trigger data post-processing and production #
# of figures to the next observer (which is performed in another observer)    #
###############################################################################
replaceObsmonPlotObj <- reactiveVal(NA)
observeEvent(input$doPlot, {
  # Make sure a plot cannot be requested if another is being produced.
  if(currentPlotPid() > -1) {
    showNotification(
      "Another plot is being produced. Please wait.",
      type="warning", duration=1
    )
  }
  req(currentPlotPid() == -1)

  # Erase any plot currently on display
  replaceObsmonPlotObj(NULL)

  if(activePlotType()$requiresSingleStation && length(input$station) !=1) {
    showNotification(
      "This plot requires choosing one station!",
      type="error", duration=2
    )
    replaceObsmonPlotObj(NA)
    return(NULL)
  }

  # All checks performed: We can now proceed with the plot request
  plotStartedNotifId(showNotification(
    "Processing plot request...", type="message", duration=NULL
  ))

  # Trigger creation of progress bar
  plotProgressFile(tempfile(pattern="plotProgress"))

  # Fetch raw data asyncronously so that the app ramins responsive
  asyncNewPlotAndOutput <- futureCall(
    FUN=function(parentType, db, paramsAsInUiInput, modelDomain, ...) {
      output <- capture.output({
        newPlot <- obsmonPlotClass$new(
          parentType=parentType,
          db=db,
          modelDomain=modelDomain,
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
      progressFile=plotProgressFile(),
      modelDomain=sessionDomain()
    )
  )
  currentPlotPid(asyncNewPlotAndOutput$job$pid)

  then(asyncNewPlotAndOutput,
    onFulfilled=function(value) {
      replaceObsmonPlotObj(value$newPlot)
    },
    onRejected=function(e) {
      replaceObsmonPlotObj(NA)
      if(!plotInterrupted()) {
        showNotification("Could not fetch plot data", duration=1, type="error")
        flog.error(e)
      }
    }
  )
  plotCleanup <- finally(asyncNewPlotAndOutput, function() {
    # Force-kill eventual lingering forked processes and reset pid
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

    # Printing output produced during async plot, if any
    producedOutput <- value(asyncNewPlotAndOutput)$output
    if(length(producedOutput)>0) message(paste0(producedOutput, "\n"))
  })
  catch(plotCleanup, function(e) {if(!plotInterrupted()) flog.error(e)})
  # This NULL is necessary in order to prevent the future from blocking
  NULL
}, priority=2000)


###############################################################################
# Observe replaceObsmonPlotObj and update the obsmonPlotObj reactiveVal when  #
# needed. obsmonPlotObj holds the latest used obsmonPlot object. Keeping this #
# in a variable enables us to modify the plot without making news queries if  #
# the modifications don't change the plot's raw data. This is handy whenever  #
# users request things like changes in color schemes, units, domain, etc.     #
###############################################################################
obsmonPlotObj <- reactiveVal(NA)
obsmonPlotObjID <- eventReactive(obsmonPlotObj(), {
  if(is(obsmonPlotObj(), "obsmonPlot")) obsmonPlotObj()$hash
  else if(is.null(obsmonPlotObj())) "WAITING"
  else "INVALID"
}, ignoreNULL=FALSE)

chart <- eventReactive(obsmonPlotObjID(), {
  if(obsmonPlotObjID() == "WAITING") return(SPINNER_CHART)
  else if(obsmonPlotObjID() == "INVALID") return(EMPTY_CHART)
  return(obsmonPlotObj()$chart)
}, ignoreNULL=FALSE)
observeEvent(replaceObsmonPlotObj(), {
  plotInterrupted(FALSE)

  newPlot <- replaceObsmonPlotObj()

  newValueIsObsmonPlotObj <- is(newPlot, "obsmonPlot")
  # The NULL value signals that we're waiting for a new plot to be computed,
  # which will be performed in the futureCall below
  if(newValueIsObsmonPlotObj || is.null(newPlot)) obsmonPlotObj(NULL)
  else obsmonPlotObj(newPlot)

  req(newValueIsObsmonPlotObj)
  notifId <- showNotification("Producing plot...", duration=NULL, type="message")

  # Process data and gen plots asyncronously so that the app ramins responsive
  # Using an environment as a trick to pass newPlot by reference to the
  # function used in the futureCall. If we don't do this, and pass newPlot to
  # the function instead, then the computations triggered by newPlot$data are
  # initiated not inside future call, but rather at the time newPlot is copied
  # to be passed to the function (as R passes by value). See, for instance,
  # https://stat.ethz.ch/pipermail/r-devel/2009-January/051899.html
  env <- new.env(parent=emptyenv())
  env$newObsmonPlotObj <- newPlot

  futureNewObsmonPlotObj <- futureCall(FUN=function(env) {
    newObsmonPlotObj <- env$newObsmonPlotObj
    # Long computations may be required when producing the charts/leaflet plots.
    # We'll produce these in an async manner before rendering, to keep the UI
    # responsive.
    # Trigger processing of data, chart and leafletMap
    invisible(newObsmonPlotObj$chart)
    invisible(newObsmonPlotObj$leafletMap)
    return(newObsmonPlotObj)
  },
    args=list(env=env)
  )
  currentPlotPid(futureNewObsmonPlotObj$job$pid)

  then(futureNewObsmonPlotObj,
    onFulfilled=function(value) {obsmonPlotObj(value)},
    onRejected=function(e) {
      obsmonPlotObj(NA)
      if(!plotInterrupted()) {
        showNotification("Could not produce plot", duration=1, type="error")
        flog.error(e)
      }
    }
  )
  futureNewObsmonPlotObjCleanup <- finally(futureNewObsmonPlotObj, function() {
    removeNotification(notifId)
    # Force-kill eventual zombie forked processes and reset pid
    killProcessTree(currentPlotPid())
    currentPlotPid(-1)
  })
  catch(futureNewObsmonPlotObjCleanup, function(e) {
    if(!plotInterrupted()) flog.error(e)
  })

  # This NULL is necessary in order to prevent the future from blocking
  NULL
}, ignoreNULL=FALSE)


#########################################################
# Modifying the current plot without making a new query #
#########################################################
# If units are changed
updatePlotAfterUnitsChange <- reactive({
  variableUnits()
  levelsUnits()
}) %>% debounce(1000)
observeEvent(updatePlotAfterUnitsChange(), {
  newObsmonPlotObj <- req(obsmonPlotObj())
  newObsmonPlotObj$paramsAsInUiInput$levelsUnits <- levelsUnits()
  newObsmonPlotObj$paramsAsInUiInput$variableUnits <- variableUnits()
  replaceObsmonPlotObj(newObsmonPlotObj)
}, ignoreNULL=FALSE)

# If sessionDomain is changed
observeEvent(sessionDomain(), {
  newObsmonPlotObj <- req(obsmonPlotObj())
  req(grepl("maps", tolower(newObsmonPlotObj$parentType$category)))
  newObsmonPlotObj$modelDomain <- sessionDomain()
  replaceObsmonPlotObj(newObsmonPlotObj)
})

# If user changes min number of obs for grid-averaged data
newMinNObsGriddedAvgs <- eventReactive(input$minNobsForGriddedAverages, {
  req(input$minNobsForGriddedAverages > 0)
  req(grepl("average maps", tolower(req(obsmonPlotObj())$parentType$category)))
  return(input$minNobsForGriddedAverages)
}) %>% debounce(1000)
observeEvent(newMinNObsGriddedAvgs(), {
  newObsmonPlotObj <- req(obsmonPlotObj())
  newObsmonPlotObj$paramsAsInUiInput$minNobsForGriddedAverages <-
    newMinNObsGriddedAvgs()
  replaceObsmonPlotObj(newObsmonPlotObj)
})

# If user asks for levels to be grouped into standard ones
observeEvent(input$groupLevelsIntoStandardSwitch, {
  newObsmonPlotObj <- req(obsmonPlotObj())
  newObsmonPlotObj$paramsAsInUiInput$groupLevelsIntoStandardSwitch <-
    input$groupLevelsIntoStandardSwitch
  replaceObsmonPlotObj(newObsmonPlotObj)
})

# If user requests updates in colorbar (when applicable)
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
      ),
      inline=TRUE
    ),
    materialSwitch(
      inputId='reverseColorscaleSwitch',
      label="Reverse",
      status="info",
      inline=TRUE,
      right=TRUE,
      value=TRUE
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
  input$reverseColorscaleSwitch
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
  palette <- colorRampPalette(palette)(25)
  if(isTRUE(input$reverseColorscaleSwitch)) palette <- rev(palette)

  return(list(
    name=colorScaleName,
    palette=palette,
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

###########################################################################
# Enable/disable, show/hide appropriate outputs depending on type of plot #
###########################################################################
observe({
  isAvgMap <- isTRUE(grepl("average maps", tolower(activePlotType()$category)))
  shinyjs::toggle(
    "minNobsForGriddedAverages",
    condition=isAvgMap && sessionDomain()$grid$hasPoints
  )
})

# Maps tab
observe({
  hasLeaflet <- any(grepl("^leaflet$", class(req(obsmonPlotObj())$leafletMap)))
  if(isTRUE(input$mainAreaTabsetPanel=="mapTab") && !hasLeaflet) {
    updateTabsetPanel(session, "mainAreaTabsetPanel", "plotlyTab")
  }
  toggleTab("mainAreaTabsetPanel", "mapTab", condition=hasLeaflet)
})

# Interactive or regular plot tabs
observe({
  interactive <- isTRUE("plotly" %in% class(chart()))

  if(isTRUE(interactive && input$mainAreaTabsetPanel=="plotTab")) {
    updateTabsetPanel(session, "mainAreaTabsetPanel", "plotlyTab")
  } else if(isTRUE(!interactive && input$mainAreaTabsetPanel=="plotlyTab")) {
    updateTabsetPanel(session, "mainAreaTabsetPanel", "plotTab")
  }

  toggleTab("mainAreaTabsetPanel", "plotlyTab", condition=interactive)
  toggleTab("mainAreaTabsetPanel", "plotTab", condition=!interactive)
})

#################################
# Finally, rendering the output #
#################################
# (i) Rendering plots
# (i.i) Interactive plot, if plot is a plotly object
output$plotly <- renderPlotly({
  req("plotly" %in% class(chart()))
  chart()
}) %>% bindCache(obsmonPlotObjID())
# (i.ii) Non-interactive plot, if plot is not a plotly object
output$plot <- renderPlot({
  req(!("plotly" %in% class(req(chart()))))
  chart()
},
  res=96, pointsize=18
) %>% bindCache(obsmonPlotObjID())

# (ii) Rendering dataTables
output$rawDataTable <- renderDataTable({
  req(obsmonPlotObj())$rawData
},
  options=list(scrollX=TRUE, scrollY="300px")
)
output$queryUsed <- renderText(req(obsmonPlotObj())$sqliteQuery)

output$rawDataTableDownloadAsTxt <- downloadHandler(
  filename = function() "raw_data.txt",
  content = function(file) req(obsmonPlotObj())$exportData(file, format="txt", raw=TRUE)
)
output$rawDataTableDownloadAsCsv <- downloadHandler(
  filename = function() "raw_data.csv",
  content = function(file) req(obsmonPlotObj())$exportData(file, format="csv", raw=TRUE)
)

output$plotDataTable <- renderDataTable({
  req(obsmonPlotObj())$dataWithUnits
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
  req(req(obsmonPlotObj())$leafletMap)
}) %>% bindCache(obsmonPlotObjID())
output$mapTitle <- renderText(req(obsmonPlotObj())$title)
