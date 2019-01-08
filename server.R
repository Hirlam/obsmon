if(!exists("initFileSourced")) source("init.R")

clamp <- function(value, min, max, default=max) {
  if (is.null(value)) {
    default
  } else if (value < min) {
    min
  } else if (value > max) {
    max
  } else {
    value
  }
}

signalError <- function(message, title="Error") {
  showModal(modalDialog(
      title=title,
      message,
      easyClose=TRUE
  ))
}

getSelection <- function(session, inputId, choices, select=c("NORMAL", "ALL", "NONE")) {
  select <- match.arg(select)
  switch(select,
         "NORMAL"={
           oldSelection <- isolate(session$input[[inputId]])
           validChoices <- unlist(choices, use.names=FALSE)
           validSelections <- oldSelection %in% validChoices
           if (is.null(oldSelection)) {
             choices
           } else if (any(validSelections)) {
             oldSelection[validSelections]
           } else {
             NULL
           }
         },
         "ALL"={
           choices
         },
         "NONE"={
           c()
         })
}

# Updates choices for selection
#
# Updates a selectInput, preserving the selected
# option(s) if available
updateSelection <- function(session, inputId, choices, select="NORMAL") {
    if (is.null(choices)) {
      return(NULL)
    }
    selection <- getSelection(session, inputId, choices, select)
    updateSelectInput(session, inputId,
                      choices=choices, selected=selection)
}

updateCheckboxGroup <- function(session, inputId, choices, select="NORMAL") {
    if (is.null(choices)) {
      return(NULL)
    }
    selection <- getSelection(session, inputId, choices, select)
    updateCheckboxGroupInput(session, inputId,
                             choices=choices, selected=selection, inline=TRUE)
}

disableShinyInputs <- function(input, except=c()) {
  allInputs <- names(input)
  if(is.null(allInputs)) allInputs <- input
  inputsToDisable <- allInputs[!(allInputs %in% except)]
  for(inp in inputsToDisable) shinyjs::disable(inp)
}
enableShinyInputs <- function(input, except=c()) {
  allInputs <- names(input)
  if(is.null(allInputs)) allInputs <- input
  inputsToEnable <- allInputs[!(allInputs %in% except)]
  for(inp in inputsToEnable) shinyjs::enable(inp)
}

getReqDateType <- function(input) {
  rtn <- tryCatch(
    plotTypesFlat[[req(input$plottype)]]$dateType,
    error=function(e) NA,
    warning=function(w) NA
  )
  return(rtn)
}

getCurrentDatesAndCycles <- function(input) {
  dateType <- getReqDateType(input)
  if(dateType %in% c("range")) {
    dates <- expandDateRange(input$dateRange[[1]], input$dateRange[[2]])
    cycles <- input$cycles
  } else {
    dates <- strftime(input$date, format="%Y%m%d")
    cycles <- input$cycle
  }
  return(list(dates=dates, cycles=cycles))
}

cacheFilesLatestMdate <- function(db) {
  mtimes <- c(-1)
  for(cacheFilePath in db$cachePaths) {
    mtime <- tryCatch(
      file.mtime(cacheFilePath),
      error=function(e) NULL,
      warning=function(w) NULL
    )
    mtimes <- c(mtimes, mtime)
  }
  return(max(mtimes))
}

getSelectedDtgs <- function(input) {
  datesCycles <- getCurrentDatesAndCycles(input)
  dates <- sort(datesCycles$dates, decreasing=TRUE)
  cycles <- sort(datesCycles$cycles, decreasing=FALSE)
  if(is.null(dates) || is.na(dates)) return(NULL)
  if(all(cycles=="")) return(NULL)

  dtgs <- c()
  for(date in dates) {
    for(cycle in cycles) {
      dtgs <- c(dtgs, sprintf("%s%s", date, cycle))
    }
  }

  return(dtgs)
}

getFilePathsToCache <- function(db, input) {
  dtgs <- getSelectedDtgs(input)
  fPathsToCache <- db$paths[dtgs]
  fPathsToCache <- fPathsToCache[!is.na(fPathsToCache)]

  if(length(fPathsToCache)==0) fPathsToCache <- NULL
  return(fPathsToCache)
}

shinyServer(function(input, output, session) {
  # Start GUI with all inputs disabled.
  # They will be enabled once experiments are loaded
  isolate(disableShinyInputs(input, except="experiment"))

  # Initial population of experiments; triggers cascade for other form fields
  exptNames <- c("")
  experiments <- reactive ({
    # Keep checking if experiments finished initialisation
    invalidateLater(5000, session)
    flagNotReadyExpts(experimentsAsPromises)
  })
  observe({
      newExptNames <- names(experiments())
      if(length(newExptNames)==0) {
        exptPlaceholder <- "ERROR: Could not read experiment data"
        disableShinyInputs(input)
      } else {
        exptPlaceholder <- "Please select experiment"
        shinyjs::enable("experiment")
      }
      if((length(newExptNames) != length(exptNames)) ||
         !all(exptNames==newExptNames)) {
        selectedExpt <- tryCatch({
          iExpt <- which(exptNames==input$experiment)[1]
          if(is.na(iExpt)) NULL else newExptNames[iExpt]
          },
          error=function(e) NULL
        )
        updateSelectizeInput(session, "experiment",
          choices=newExptNames, selected=selectedExpt,
          options=list(placeholder=exptPlaceholder)
        )
        exptNames <<- newExptNames
      }
  })
  shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")
  shinyjs::show("app-content")

  # Update database options according to chosen experiment
  observe({
    expName <- req(input$experiment)
    expDbs <- isolate(experiments()[[expName]]$dbs)
    dbType2DbDescription <- list(
      "ecma"="Upper Air (3D/4D-VAR) - Screening",
      "ccma"="Upper Air (3D/4D-VAR) - Minimization",
      "ecma_sfc"="Surface (CANARI)"
    )
    choices <- list()
    for(dbType in names(dbType2DbDescription)) {
      if(is.null(expDbs[[dbType]])) next
      choices[[dbType2DbDescription[[dbType]]]] <- dbType
    }

    updateSelectizeInput(session, "odbBase", choices=choices)
    if(length(choices)==0) disableShinyInputs(input, except=c("experiment"))
    else enableShinyInputs(input)
  })

  activeDb <- reactive({
    expName <- req(input$experiment)
    dbType <- req(input$odbBase)
    isolate(experiments()[[expName]]$dbs[[dbType]])
  })

  # Update date related fields dateRange, date, and cycle with new experiment
  observeEvent(activeDb(), {
    db <- req(activeDb())
    min <- db$maxDateRange[1]
    max <- db$maxDateRange[2]
    start <- clamp(input$dateRange[1], min, max, min)
    end <- clamp(input$dateRange[2], min, max)
    single <- clamp(input$date, min, max)
    updateDateRangeInput(session, "dateRange",
                         start = start, end = end,
                         min = db$maxDateRange[1], max = db$maxDateRange[2])
    updateDateInput(session, "date", value = single,
                    min = db$maxDateRange[1], max = db$maxDateRange[2])
  })

  # React when required dateType of plot changes value
  dateTypeReqByPlotType <- reactiveVal(character(0))
  observe({
    invalidateLater(500)
    dateTypeReqByPlotType(getReqDateType(input))
  })
  # Offer single date or dateRange input according to selected plottype
  output$dateType <- reactive({dateTypeReqByPlotType()})
  outputOptions(output, 'dateType', suspendWhenHidden=FALSE)

  # Update available cycle choices when relevant fields change
  availableCycles <- eventReactive({
      activeDb()
      input$date
      input$dateRange
      dateTypeReqByPlotType()
    }, {
      db <- req(activeDb())
      datesCycles <- getCurrentDatesAndCycles(req(input))
      dates <- as.character(datesCycles$dates)
      getAvailableCycles(db, dates)
  })

  observe({
    cycles <- availableCycles()
    updateSelection(session, "cycle", cycles)
    updateCheckboxGroup(session, "cycles", cycles)
  })

  observeEvent(input$cyclesSelectAll, {
    cycles <- isolate(availableCycles())
    updateCheckboxGroupInput(session, "cycles",
      choices=cycles, selected=cycles, inline=TRUE
    )
  })

  observeEvent(input$cyclesSelectNone, {
    cycles <- isolate(availableCycles())
    updateCheckboxGroupInput(session, "cycles",
      choices=cycles, selected=character(0), inline=TRUE
    )
  })

  # Keep track of selected DTGs
  selectedDtgs <- eventReactive({
      activeDb()
      input$date
      input$dateRange
      input$cycle
      input$cycles
      dateTypeReqByPlotType()
    }, {
      getSelectedDtgs(req(input))
  })

  # Put observations in cache when dB and/or DTG selection are modified
  observeEvent({
      activeDb()
      selectedDtgs()
    }, {
      db <- req(activeDb())
      dtgs <- req(selectedDtgs())
      fPathsToCache <- getFilePathsToCache(db, input)
      assyncPutObsInCache(fPathsToCache, cacheDir=db$cacheDir)
  })
  # Re-cache observations if requested by user
  observeEvent({
      input$recacheCacheButton
    }, {
      db <- req(activeDb())
      fPathsToCache <- getFilePathsToCache(db, input)
      assyncPutObsInCache(fPathsToCache, cacheDir=db$cacheDir, replaceExisting=TRUE)
  },
    ignoreInit=TRUE
  )

  # Detect when the relevant cache files have been updated
  cacheFileUpdated <- function() NULL
  observe({
    db <- req(activeDb())
    cacheFileUpdated <<- reactivePoll(5000, session,
      partial(cacheFilesLatestMdate, db=db), function() NULL)
  })

  # Flagging that it's time to read info from cache
  reloadInfoFromCache <- reactiveVal(0)
  observeEvent({
      input$reloadCacheButton
      activeDb()
      cacheFileUpdated()
      selectedDtgs()
    }, {
      reloadInfoFromCache((reloadInfoFromCache() + 1) %% 2)
  },
    ignoreNULL=TRUE
  )

  # Keep track of whether selected DTGs are cached or not
  selectedDtgsAreCached <- reactiveVal(FALSE)
  observeEvent({
      reloadInfoFromCache()
    }, {
    db <- req(activeDb())
    dtgs <- req(selectedDtgs())
    selectedDtgsAreCached(dtgsAreCached(db, dtgs))
  })

  # Update obtype
  observeEvent({
      reloadInfoFromCache()
      selectedDtgsAreCached()
    }, {
    db <- req(activeDb())
    if(db$dbType=="ecma_sfc") {
      updateSelection(session, "obtype", c("surface"))
    } else {
      dtgs <- req(selectedDtgs())
      datesCycles <- getCurrentDatesAndCycles(isolate(input))
      obtypes <- getObtypes(db, datesCycles$dates, datesCycles$cycles)

      if(is.null(obtypes$cached) || !selectedDtgsAreCached()) {
        updateSelection(session, "obtype", obtypes$general)
        updateSelectInput(session, "obtype", label="Observation Type (cache info not available)")
      } else {
        updateSelection(session, "obtype", obtypes$cached)
        updateSelectInput(session, "obtype", label="Observation Type")
      }
    }
  })

  # Update obnames
  observeEvent({
      reloadInfoFromCache()
      selectedDtgsAreCached()
      input$obtype
    }, {
    req(input$obtype!="satem")
    obsCategory <- req(input$obtype)
    db <- req(activeDb())
    dtgs <- req(selectedDtgs())
    datesCycles <- getCurrentDatesAndCycles(isolate(input))

    obnames <- getObnames(db, obsCategory, datesCycles$dates, datesCycles$cycles)
    if(is.null(obnames$cached) || !selectedDtgsAreCached()) {
      updateSelection(session, "obname", obnames$general)
      updateSelectInput(session, "obname", label="Observation Name (cache info not available)")
    } else {
      updateSelection(session, "obname", obnames$cached)
      updateSelectInput(session, "obname", label="Observation Name")
    }
  })

  # Update variable
  observeEvent({
      reloadInfoFromCache()
      selectedDtgsAreCached()
      input$obtype
      input$obname
    }, {
    req(input$obtype!="satem")

    db <- req(activeDb())
    obname <- req(input$obname)
    dtgs <- req(selectedDtgs())
    datesCycles <- getCurrentDatesAndCycles(isolate(input))

    variables <- getVariables(db,datesCycles$dates,datesCycles$cycles,obname)
    if(is.null(variables$cached) || !selectedDtgsAreCached()) {
      updateSelection(session, "variable", variables$general)
      updateSelectInput(session, "variable", label="Variable (cache info not available)")
    } else {
      updateSelection(session, "variable", variables$cached)
      updateSelectInput(session, "variable", label="Variable")
    }
  })

  # Update stations
  stationsAlongWithLabels <- eventReactive({
      reloadInfoFromCache()
      selectedDtgsAreCached()
      input$obtype
      input$obname
      input$variable
    }, {
    req(input$obtype!="satem")

    db <- req(activeDb())
    obname <- req(input$obname)
    variable <- req(input$variable)
    dtgs <- req(selectedDtgs())
    datesCycles <- getCurrentDatesAndCycles(isolate(input))

    stations <- getStationsFromCache(
      db, datesCycles$dates, datesCycles$cycles,
      obname, variable
    )
    if(length(stations)>0) {
      if(obname=="synop") {
        stationLabels <- c()
        for(statID in stations) {
          label <- synopStations[statID]
          if(is.null(label) || is.na(label)) label <- statID
          stationLabels <- c(stationLabels, label)
        }
        names(stations) <- stationLabels
      } else {
        names(stations) <- stations
      }
    }
    stations <- c("Any"="", stations)
    stations
  })
  observeEvent({
      stationsAlongWithLabels()
    }, {
      stations <- stationsAlongWithLabels()
      if(length(stations)==1 || !selectedDtgsAreCached()) {
        updateSelectInput(session, "station", label="Station (cache info not available)",
          choices=stations, selected=stations
        )
      } else {
        updateSelectInput(session, "station", label="Station")
        updateSelection(session, "station", stations)
      }
  })

  # Update level choice for given variable
  avLevels <- list(obsmon=NULL, usage=NULL, all=NULL)
  observeEvent({
    reloadInfoFromCache()
    input$obtype
    input$obname
    input$variable
    }, {
    req(input$obtype!="satem")

    db <- req(activeDb())
    obname <- req(input$obname)
    var <- req(input$variable)
    datesCycles <- getCurrentDatesAndCycles(input)

    avLevels <<- getAvailableLevels(db, datesCycles$dates, datesCycles$cycles, obname, var)
    if(is.null(avLevels$all)) {
      updateSelectInput(session, "levels", choices=list(), selected=list())
    } else {
      updateSelection(session, "levels", choices=avLevels$all)
    }
  })

  observeEvent(input$levelsSelectStandard, {
    updateSelectInput(session, "levels",
      choices=avLevels$all, selected=avLevels$obsmon)
  })

  observeEvent(input$levelsSelectAll, {
    updateSelectInput(session, "levels", choices=avLevels$all, selected=avLevels$all)
  })

  observeEvent(input$levelsSelectNone, {
    updateSelectInput(session, "levels", choices=avLevels$all, selected=c())
  })

  # Update sensornames
  observeEvent({
      reloadInfoFromCache()
      selectedDtgsAreCached()
      input$obtype
    }, {
    req(input$obtype=="satem")
    updateSelection(session, "obname", c("satem"))
    db <- req(activeDb())
    dtgs <- req(selectedDtgs())
    datesCycles <- getCurrentDatesAndCycles(isolate(input))

    sens <- getAvailableSensornames(db, datesCycles$dates, datesCycles$cycles)
    if(is.null(sens$cached) || !selectedDtgsAreCached()) {
      updateSelection(session, "sensor", sens$general)
      updateSelectInput(session, "sensor", label="Sensor (cache info not available)")
    } else {
      updateSelection(session, "sensor", sens$cached)
      updateSelectInput(session, "sensor", label="Sensor")
    }
  })

  # Update satellite choices for given sensor
  observeEvent({
    reloadInfoFromCache()
    selectedDtgsAreCached()
    input$obtype
    input$obname
    input$sensor
    }, {
    req(input$obtype=="satem")
    db <- req(activeDb())
    sens <- req(input$sensor)
    dtgs <- req(selectedDtgs())
    datesCycles <- getCurrentDatesAndCycles(isolate(input))

    sats <- getAvailableSatnames(db,datesCycles$dates,datesCycles$cycles,sens)
    if(is.null(sats$cached) || !selectedDtgsAreCached()) {
      updateSelection(session, "satellite", sats$general)
      updateSelectInput(session, "satellite", label="Satellite (cache info not available)")
    } else {
      updateSelection(session, "satellite", sats$cached)
      updateSelectInput(session, "satellite", label="Satellite")
    }
  })

  # Update channel choice for given satellite
  channels <- NULL
  observeEvent({
    reloadInfoFromCache()
    selectedDtgsAreCached()
    input$obtype
    input$obname
    input$sensor
    input$satellite
    },{
    req(input$obtype=="satem")

    db <- req(activeDb())
    sat <- req(input$satellite)
    sens <- req(input$sensor)
    datesCycles <- getCurrentDatesAndCycles(input)

    channels <<- getAvailableChannels(
      db, datesCycles$dates, datesCycles$cycles, satname=sat, sensorname=sens
    )
    if(is.null(channels)) {
      updateSelectInput(session, "channels", choices=list(), selected=list())
    } else {
      updateSelection(session, "channels", channels)
    }
  })

  observeEvent(input$channelsSelectAll, {
    updateSelectInput(
      session, "channels", choices=channels, selected=channels
    )
  })

  observeEvent(input$channelsSelectNone, {
    updateSelectInput(
      session, "channels", choices=channels, selected=list()
    )
  })

  # Build named list of criteria
  buildCriteria <- function() {
    exp <- isolate(experiments()[[req(input$experiment)]])
    db <- req(input$odbBase)
    adb <- req(activeDb())
    res <- list()
    res$info <- list()
    obname <- req(input$obname)
    res$obnumber <- getAttrFromMetadata('obnumber', obname=obname)
    res$levels <- list()
    if (obname == 'satem') {
      sensor <- req(input$sensor)
      res$obname <- sensor
      res$satname <- req(input$satellite)
      if (!is.null(input$channels)) res$levels <- input$channels
    } else {
      res$obname <- obname
      res$varname <- req(input$variable)
      if (!is.null(input$levels)) res$levels <- input$levels

      station <- input$station
      if("" %in% station) station <- ""
      res$station <- station
    }

    res
  }

  # Update plottype choices with available plottypes according to criteria
  updatePlotTypes <- function() {
    criteria <- buildCriteria()
    choices <- applicablePlots(criteria)
    updateSelection(session, "plottype", choices)
  }

  # Trigger plottype update on criteria change
  observeEvent({
    reloadInfoFromCache()
    input$obtype
    input$obname
    input$sensor
    input$satellite
    input$channels
    input$variable
    input$levels
    input$station
  }, {
    updatePlotTypes()
  },
    ignoreNULL=FALSE
  )

  # Perform plotting
  observeEvent(input$doPlot, {
    t <- createShinyProgressTracker()
    plotRequest <- list()
    plotter <- plotTypesFlat[[req(input$plottype)]]
    plotRequest$expName <- req(input$experiment)
    db <- req(activeDb())
    plotRequest$dbType <- db$dbType
    plotRequest$criteria <- buildCriteria()
    plotRequest$criteria$dtg <-
      switch(plotter$dateType,
             "single"={
               cycle <- req(input$cycle)
               date2dtg(req(input$date), cycle)
             },
             "range"={
                   dateRange <- req(input$dateRange)
                   cycles <- input$cycles
                   if (is.null(cycles)) {
                     signalError("Please select at least one cycle.")
                     return(NULL)
                   }
                   list(dateRange[1], dateRange[2], cycles)
             })
    isWindspeed <- "varname" %in% names(plotRequest$criteria) &&
      plotRequest$criteria$varname %in% c("ff", "ff10m")
    if (isWindspeed) {
      plotData <- buildFfData(db, plotter, plotRequest)
    } else {
      query <- plotBuildQuery(plotter, plotRequest)
      output$queryUsed <- renderText(query)
      t <- addTask(t, "Querying database")
      plotData <- performQuery(db, query, plotRequest$criteria$dtg,
                               progressTracker=t)
      # Postprocessing plotData returned by performQuery.
      # This may be useful, e.g., if performing averages over a
      # picked date range.
      plotData <- postProcessQueriedPlotData(plotter, plotData)
    }
    if(!is.null(plotData) && nrow(plotData)>0) {
      statLabels <- c()
      allStations <- isolate(stationsAlongWithLabels())
      for(statid in plotData$statid) {
        statid <- gsub(" ", "", gsub("'", "", statid))
        statLabels <- c(statLabels, names(allStations)[allStations==statid])
      }
      if(nrow(plotData)==length(statLabels)) {
        plotData$statLabel <- statLabels
      } else {
        plotData$statLabel <- plotData$statid
      }
    }

    output$dataTable <- renderDataTable(plotData,
                                        options=list(pageLength=100))
    res <- plotGenerate(plotter, plotRequest, plotData, t)
    output$plot <- renderPlot(grid.arrange(res$obplot,
                                           top=textGrob(res$title)),
                              res=96, pointsize=18)
    if (is.null(res$obmap)) {
      js$disableTab("mapTab")
      if (input$mainArea == "mapTab") {
        updateTabsetPanel(session, "mainArea", "plotTab")
      }
    } else {
      output$map <- renderLeaflet(res$obmap)
      output$mapTitle <- renderText(res$title)
      js$enableTab("mapTab")
    }
    closeTracker(t)
  })
})
