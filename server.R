if(!exists("initFileSourced")) source("init.R")

# To keep track of all menu labels currently in use
allMenuLabels <- list()
# To keep track of all choices currently made in the menus
allMenuChoices <- list()

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
updateSelectInputWrapper <- function(
  session, inputId, label=NULL, choices=NULL, selected=NULL,
  choicesFoundIncache=TRUE, ...
){

  # First, update label
  notCachedLabelMsg <- "(cache info not available)"
  currentLabel <- allMenuLabels[[inputId]]
  if(is.null(currentLabel)) currentLabel <- getDefLabel(inputId)

  currLabelFlaggedAsNotCached <- grepl(notCachedLabelMsg,currentLabel)==TRUE
  needsLabelChange <- {
    (!is.null(label) && label!=currentLabel) ||
    (choicesFoundIncache && currLabelFlaggedAsNotCached) ||
    (!choicesFoundIncache && !currLabelFlaggedAsNotCached)
  }

  if(needsLabelChange) {
    if(is.null(label)) label <- currentLabel
    label <- gsub(notCachedLabelMsg, "", label, fixed=TRUE)
    if(!choicesFoundIncache) label <- paste(label, notCachedLabelMsg)
    updateSelectInput(session, inputId, label=label)
    allMenuLabels[[inputId]] <<- label
  }

  # Now, update items and choices
  currentChoices <- allMenuChoices[[inputId]]
  validUpdate <- !is.null(choices) && (is.null(currentChoices) ||
    length(unlist(currentChoices)) != length(unlist(choices)) ||
    !all(sort(unlist(currentChoices))==sort(unlist(choices)))
  )
  if(!validUpdate) return(NULL)

  selection <- getSelection(session, inputId, choices)
  updateSelectInput(
    session, inputId, choices=choices, selected=selection, label=NULL, ...
  )
  allMenuChoices[[inputId]] <<- choices
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
  validDtgs <- NULL
  for(dtg in dtgs) {
    fPath <- db$paths[dtg]
    if(is.null(fPath) || is.na(fPath) || length(fPath)==0) next
    validDtgs <- c(validDtgs, dtg)
  }
  fPathsToCache <- tryCatch(
    db$paths[validDtgs],
    error=function(e) {flog.error(e); NULL}
  )
  if(length(fPathsToCache)==0) fPathsToCache <- NULL
  return(fPathsToCache)
}

shinyServer(function(input, output, session) {
  # Make sure all menu choices and labels are reset when server starts
  allMenuLabels <- list()
  allMenuChoices <<- list()
  # Start GUI with all inputs disabled.
  # They will be enabled once experiments are loaded
  isolate(disableShinyInputs(input, except="experiment"))

  # Initial population of experiments; triggers cascade for other form fields
  exptNames <- c("")
  experiments <- reactive ({
    # Keep checking if experiments finished initialisation
    if(!all(resolved(experimentsAsPromises))) invalidateLater(5000, session)
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

    updateSelectInputWrapper(session, "odbBase", choices=choices)
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
    updateSelectInputWrapper(session, "cycle", choices=cycles)
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
  assyncCachingProcs <- reactiveValues()
  fPathsToCache <- eventReactive({
    activeDb()
    selectedDtgs()
  }, {
    allFiles <- getFilePathsToCache(req(activeDb()), input)
    # We don't want to schedule caching if file is already being cached
    # resolved(arg) returns TRUE unless arg is a non-resolved future
    rtn <- c()
    for(fPath in allFiles) {
      if(resolved(assyncCachingProcs[[fPath]])) {
        rtn <- c(rtn, fPath)
        assyncCachingProcs[[fPath]] <- NULL
      }
    }
    return(rtn)
  })
  observeEvent({fPathsToCache()}, {
    db <- req(activeDb())
    fPaths <- req(fPathsToCache())
    cacheProc <- assyncPutObsInCache(fPaths, cacheDir=db$cacheDir)
    for(fPath in fPaths) assyncCachingProcs[[fPath]] <- cacheProc
  })
  # Re-cache observations if requested by user
  observeEvent({input$recacheCacheButton}, {
    db <- req(activeDb())
    assyncPutObsInCache(fPathsToCache(), cacheDir=db$cacheDir, replaceExisting=TRUE)
  },
    ignoreInit=TRUE
  )

  # Detect when the relevant cache files have been updated
  cacheFileUpdated <- eventReactive({activeDb()}, {
    reactivePoll(5000, session,
      partial(cacheFilesLatestMdate, db=req(activeDb())), function() NULL)
  })

  # Flagging that it's time to read info from cache
  latestTriggerReadCache <- reactiveVal(0)
  triggerReadCache <- function() latestTriggerReadCache(Sys.time())
  reloadInfoFromCache <- eventReactive({
      latestTriggerReadCache()
      input$reloadCacheButton
      activeDb()
      cacheFileUpdated()
      selectedDtgs()
    }, {
    character(0)
  },
    ignoreNULL=TRUE
  ) %>% throttle(1000)

  # Keep track of whether selected DTGs are cached or not
  selectedDtgsAreCached <- eventReactive({
      reloadInfoFromCache()
    }, {
      dtgsAreCached(req(activeDb()), req(selectedDtgs()))
  })
  # Attempt to cache DTGs if they remain uncached even after
  # the processes responsible for caching them have finished
  # This is useful to retry caching if former attempts fail
  observeEvent({if(!selectedDtgsAreCached()) invalidateLater(5000)}, {
    req(!selectedDtgsAreCached())
    db <- req(activeDb())
    fPaths <- c()
    for(fPath in fPathsToCache()) {
      if(resolved(assyncCachingProcs[[fPath]])) fPaths <- c(fPaths, fPath)
    }
    req(length(fPaths)>0)
    # Here we end up with files for which the caching process has finished
    # but the corresponding DTGs stil remain uncached
    showNotification("Attempting to recache", type="warning", duration=1)
    cacheProc <- assyncPutObsInCache(fPaths, cacheDir=db$cacheDir, replaceExisting=TRUE)
    for(fPath in fPaths) assyncCachingProcs[[fPath]] <- cacheProc
  },
    ignoreInit=TRUE
  )

  # Update obtype
  observeEvent({
      reloadInfoFromCache()
    }, {
    db <- req(activeDb())
    if(db$dbType=="ecma_sfc") {
      updateSelectInputWrapper(session, "obtype", choices=c("surface"))
    } else {
      dtgs <- req(selectedDtgs())
      datesCycles <- getCurrentDatesAndCycles(isolate(input))
      obtypes <- getObtypes(db, datesCycles$dates, datesCycles$cycles)

      isCached <- selectedDtgsAreCached() && !is.null(obtypes$cached)
      if(isCached) {
        newChoices <- obtypes$cached
      } else {
        newChoices <- obtypes$general
        delay(5000, triggerReadCache())
      }
      updateSelectInputWrapper(
        session, "obtype", choices=newChoices, choicesFoundIncache=isCached
      )
    }
  })

  # Update obnames
  observeEvent({
      reloadInfoFromCache()
      input$obtype
    }, {
    req(input$obtype!="satem")
    obsCategory <- req(input$obtype)
    db <- req(activeDb())
    dtgs <- req(selectedDtgs())
    datesCycles <- getCurrentDatesAndCycles(isolate(input))

    obnames <- getObnames(db, obsCategory, datesCycles$dates, datesCycles$cycles)
    isCached <- selectedDtgsAreCached() && !is.null(obnames$cached)
    if(isCached) {
      newChoices <- obnames$cached
    } else {
      newChoices <- obnames$general
      if(!(obsCategory %in% c("radar", "scatt"))) {
        # In these cases obnames$cached will always be NULL, since
        # obname=obsCategory and this info is therefore not stored in cache
        delay(5000, triggerReadCache())
      }
    }
    updateSelectInputWrapper(
      session, "obname", choices=newChoices, choicesFoundIncache=isCached
    )
  })

  # Update variable
  observeEvent({
      reloadInfoFromCache()
      input$obtype
      input$obname
    }, {
    req(input$obtype!="satem")

    db <- req(activeDb())
    obname <- req(input$obname)
    dtgs <- req(selectedDtgs())
    datesCycles <- getCurrentDatesAndCycles(isolate(input))

    variables <- getVariables(db,datesCycles$dates,datesCycles$cycles,obname)
    isCached <- selectedDtgsAreCached() && !is.null(variables$cached)
    if(isCached) {
      newChoices <- variables$cached
    } else {
      newChoices <- variables$general
      delay(5000, triggerReadCache())
    }
    updateSelectInputWrapper(
      session, "variable", choices=newChoices, choicesFoundIncache=isCached
    )
  })

  # Decide whether to allow users to select stations
  allowChoosingStation <- eventReactive({
    input$plottype
  }, {
    infoAboutSelectedPlotType <- plotTypesFlat[[req(input$plottype)]]
    query <- infoAboutSelectedPlotType$queryStub
    # StationIDs are not stored in the "obsmon" table, only in "usage"
    queryFromUsage <- grepl("FROM{1}[[:space:]]+usage",query,ignore.case=TRUE)
    queryFromUsage
  })
  observeEvent({
    allowChoosingStation()
    }, {
      shinyjs::toggleState("station", condition=allowChoosingStation())
      shinyjs::toggleElement("station", condition=allowChoosingStation())
  })

  # Update stations
  stationsAlongWithLabels <- eventReactive({
      reloadInfoFromCache()
      allowChoosingStation()
      input$obtype
      input$obname
      input$variable
    }, {
    req(input$obtype!="satem")

    if(!allowChoosingStation()) return(c("Any"=""))
    if(!selectedDtgsAreCached()) {
      delay(5000, triggerReadCache())
      return(c("Any (cache info not available)"=""))
    }

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
          statName <- synopStations[statID]
          label <- statID
          if(is.character(statName)) label<-sprintf("%s (%s)",statID,statName)
          stationLabels <- c(stationLabels, label)
        }
        names(stations) <- stationLabels
      } else {
        names(stations) <- stations
      }
    }
    return(c("Any"="", stations))
  })
  observeEvent({stationsAlongWithLabels()}, {
    stations <- stationsAlongWithLabels()
    updateSelectInputWrapper(session, "station", choices=stations)
  })

  # Update level choice for given variable
  availableLevels <- eventReactive({
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

    if(selectedDtgsAreCached()) {
      getAvailableLevels(db,datesCycles$dates,datesCycles$cycles,obname,var)
    } else {
      list(obsmon=NULL, usage=NULL, all=NULL)
    }
  })
  observeEvent({availableLevels()}, {
    if(is.null(availableLevels()$all)) {
      updateSelectInputWrapper(session,"levels",
        choices=c("Any (cache info not available)"=""), selected=character(0))
      delay(5000, triggerReadCache())
    } else {
      updateSelectInputWrapper(session,"levels",choices=availableLevels()$all)
    }
  })

  observeEvent(input$levelsSelectStandard, {
    updateSelectInput(session, "levels",
      choices=availableLevels()$all, selected=availableLevels()$obsmon)
  })
  observeEvent(input$levelsSelectAll, {
    updateSelectInput(session, "levels",
      choices=availableLevels()$all, selected=availableLevels()$all)
  })
  observeEvent(input$levelsSelectNone, {
    updateSelectInput(session, "levels",
      choices=availableLevels()$all, selected=c())
  })

  # Update sensornames
  observeEvent({
      reloadInfoFromCache()
      input$obtype
    }, {
    req(input$obtype=="satem")
    updateSelectInputWrapper(session, "obname", choices=c("satem"))
    db <- req(activeDb())
    dtgs <- req(selectedDtgs())
    datesCycles <- getCurrentDatesAndCycles(isolate(input))

    sens <- getAvailableSensornames(db, datesCycles$dates, datesCycles$cycles)
    isCached <- selectedDtgsAreCached() && !is.null(sens$cached)
    if(isCached) {
      newChoices <- sens$cached
    } else {
      newChoices <- sens$general
      delay(5000, triggerReadCache())
    }
    updateSelectInputWrapper(
      session, "sensor", choices=newChoices, choicesFoundIncache=isCached
    )
  })

  # Update satellite choices for given sensor
  observeEvent({
    reloadInfoFromCache()
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
    isCached <- selectedDtgsAreCached() && !is.null(sats$cached)
    if(isCached) {
      newChoices <- sats$cached
    } else {
      newChoices <- sats$general
      delay(5000, triggerReadCache())
    }
    updateSelectInputWrapper(
      session, "satellite", choices=newChoices, choicesFoundIncache=isCached
    )
  })

  # Update channel choice for given satellite
  channels <- eventReactive({
    reloadInfoFromCache()
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

    newChannels <- NULL
    if(selectedDtgsAreCached()) {
      newChannels <- getAvailableChannels(
        db,datesCycles$dates,datesCycles$cycles,satname=sat,sensorname=sens
      )
    }
    if(is.null(newChannels))newChannels<-c("Any (cache info not available)"="")
    newChannels
  })
  observeEvent({channels()}, {
    updateSelectInputWrapper(session, "channels", choices=channels())
  })

  observeEvent(input$channelsSelectAll, {
    updateSelectInput(
      session, "channels", choices=channels(), selected=channels()
    )
  })
  observeEvent(input$channelsSelectNone, {
    updateSelectInput(
      session, "channels", choices=channels(), selected=character(0)
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
    if (obname == 'satem') {
      sensor <- req(input$sensor)
      res$obname <- sensor
      res$satname <- req(input$satellite)
      levels <- input$channels
    } else {
      res$obname <- obname
      res$varname <- req(input$variable)
      levels <- input$levels

      station <- input$station
      if("" %in% station) station <- ""
      res$station <- station
    }
    res$levels <- list()
    if(length(levels)>0 && levels!="") res$levels <- levels

    res
  }

  # Update plottype choices according to criteria
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
    choices <- applicablePlots(req(buildCriteria()))
    updateSelectInputWrapper(session, "plottype", choices=choices)
  },
    ignoreNULL=FALSE
  )

  # Perform plotting
  preparePlots <- function(plotter, plotRequest, db, stations) {
    isWindspeed <- "varname" %in% names(plotRequest$criteria) &&
      plotRequest$criteria$varname %in% c("ff", "ff10m")
    query <- NULL
    if (isWindspeed) {
      plotData <- buildFfData(db, plotter, plotRequest)
    } else {
      query <- plotBuildQuery(plotter, plotRequest)
      plotData <- performQuery(db, query, plotRequest$criteria$dtg)
      # Postprocessing plotData returned by performQuery.
      # This may be useful, e.g., if performing averages over a
      # picked date range.
      plotData <- postProcessQueriedPlotData(plotter, plotData)
    }
    if(!is.null(plotData) && nrow(plotData)>0) {
      statLabels <- c()
      for(statid in plotData$statid) {
        statid <- gsub(" ", "", gsub("'", "", statid))
        statLabels <- c(statLabels, names(stations)[stations==statid])
      }
      if(nrow(plotData)==length(statLabels)) {
        plotData$statLabel <- statLabels
      } else {
        plotData$statLabel <- plotData$statid
      }
    }

    res <- plotGenerate(plotter, plotRequest, plotData)
    res[["queryUsed"]] <- query
    res[["plotData"]] <- plotData
    return(res)
  }

  currentPlotPid <- reactiveVal(-1)
  plotStartedNotifId <- reactiveVal(-1)
  onclick("cancelPlot", {
    removeNotification(plotStartedNotifId())
    showNotification("Cancelling plot", type="warning", duration=1)
    tools::pskill(currentPlotPid())
    shinyjs::hide("cancelPlot")
    shinyjs::show("doPlot")
    enableShinyInputs(input)
  })
  futurePlot <- eventReactive(input$doPlot, {
    disableShinyInputs(input)
    shinyjs::hide("doPlot")
    shinyjs::show("cancelPlot")
    shinyjs::enable("cancelPlot")

    plotter <- plotTypesFlat[[req(input$plottype)]]
    db <- req(activeDb())
    if(input$obtype=="satem") {
      stations <- NULL
    } else {
      stations <- stationsAlongWithLabels()
    }
    plotRequest <- list()
    plotRequest$expName <- req(input$experiment)
    plotRequest$dbType <- db$dbType
    plotRequest$criteria <- buildCriteria()
    plotRequest$criteria$dtg <- switch(plotter$dateType,
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
      }
    )

    rtn <- future({
      tryCatch(
        preparePlots(plotter, plotRequest, db, stations),
        error=function(e) {flog.error(e); NULL}
      )
    })
    if(!is.null(rtn)) {
      currentPlotPid(rtn$job$pid)
      plotStartedNotifId(showNotification("Plot initiated", type="message"))
    }
    rtn
  })

  readyPlot <- reactive({
    myFutPlot <- futurePlot()
    req(!is.null(myFutPlot), cancelOutput=TRUE)
    isReady <- resolved(myFutPlot)
    if(!isReady) invalidateLater(1000)
    req(isReady, cancelOutput=TRUE)
    shinyjs::disable("cancelPlot")
    myPlot <- tryCatch(
      value(myFutPlot),
      error=function(e) {flog.error(e); NULL}
    )
    myPlot
  })

  # Notify the if plot went successfully or not
  observeEvent({readyPlot()}, {
    removeNotification(plotStartedNotifId())
    plotData <- readyPlot()$plotData
    if(is.null(plotData) || nrow(plotData)==0) {
      if(is.null(plotData)) msg<-"A problem occurred. Please check the logs."
      else msg <- "Query returned no data."
      signalError(title="Could not produce plot", message=msg)
    } else {
      showNotification("Redering plot", duration=1, type="message")
    }
  })

  # Finally, producing the output
  output$plot <- renderPlot(
    grid.arrange(req(readyPlot()$obplot),top=textGrob(req(readyPlot()$title))),
    res=96, pointsize=18
  )
  output$dataTable <- renderDataTable(
    req(readyPlot()$plotData), options=list(pageLength=100)
  )
  output$queryUsed <- renderText(req(readyPlot()$queryUsed))
  output$map <- renderLeaflet(req(readyPlot()$obmap))
  output$mapTitle <- renderText(req(readyPlot()$title))

  observeEvent(readyPlot(), {
      on.exit({
        shinyjs::hide("cancelPlot")
        shinyjs::show("doPlot")
        enableShinyInputs(input)
      })

      if(is.null(readyPlot()$obmap)) {
        if(input$mainArea=="mapTab") {
          updateTabsetPanel(session, "mainArea", "plotTab")
        }
        js$disableTab("mapTab")
      } else {
        js$enableTab("mapTab")
      }
  })
})
