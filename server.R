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

getFilePathsToCache <- function(db, dtgs) {
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
  # Source some useful shiny-related helper functions and wrappers
  source("shiny_wrappers.R")
  # User options parsed from config file
  config <- obsmonConfig

  ############################################################################
  #                          Handling of main tab                            #
  ############################################################################

  # Start GUI with all inputs disabled.
  # They will be enabled once experiments are loaded
  isolate(disableShinyInputs(input, except="experiment"))

  # Deciding whether to show or hide cache-related options.
  # It is advisable not to show them by default -- especially when running on
  # a web server for multiple users, as these options cause changes in the
  # shared cache files for a given experiment.
  #
  # Mind that the cache options will always be shown if a file named
  # ".obsmon_show_cache_options" exists in the obsmon directory. This was
  # designed as a simple way to allow changing this configuration without
  # having to restart obsmon (useful when running on servers)
  output$showCacheOptions <- renderText({
    file.exists(".obsmon_show_cache_options") ||
    config$general[["showCacheOptions"]]
  })
  outputOptions(output, "showCacheOptions", suspendWhenHidden = FALSE)

  # Initial population of experiments
  exptNames <- c("")
  experiments <- reactive ({
    # Keep checking if experiments finished initialisation
    if(!all(resolved(experimentsAsPromises))) invalidateLater(2000, session)
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

  # Hide "Loading Obsmon" screen and show the app
  shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")
  shinyjs::show("app-content")

  # Update database options according to chosen experiment
  observe({
    expName <- req(input$experiment)
    expDbs <- isolate(experiments()[[expName]]$dbs)
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

  # DTG-related reactives and observers
  # Update available choices of dates when changing active database
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

  # Signal when required dateType of plot changes value
  dateTypeReqByPlotType <- reactiveVal("single")
  observeEvent(input$plottype, {
    dateType <- tryCatch(
      plotTypesFlat[[req(input$plottype)]]$dateType,
      error=function(e) {"single"}
    )
    dateTypeReqByPlotType(dateType)
  })
  # Offer single date or dateRange input according to selected plottype
  # Used to be done via conditionalPanel in ui.R, but that was slow
  observeEvent(dateTypeReqByPlotType(), {
    shinyjs::toggle("date", condition=dateTypeReqByPlotType()=="single")
    shinyjs::toggle("dateRange", condition=dateTypeReqByPlotType()=="range")
    shinyjs::toggle("cycle", condition=dateTypeReqByPlotType()=="single")
    shinyjs::toggle("cycles", condition=dateTypeReqByPlotType()=="range")
  })

  # Keep track of date(s), cycle(s) and consequently DTG(s) selected in the UI
  # and store them in a convenient order and format
  selectedDates <- reactiveVal()
  observeEvent({
    input$date
    input$dateRange
    dateTypeReqByPlotType()
   }, {
    if(dateTypeReqByPlotType() %in% c("range")) {
      dates <- expandDateRange(input$dateRange[[1]], input$dateRange[[2]])
    } else {
      dates <- strftime(input$date, format="%Y%m%d")
    }
    selectedDates(sort(dates, decreasing=TRUE))
  })
  selectedCycles <- reactiveVal()
  observeEvent({
    input$cycle
    input$cycles
    dateTypeReqByPlotType()
   }, {
    if(dateTypeReqByPlotType() %in% c("range")) {
      cycles <- input$cycles
    } else {
      cycles <- input$cycle
    }
    selectedCycles(sort(cycles, decreasing=FALSE))
  })
  selectedDtgs <- reactiveVal()
  observeEvent({
    selectedDates()
    selectedCycles()
   }, {
    dtgs <- c()
    for(date in req(selectedDates())) {
      for(cycle in req(selectedCycles())) {
        dtgs <- c(dtgs, sprintf("%s%s", date, cycle))
      }
    }
    selectedDtgs(dtgs)
  })

  # Update available cycle choices when relevant fields change
  availableCycles <- reactiveVal()
  observe({
    availableCycles(getAvailableCycles(req(activeDb()), req(selectedDates())))
  })
  observeEvent(availableCycles(), {
    updateSelectInputWrapper(session, "cycle", choices=req(availableCycles()))
    updateCheckboxGroup(session, "cycles", req(availableCycles()))
  })
  observeEvent(input$cyclesSelectAll, {
    cycles <- req(availableCycles())
    updateCheckboxGroupInput(session, "cycles",
      choices=cycles, selected=cycles, inline=TRUE
    )
  })
  observeEvent(input$cyclesSelectNone, {
    cycles <- req(availableCycles())
    updateCheckboxGroupInput(session, "cycles",
      choices=cycles, selected=character(0), inline=TRUE
    )
  })


  # Initialise some cache vars for which proper reactives will be set up later
  # This early init is to avoid triggering caching at startup
  selectedDtgsAreCached <- reactiveVal(FALSE)
  reloadInfoFromCache <- reactiveVal(Sys.time())
  # triggerReadCache and latestTriggerReadCache will be employed to flag the
  # need to retry reading info from cache (eg. if it cannot be found at first)
  latestTriggerReadCache <- reactiveVal(0)
  triggerReadCache <- function() latestTriggerReadCache(Sys.time())


  # Update obtype
  observeEvent({
    req(activeDb())
    reloadInfoFromCache()
   }, {
    db <- activeDb()
    if(isTRUE(db$dbType=="ecma_sfc")) {
      updateSelectInputWrapper(session, "obtype", choices=c("surface"))
    } else {
      obtypes <- getObtypes(db, selectedDates(), selectedCycles())

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
  updateObnames <- reactive({
    req(input$obtype)
    reloadInfoFromCache()
  }) %>% throttle(100)
  observeEvent(updateObnames(), {
    if(input$obtype=="satem") {
      updateSelectInputWrapper(session, "obname", choices=c("satem"))
      return()
    }
    obsCategory <- input$obtype
    db <- req(activeDb())

    obnames <- getObnames(db, obsCategory, selectedDates(), selectedCycles())
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
  updateVariables <- reactive({
    req(input$obtype!="satem")
    updateObnames()
    req(input$obname)
  }) %>% throttle(100)
  observeEvent(updateVariables(), {
    db <- req(activeDb())

    variables <- getVariables(db, selectedDates(), selectedCycles(), input$obname)
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

  # Update level choice for given variable
  updateLevels <- reactive({
    updateVariables()
    req(input$variable)
  }) %>% throttle(100)
  availableLevels <- eventReactive(updateLevels(), {
    db <- activeDb()
    obname <- input$obname
    var <- input$variable

    levels <- getAvailableLevels(db, selectedDates(), selectedCycles(), obname, var)
    if(length(levels$all)==0) {
      levels$all <- c("Any (cache info not available)"="")
    } else if(!selectedDtgsAreCached()) {
      levels$all <- c("Any (cache info incomplete)"="", levels$all)
    }
    if(!selectedDtgsAreCached()) delay(5000, triggerReadCache())
    return(levels)
  })
  observe({
    updateSelectInputWrapper(session,"levels",choices=availableLevels()$all)
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
  updateSensor <- reactive({
    reloadInfoFromCache()
    req(input$obtype=="satem")
  })  %>% throttle(100)
  observeEvent(updateSensor(), {
    db <- req(activeDb())
    sens <- getAvailableSensornames(db, selectedDates(), selectedCycles())
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
  updateSatellite <- reactive({
    updateSensor()
    req(input$sensor)
  })  %>% throttle(100)
  observeEvent(updateSatellite(), {
    db <- req(activeDb())
    sens <- input$sensor

    sats <- getAvailableSatnames(db, selectedDates(), selectedCycles(), sens)
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
  updateChannels <- reactive({
    updateSatellite()
    req(input$satellite)
  })  %>% throttle(100)
  channels <- eventReactive(updateChannels(), {
    db <- req(activeDb())
    dates <- req(selectedDates())
    cycles <- req(selectedCycles())
    sat <- input$satellite
    sens <- input$sensor

    newChannels <- NULL
    if(selectedDtgsAreCached()) {
      newChannels <- getAvailableChannels(
        db, dates, cycles, satname=sat, sensorname=sens
      )
    }
    if(is.null(newChannels))newChannels<-c("Any (cache info not available)"="")
    newChannels
  })
  observeEvent(channels(), {
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

  # Update plottype choices according to criteria
  updatePlotType <- reactive({
    input$obtype
    input$obname
    req(!is.null(input$variable) ||
      (!is.null(input$satellite) && !is.null(input$sensor))
    )
  }) %>% throttle(100)
  observeEvent(updatePlotType(), {
    choices <- applicablePlots(req(plotsBuildCriteria(input)))
    updateSelectInputWrapper(session, "plottype", choices=choices)
  })

  # Decide whether to allow users to select stations
  allowChoosingStation <- reactive(
     plotSupportsChoosingStations(input$plottype, input$obtype)
  )
  observeEvent(allowChoosingStation(), {
      shinyjs::toggleState("station", condition=allowChoosingStation())
      shinyjs::toggleElement("station", condition=allowChoosingStation())
  })

  # Update stations
  stationsAlongWithLabels <- reactiveVal(c("Any"=""))
  updateStations <- reactive({
    allowChoosingStation()
    reloadInfoFromCache()
    input$obtype
    input$obname
    input$variable
  }) %>% throttle(100)
  observeEvent(updateStations(), {
    if(!allowChoosingStation()) stationsAlongWithLabels(c("Any"=""))
    req(allowChoosingStation())

    db <- req(activeDb())
    dates <- req(selectedDates())
    cycles <- req(selectedCycles())
    obname <- req(input$obname)
    variable <- req(input$variable)

    stations <- getStationsFromCache(db, dates, cycles, obname, variable)
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
    if(selectedDtgsAreCached()) {
      stationsAlongWithLabels(c("Any"="", stations))
    } else {
      if(length(stations)==0) {
        stationsAlongWithLabels(c("Any (cache info not available)"=""))
      } else {
        stationsAlongWithLabels(c("Any (cache info incomplete)"="", stations))
      }
      delay(5000, triggerReadCache())
    }
  }, ignoreNULL=TRUE)
  observe({
    updateSelectInputWrapper(session, "station", choices=stationsAlongWithLabels())
  })

  ######################################
  # Caching-related observers/reactives#
  ######################################
  # Caching is performed assyncronously. "assyncCachingProcs" will keep
  # track of the processes responsible for caching the various data files.
  # These processes are "Future" objects (from R pkg "future"), and their
  # statuses can be checked using the function "resolved"
  assyncCachingProcs <- reactiveValues()
  observe({
    # Periodic cleanup of assyncCachingProcs
    procs <- assyncCachingProcs
    stillAciveProcs <- list()
    for(fPath in names(procs)) {
      proc <- procs[[fPath]]
      if(!resolved(proc)) stillAciveProcs[[fPath]] <- proc
    }
    isolate(assyncCachingProcs <- stillAciveProcs)
    if(length(stillAciveProcs)>0) invalidateLater(300000)
  })

  # Put observations in cache when dB and/or DTG selection are modified
  fPathsToCache <- reactiveVal(NULL)
  observe({
    db <- req(activeDb())
    dtgs <- req(selectedDtgs())
    allFiles <- getFilePathsToCache(db, dtgs)
    # We don't want to schedule caching if file is already being cached
    # resolved(arg) returns TRUE unless arg is a non-resolved future
    rtn <- c()
    for(fPath in allFiles) {
      if(resolved(assyncCachingProcs[[fPath]])) rtn <- c(rtn, fPath)
    }
    isolate(fPathsToCache(rtn))
  })
  observeEvent(fPathsToCache(), {
    db <- req(activeDb())
    fPaths <- req(fPathsToCache())
    cacheProc <- assyncPutObsInCache(fPaths, cacheDir=db$cacheDir)
    for(fPath in fPaths) assyncCachingProcs[[fPath]] <- cacheProc
  })
  # Re-cache observations if requested by user
  observeEvent(input$recacheCacheButton, {
    db <- req(activeDb())
    showNotification("Recaching selected DTG(s)", type="warning", duration=1)
    assyncPutObsInCache(fPathsToCache(), cacheDir=db$cacheDir, replaceExisting=TRUE)
  },
    ignoreInit=TRUE
  )
  # Reset cache if requested by user
  # Doing this in two steps to require confirmation
  observeEvent(input$resetCacheButton, {
    showConfirmationDialog(
      inputId="resetCacheConfirmationButton",
      title="Are you sure?",
      msg=HTML(sprintf(paste(
          "Please confirm that you want to RESET all cached information ",
          "available for experiment %s%s%s",
          "This action cannot be undone!"
        ), "<br><br>", req(input$experiment), "<br><br>")
      )
    )
  },
    ignoreInit=TRUE
  )
  observeEvent(input$resetCacheConfirmationButton, {
    status <- createCacheFiles(cacheDir=req(activeDb()$cacheDir), reset=TRUE)
    removeModal()
    if(status==0) {
      showModal(
        modalDialog("The experiment cache has been reset", easyClose=TRUE)
      )
    } else {
        signalError("Problems resetting experiment cache. Please check logs.")
    }
  })

  # Detect when the relevant cache files have been updated
  cacheFileUpdated <- eventReactive(activeDb(), {
    reactivePoll(5000, session,
      partial(cacheFilesLatestMdate, db=req(activeDb())), function() NULL)
  })

  # Flagging that it's time to read info from cache
  reloadInfoFromCache <- eventReactive({
      latestTriggerReadCache()
      activeDb()
      cacheFileUpdated()
      selectedDtgs()
    }, {
    Sys.time()
  },
    ignoreNULL=TRUE
  ) %>% throttle(1000)

  # Keep track of whether selected DTGs are cached or not
  observeEvent(reloadInfoFromCache(), {
      selectedDtgsAreCached(dtgsAreCached(req(activeDb()),req(selectedDtgs())))
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


  ########
  # Plots#
  ########
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
    readyPlot(NULL)
    if(dateTypeReqByPlotType()=="range") {
      if(is.null(input$cycles))signalError("Please select at least one cycle")
      req(!is.null(input$cycles))
    }

    plotInterrupted(FALSE)
    disableShinyInputs(input)
    shinyjs::hide("doPlot")
    shinyjs::show("cancelPlot")
    shinyjs::enable("cancelPlot")

    db <- req(activeDb())
    stations <- stationsAlongWithLabels()

    plotter <- plotTypesFlat[[req(input$plottype)]]
    plotRequest <- list()
    plotRequest$expName <- req(input$experiment)
    plotRequest$dbType <- db$dbType
    plotRequest$criteria <- plotsBuildCriteria(input)

    plotStartedNotifId(showNotification("Gathering data", type="message"))
    # The suppressWarnings is because of the warning
    # "Warning in serialize(what, NULL, xdr = FALSE) :
    # 'package:DBI' may not be available when loading"
    newFutPlot <- suppressWarnings(futureCall(
      FUN=preparePlots,
      args=list(plotter=plotter, plotRequest=plotRequest, db=db, stations=stations)
    ))
    currentPlotPid(newFutPlot$job$pid)

    then(newFutPlot,
      onFulfilled=function(value) {
        showNotification("Preparing to render plot",duration=1,type="message")
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
    finally(newFutPlot, function() {
      removeNotification(plotStartedNotifId())
      shinyjs::hide("cancelPlot")
      shinyjs::show("doPlot")
      enableShinyInputs(input)
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

  # Rendering plots
  output$plot <- renderPlot({
    tryCatch(
      grid.arrange(req(readyPlot()$obplot),top=textGrob(req(readyPlot()$title))),
      error=function(e) NULL
    )
  },
    res=96, pointsize=18
  )
  output$dataTable <- renderDataTable(
    req(readyPlot()$plotData), options=list(pageLength=100)
  )
  output$queryUsed <- renderText(req(readyPlot()$queryUsed))
  output$map <- renderLeaflet(req(readyPlot()$obmap))
  output$mapTitle <- renderText(req(readyPlot()$title))


  ############################################################################
  #                        Handling of Quick plots tab                       #
  ############################################################################

  # Add quickPlots tab to UI if quickPlots are available
  if(!is.null(config$quickPlots)) {
    appendTab("appNavbarPage", tabPanel("Quick plots", value="quickPlotsTab", quickPlotsTab()))
  }

  quickPlotChoices <- c()
  for(plotConfig in config$quickPlots) {
    quickPlotChoices <- c(quickPlotChoices, plotConfig$displayName)
  }
  updateSelectInput(session, "quickPlotTitle", choices=quickPlotChoices)

  quickPlotConfigInfo <- eventReactive(input$quickPlotTitle, {
    pConfig <- NULL
    for(pConf in config$quickPlots) {
      if(!pConf$displayName==input$quickPlotTitle) next
      pConfig <- pConf
      break
    }
    pConfig
  })

  quickPlotExperiment <- eventReactive(quickPlotConfigInfo(), {
    pConfig <- quickPlotConfigInfo()
    experiments()[[pConfig$experiment]]
  })

  quickPlotActiveDb <- eventReactive(quickPlotExperiment(), {
    pConfig <- quickPlotConfigInfo()
    dbType <- pConfig$database
    quickPlotExperiment()$dbs[[dbType]]
  })

  quickPlot <- eventReactive(input$quickPlotsDoPlot, {
    pConfig <- quickPlotConfigInfo()

    # Stuff shared among all subplots
    plotter <- plotTypesFlat[[req(pConfig$plotType)]]
    if(plotter$dateType=="range") {
      if(is.null(pConfig$startDate) || is.null(pConfig$endDate)) {
        flog.error(paste(
          "Selected plot requires startDate and endDate (or",
          "startDate and nDays) to be set in the config file"
        ))
      }
      req(!is.null(pConfig$startDate) && !is.null(pConfig$endDate))
    } else {
      if(is.null(pConfig$date) || is.null(pConfig$cycle)) {
        msg <- "Selected plot requires date and cycle to be set in the config file"
        flog.error(msg)
        signalError(msg)
      }
      req(!is.null(pConfig$date) && !is.null(pConfig$cycle))
    }
    # Initialising a "shiny input"-like list that will be passed to the
    # ordinary plotting routines
    plotsCommonInput <- list(
      experiment=pConfig$experiment,
      plottype=pConfig$plotType,
      date=pConfig$date,
      cycle=pConfig$cycle,
      dateRange=c(pConfig$startDate, pConfig$endDate),
      cycles=pConfig$cycles
    )

    obnames <- names(pConfig$obs)
    inputsForAllPlots <- list()
    iPlot <- 0
    for(iObname in seq_along(pConfig$obs)) {
      obname <- obnames[iObname]
      levelsConfig <- pConfig$levels[[obname]]
      for(variable in unlist(pConfig$obs[iObname])) {
        inputsThisPlotOnly <- list(
          obname=obname,
          variable=variable,
          levels=c(levelsConfig[["allVars"]], levelsConfig[[variable]])
        )
        iPlot <- iPlot + 1
        inputsForAllPlots[[iPlot]] <- c(plotsCommonInput, inputsThisPlotOnly)
      }
    }
    # TODO: Support to setting stations and
    #       satem-related fields (sensor, satname, channels)
    stations <- NULL

    allPlots <- list()
    for(iPlot in seq_along(inputsForAllPlots)) {
     inputOneClickPlot <- inputsForAllPlots[[iPlot]]
     plotRequest <- list()
     plotRequest$expName <- req(inputOneClickPlot$experiment)
     plotRequest$dbType <- inputOneClickPlot$database
     plotRequest$criteria <- plotsBuildCriteria(inputOneClickPlot)

     newPlot <- tryCatch(
       preparePlots(plotter, plotRequest, quickPlotActiveDb(), stations),
       error=function(e) {flog.error(e); NULL}
     )
     allPlots[[quickPlotsGenId(iPlot)]] <- newPlot
    }
    return(allPlots)
  })

  # Prepare the correct output slots for plots, maps and dataTables
  # Code adapted from <https://gist.github.com/wch/5436415>
  # (i) Plots
  output$quickPlotsPlotContainer <- renderUI({
    plotOutList <- lapply(seq_along(quickPlot()), function(iPlot) {
      plotOutputInsideFluidRow(quickPlotsGenId(iPlot, type="plot"))
    })
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plotOutList)
  })
  # (ii) Maps
  output$MapAndMapTitleContainer <- renderUI({
    mapAndMapTitleOutList <- lapply(seq_along(quickPlot()), function(iPlot) {
      mapId <- quickPlotsGenId(iPlot, type="map")
      mapTitleId <- quickPlotsGenId(iPlot, type="mapTitle")
      mapAndMapTitleOutput(mapId, mapTitleId)
    })
    do.call(tagList, mapAndMapTitleOutList)
  })
  # (iii) dataTables
  output$quickPlotsQueryAndTableContainer <- renderUI({
    queryAndDataTableOutList <- lapply(seq_along(quickPlot()), function(iPlot){
      queryUsedId <- quickPlotsGenId(iPlot, type="queryUsed")
      dataTableId <- quickPlotsGenId(iPlot, type="dataTable")
      queryUsedAndDataTableOutput(queryUsedId, dataTableId)
    })
    do.call(tagList, queryAndDataTableOutList)
  })

  # Assign each plot/map/title/query/table to the respective outputs
  quickPlotsPrevQuantity <- reactiveVal()
  observeEvent(quickPlot(), {
    # Clean up old quickPlot outputs
    for(iPlot in seq(quickPlotsPrevQuantity())) {
      for(type in c("plot", "map", "mapTitle", "queryUsed", "dataTable")) {
        outId <- quickPlotsGenId(iPlot, type=type)
        output[[outId]] <- NULL
      }
    }
    quickPlotsPrevQuantity(length(quickPlot()))
    gc()

    # Assign the new quickPlots
    for(iPlot0 in seq_along(quickPlot())) {
      local({
        iPlot <- iPlot0
        pName <- quickPlotsGenId(iPlot)
        # Assign plots
        plotOutId <- quickPlotsGenId(iPlot, type="plot")
        output[[plotOutId]] <- renderPlot({
          myPlot <- quickPlot()[[pName]]
          grid.arrange(req(myPlot$obplot),top=textGrob(req(myPlot$title)))
        },
           res=96, pointsize=18
        )
        # Assign maps and map titles
        mapId <- quickPlotsGenId(iPlot, type="map")
        mapTitleId <- quickPlotsGenId(iPlot, type="mapTitle")
        output[[mapId]] <- renderLeaflet(req(quickPlot()[[pName]]$obmap))
        output[[mapTitleId]] <- renderText(req(quickPlot()[[pName]]$title))
        # Assign queryUsed and dataTable
        queryUsedId <- quickPlotsGenId(iPlot, type="queryUsed")
        dataTableId <- quickPlotsGenId(iPlot, type="dataTable")
        output[[queryUsedId]] <- renderText(req(quickPlot()[[pName]]$queryUsed))
        output[[dataTableId]] <- renderDataTable(
          req(quickPlot()[[pName]]$plotData), options=list(pageLength=10)
        )
      })
    }
  })

}) # End of shinyServer
