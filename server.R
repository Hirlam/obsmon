# Keep track of how many sessions are connected
sessionsConnected <- reactiveVal(0)

# The server
shinyServer(function(input, output, session) {
  # Start GUI with all inputs disabled.
  # They will be enabled once experiments are loaded
  isolate(disableShinyInputs(input, except="experiment"))

  # Increment global connected sessions count when session starts
  isolate(sessionsConnected(sessionsConnected() + 1))
  # Decrement global connected sessions count once session finishes
  session$onSessionEnded(function() {
    isolate({sessionsConnected(sessionsConnected() - 1)})
  })
  # Show, below the title, the number of currently connected sessions
  output$pageTitle <- renderUI({
    nSessions <- sessionsConnected()
    obsmonVersionText <- sprintf("Obsmon v%s", obsmonVersion)
    if(nSessions>0) {
      sessionsConnectedText <- sprintf(
        '<p style="font-size:11px">#Sessions connected: %d</p>', nSessions
      )
      HTML(paste0(obsmonVersionText, sessionsConnectedText))
    } else {
      flog.debug("server.R: sessionsConnected()<1!")
      obsmonVersionText
    }
  })

  ############################################################################
  #                          Handling of main tab                            #
  ############################################################################

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
    obsmonConfig$general[["showCacheOptions"]]
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
    dtgs <- NULL
    for(date in selectedDates()) {
      for(cycle in selectedCycles()) {
        dtgs <- c(dtgs, sprintf("%s%s", date, cycle))
      }
    }
    # Do not allow plot without selected DTGs
    if(is.null(dtgs)) shinyjs::disable("doPlot")
    else shinyjs::enable("doPlot")
    selectedDtgs(dtgs)
  },
    ignoreNULL=FALSE
  )


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
        delay(1000, triggerReadCache())
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
        delay(1000, triggerReadCache())
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
      delay(1000, triggerReadCache())
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
    if(!selectedDtgsAreCached()) delay(1000, triggerReadCache())
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
      delay(1000, triggerReadCache())
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
      delay(1000, triggerReadCache())
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

    newChannels <- getAvailableChannels(
      db, dates, cycles, satname=sat, sensorname=sens
    )
    if(length(newChannels)==0) {
      newChannels <- c("Any (cache info not available)"="")
    } else if(!selectedDtgsAreCached()) {
      newChannels <- c("Any (cache info incomplete)"="", newChannels)
    }
    if(!selectedDtgsAreCached()) delay(1000, triggerReadCache())

    return(newChannels)
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
  allowChoosingStation <- reactive({
     plotSupportsChoosingStations(req(input$plottype), req(input$obtype))
  })
  requireSingleStation <- reactive({
     plotRequiresSingleStation(req(input$plottype))
  })
  observeEvent({
    allowChoosingStation()
    requireSingleStation()
    }, {
    shinyjs::toggleState("station",
      condition=(allowChoosingStation() && !requireSingleStation())
    )
    shinyjs::toggleElement("station",
      condition=(allowChoosingStation() && !requireSingleStation())
    )
    shinyjs::toggleState("stationSingle",
      condition=(allowChoosingStation() && requireSingleStation())
    )
    shinyjs::toggleElement("stationSingle",
      condition=(allowChoosingStation() && requireSingleStation())
    )
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
    stations <- putLabelsInStations(stations, obname)

    entryForAnyStation <- NULL
    if(!requireSingleStation()) {
      if(selectedDtgsAreCached()) {
        entryForAnyStation <- c("Any"="")
      } else {
        if(length(stations)==0) {
          entryForAnyStation <- c("Any (cache info not available)"="")
        } else {
          entryForAnyStation <- c("Any (cache info incomplete)"="")
        }
      }
    } else {
      if(selectedDtgsAreCached()) {
        updateSelectInput(session, "stationSingle", label="Station")
      } else {
        if(length(stations)==0) {
          updateSelectInput(session, "stationSingle",
            label="Station (cache info not available)"
          )
        } else {
          updateSelectInput(session, "stationSingle",
            label="Station (cache info incomplete)"
          )
        }
      }
    }
    stationsAlongWithLabels(c(entryForAnyStation, stations))
    if(!selectedDtgsAreCached()) delay(1000, triggerReadCache())

  }, ignoreNULL=TRUE)
  observeEvent(stationsAlongWithLabels(), {
    # station is always going to be updated, as stationSingle is just a wrap
    # for station to account for the fact that updateSelectInput does not have
    # a "multiple" argument
    updateSelectInputWrapper(
      session, "station", choices=stationsAlongWithLabels()
    )
    if(requireSingleStation()) {
      if(length(stationsAlongWithLabels())>0) {
        shinyjs::enable("stationSingle")
        shinyjs::enable("doPlot")
        stations <- stationsAlongWithLabels()
      } else {
        shinyjs::disable("stationSingle")
        shinyjs::disable("doPlot")
        stations <- c("No stations available to choose"="")
      }
      updateSelectInputWrapper(session, "stationSingle", choices=stations)
    }
  })
  # Send selection from stationSingle to station
  observeEvent(input$stationSingle, {
    updateSelectInput(session, "station", selected=input$stationSingle)
  })

  ##########################################################################
  #                Caching-related observers/reactives                     #
  ##########################################################################
  # Perform caching assyncronously as the user selects new DTGs and/or dBs.
  # The reactive/observers defined here stablish a queue/schedule for the
  # files to be cached. Once the user selects new DTGs/databases, the
  # associated files are put in a queue to be cached. The files in that
  # queue are sent to the caching routine in small batches, so that it is
  # possible to reassign the order in which they should be parsed (e.g., to
  # make sure the currently selected DTGs/dB have priority).

  # Get paths to data files associated with currently selected DTG(s) and
  # and dB, and which are not currently being cached
  dataFilesForDbAndDtgs <- reactive({
    return(getFilePathsToCache(req(activeDb()), req(selectedDtgs())))
  })

  # "assyncCachingProcs" keeps track of the ongoing processes for the caching
  # of the various data files. These processes are "Future" objects (from R
  # pkg "future").
  assyncCachingProcs <- list()

  # Establish/update queue of files that need to be cached
  filesPendingCache <- reactiveVal(character(0))
  observeEvent(dataFilesForDbAndDtgs(), {
    newFiles <- dataFilesForDbAndDtgs()
    # Remove files for which caching is ongoing
    filesNotCachingNow <- newFiles[!(newFiles %in% names(assyncCachingProcs))]
    filesPendingCache(unique(c(filesNotCachingNow, filesPendingCache())))
  })

  # recacheRequested: To be used if the user manually requests recache or if
  # obsmon detects that cache has finished but DTGs remain uncached
  recacheRequested <- reactiveVal(FALSE)
  # Establish/update queue of files that need to be recached (if requested)
  filesPendingRecache <- reactiveVal(character(0))
  observeEvent(recacheRequested(), {
    req(isTRUE(recacheRequested()))
    filesPendingRecache(unique(c(filesPendingRecache(), dataFilesForDbAndDtgs())))
    recacheRequested(FALSE)
  })

  # Keep track of caching activity
  cacheIsOngoing <- reactiveVal(FALSE)

  # Prepare and send batches of data files to be cached
  newBatchFilesToCache <- eventReactive({
    filesPendingCache()
    cacheIsOngoing()
    }, {
    req(!isTRUE(cacheIsOngoing()))
    filesToCacheInThisBatch <- filesPendingCache()[1:2]
    filesToCacheInThisBatch <- Filter(Negate(anyNA), filesToCacheInThisBatch)
    return(filesToCacheInThisBatch)
  })

  # Prepare and send, if requested, batches of data files to be re-cached
  newBatchFilesToRecache <- eventReactive({
    filesPendingRecache()
    cacheIsOngoing()
    }, {
    req(!isTRUE(cacheIsOngoing()))
    filesToRecacheInThisBatch <- filesPendingRecache()[1:2]
    filesToRecacheInThisBatch <- Filter(Negate(anyNA), filesToRecacheInThisBatch)
    return(filesToRecacheInThisBatch)
  })

  # Cache (or recache) observations as new batches of file paths arrive
  observeEvent({
    newBatchFilesToCache()
    newBatchFilesToRecache()
    }, {
    if(length(newBatchFilesToRecache())>0) {
      fPaths <- newBatchFilesToRecache()
      isRecache <- TRUE
    } else {
      fPaths <- newBatchFilesToCache()
      isRecache <- FALSE
      req(!selectedDtgsAreCached())
    }
    req(length(fPaths)>0)
    db <- req(activeDb())

    cacheProc <- suppressWarnings(futureCall(
      FUN=putObsInCache,
      args=list(
        sourceDbPaths=fPaths,
        cacheDir=db$cacheDir,
        replaceExisting=isRecache
      )
    ))
    # Register caching as "onging" for the relevant files
    cacheIsOngoing(TRUE)
    for(fPath in fPaths) assyncCachingProcs[[fPath]] <<- cacheProc

    then(cacheProc,
      onRejected=function(e) {flog.error(e)}
    )
    finally(cacheProc, function() {
      triggerReadCache()
      # Clean up entries from list of ongoing cache processes
      assyncCachingProcs[fPaths] <<- NULL
      if(isRecache) {
        recacheQueue <- filesPendingRecache()
        newRecacheQueue <- recacheQueue[!(recacheQueue %in% fPaths)]
        filesPendingRecache(newRecacheQueue)
      } else {
        cacheQueue <- filesPendingCache()
        newCacheQueue <- cacheQueue[!(cacheQueue %in% fPaths)]
        filesPendingCache(newCacheQueue)
      }
      cacheIsOngoing(FALSE)
    })

    # This NULL is necessary in order to avoid the future from blocking
    NULL
  })

  # Re-cache observations if requested by user
  observeEvent(input$recacheCacheButton, {
    db <- req(activeDb())
    showNotification("Recaching selected DTG(s)", type="warning", duration=1)
    recacheRequested(TRUE)
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

  # Flagging that it's time to read info from cache
  reloadInfoFromCache <- eventReactive({
      latestTriggerReadCache()
      activeDb()
      selectedDtgs()
    }, {
    Sys.time()
  },
    ignoreNULL=TRUE
  ) %>% throttle(2000)

  # Keep track of whether selected DTGs are cached or not
  observeEvent(reloadInfoFromCache(), {
      selectedDtgsAreCached(dtgsAreCached(req(activeDb()),req(selectedDtgs())))
  })

  # Periodically attempt to cache DTGs if they remain uncached even
  # after the processes responsible for caching them have finished.
  # This is useful to retry caching if former attempts fail
  observeEvent({if(!selectedDtgsAreCached()) invalidateLater(10000)}, {
    req(!selectedDtgsAreCached())
    req(!cacheIsOngoing())
    showNotification("Attempting to recache", type="warning", duration=1)
    recacheRequested(TRUE)
  },
    ignoreInit=TRUE
  )


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
    readyPlot(NULL)
    plotInterrupted(FALSE)
    disableShinyInputs(input)
    shinyjs::hide("doPlot")
    shinyjs::show("cancelPlot")
    shinyjs::enable("cancelPlot")

    db <- req(activeDb())
    plotter <- plotTypesFlat[[req(input$plottype)]]
    plotRequest <- list()
    plotRequest$expName <- req(input$experiment)
    plotRequest$dbType <- db$dbType
    plotRequest$criteria <- plotsBuildCriteria(input)

    plotStartedNotifId(showNotification(
      "Gathering data for plot...", type="message", duration=NULL
    ))
    # The suppressWarnings is because of the warning
    # "Warning in serialize(what, NULL, xdr = FALSE) :
    # 'package:DBI' may not be available when loading"
    newFutPlot <- suppressWarnings(futureCall(
      FUN=preparePlots,
      args=list(plotter=plotter, plotRequest=plotRequest, db=db)
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
      grid.arrange(req(readyPlot()$obplot), top=textGrob(readyPlot()$title)),
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
  #                        Handling of multiPlots tab                       #
  ############################################################################

  # Add multiPlots tab to UI if multiPlots are available
  if(!is.null(obsmonConfig$multiPlots)) {
    insertTab("appNavbarPage",
      tabPanel(
        title="User-configured multiPlots",
        value="multiPlotsTab",
        multiPlotsTab()
      ),
      target="mainTab", position="after"
    )
  }

  multiPlotChoices <- c()
  for(plotConfig in obsmonConfig$multiPlots) {
    multiPlotChoices <- c(multiPlotChoices, plotConfig$displayName)
  }
  updateSelectInput(session, "multiPlotTitle", choices=multiPlotChoices)

  multiPlotConfigInfo <- eventReactive(input$multiPlotTitle, {
    pConfig <- NULL
    for(pConf in obsmonConfig$multiPlots) {
      if(!pConf$displayName==input$multiPlotTitle) next
      pConfig <- pConf
      break
    }
    pConfig
  })

  multiPlotExperiment <- eventReactive(multiPlotConfigInfo(), {
    pConfig <- multiPlotConfigInfo()
    experiments()[[pConfig$experiment]]
  })

  multiPlotActiveDb <- eventReactive(multiPlotExperiment(), {
    pConfig <- multiPlotConfigInfo()
    dbType <- pConfig$database
    multiPlotExperiment()$dbs[[dbType]]
  })

  # Management of multiPlot progress bar
  multiPlotsProgressFile <- reactiveVal(NULL)
  multiPlotsProgressStatus <- reactiveVal(function() NULL)
  multiPlotsProgressBar <- reactiveVal(NULL)
  readProgressFile <- function(path) {
    tryCatch(read.table(path),
      error=function(e) NULL,
      warning=function(w) NULL
    )
  }
  observeEvent(multiPlotsProgressFile(), {
    multiPlotsProgressStatus(reactiveFileReader(
      500, session, isolate(multiPlotsProgressFile()), readProgressFile
    ))
  })
  observeEvent(multiPlotsProgressStatus()(), {
    mpProgress <- unlist(multiPlotsProgressStatus()(), use.names=FALSE)
    progress <- multiPlotsProgressBar()
    progress$set(
      value=mpProgress[1],
      message="Preparing multiPlot",
      detail=sprintf(
        "Gathering data for plot %s of %s", mpProgress[1], mpProgress[2]
      )
    )
    multiPlotsProgressBar(progress)
  })

  # Keep track of multiPlot assync process PID, in case user wants to cancel it
  multiPlotCurrentPid <- reactiveVal(-1)

  # Management of "Cancel multiPlot" button
  multiPlotInterrupted <- reactiveVal(FALSE)
  onclick("multiPlotsCancelPlot", {
    showNotification("Cancelling multiPlot", type="warning", duration=1)
    multiPlotInterrupted(TRUE)
    tools::pskill(multiPlotCurrentPid(), tools::SIGINT)
  })

  # Producing multiPlots
  multiPlot <- reactiveVal(NULL)
  observeEvent(input$multiPlotsDoPlot, {
    # Make sure one cannot request a multiPlot while another is being produced
    if(multiPlotCurrentPid()>-1) {
      showNotification(
        "Another multiPlot is being produced. Please wait.",
        type="warning", duration=1
      )
    }
    req(multiPlotCurrentPid()==-1)

    multiPlot(NULL)

    # Prevent another plot from being requested
    disableShinyInputs(input)
    shinyjs::hide("multiPlotsDoPlot")
    # Offer possibility to cancel multiPlot
    shinyjs::show("multiPlotsCancelPlot")
    shinyjs::enable("multiPlotsCancelPlot")
    multiPlotInterrupted(FALSE)

    db <- multiPlotActiveDb()
    # Initialising a "shiny input"-like list that will be passed to the
    # ordinary plotting routines
    pConfig <- multiPlotConfigInfo()
    plotsCommonInput <- list(
      experiment=pConfig$experiment,
      plottype=pConfig$plotType,
      database=db$dbType,
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
      if(obname=="satem") {
        for(satemConfig in pConfig$obs[[obname]]) {
          inputsThisPlotOnly <- list(
            obname="satem",
            sensor=satemConfig$sensor,
            satellite=satemConfig$satellite,
            channels=satemConfig$channels,
            excludeChannels=satemConfig$excludeChannels
          )
          iPlot <- iPlot + 1
          inputsForAllPlots[[iPlot]] <- c(plotsCommonInput,inputsThisPlotOnly)
        }
      } else {
        levelsConfig <- pConfig$levels[[obname]]
        excludeLevelsConfig <- pConfig$excludeLevels[[obname]]
        stationsConfig <- pConfig$stations[[obname]]
        if(!plotRequiresSingleStation(pConfig$plotType)) {
          # One plot for each variable, allowing multiple stations in a
          # single plot (if stations are applicable at all)
          for(variable in unlist(pConfig$obs[iObname])) {
            stations <- unique(
              c(stationsConfig[["allVars"]], stationsConfig[[variable]])
            )
            inputsThisPlotOnly <- list(
              obname=obname,
              variable=variable,
              levels=sort(unique(
                c(levelsConfig[["allVars"]], levelsConfig[[variable]])
              )),
              excludeLevels=sort(unique(c(
                excludeLevelsConfig[["allVars"]],
                excludeLevelsConfig[[variable]])
              )),
              station=stations
            )
            iPlot <- iPlot + 1
            inputsForAllPlots[[iPlot]]<-c(plotsCommonInput,inputsThisPlotOnly)
          }
        } else {
          # One plot for each variable and station
          for(variable in unlist(pConfig$obs[iObname])) {
            stations <- unique(
              c(stationsConfig[["allVars"]], stationsConfig[[variable]])
            )
            for(station in stations) {
              inputsThisPlotOnly <- list(
                obname=obname,
                variable=variable,
                levels=sort(unique(
                  c(levelsConfig[["allVars"]], levelsConfig[[variable]])
                )),
                excludeLevels=sort(unique(c(
                  excludeLevelsConfig[["allVars"]],
                  excludeLevelsConfig[[variable]])
                )),
                station=station
              )
              iPlot <- iPlot + 1
              inputsForAllPlots[[iPlot]]<-c(plotsCommonInput,inputsThisPlotOnly)
            }
          }
        }
      }
    }

    if(length(inputsForAllPlots)==0) {
      showNotification(
        "Selected multiPlot generated no plots",
        type="warning", duration=1
      )
    }
    req(length(inputsForAllPlots)>0)

    # Create multiPlot progess bar
    progress <- shiny::Progress$new(max=length(inputsForAllPlots))
    progress$set(message="Gathering data for multiPlot...", value=0)
    multiPlotsProgressBar(progress)
    multiPlotsProgressFile(tempfile(pattern = "multiPlotsProgress"))

    # Prepare individual plots assyncronously
    plotter <- plotTypesFlat[[req(pConfig$plotType)]]
    multiPlotsAsync <- suppressWarnings(futureCall(
      FUN=prepareMultiPlots,
      args=list(
        plotter=plotter, inputsForAllPlots=inputsForAllPlots, db=db,
        progressFile=multiPlotsProgressFile()
      )
    ))
    multiPlotCurrentPid(multiPlotsAsync$job$pid)

    then(multiPlotsAsync,
      onFulfilled=function(value) {
        showNotification(
          "Preparing to render multiPlot", duration=1, type="message"
        )
        multiPlot(value)
        somePlotHasMap <- FALSE
        for(individualPlot in value) {
          if(!is.null(individualPlot$obmap)) {
            somePlotHasMap <- TRUE
            break
          }
        }
        if(somePlotHasMap) {
          js$enableTab("mapTab")
        } else {
          if(input$multiPlotsMainArea=="mapTab") {
            updateTabsetPanel(session, "multiPlotsMainArea", "plotTab")
          }
          js$disableTab("mapTab")
        }
      },
      onRejected=function(e) {
        if(!multiPlotInterrupted()) {
          flog.error(e)
          showNotification("Could not produce plot", duration=1, type="error")
        }
        multiPlot(NULL)
      }
    )
    finally(multiPlotsAsync, function() {
      multiPlotCurrentPid(-1)
      # Reset items related to multiPlot progress bar
      unlink(multiPlotsProgressFile())
      multiPlotsProgressBar()$close()
      multiPlotsProgressFile(NULL)
      multiPlotsProgressBar(NULL)
      # Hide/show and disable/enable relevant inputs
      shinyjs::hide("multiPlotsCancelPlot")
      shinyjs::show("multiPlotsDoPlot")
      enableShinyInputs(input)
    })
    # This NULL is necessary in order to avoid the future from blocking
    NULL
  })

  # Prepare the correct output slots for plots, maps and dataTables
  # Code adapted from <https://gist.github.com/wch/5436415>
  # (i) Plots
  output$multiPlotsPlotContainer <- renderUI({
    plotOutList <- lapply(seq_along(multiPlot()), function(iPlot) {
      plotOutputInsideFluidRow(multiPlotsGenId(iPlot, type="plot"))
    })
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plotOutList)
  })
  # (ii) Maps
  output$multiPlotsMapAndMapTitleContainer <- renderUI({
    mapAndMapTitleOutList <- lapply(seq_along(multiPlot()), function(iPlot) {
      mapId <- multiPlotsGenId(iPlot, type="map")
      mapTitleId <- multiPlotsGenId(iPlot, type="mapTitle")
      mapAndMapTitleOutput(mapId, mapTitleId)
    })
    do.call(tagList, mapAndMapTitleOutList)
  })
  # (iii) dataTables
  output$multiPlotsQueryAndTableContainer <- renderUI({
    queryAndDataTableOutList <- lapply(seq_along(multiPlot()), function(iPlot){
      queryUsedId <- multiPlotsGenId(iPlot, type="queryUsed")
      dataTableId <- multiPlotsGenId(iPlot, type="dataTable")
      queryUsedAndDataTableOutput(queryUsedId, dataTableId)
    })
    do.call(tagList, queryAndDataTableOutList)
  })

  # Assign each plot/map/title/query/table to the respective outputs
  multiPlotsPrevQuantity <- reactiveVal()
  observeEvent(multiPlot(), {
    # Clean up old multiPlot outputs
    for(iPlot in seq(multiPlotsPrevQuantity())) {
      for(type in c("plot", "map", "mapTitle", "queryUsed", "dataTable")) {
        outId <- multiPlotsGenId(iPlot, type=type)
        output[[outId]] <- NULL
      }
    }
    multiPlotsPrevQuantity(length(multiPlot()))
    gc()

    # Assign the new multiPlots
    for(iPlot0 in seq_along(multiPlot())) {
      local({
        iPlot <- iPlot0
        pName <- multiPlotsGenId(iPlot)
        # Assign plots
        plotOutId <- multiPlotsGenId(iPlot, type="plot")
        output[[plotOutId]] <- renderPlot({
          myPlot <- multiPlot()[[pName]]
          grid.arrange(req(myPlot$obplot),top=textGrob(myPlot$title))
        },
           res=96, pointsize=18
        )
        # Assign maps and map titles
        mapId <- multiPlotsGenId(iPlot, type="map")
        mapTitleId <- multiPlotsGenId(iPlot, type="mapTitle")
        output[[mapId]] <- renderLeaflet(req(multiPlot()[[pName]]$obmap))
        output[[mapTitleId]] <- renderText(req(multiPlot()[[pName]]$title))
        # Assign queryUsed and dataTable
        queryUsedId <- multiPlotsGenId(iPlot, type="queryUsed")
        dataTableId <- multiPlotsGenId(iPlot, type="dataTable")
        output[[queryUsedId]] <- renderText(req(multiPlot()[[pName]]$queryUsed))
        output[[dataTableId]] <- renderDataTable(
          req(multiPlot()[[pName]]$plotData), options=list(pageLength=10)
        )
      })
    }
  })

  ############################################################################
  #            Add user guide tab if pdf file for guide exists               #
  ############################################################################
  docPath <- file.path("docs", "obsmon_documentation.pdf")
  if(file.exists(docPath)) {
    appendTab("appNavbarPage",
      tabPanel(
        title="Documentation",
        value="docTab",
        docTab(docPath)
      )
    )
  }

}) # End of shinyServer
