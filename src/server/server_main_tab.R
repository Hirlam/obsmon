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
  if(length(choices)==0) disableShinyInputs(input, except="experiment")
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
  if(length(cycles)==0) cycles <- sprintf("%02d", 0:24)
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
observeEvent(input$cyclesSelectAny, {
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
observeEvent(input$levelsSelectAny, {
  updateSelectInput(session, "levels",
    choices=availableLevels()$all, selected=character(0))
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
observeEvent(input$channelsSelectAny, {
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
  useStationSingle <- allowChoosingStation() && requireSingleStation()
  useStationMulti <- allowChoosingStation() && !requireSingleStation()

  shinyjs::toggleState("station", condition=useStationMulti)
  shinyjs::toggleElement("station", condition=useStationMulti)
  shinyjs::toggleState("stationSingle", condition=useStationSingle)
  shinyjs::toggleElement("stationSingle", condition=useStationSingle)
})

# Update stations
updateStations <- reactive({
  allowChoosingStation()
  reloadInfoFromCache()
  input$obtype
  input$obname
  input$variable
}) %>% throttle(100)
observeEvent(updateStations(), {
  if(!allowChoosingStation()) return(c("Any"=""))
  req(allowChoosingStation())

  db <- req(activeDb())
  dates <- req(selectedDates())
  cycles <- req(selectedCycles())
  obname <- req(input$obname)
  variable <- req(input$variable)

  stations <- getStationsFromCache(db, dates, cycles, obname, variable)
  stations <- putLabelsInStations(stations, obname)

  stationsAvailable <- length(stations)>0

  notFullyCachedMsg <- NULL
  if(!selectedDtgsAreCached()) {
    notFullyCachedMsg <- "(cache info not available)"
    if(stationsAvailable) notFullyCachedMsg<-"(cache info incomplete)"
  }

  if(requireSingleStation()) {
    inputName <- "stationSingle"
    label <- gsub(" $", "", paste("Station", notFullyCachedMsg))
    if(!stationsAvailable) stations <- c("No stations available to choose"="")
  } else {
    inputName <- "station"
    label <- "Station"
    entryForAnyStation <- c("")
    names(entryForAnyStation) <- gsub(" $","",paste("Any",notFullyCachedMsg))
    stations <- c(entryForAnyStation, stations)
  }
  updateSelectInputWrapper(session, inputName, choices=stations, label=label)

  if(!selectedDtgsAreCached()) delay(1000, triggerReadCache())
},
  ignoreNULL=TRUE
)