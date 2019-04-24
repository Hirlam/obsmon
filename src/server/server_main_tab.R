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
outputOptions(output, "showCacheOptions", suspendWhenHidden=FALSE)

# Initial separation between available and unavailable experiments.
avExpts <- reactiveVal()
unavExpts <- reactiveVal()
isolate({
  for(expt in expts) {
    if(isTRUE(expt$hasData)) avExpts(c(avExpts(), expt$name))
    else unavExpts(c(unavExpts(), expt$name))
  }
})
exptChoices <- reactive(list(av=avExpts(),unav=unavExpts())) %>% throttle(500)

observeEvent(exptChoices(), {
  choices <- vector("list", length=2)
  # Present expt choices in same order as written in config file
  for(expt in expts) {
    newExptEntry <- list(expt$name); names(newExptEntry) <- expt$guiName
    if(expt$name %in% avExpts()) choices[[1]] <- c(choices[[1]], newExptEntry)
    else choices[[2]] <- c(choices[[2]], newExptEntry)
  }

  pHolder <- "Please select experiment"
  if(length(choices[[1]])==0) {
    pHolder <- "ERROR: No experiment data found!"
    if(length(choices[[2]])==0) pHolder <- "ERROR: No experiment configured!"
    choices[[1]] <- list(" "); names(choices[[1]]) <- pHolder
  }
  names(choices) <- c(" ", "Unavailable experiments")

  selectedExpt <- input$experiment
  if(selectedExpt=="") selectedExpt <- NULL
  updateSelectizeInput(session, "experiment",
    choices=choices, options=list(placeholder=pHolder),
    selected=selectedExpt
  )
})

# Hide "Loading Obsmon" screen and show the app
shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")
shinyjs::show("app-content")

# Update database options according to chosen experiment
observeEvent(input$experiment, {
  disableShinyInputs(input, except=c("experiment", "^multiPlots*"))
  expt <- expts[[input$experiment]]
  choices <- list()
  for(db in expt$dbs) {
    if(!isTRUE(db$hasData)) next
    choices[[dbType2DbDescription(db$dbType)]] <- db$dbType
  }
  updateSelectInputWrapper(session, "odbBase", choices=choices)

  # Update experiment choices if disponibility changes
  foundUsableDb <- length(choices)>0
  if(foundUsableDb) {
    if(req(expt$name) %in% unavExpts()) {
      unavExpts(unavExpts()[!(unavExpts()==expt$name)])
      avExpts(c(avExpts(), expt$name))
    }
    enableShinyInputs(input, except="^multiPlots*")
  } else if(req(expt$name) %in% avExpts()) {
    avExpts(avExpts()[!(avExpts()==expt$name)])
    unavExpts(c(unavExpts(), expt$name))
  }
})
activeDb <- reactive(req(expts[[input$experiment]])$dbs[[req(input$odbBase)]])

# DTG-related reactives and observers
# Update available choices of dates when changing active database
observeEvent(activeDb(), {
  db <- req(activeDb())
  dbDateRange <- req(db$dateRange)
  dbMinDate <- dbDateRange[1]
  dbMaxDate <- dbDateRange[2]
  start <- clamp(input$dateRange[1], dbMinDate, dbMaxDate, dbMinDate)
  end <- clamp(input$dateRange[2], dbMinDate, dbMaxDate)
  single <- clamp(input$date, dbMinDate, dbMaxDate)
  updateDateRangeInput(
    session, "dateRange", start=start, end=end, min=dbMinDate, max=dbMaxDate
  )
  updateDateInput(session, "date", value=single, min=dbMinDate, max=dbMaxDate)
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
    if(length(cycles)==0) cycles <- sprintf("%02d", 0:24)
  } else {
    cycles <- input$cycle
    if(trimws(cycles)=="") cycles <- NULL
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
  selectedDtgs(dtgs)
})


# Update available cycle choices when relevant fields change
availableCycles <- reactive({
  req(activeDb())$getAvailableCycles(req(selectedDates()))
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
    updateSelectInputWrapper(session,"obtype",choices=c("Surface"="surface"))
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
