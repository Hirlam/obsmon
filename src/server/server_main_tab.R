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

# Populate experiment choices
exptChoices <- reactiveVal(getNewExptChoices())
observeEvent(exptChoices(), {
  updateSelectizeInput(session, "experiment", choices=exptChoices())
},
  ignoreNULL=FALSE
)

# Update dB choices for currently selected experiment
observeEvent(input$experiment, {
  expt <- expts[[input$experiment]]
  choices <- unlist(lapply(expt$dbs, function(db) {
    if(isTRUE(dir.exists(db$dir))) db$dbType
  }))
  names(choices) <- unlist(lapply(choices, dbType2DbDescription))
  exptHasData <- TRUE
  if(length(choices)==0) {
    choices <- c("ERROR: No usable database!"=" ")
    exptHasData <- FALSE
  }
  updateSelectInputWrapper(session, "odbBase", choices=choices)

  # Refresh expt choices to reflect their available/unavailable status
  if(!exptHasData) {
    newExptChoices <- getNewExptChoices(exptChoices(), markAsUnav=expt$name)
    if(length(expt$name)>0) {
      showNotification(
        ui=sprintf('Expt "%s" unavailable',expt$name),type="error",duration=2
      )
    }
  } else {
    newExptChoices <- getNewExptChoices(exptChoices(), markAsAv=expt$name)
  }
  exptChoices(newExptChoices)
})
activeDb <- reactive({
  showNotification(id="notifIDUpdDbs",
    ui="Retrieving Db info...", type="message", duration=NULL
  )
  # Make sure user cannot request plots before we certify later on that there
  # are available DTGs
  disableShinyInputs(input, except=c("experiment", "odbBase", "^multiPlots*"))
  expts[[input$experiment]]$dbs[[req(input$odbBase)]]
}) %>% throttle(100)

# Hide "Loading Obsmon" screen and show the app
shinyjs::hide(id="loading-content", anim=FALSE, animType="fade")
shinyjs::show("app-content")

# DTG-related reactives and observers
# Update available choices of dates when changing active database
observeEvent(activeDb(), {
  on.exit(removeNotification("notifIDUpdDbs"))

  dbMinDate <- Sys.Date(); dbMaxDate <- dbMinDate
  single <- NA; start <- NA; end <- NA
  errMsg <- ""
  hasDtgs <- activeDb()$hasDtgs
  if(isTRUE(hasDtgs)) {
    errMsg <- character(0)
    dbDateRange <- activeDb()$dateRange
    dbMinDate <- dbDateRange[1]
    dbMaxDate <- dbDateRange[2]
    start <- clamp(input$dateRange[1], dbMinDate, dbMaxDate)
    end <- clamp(input$dateRange[2], dbMinDate, dbMaxDate)
    single <- clamp(input$date, dbMinDate, dbMaxDate)
    # Allow users to plot now that we have updated DTGs with valid values
    enableShinyInputs(input, except="^multiPlots*")
  } else if(isFALSE(hasDtgs)) {
    # Mind that isFALSE and isTRUE are not the opposite of each other
    errMsg <- sprintf("(%s has no DTGs!)", activeDb()$dbType)
  }
  labelSingle <- paste("Date", errMsg)
  labelRange <- paste("Date Range", errMsg)
  updateDateRangeInput(
    session, "dateRange", label=labelRange,
    start=start, end=end, min=dbMinDate, max=dbMaxDate
  )
  updateDateInput(
    session, "date", label=labelSingle,
    value=single, min=dbMinDate, max=dbMaxDate
  )
}, ignoreNULL=FALSE)

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
  tryCatch(
    activeDb()$getAvailableCycles(req(selectedDates())),
    error=function(w) character(0)
  )
})
observeEvent(availableCycles(), {
  updateSelectInputWrapper(session, "cycle", choices=availableCycles())
  updateCheckboxGroup(session, "cycles", availableCycles())
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
  selectedDtgsAreCached()
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
      newChoices <- combineCachedAndGeneralChoices(obtypes)
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
}) %>% throttle(500)
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
    newChoices <- combineCachedAndGeneralChoices(obnames)
  }
  updateSelectInputWrapper(
    session, "obname", choices=newChoices, choicesFoundIncache=isCached
  )
})

# Update scatt satnames
updateScattSatellite <- reactive({
  req(input$obtype=='scatt')
  reloadInfoFromCache()
})  %>% throttle(500)
observeEvent(updateScattSatellite(), {
  db <- req(activeDb())

  sats <- getAvailableScattSatnames(db, selectedDates(), selectedCycles())
  isCached <- selectedDtgsAreCached() && !is.null(sats$cached)
  if(isCached) {
    newChoices <- sats$cached
  } else {
    newChoices <- combineCachedAndGeneralChoices(sats)
  }
  updateSelectInputWrapper(
    session, "scatt_satellite",
    choices=newChoices, choicesFoundIncache=isCached
  )
})

# Update variable
updateVariables <- reactive({
  req(input$obtype!="satem")
  updateObnames()
  req(input$obname)
  if(input$obtype == 'scatt') {
    updateScattSatellite()
    req(input$scatt_satellite)
  } else {
    TRUE
  }
}) %>% throttle(500)
observeEvent(updateVariables(), {
  db <- req(activeDb())

  satname = NULL
  if(input$obtype=='scatt') satname = req(input$scatt_satellite)

  variables <- getVariables(
      db, selectedDates(), selectedCycles(), input$obname, satname
  )
  isCached <- selectedDtgsAreCached() && !is.null(variables$cached)
  if(isCached) {
    newChoices <- variables$cached
  } else {
    newChoices <- combineCachedAndGeneralChoices(variables)
  }
  updateSelectInputWrapper(
    session, "variable", choices=newChoices, choicesFoundIncache=isCached
  )
})

# Update sensornames
updateSensor <- reactive({
  reloadInfoFromCache()
  req(input$obtype=="satem")
})  %>% throttle(500)
observeEvent(updateSensor(), {
  db <- req(activeDb())
  sens <- getAvailableSensornames(db, selectedDates(), selectedCycles())
  isCached <- selectedDtgsAreCached() && !is.null(sens$cached)
  if(isCached) {
    newChoices <- sens$cached
  } else {
    newChoices <- combineCachedAndGeneralChoices(sens)
  }
  updateSelectInputWrapper(
    session, "sensor", choices=newChoices, choicesFoundIncache=isCached
  )
})

# Update satellite choices for given sensor
updateSatellite <- reactive({
  updateSensor()
  req(input$sensor)
})  %>% throttle(500)
observeEvent(updateSatellite(), {
  db <- req(activeDb())
  sens <- input$sensor

  sats <- getAvailableSatnames(db, selectedDates(), selectedCycles(), sens)
  isCached <- selectedDtgsAreCached() && !is.null(sats$cached)
  if(isCached) {
    newChoices <- sats$cached
  } else {
    newChoices <- combineCachedAndGeneralChoices(sats)
  }
  updateSelectInputWrapper(
    session, "satellite", choices=newChoices, choicesFoundIncache=isCached
  )
})

# Update channel choice for given satellite
updateChannels <- reactive({
  updateSatellite()
  req(input$satellite)
})  %>% throttle(500)
channels <- eventReactive(updateChannels(), {
  db <- req(activeDb())
  dates <- req(selectedDates())
  cycles <- req(selectedCycles())
  sat <- input$satellite
  sens <- input$sensor

  newChannels <- getChannelsFromCache(
    db, dates, cycles, satname=sat, sensorname=sens
  )
  if(length(newChannels)==0) {
    newChannels <- c("Any (cache info not available)"="")
  } else if(!selectedDtgsAreCached()) {
    newChannels <- c("Any (cache info incomplete)"="", newChannels)
  }

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
}) %>% throttle(500)
observeEvent(updatePlotType(), {
  choices <- applicablePlots(req(plotsBuildCriteria(input)))
  updateSelectInputWrapper(session, "plottype", choices=choices)
})

# Decide whether to allow users to select stations
allowChoosingStation <- reactive({
   return(
     obSupportsStationChoice(req(input$obname)) &&
     plotSupportsChoosingStations(req(input$plottype), req(input$obtype))
   )
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
  if(input$obtype=='scatt') {
    input$scatt_satellite
  } else {
    TRUE
  }
}) %>% throttle(500)
observeEvent(updateStations(), {
  if(!allowChoosingStation()) {
    updatePickerInputWrapper(session, "station", choices=c(""))
    updatePickerInputWrapper(session, "stationSingle", choices=c(""))
  }
  req(allowChoosingStation())

  db <- req(activeDb())
  dates <- req(selectedDates())
  cycles <- req(selectedCycles())
  obname <- req(input$obname)
  variable <- req(input$variable)

  satname = NULL
  if(input$obtype=='scatt') satname = req(input$scatt_satellite)

  stations <- getStationsFromCache(
      db, dates, cycles, obname, variable, satname=satname
  )
  stations <- putLabelsInStations(stations, obname)

  stationsAvailable <- length(stations)>0

  notFullyCachedMsg <- NULL
  if(!selectedDtgsAreCached()) {
    notFullyCachedMsg <- "(cache info not available)"
    if(stationsAvailable) notFullyCachedMsg<-"(cache info incomplete)"
  }

  inputName <- "station"
  if(requireSingleStation()) inputName <- "stationSingle"
  label <- gsub(" $", "", paste("Station", notFullyCachedMsg))

  # Lock input if there are no stations to be chosen
  shinyjs::toggleState(inputName, condition=stationsAvailable)

  updatePickerInputWrapper(session, inputName, choices=stations, label=label)
},
  ignoreNULL=TRUE
)
# Keep track of selected stations
selectedStations <- reactiveVal(character(0))
observeEvent({
  input$station
  input$stationSingle
}, {
  if(allowChoosingStation()) {
    if(requireSingleStation()) selectedStations(input$stationSingle)
    else selectedStations(input$station[trimws(input$station) != ""])
  } else {
    selectedStations(character(0))
  }
},
  ignoreNULL=TRUE
)

# Update level choices for selected station(s) and variable
# Defining availableLevels as an eventReactive was causing an issue
# that could leave a blank Levels field on the UI upon page refresh
availableLevels <- reactiveVal(NULL)
updateLevels <- reactive({
  selectedStations()
  updateVariables()
  req(input$variable)
  if(length(availableLevels())==0) invalidateLater(500)
}) %>% throttle(500)
observeEvent({
  updateLevels()
  }, {
  db <- activeDb()
  obname <- input$obname
  var <- input$variable

  stations <- NULL
  if(allowChoosingStation()) stations <- selectedStations()
  levels <- getLevelsFromCache(
    db, selectedDates(), selectedCycles(), obname, var, stations
  )

  # Toggle the choice between all levels or standard-only
  hasStandardLevels <- length(levels$obsmon) > 0
  allLevelsAreStandard <- all(levels$all %in% levels$obsmon)
  showStandardLevelsToggle <- hasStandardLevels && !allLevelsAreStandard
  shinyjs::toggle("standardLevelsSwitch", condition=showStandardLevelsToggle)

  availableLevels(levels)
}, ignoreNULL=FALSE, ignoreInit=FALSE)

observeEvent({
  availableLevels()
  input$standardLevelsSwitch
  selectedDtgsAreCached()
}, {
    if(isTRUE(input$standardLevelsSwitch)) choices <- availableLevels()$obsmon
    else choices <- availableLevels()$all
    updatePickerInputWrapper(session, "levels", choices=choices)
}, ignoreNULL=FALSE)
