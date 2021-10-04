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
  updatePickerInput(session, "experiment", choices=exptChoices())
},
  ignoreNULL=FALSE
)

# Update dB choices for currently selected experiment
observeEvent(input$experiment, {
  expt <- req(expts[[input$experiment]])
  choices <- unlist(lapply(expt$dbs, function(db) {
    if(isTRUE(dir.exists(db$dir))) db$dbType
  }))
  names(choices) <- unlist(lapply(choices, dbType2DbDescription))
  exptHasData <- TRUE
  if(length(choices)==0) {
    choices <- c("ERROR: No usable database!"=" ")
    exptHasData <- FALSE
  }
  updatePickerInputWrapper(session, "odbBase", choices=choices)

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
  req(input$odbBase)
  req(input$experiment)
  showNotification(id="notifIDUpdDbs",
    ui="Retrieving Db info...", type="message", duration=NULL
  )
  # Make sure user cannot request plots before we certify later on that there
  # are available DTGs
  disableShinyInputs(input, except=c("experiment", "odbBase", "^multiPlots*"))
  expts[[input$experiment]]$dbs[[input$odbBase]]
}) %>% throttle(100)


# DTG-related reactives and observers
# Update available choices of dates when changing active database
observeEvent(activeDb(), {
  on.exit(removeNotification("notifIDUpdDbs"))
  dbMinDate <- Sys.Date()
  dbMaxDate <- dbMinDate
  single <- NULL
  start <- NULL
  end <- NULL
  errMsg <- character(0)
  hasDtgs <- activeDb()$hasDtgs
  if(isTRUE(hasDtgs)) {
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
isMultiDtgInput <- reactive(isTRUE(req(activePlotType()$dateType) == "range"))
# Offer single date or dateRange input according to selected plottype
# Used to be done via conditionalPanel in ui.R, but that was slow
observeEvent(isMultiDtgInput(), {
  shinyjs::toggle(selector=".single_dtg_inputs", condition=!isMultiDtgInput())
  shinyjs::toggle(selector=".multiple_dtg_inputs", condition=isMultiDtgInput())
})

# Keep track of date(s), cycle(s) and consequently DTG(s) selected in the UI
# and store them in a convenient order and format
selectedDates <- reactiveVal()
observeEvent({
  input$date
  input$dateRange
  isMultiDtgInput()
 }, {
  if(isMultiDtgInput()) {
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
  isMultiDtgInput()
 }, {
  if(isMultiDtgInput()) {
    cycles <- input$cycles
    if(length(cycles)==0) cycles <- sprintf("%02d", 0:24)
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
  selectedDtgs(dtgs)
})

# Update available cycle choices when relevant fields change
availableCycles <- reactive({
  tryCatch(
    activeDb()$getAvailableCycles(req(selectedDates())),
    error=function(w) character(0)
  )
})
observe({
  updatePickerInputWrapper(session, "cycle", choices=availableCycles())
  updatePickerInputWrapper(session, "cycles", choices=availableCycles())
})


# Initialise some cache-related vars used here but for which proper reactives
# will be set up in server_cache.R.
# Flag that it's time to reload info from cache
cacheIsOngoing <- reactiveVal(FALSE)
reloadInfoFromCache <- reactive({
  activeDb()
  selectedDtgs()
  # Use "req" to react only after finished writing to cache files
  return(req(!cacheIsOngoing()))
}) %>% throttle(1000)

# Keep track of whether data for the selected DTGs are cached or not
selectedDtgsAreCached <- reactive({
  req(reloadInfoFromCache())
  dtgsAreCached(activeDb(), selectedDtgs())
})


# Update obtype
observeEvent(reloadInfoFromCache(), {
  db <- activeDb()
  if(isTRUE(db$dbType=="ecma_sfc")) {
    choices <- c("Surface"="surface")
  } else {
    obtypes <- getObtypes(db, selectedDates(), selectedCycles())
    isCached <- selectedDtgsAreCached() && length(obtypes$cached)>0
    if(isCached) {
      choices <- obtypes$cached
    } else {
      choices <- combineCachedAndGeneralChoices(obtypes)
    }
  }
  updatePickerInputWrapper(session, "obtype", choices=choices)
})

# Update obnames
updateObnames <- reactive({
  req(input$obtype)
  reloadInfoFromCache()
}) %>% throttle(500)
observeEvent(updateObnames(), {
  if(input$obtype=="satem") {
    updatePickerInputWrapper(session, "obname", choices=c("satem"))
    return()
  }
  obsCategory <- input$obtype
  db <- req(activeDb())

  obnames <- getObnames(db, obsCategory, selectedDates(), selectedCycles())
  isCached <- selectedDtgsAreCached() && length(obnames$cached)>0
  if(isCached) {
    newChoices <- obnames$cached
  } else {
    newChoices <- combineCachedAndGeneralChoices(obnames)
  }
  updatePickerInputWrapper(session, "obname", choices=newChoices)
})

# Update scatt satnames
updateScattSatellite <- reactive({
  req(input$obtype=='scatt')
  reloadInfoFromCache()
}) %>% throttle(500)
observeEvent(updateScattSatellite(), {
  db <- req(activeDb())

  sats <- getAvailableScattSatnames(db, selectedDates(), selectedCycles())
  isCached <- selectedDtgsAreCached() && length(sats$cached)>0
  if(isCached) {
    newChoices <- sats$cached
  } else {
    newChoices <- combineCachedAndGeneralChoices(sats)
  }
  updatePickerInputWrapper(session, "scatt_satellite", choices=newChoices)
})

# Update variable
updateVariables <- reactive({
  req(input$obname)
  reloadInfoFromCache()
  if(input$obtype == 'scatt') {
    updateScattSatellite()
    req(input$scatt_satellite)
  } else {
    TRUE
  }
}) %>% throttle(500)
observeEvent(updateVariables(), {
  db <- req(activeDb())

  satname <- NULL
  if(input$obtype == "satem") {
    satname <- input$satellite
  } else if(input$obtype=='scatt') {
    satname <- req(input$scatt_satellite)
  }

  variables <- getVariables(
      db, selectedDates(), selectedCycles(), input$obname, satname
  )
  isCached <- selectedDtgsAreCached() && length(variables$cached)>0
  if(isCached) {
    newChoices <- variables$cached
  } else {
    newChoices <- combineCachedAndGeneralChoices(variables)
  }
  updatePickerInputWrapper(session, "variable", choices=newChoices)
})

# Update and validate variable units input
observeEvent(input$variable, {
  req(length(input$variable)>0 && input$variable != "")
  defaultUnits <- getUnits(input$variable)
  if(length(defaultUnits)==0) defaultUnits <- "unitless"
  updateTextInput(
    session, "variableUnits",
    value=character(0),
    placeholder=as.character(defaultUnits)
  )
})

variableUnits <- reactiveVal(character(0))
observe({
  req(length(input$variableUnits)==0 || input$variableUnits=="")
  variableUnits(character(0))
})

validateVarUnits <- reactive({
  req(length(input$variable)>0 && input$variable != "")
  req(length(input$variableUnits)>0 && input$variableUnits != "")
}) %>% debounce(1250)

observeEvent(validateVarUnits(), {
  tryCatch({
    testValue <- 1
    units(testValue) <- getUnits(input$variable)
    units(testValue)  <- input$variableUnits
    variableUnits(input$variableUnits)
  },
    error=function(e) {
      showNotification(
        ui=paste0(gsub("\\..*","", e$message), "!"),
        type="error",
        duration=2
      )
      updateTextInput(session, "variableUnits", value=variableUnits())
    }
  )
})

# Update sensornames
updateSensor <- reactive({
  req(input$obtype=="satem")
  reloadInfoFromCache()
})  %>% throttle(500)
observeEvent(updateSensor(), {
  db <- req(activeDb())
  sens <- getAvailableSensornames(db, selectedDates(), selectedCycles())
  isCached <- selectedDtgsAreCached() && length(sens$cached)>0
  if(isCached) {
    newChoices <- sens$cached
  } else {
    newChoices <- combineCachedAndGeneralChoices(sens)
  }
  updatePickerInputWrapper(session, "sensor", choices=newChoices)
})

# Update satellite choices for given sensor
updateSatellite <- reactive({
  req(input$sensor)
  updateSensor()
})  %>% throttle(500)
observeEvent(updateSatellite(), {
  db <- req(activeDb())
  sens <- input$sensor

  sats <- getAvailableSatnames(db, selectedDates(), selectedCycles(), sens)
  isCached <- selectedDtgsAreCached() && length(sats$cached)>0
  if(isCached) {
    newChoices <- sats$cached
  } else {
    newChoices <- combineCachedAndGeneralChoices(sats)
  }
  updatePickerInputWrapper(session, "satellite", choices=newChoices)
})

# Update channel choice for given satellite
updateChannels <- reactive({
  req(input$satellite)
  updateSatellite()
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
  return(newChannels)
})
observe(updatePickerInputWrapper(session, "channels", choices=channels()))

# Update plottype choices according to criteria
updatePlotType <- reactive({
  input$obtype
  input$obname
  req(length(input$variable)>0 ||
    (length(input$satellite) * length(input$sensor)>0)
  )
}) %>% throttle(500)
observeEvent(updatePlotType(), {
  choices <- plotRegistry$getCategorisedPlotTypeNames(
    compatibleWithUiInputParams=input
  )
  updatePickerInputWrapper(session, "plottype", choices=choices)
})
activePlotType <- reactive(plotRegistry$plotTypes[[req(input$plottype)]])

# Decide whether to allow users to select stations
allowChoosingStation <- reactive({
   return(
     obSupportsStationChoice(req(input$obname)) &&
     activePlotType()$supportsStationSelection
   )
})
observe(shinyjs::toggleElement("station", condition=allowChoosingStation()))

requireSingleStation <- reactive(activePlotType()$requiresSingleStation)
observeEvent(requireSingleStation(), {
  req(allowChoosingStation())
  updatePickerInputWrapper(
    session, "station",
    selected=character(0),
    options=list(
      `max-options`=ifelse(requireSingleStation(), 1, FALSE),
      `actions-box`=!requireSingleStation(),
      `none-selected-text`=ifelse(requireSingleStation(), "Select station", "Any")
    )
  )
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
  }
  req(allowChoosingStation())

  db <- req(activeDb())
  dates <- req(selectedDates())
  cycles <- selectedCycles()
  obname <- req(input$obname)
  variable <- req(input$variable)

  satname = NULL
  if(input$obtype=='scatt') satname = req(input$scatt_satellite)

  stations <- getStationsFromCache(
      db, dates, cycles, obname, variable, satname=satname
  )
  stations <- putLabelsInStations(stations, obname)
  updatePickerInputWrapper(session, "station", choices=stations)
})

# Update level choices for selected station(s) and variable
# Defining availableLevels as an eventReactive was causing an issue
# that could leave a blank Levels field on the UI upon page refresh
defaultLevelsUnits <- reactiveVal(NULL)
observe({
  req(length(input$variable)>0 && input$variable != "")
  defaultLevelsUnits(getUnitsForLevels(
    obname=req(input$obname),
    varname=input$variable
  ))
})
observeEvent(req(defaultLevelsUnits()), {
  defaultUnits <- defaultLevelsUnits()
  if(length(defaultUnits)==0) defaultUnits <- "unitless"
  updateTextInput(
    session, "levelsUnits",
    placeholder=as.character(defaultUnits)
  )
})

availableLevels <- reactiveVal(NULL)
updateLevels <- reactive({
  req(input$variable)
  input$station
  updateVariables()
  reloadInfoFromCache()
  activePlotType()
  if(length(availableLevels())==0) invalidateLater(500)
}) %>% throttle(500)
observeEvent({
  updateLevels()
  }, {
  db <- activeDb()
  obname <- input$obname
  var <- input$variable

  stations <- NULL
  if(allowChoosingStation()) stations <- input$station
  levels <- getLevelsFromCache(
    db, selectedDates(), selectedCycles(), obname, var, stations
  )

  # Toggle the choice between all levels or standard-only
  queryFromUsageDbTable <- activePlotType()$queriedDbTable == "usage"
  retrievedDataHasLevels <- "level" %in% activePlotType()$getRetrievedSqliteFields()
  usageHasStandardLevels <- length(intersect(levels$obsmon, levels$usage)) > 0
  allUsageLevelsAreStandard <- all(levels$usage %in% levels$obsmon)
  shinyjs::toggle(
    "standardLevelsSwitch",
    condition=queryFromUsageDbTable && usageHasStandardLevels && !allUsageLevelsAreStandard
  )
  shinyjs::toggle(
    "groupLevelsIntoStandardSwitch",
    condition=retrievedDataHasLevels && queryFromUsageDbTable && !allUsageLevelsAreStandard
  )

  availableLevels(levels)
}, ignoreNULL=FALSE)

levelsUnitsChanged <- reactive(input$levelsUnits) %>% debounce(1250)
observeEvent({
  availableLevels()
  input$standardLevelsSwitch
  levelsUnitsChanged()
  activePlotType()
}, {
    queryFromUsageDbTable <- activePlotType()$queriedDbTable == "usage"
    if(queryFromUsageDbTable) {
      availableStandardLevels <- intersect(
        availableLevels()$obsmon,
        availableLevels()$usage
      )
      if(isTRUE(input$standardLevelsSwitch)) {
        choices <- availableStandardLevels
      } else {
        choices <- unique(c(availableStandardLevels, availableLevels()$usage))
      }
    } else {
      choices <- availableLevels()$obsmon
    }

    # Present level choices in the units picked by the user, but make sure
    # to pass it to the query with the expected (default) units
    if(length(choices)>0 && length(input$levelsUnits)>0 && input$levelsUnits != "") {
      choicesWithPickedUnits <- as.numeric(choices)
      units(choicesWithPickedUnits) <- defaultLevelsUnits()
      tryCatch({
        units(choicesWithPickedUnits) <- input$levelsUnits
        names(choices) <- choicesWithPickedUnits
      },
        error=function(e) NULL
      )
    }

    updatePickerInputWrapper(session, "levels", choices=choices)
}, ignoreNULL=FALSE)

# Update and validate levelsUnits input
levelsUnits <- reactiveVal()
observe({
  req(length(input$levelsUnits)==0 || input$levelsUnits=="")
  levelsUnits(character(0))
})
validateLevelUnits <- reactive({
  req(defaultLevelsUnits())
  req(length(input$levelsUnits)>0 && input$levelsUnits != "")
}) %>% debounce(1250)
observeEvent(validateLevelUnits(), {
  tryCatch({
    testValue <- 1
    units(testValue) <- defaultLevelsUnits()
    units(testValue)  <- input$levelsUnits
    levelsUnits(input$levelsUnits)
  },
    error=function(e) {
      showNotification(
        ui=paste0(gsub("\\..*","", e$message), "!"),
        type="error",
        duration=2
      )
      updateTextInput(session, "levelsUnits", value=levelsUnits())
    }
  )
})
