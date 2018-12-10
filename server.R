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

separateReadyAndCachingExpts <- function(experiments) {
  # Checks whether experiments have been initialised
  # Experiments which have not yet been initialised will be replaced by
  # empty ones (placeholders).
  # This allows using experiments that are ready even if there are others
  # that are not.
  rtn <- list()
  # Using suppressWarnings because the "future" package started to issue
  # loads of "cannot wait for child xxx as it does not exist" warnings
  # after R was upgraded to v3.5. For more info, see, e.g.,
  # <https://github.com/HenrikBengtsson/future/issues/218>
  resolvedStatus <- suppressWarnings(resolved(experiments))

  readyExpts <- list()
  stillCachingExpts <- list()
  exptNames <- exptNamesinConfig[exptNamesinConfig %in% ls(experiments)]
  for (exptName in exptNames) {
    if(resolvedStatus[[exptName]]) {
      # Using suppressWarnings to mitigate "Rv3.5 + future" issue
      # See previous comment.
      suppressWarnings(readyExpts[[exptName]] <- experiments[[exptName]])
    } else {
      newName <- paste0(exptName, ': Loading experiment...')
      stillCachingExpts[[newName]] <- emptyExperiment(newName)
    }
  }
  return(c(readyExpts, stillCachingExpts))
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

getCurrentDateType <- function(input) {
  rtn <- tryCatch(
    plotTypesFlat[[req(input$plottype)]]$dateType,
    error=function(e) NA,
    warning=function(w) NA
  )
  return(rtn)
}

getCurrentDatesAndCycles <- function(input) {
  dateType <- getCurrentDateType(input)
  if(dateType %in% c("range")) {
    dates <- input$dateRange
    cycles <- input$cycles
  } else {
    dates <- input$date
    cycles <- input$cycle
  }
  return(list(dates=dates, cycles=cycles))
}

shinyServer(function(input, output, session) {
  # Start GUI with all inputs disabled.
  # They will be enabled once experiments are loaded
  isolate(disableShinyInputs(input, except="experiment"))

  # Initial population of experiments; triggers cascade for other form fields
  exptNames <- c("")
  experiments <- reactive ({
    # Keep checking for updates in the experiments. Useful when chaching.
    invalidateLater(5000, session)
    separateReadyAndCachingExpts(experimentsAsPromises)
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

  levelChoices <- list()
  levelChoicesObsmonTable <- list()
  levelChoicesUsageTable <- list()
  channelChoices <- list()
  channelChoicesObsmonTable <- list()
  channelChoicesUsageTable <- list()

  # Update database options according to chosen experiment
  observe({
    expName <- req(input$experiment)
    expDbs <- isolate(experiments()[[expName]]$dbs)
    dbName2DbDescription <- list(
      "ecma"="Upper Air (3D/4D-VAR) - Screening",
      "ccma"="Upper Air (3D/4D-VAR) - Minimization",
      "ecma_sfc"="Surface (CANARI)"
    )
    choices <- list()
    for(dbName in names(dbName2DbDescription)) {
      if(is.null(expDbs[[dbName]])) next
      choices[[dbName2DbDescription[[dbName]]]] <- dbName
    }

    updateSelectizeInput(session, "odbBase", choices=choices)
    if(length(choices)==0) disableShinyInputs(input, except=c("experiment"))
    else enableShinyInputs(input)
  })

  activeDb <- reactive({
    expName <- req(input$experiment)
    dbName <- req(input$odbBase)
    isolate(experiments()[[expName]]$dbs[[dbName]])
  })

  # Update date related fields dateRange, date, and cycle with new experiment
  observeEvent(activeDb(), {
    db <- activeDb()
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
    updateSelection(session, "cycle", db$cycles)
    updateCheckboxGroup(session, "cycles", db$cycles)
  })

  observeEvent(input$cyclesSelectAll, {
    db <- activeDb()
    updateCheckboxGroup(session, "cycles", db$cycles, "ALL")
  })

  observeEvent(input$cyclesSelectNone, {
    db <- activeDb()
    updateCheckboxGroup(session, "cycles", db$cycles, "NONE")
  })

  # Offer single date or dateRange input according to selected plottype
  output$dateType <- reactive({
    plotType <- plotTypesFlat[[req(input$plottype)]]
    plotType$dateType
  })
  outputOptions(output, 'dateType', suspendWhenHidden=FALSE)

  # Put observations in cache when a date/dateRange is selected
  observeEvent({
      activeDb()
      input$date
      input$dateRange
    }, {
      db <- activeDb()
      dateType <- getCurrentDateType(input)
      if(dateType %in% c("range")) {
        startDtg <- 100 * date2dtg(input$dateRange[[1]])
        endDtg <- 100 * date2dtg(input$dateRange[[2]]) + 24
        dtgs <- sort(
          db$dtgs[((startDtg <= db$dtgs) & (db$dtgs <= endDtg))],
          decreasing=TRUE
        )
        fPathsToCache <- db$paths[as.character(dtgs)]
      } else {
        datePatt <- paste0('^',date2dtg(input$date),'{1}[0-9]{2}')
        dtgs <- sort(grep(datePatt, db$dtgs, value=TRUE), decreasing=TRUE)
        fPathsToCache <- db$paths[as.character(dtgs)]
      }
      assyncPutObsInCache(fPathsToCache, cacheDir=db$cacheDir)
  })

  # Update obtype with choices for given experiment and database
  observeEvent({
      activeDb()
      input$date
      input$dateRange
      input$cycle
      input$cycles
    }, {
    db <- activeDb()
    datesCycles <- getCurrentDatesAndCycles(input)
    obtypes <- getObtypes(db, datesCycles$dates, datesCycles$cycles)
    if(is.null(obtypes$cached)) {
      updateSelection(session, "obtype", obtypes$general)
      updateSelectInput(session, "obtype", label="Observation Type (not cached)")
    } else {
      updateSelection(session, "obtype", obtypes$cached)
      updateSelectInput(session, "obtype", label="Observation Type (cached)")
    }
  })
  # Update obnames with choices for given experiment, database and obtype
  observeEvent({
      activeDb()
      input$obtype
      input$date
      input$dateRange
      input$cycle
      input$cycles
    }, {
    obsCategory <- req(input$obtype)
    db <- activeDb()
    datesCycles <- getCurrentDatesAndCycles(input)

    if(obsCategory=="satem") {
      sens.sats <- getAttrFromMetadata('sensors.sats', category=obsCategory)
      sens <- gsub('\\.{1}.*', '', sens.sats)
      updateSelection(session, "sensor", sens)
      updateSelection(session, "obname", c("satem"))
    } else {
      obnames <- getObnames(db, obsCategory, datesCycles$dates, datesCycles$cycles)
      if(is.null(obnames$cached)) {
        updateSelection(session, "obname", obnames$general)
        updateSelectInput(session, "obname", label="Observation Name (not cached)")
      } else {
        updateSelection(session, "obname", obnames$cached)
        updateSelectInput(session, "obname", label="Observation Name")
      }
    }
  })

  # Update sensor for satem obname, variable else
  observe({
    db <- activeDb()
    obname <- req(input$obname)
    datesCycles <- getCurrentDatesAndCycles(input)

    variables <- getVariables(db,datesCycles$dates,datesCycles$cycles,obname)
    if(is.null(variables$cached)) {
      updateSelection(session, "variable", variables$general)
      updateSelectInput(session, "variable", label="Variable (not cached)")
    } else {
      updateSelection(session, "variable", variables$cached)
      updateSelectInput(session, "variable", label="Variable")
    }

    stationChoices <- db$stations[[obname]]
    updateSelectizeInput(session, "station", stationChoices)
  })

  # Update satellite choices for given sensor
  observeEvent({
    input$sensor
    }, {
    sens <- req(input$sensor)
    obsCategory <- req(input$obtype)
    db <- activeDb()
    # TODO: Get sen.sats from cache as well, if available
    sens.sats <- getAttrFromMetadata('sensors.sats', category=obsCategory)
    sens.sats <- sens.sats[startsWith(sens.sats, paste0(sens, '.'))]
    sats <- gsub(paste0(sens, '.'), '', sens.sats, fixed=TRUE)
    updateSelection(session, "satellite", sats)
  })

  # Update channel choice for given satellite
  observeEvent({
    input$satellite
    },{
    db <- activeDb()
    sat <- req(input$satellite)
    sens <- req(input$sensor)
    datesCycles <- getCurrentDatesAndCycles(input)
    channels <- getAvailableChannels(db, datesCycles$dates, datesCycles$cycles,
      satname=sat, sensorname=sens
    )
    updateSelection(session, "channels", channels)
  })

  observeEvent(input$channelsSelectAll, {
    updateSelection(session, "channels", channelChoices, "ALL")
  })

  observeEvent(input$channelsSelectNone, {
    updateSelection(session, "channels", channelChoices, "NONE")
  })

  # Update level choice for given variable
  observe({
    obname <- req(input$obname)
    if (obname != "satem") {
      db <- activeDb()
      var <- req(input$variable)
      levelChoicesObsmonTable <<- db$obnames[[obname]][[var]]$levelsObsmon
      levelChoicesUsageTable <<- db$obnames[[obname]][[var]]$levelsUsage
      levelChoices <<- unique(c(levelChoicesObsmonTable, levelChoicesUsageTable))
      updateSelection(session, "levels", levelChoices)
    }
  })

  observeEvent(input$levelsSelectStandard, {
    updateSelectInput(session, "levels",
                      choices=levelChoices, selected=levelChoicesObsmonTable)
  })

  observeEvent(input$levelsSelectAll, {
    updateSelection(session, "levels", levelChoices, "ALL")
  })

  observeEvent(input$levelsSelectNone, {
    updateSelection(session, "levels", levelChoices, "NONE")
  })

  # Build named list of criteria
  buildCriteria <- function() {
    exp <- isolate(experiments()[[req(input$experiment)]])
    db <- req(input$odbBase)
    adb <- activeDb()
    res <- list()
    res$info <- list()
    obname <- req(input$obname)
    res$obnumber <- getAttrFromMetadata('obnumber', obname=obname)
    if (obname == 'satem') {
      sensor <- req(input$sensor)
      #res$obnumber <- adb$obnumbers[[sensor]]
      res$obname <- sensor
      res$satname <- req(input$satellite)
      if (!is.null(input$channels)) {
        res$levels <- input$channels
      } else {
        res$levels <- channelChoices
      }
    } else {
      #res$obnumber <- adb$obnumbers[[obname]]
      res$obname <- obname
      res$varname <- req(input$variable)
      if (!is.null(input$levels)) {
        res$levels <- input$levels
      } else if (!is.null(levelChoices)) {
        res$levels <- levelChoices
      } else {
        res$levels <- list()
      }
    }
    if (!(input$station=="" | is.null(input$station) | is.na(input$station))){
      station <- input$station
      res$station <- station
      label <- exp$stationLabels[[adb$name]][[obname]][[station]]
      res$info$stationLabel <- ifelse(is.null(label), as.character(station), label)
    }
    res
  }

  # Turn criteria into reactive expression so they can trigger plottype update
  criteria <- eventReactive(
  {
    input$obname
    input$sensor
    input$satellite
    input$channels
    input$variable
    input$levels
    input$station
  },
  {
    buildCriteria()
  })

  criteriaDebounced <- criteria %>% debounce(200)

  # Update plottype choices with available plottypes according to criteria
  updatePlotTypes <- function() {
    criteria <- buildCriteria()
    choices <- applicablePlots(criteria)
    updateSelection(session, "plottype", choices)
  }

  # Trigger plottype update on criteria change
  observeEvent(criteriaDebounced(), {
    updatePlotTypes()
  })
  observeEvent(criteria(), {
    updatePlotTypes()
  }, once=TRUE)

  # Perform plotting
  observeEvent(input$doPlot, {
    t <- createShinyProgressTracker()
    plotRequest <- list()
    plotter <- plotTypesFlat[[req(input$plottype)]]
    plotRequest$expName <- req(input$experiment)
    db <- activeDb()
    plotRequest$dbName <- db$name
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
