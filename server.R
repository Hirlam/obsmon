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
  # Checks whether caching of each experiment has finished.
  # Experiments for which caching is finished will be returned, and
  # empty, placeholder experiments will be returned for those for which
  # caching is ongoing. This makes it possible to access data from
  # experiments that are ready even if there are experiments that are not.
  rtn <- list()
  resolvedStatus <- resolved(experiments)
  exptNamesinConfig <- c()
  for(config in obsmonConfig$experiments) {
    exptNamesinConfig <- c(exptNamesinConfig, config$displayName)
  }
  exptNames <- exptNamesinConfig[exptNamesinConfig %in% ls(experiments)]

  readyExpts <- list()
  stillCachingExpts <- list()
  for (exptName in exptNames) {
    if(resolvedStatus[[exptName]]) {
      readyExpts[[exptName]] <- experiments[[exptName]]
    } else {
      newName <- paste(exptName, "(Caching. Please check later...)")
      stillCachingExpts[[newName]] <- emptyExperiment(newName)
    }
  }
  return(c(readyExpts, stillCachingExpts))
}

shinyServer(function(input, output, session) {
  # Initial population of experiments; triggers cascade for other form fields
  exptNames <- c("")
  experiments <- reactive ({
    # Keep checking for updates in the experiments. Useful when chaching.
    invalidateLater(1000, session)
    separateReadyAndCachingExpts(experimentsAsPromises)
  })
  observe({
      newExptNames <- names(experiments())
      if(!all(exptNames==newExptNames)) {
        updateSelectInput(session, "experiment", choices=newExptNames)
        exptNames <<- newExptNames
      }
  })
  shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")
  shinyjs::show("app-content")

  levelChoices <- list()
  channelChoices <- list()

  activeDb <- reactive({
    expName <- req(input$experiment)
    dbName <- req(input$odbBase)
    isolate(experiments()[[expName]]$dbs[[dbName]])
  })

  # Update database options according to chosen category
  observe({
    category <- req(input$category)
    if (category == "upperAir") {
      choices <- list("Screening"="ecma", "Minimization"="ccma")
      updateSelection(session, "odbBase", choices)
      shinyjs::enable("odbBase")
      shinyjs::enable("levels")
    } else {
      updateSelectInput(session, "odbBase", choices=list("Surface"="ecmaSfc"))
      shinyjs::disable("odbBase")
      shinyjs::disable("levels")
    }
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

  # Update obtype with choices for given experiment and database
  observe({
    db <- activeDb()
    updateSelection(session, "obtype", names(db$obtypes))
  })

  # Update sensor for satem obtype, variable else
  observe({
    db <- activeDb()
    obtype <- req(input$obtype)
    if (obtype == "satem") {
      updateSelection(session, "sensor", names(db$obtypes[[obtype]]))
    } else {
      updateSelection(session, "variable", names(db$obtypes[[obtype]]))
    }
    updateSelection(session, "station", db$stations[[obtype]])
  })

  # Update satellite choices for given sensor
  observe({
    obtype <- req(input$obtype)
    if (obtype == "satem") {
      db <- activeDb()
      sens <- req(input$sensor)
      updateSelection(session, "satellite", names(db$obtypes[[obtype]][[sens]]))
    }
  })

  # Update channel choice for given satellite
  observe({
    obtype <- req(input$obtype)
    if (obtype == "satem") {
      db <- activeDb()
      sens <- req(input$sensor)
      sat <- req(input$satellite)
      channelChoices <<- db$obtypes[[obtype]][[sens]][[sat]]
      updateSelection(session, "channels", channelChoices)
    }
  })

  observeEvent(input$channelsSelectAll, {
    updateSelection(session, "channels", channelChoices, "ALL")
  })

  observeEvent(input$channelsSelectNone, {
    updateSelection(session, "channels", channelChoices, "NONE")
  })

  # Update level choice for given variable
  observe({
    obtype <- req(input$obtype)
    if (obtype != "satem") {
      db <- activeDb()
      var <- req(input$variable)
      levelChoices <<- db$obtypes[[obtype]][[var]]
      updateSelection(session, "levels", levelChoices)
    }
  })

  observeEvent(input$levelsSelectAll, {
    updateSelection(session, "levels", levelChoices, "ALL")
  })

  observeEvent(input$levelsSelectNone, {
    updateSelection(session, "levels", levelChoices, "NONE")
  })

  # Offer single date or dateRange input according to selected plottype
  output$dateType <- reactive({
    plotType <- plotTypesFlat[[req(input$plottype)]]
    plotType$dateType
  })
  outputOptions(output, 'dateType', suspendWhenHidden=FALSE)

  # Build named list of criteria
  buildCriteria <- function() {
    exp <- isolate(experiments()[[req(input$experiment)]])
    db <- req(input$odbBase)
    adb <- activeDb()
    res <- list()
    res$info <- list()
    obtype <- req(input$obtype)
    if (obtype == 'satem') {
      sensor <- req(input$sensor)
      res$obnumber <- adb$obnumbers[[sensor]]
      res$obname <- sensor
      res$satname <- req(input$satellite)
      if (!is.null(input$channels)) {
        res$levels <- input$channels
      } else {
        res$levels <- channelChoices
      }
    } else {
      res$obnumber <- adb$obnumbers[[obtype]]
      res$obname <- obtype
      res$varname <- req(input$variable)
      if (!is.null(input$levels)) {
        res$levels <- input$levels
      } else {
        res$levels <- levelChoices
      }
    }
    if (req(input$station) != "Any") {
      station <- input$station
      res$station <- station
      label <- exp$stationLabels[[adb$name]][[obtype]][[station]]
      res$info$stationLabel <- ifelse(is.null(label), as.character(station), label)
    }
    res
  }

  # Turn criteria into reactive expression so they can trigger plottype update
  criteria <- eventReactive(
  {
    input$obtype
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
