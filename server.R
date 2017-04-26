library(Cairo)
options(shiny.usecairo=TRUE)
library(yaml)

source("utils.R")
source("experiments.R")
source("plots.R")
source("progress.R")
source("windspeed.R")

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

# Updates choices for selection
#
# Updates a selectInput, preserving the selected
# option(s) if available
updateSelection <- function(session, inputId,
                            choices, oldSelection) {
    if (is.null(choices)) {
      return(NULL)
    }
    validChoices <- unlist(choices, use.names=FALSE)
    validSelections <- oldSelection %in% validChoices
    if(!is.null(oldSelection)
       && any(validSelections)) {
      selection <- oldSelection[validSelections]
    } else {
      selection <- NULL
    }
    updateSelectInput(session, inputId,
                       choices=choices, selected=selection)
}

shinyServer(function(input, output, session) {
  # Initial population of experiments; triggers cascade for other form fields
  updateSelectInput(session, "experiment", choices=names(experiments))

  levelChoices <- list()
  channelChoices <- list()

  activeDb <- reactive({
    expName <- req(input$experiment)
    dbName <- req(input$odbBase)
    experiments[[expName]]$dbs[[dbName]]
  })

  # Update database options according to chosen category
  observeEvent({
    input$experiment
    input$category
  }, {
    category <- req(input$category)
    if (category == "upperAir") {
      choices <- list("Screening"="ecma", "Minimization"="ccma")
      updateSelection(session, "odbBase", choices, input$odbBase)
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
    updateSelection(session, "cycle", db$cycles, input$cycle)
  })

  # Update obtype with choices for given experiment and database
  observeEvent(activeDb(), {
    db <- activeDb()
    updateSelection(session, "obtype",
                    names(db$obtypes), input$obtype)
  })

  # Update sensor for satem obtype, variable else
  observeEvent(input$obtype, {
    obtype <- req(input$obtype)
    db <- activeDb()
    if (obtype=="satem") {
      updateSelection(session, "sensor",
                      names(db$obtypes[[obtype]]), input$sensor)
    } else {
      updateSelection(session, "variable",
                      names(db$obtypes[[obtype]]), input$variable)
    }
    updateSelection(session, "station",
                    db$stations[[obtype]], input$station)
  })

  # Update satellite choices for given sensor
  observeEvent(input$sensor, {
    db <- activeDb()
    obtype <- req(input$obtype)
    sens <- req(input$sensor)
    updateSelection(session, "satellite",
                    names(db$obtypes[[obtype]][[sens]]), input$satellite)
  })

  # Update channel choice for given satellite
  observeEvent(input$satellite, {
    db <- activeDb()
    obtype <- req(input$obtype)
    sens <- req(input$sensor)
    sat <- req(input$satellite)
    channelChoices <<- names(db$obtypes[[obtype]][[sens]][[sat]])
    updateSelection(session, "channels", channelChoices, input$channels)
  })

  # Update level choice for given variable
  observeEvent(input$variable, {
    db <- activeDb()
    obtype <- req(input$obtype)
    var <- req(input$variable)
    levelChoices <<- names(db$obtypes[[obtype]][[var]])
    updateSelection(session, "levels", levelChoices, input$levels)
  })

  # Offer single date or dateRange input according to selected plottype
  observeEvent(input$plottype, {
    plotType <- plotTypesFlat[[req(input$plottype)]]
    switch(plotType$dateType,
           "range"={
             shinyjs::hide("date")
             shinyjs::show("dateRange")
           },
           "single"={
             shinyjs::hide("dateRange")
             shinyjs::show("date")
           })
  })

  # Build named list of criteria
  buildCriteria <- function() {
    exp <- experiments[[req(input$experiment)]]
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
    updateSelection(session, "plottype", choices, input$plottype)
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
    t <- addTask(t, "Building query")
    t <- updateTask(t, "Building query", 0.)
    plotRequest <- list()
    plotter <- plotTypesFlat[[req(input$plottype)]]
    plotRequest$expName <- req(input$experiment)
    db <- activeDb()
    plotRequest$dbName <- db$name
    plotRequest$criteria <- buildCriteria()
    cycle <- req(input$cycle)
    plotRequest$criteria$dtg <-
      switch(plotter$dateType,
             "single"=date2dtg(req(input$date), cycle),
             "range"={
                   dateRange <- req(input$dateRange)
                   list(date2dtg(dateRange[1], cycle),
                        date2dtg(dateRange[2], cycle))
             })
    isWindspeed <- "varname" %in% names(plotRequest$criteria) &&
      plotRequest$criteria$varname %in% c("ff", "ff10m")
    if (isWindspeed) {
      plotData <- buildFfData(db, plotter, plotRequest)
    } else {
      query <- plotBuildQuery(plotter, plotRequest)
      output$queryUsed <- renderText(query)
      t <- updateTask(t, "Building query", 1.)
      t <- addTask(t, "Querying database")
      plotData <- performQuery(db, query, plotRequest$criteria$dtg,
                               progressTracker=t)
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
