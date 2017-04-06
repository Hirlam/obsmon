library(Cairo)
options(shiny.usecairo=TRUE)
library(yaml)

source("utils.R")
source("experiments.R")
source("plots.R")
source("progress.R")

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

  # Update date related fields dateRange, date, and cycle with new experiment
  observeEvent(input$experiment, {
    exp <- experiments[[req(input$experiment)]]
    updateDateRangeInput(session, "dateRange",
                         start = exp$dateRange[1], end = exp$dateRange[2],
                         min = exp$maxDateRange[1], max = exp$maxDateRange[2])
    updateDateInput(session, "date", value=exp$date,
                    min = exp$maxDateRange[1], max = exp$maxDateRange[2])
    updateSelection(session, "cycle", exp$cycles, input$cycle)
  })

  # Update dateRange in experiment to persist across experiment changes
  observeEvent(input$dateRange, {
    dateRange <- req(input$dateRange)
    expName <- req(input$experiment)
    experiments[[expName]]$dateRange <<- dateRange
  })

  # Update date in experiment to persist across experiment changes
  observeEvent(input$date, {
    expName <- req(input$experiment)
    experiments[[expName]]$date <<- req(input$date)
  })

  # Update database options according to chosen category
  observeEvent(input$category, {
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

  # Update obtype with choices for given experiment and database
  observeEvent(input$odbBase, {
    exp <- experiments[[req(input$experiment)]]
    db <- req(input$odbBase)
    updateSelection(session, "obtype",
                    names(exp$obtypes[[db]]), input$obtype)
  })

  # Update sensor for satem obtype, variable else
  observeEvent(input$obtype, {
    exp <- experiments[[req(input$experiment)]]
    db <- req(input$odbBase)
    obtype <- req(input$obtype)
    if (obtype=="satem") {
      updateSelection(session, "sensor",
                      names(exp$obtypes[[db]][[obtype]]), input$sensor)
    } else {
      updateSelection(session, "variable",
                      names(exp$obtypes[[db]][[obtype]]), input$variable)
    }
    updateSelection(session, "station",
                    exp$stations[[db]][[obtype]], input$station)
  })

  # Update satellite choices for given sensor
  observeEvent(input$sensor, {
    exp <- experiments[[req(input$experiment)]]
    db <- req(input$odbBase)
    obtype <- req(input$obtype)
    sens <- req(input$sensor)
    updateSelection(session, "satellite",
                    names(exp$obtypes[[db]][[obtype]][[sens]]), input$satellite)
  })

  # Update channel choice for given satellite
  observeEvent(input$satellite, {
    exp <- experiments[[req(input$experiment)]]
    db <- req(input$odbBase)
    obtype <- req(input$obtype)
    sens <- req(input$sensor)
    sat <- req(input$satellite)
    channelChoices <<- names(exp$obtypes[[db]][[obtype]][[sens]][[sat]])
    updateSelection(session, "channels", channelChoices, input$channels)
  })

  # Update level choice for given variable
  observeEvent(input$variable, {
    exp <- experiments[[req(input$experiment)]]
    db <- req(input$odbBase)
    obtype <- req(input$obtype)
    var <- req(input$variable)
    levelChoices <<- names(exp$obtypes[[db]][[obtype]][[var]])
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
    res <- list()
    obtype <- req(input$obtype)
    if (obtype == 'satem') {
      sensor <- req(input$sensor)
      res$obnumber <- exp$obnumbers[[db]][[sensor]]
      res$obname <- sensor
      res$satname <- req(input$satellite)
      if (!is.null(input$channels)) {
        res$levels <- input$channels
      } else {
        res$levels <- channelChoices
      }
    } else {
      res$obnumber <- exp$obnumbers[[db]][[obtype]]
      res$obname <- obtype
      res$varname <- req(input$variable)
      if (!is.null(input$levels)) {
        res$levels <- input$levels
      } else {
        res$levels <- levelChoices
      }
    }
    if (req(input$station) != "Any") {
      res$station <- input$station
    }
    res
  }

  # Turn criteria into reactive expression so they can trigger plottype update
  criteria <- reactive({
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
    exp <- experiments[[req(input$experiment)]]
    plotRequest$exp <- exp
    db <- req(input$odbBase)
    plotRequest$db <- db
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
    query <- plotBuildQuery(plotter, plotRequest)
    t <- updateTask(t, "Building query", 1.)
    t <- addTask(t, "Querying database")
    plotData <- expQuery(exp, db, query,
                         dtgs=plotRequest$criteria$dtg,
                         progressTracker=t)
    res <- plotGenerate(plotter, plotRequest, plotData, t)
    output$plot <- renderPlot(grid.arrange(res$obplot,
                                           top=textGrob(res$title)),
                              res=96, pointsize=18)
    if (is.null(res$obmap)) {
      js$disableTab("mapTab")
    } else {
      output$map <- renderLeaflet(res$obmap)
      output$mapTitle <- renderText(res$title)
      js$enableTab("mapTab")
    }
    closeTracker(t)
  })
})
