library(yaml)

source("utils.R")
source("experiments.R")

initExperiments <- function() {
  configs <- yaml.load_file("config.yml")
  experiments <- list()
  experimentChoices <- list()
  for(config in configs) {
    name <- config$displayName
    experiments[[name]] <-
      expCreateSqliteShardedDtg(name, config$productionSite,
                                config$baseDir, config$experiment,
                                config$ecmaDir, config$ecmaSfcDir,
                                config$ccmaDir)
    experimentChoices <- append(experimentChoices, name)
  }
  experiments
}

updateSelection <- function(session, inputId,
                            choices, oldSelection) {
    if (is.null(choices)) {
      return(NULL)
    }
    if(!is.null(oldSelection)
       && any(oldSelection %in% choices)) {
      selection <- oldSelection[oldSelection %in% choices]
    } else {
      selection <- choices[1]
    }
    updateSelectInput(session, inputId,
                       choices=choices, selected=selection)
}

experiments <- initExperiments()

shinyServer(function(input, output, session) {
  updateSelectInput(session, "experiment", choices=names(experiments))

  observeEvent(input$experiment, {
    exp <- experiments[[req(input$experiment)]]
    maxRange <- exp$maxDateRange
    dateRange <- exp$dateRange
    updateDateRangeInput(session, "dateRange",
                         start = dateRange[1], end = dateRange[2],
                         min = maxRange[1], max = maxRange[2])
    updateSelection(session, "cycle", exp$cycles, input$cycle)
  })

  observeEvent(input$dateRange, {
    dateRange <- req(input$dateRange)
    expName <- req(input$experiment)
    experiments[[expName]] <- expSetDateRange(experiments[[expName]], dateRange)
  })

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

  observeEvent(input$odbBase, {
    exp <- experiments[[req(input$experiment)]]
    db <- req(input$odbBase)
    updateSelection(session, "obtype",
                    names(exp$obtypes[[db]]), input$obtype)
  })

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
  })

  observeEvent(input$sensor, {
    exp <- experiments[[req(input$experiment)]]
    db <- req(input$odbBase)
    obtype <- req(input$obtype)
    sens <- req(input$sensor)
    updateSelection(session, "satellite",
                    names(exp$obtypes[[db]][[obtype]][[sens]]), input$satellite)
  })

  observeEvent(input$satellite, {
    exp <- experiments[[req(input$experiment)]]
    db <- req(input$odbBase)
    obtype <- req(input$obtype)
    sens <- req(input$sensor)
    sat <- req(input$satellite)
    updateSelection(session, "channels",
                    names(exp$obtypes[[db]][[obtype]][[sens]][[sat]]),
                    input$channels)
  })

  observeEvent(input$variable, {
    exp <- experiments[[req(input$experiment)]]
    db <- req(input$odbBase)
    obtype <- req(input$obtype)
    var <- req(input$variable)
    updateSelection(session, "levels",
                    names(exp$obtypes[[db]][[obtype]][[var]]), input$levels)
  })

})
