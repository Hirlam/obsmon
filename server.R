library(yaml)

source("utils.R")
source("experiments.R")
source("plots.R")

updateSelection <- function(session, inputId,
                            choices, oldSelection) {
    if (is.null(choices)) {
      return(NULL)
    }
    if(!is.null(oldSelection)
       && any(oldSelection %in% choices)) {
      selection <- oldSelection[oldSelection %in% choices]
    } else {
      selection <- NULL
    }
    updateSelectInput(session, inputId,
                       choices=choices, selected=selection)
}

shinyServer(function(input, output, session) {
  updateSelectInput(session, "experiment", choices=names(experiments))
  updateSelectInput(session, "plottype", choices=plotTypesHierarchical)
  levelChoices <- list()
  channelChoices <- list()

  observeEvent(input$experiment, {
    exp <- experiments[[req(input$experiment)]]
    updateDateRangeInput(session, "dateRange",
                         start = exp$dateRange[1], end = exp$dateRange[2],
                         min = exp$maxDateRange[1], max = exp$maxDateRange[2])
    updateSelection(session, "cycle", exp$cycles, input$cycle)
  })

  observeEvent(input$dateRange, {
    dateRange <- req(input$dateRange)
    expName <- req(input$experiment)
    experiments[[expName]]$dateRange <<- dateRange
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
    channelChoices <<- names(exp$obtypes[[db]][[obtype]][[sens]][[sat]])
    updateSelection(session, "channels", channelChoices, input$channels)
  })

  observeEvent(input$variable, {
    exp <- experiments[[req(input$experiment)]]
    db <- req(input$odbBase)
    obtype <- req(input$obtype)
    var <- req(input$variable)
    levelChoices <<- names(exp$obtypes[[db]][[obtype]][[var]])
    updateSelection(session, "levels", levelChoices, input$levels)
  })

  observeEvent(input$doPlot, {
    plotRequest <- list()
    exp <- experiments[[req(input$experiment)]]
    plotRequest$exp <- exp
    db <- req(input$odbBase)
    plotRequest$db <- db
    dateRange <- req(input$dateRange)
    cycle <-  req(input$cycle)
    plotRequest$criteria$dtgMin <- date2dtg(dateRange[1], cycle)
    plotRequest$criteria$dtgMax <- date2dtg(dateRange[2], cycle)
    obtype <- req(input$obtype)
    if (obtype == 'satem') {
      sensor <- req(input$sensor)
      plotRequest$criteria$obnumber <- exp$obnumbers[[db]][[sensor]]
      plotRequest$criteria$obname <- sensor
      plotRequest$criteria$satname <- req(input$satellite)
      if (!is.null(input$channels)) {
        plotRequest$criteria$levels <- input$channels
      } else {
        plotRequest$criteria$levels <- channelChoices
      }
    } else {
      plotRequest$criteria$obnumber <- exp$obnumbers[[db]][[obtype]]
      plotRequest$criteria$obname <- obtype
      plotRequest$criteria$varname <- req(input$variable)
      if (!is.null(input$levels)) {
        plotRequest$criteria$levels <- input$levels
      } else {
        plotRequest$criteria$levels <- levelChoices
      }
    }
    plotter <- plotTypesFlat[[req(input$plottype)]]
    obplot <- plotter(plotRequest)
    output$plot <- renderPlot({obplot}, height=600, width=800)
  })

})
