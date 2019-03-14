############################################################################
#                        Handling of multiPlots tab                       #
############################################################################

# Show multiPlots tab if multiPlots are available
if(!is.null(obsmonConfig$multiPlots)) {
  shinyjs::show(selector="#appNavbarPage li a[data-value=multiPlotsTab]")
  # Hide plotly output tab, as the multiplot outputs are generated dinamically
  # and can thus be included in the regular tab
  shinyjs::hide(selector="#multiPlotsMainArea li a[data-value=multiPlotsPlotlyTab]")
  shinyjs::disable(selector="#multiPlotsMainArea li a[data-value=multiPlotsPlotlyTab]")
}

multiPlotChoices <- c()
for(plotConfig in obsmonConfig$multiPlots) {
  multiPlotChoices <- c(multiPlotChoices, plotConfig$displayName)
}
updateSelectInput(session, "multiPlotTitle", choices=multiPlotChoices)

multiPlotConfigInfo <- eventReactive(input$multiPlotTitle, {
  pConfig <- NULL
  for(pConf in obsmonConfig$multiPlots) {
    if(!pConf$displayName==input$multiPlotTitle) next
    pConfig <- pConf
    break
  }
  pConfig
})

multiPlotExperiment <- eventReactive(multiPlotConfigInfo(), {
  pConfig <- multiPlotConfigInfo()
  experiments()[[pConfig$experiment]]
},
  ignoreNULL=FALSE
)

multiPlotActiveDb <- eventReactive(multiPlotExperiment(), {
  pConfig <- multiPlotConfigInfo()
  dbType <- pConfig$database
  multiPlotExperiment()$dbs[[dbType]]
},
  ignoreNULL=FALSE
)

# Management of multiPlot progress bar
multiPlotsProgressFile <- reactiveVal(NULL)
multiPlotsProgressStatus <- reactiveVal(function() NULL)
multiPlotsProgressBar <- reactiveVal(NULL)
readProgressFile <- function(path) {
  tryCatch(read.table(path),
    error=function(e) NULL,
    warning=function(w) NULL
  )
}
observeEvent(multiPlotsProgressFile(), {
  multiPlotsProgressStatus(reactiveFileReader(
    500, session, isolate(multiPlotsProgressFile()), readProgressFile
  ))
})
observeEvent(multiPlotsProgressStatus()(), {
  mpProgress <- unlist(multiPlotsProgressStatus()(), use.names=FALSE)
  progress <- multiPlotsProgressBar()
  progress$set(
    # Subtract 1 from value as progress is updated when the process begins
    value=mpProgress[1]-1,
    message="Preparing multiPlot",
    detail=sprintf(
      "Gathering data for plot %s of %s", mpProgress[1], mpProgress[2]
    )
  )
  multiPlotsProgressBar(progress)
})

# Keep track of multiPlot assync process PID in case user wants to cancel it
multiPlotCurrentPid <- reactiveVal(-1)

# Management of "Cancel multiPlot" button
multiPlotInterrupted <- reactiveVal(FALSE)
onclick("multiPlotsCancelPlot", {
  showNotification("Cancelling multiPlot", type="warning", duration=1)
  multiPlotInterrupted(TRUE)
  tools::pskill(multiPlotCurrentPid(), tools::SIGINT)
})

# Producing multiPlots
multiPlot <- reactiveVal(NULL)
observeEvent(input$multiPlotsDoPlot, {
  # Make sure a multiPlot cannot be requested if another is being produced.
  if(multiPlotCurrentPid()>-1) {
    showNotification(
      "Another multiPlot is being produced. Please wait.",
      type="warning", duration=1
    )
  }
  req(multiPlotCurrentPid()==-1)

  # Erase any plot currently on display
  multiPlot(NULL)

  pConfig <- multiPlotConfigInfo()
  db <- tryCatch(
    req(multiPlotActiveDb()),
    error=function(e) {
      exptName <- pConfig$experiment
      exptNames <- gsub(": Loading experiment...$", "", names(experiments))
      if(exptName %in% exptNames) {
        errMsg <- "Experiment still loading. Please try again later."
      } else {
        errMsg <- sprintf('Cannot find files for experiment "%s"', exptName)
      }
      signalError(title="Cannot produce multiPlot", errMsg)
      NULL
    }
  )
  req(db)

  # Making shiny-like inputs for each individual plot, to be passed to the
  # regular obsmon plotting routines
  inputsForAllPlots <- multiPlotsMakeShinyInputs(pConfig)
  if(length(inputsForAllPlots)==0) {
    showNotification(
      "Selected multiPlot generated no plots",
      type="warning", duration=1
    )
  }
  req(length(inputsForAllPlots)>0)

  ###############################################################
  # All checks performed: We can now proceed with the multiPlot #
  ###############################################################
  # Prevent another plot from being requested
  disableShinyInputs(input)
  shinyjs::hide("multiPlotsDoPlot")

  # Offer possibility to cancel multiPlot
  shinyjs::show("multiPlotsCancelPlot")
  shinyjs::enable("multiPlotsCancelPlot")
  multiPlotInterrupted(FALSE)

  # Create multiPlot progess bar
  progress <- shiny::Progress$new(max=length(inputsForAllPlots))
  progress$set(message="Gathering data for multiPlot...", value=0)
  multiPlotsProgressBar(progress)
  multiPlotsProgressFile(tempfile(pattern = "multiPlotsProgress"))


  # Using sink to suppress a few annoying stuff futureCall sends to stderr
  # See the analogous code in the input$doPlot observe
  futureCallStderrFilePath <- tempfile()
  tmpFile <- file(futureCallStderrFilePath, open="wt")
  sink(tmpFile, type="message")

  # Prepare individual plots assyncronously
  multiPlotsAsync <- futureCall(
    FUN=prepareMultiPlots,
    args=list(
      plotter=plotTypesFlat[[pConfig$plotType]],
      inputsForAllPlots=inputsForAllPlots, db=db,
      progressFile=multiPlotsProgressFile()
    )
  )
  multiPlotCurrentPid(multiPlotsAsync$job$pid)

  # Cancel sink, so error/warning messages can be printed again
  sink(type="message")

  then(multiPlotsAsync,
    onFulfilled=function(value) {
      showNotification(
        "Preparing to render multiPlot", duration=1, type="message"
      )
      multiPlot(value)
      somePlotHasMap <- FALSE
      for(individualPlot in value) {
        if(!is.null(individualPlot$obmap)) {
          somePlotHasMap <- TRUE
          break
        }
      }
      if(somePlotHasMap) {
        js$enableTab("multiPlotsMapTab")
      } else {
        if(input$multiPlotsMainArea=="multiPlotsMapTab") {
          updateTabsetPanel(session,"multiPlotsMainArea","multiPlotsPlotTab")
        }
        js$disableTab("multiPlotsMapTab")
      }
    },
    onRejected=function(e) {
      if(!multiPlotInterrupted()) {
        flog.error(e)
        showNotification("Could not produce plot", duration=1, type="error")
      }
      multiPlot(NULL)
    }
  )
  plotCleanup <- finally(multiPlotsAsync, function() {
    multiPlotCurrentPid(-1)
    # Reset items related to multiPlot progress bar
    unlink(multiPlotsProgressFile())
    multiPlotsProgressBar()$close()
    multiPlotsProgressFile(NULL)
    multiPlotsProgressBar(NULL)
    # Hide/show and disable/enable relevant inputs
    shinyjs::hide("multiPlotsCancelPlot")
    shinyjs::show("multiPlotsDoPlot")
    enableShinyInputs(input)
    # Cleaning temp file used for futureCall stderr
    close(tmpFile)
    unlink(futureCallStderrFilePath)
  })
  catch(plotCleanup, function(e) {
    # This prevents printing the annoying "Unhandled promise error" msg when
    # plots are cancelled
    if(!plotInterrupted()) flog.error(e)
    NULL
  })
  # This NULL is necessary in order to avoid the future from blocking
  NULL
})

# Prepare the correct output slots for plots, maps and dataTables
# Code adapted from <https://gist.github.com/wch/5436415>
# (i) Plots
output$multiPlotsPlotContainer <- renderUI({
  plotOutList <- lapply(seq_along(multiPlot()), function(iPlot) {
    if(
      isTRUE(obsmonConfig$general$multiPlotsEnableInteractivity) &&
      plotCanBeMadeInteractive(multiPlot()[[iPlot]]$obplot)
    ) {
      plotlyOutputInsideFluidRow(multiPlotsGenId(iPlot, type="plot"))
    } else {
      plotOutputInsideFluidRow(multiPlotsGenId(iPlot, type="plot"))
    }
  })
  # Convert the list to a tagList - this is necessary for the list of items
  # to display properly.
  do.call(tagList, plotOutList)
})
# (ii) Maps
output$multiPlotsMapAndMapTitleContainer <- renderUI({
  mapAndMapTitleOutList <- lapply(seq_along(multiPlot()), function(iPlot) {
    mapId <- multiPlotsGenId(iPlot, type="map")
    mapTitleId <- multiPlotsGenId(iPlot, type="mapTitle")
    mapAndMapTitleOutput(mapId, mapTitleId)
  })
  do.call(tagList, mapAndMapTitleOutList)
})
# (iii) dataTables
output$multiPlotsQueryAndTableContainer <- renderUI({
  queryAndDataTableOutList <- lapply(seq_along(multiPlot()), function(iPlot){
    queryUsedId <- multiPlotsGenId(iPlot, type="queryUsed")
    dataTableId <- multiPlotsGenId(iPlot, type="dataTable")
    queryUsedAndDataTableOutput(queryUsedId, dataTableId)
  })
  do.call(tagList, queryAndDataTableOutList)
})

# Assign each plot/map/title/query/table to the respective outputs
multiPlotsPrevQuantity <- reactiveVal()
observeEvent(multiPlot(), {
  # Clean up old multiPlot outputs
  for(iPlot in seq(multiPlotsPrevQuantity())) {
    for(type in c("plot", "map", "mapTitle", "queryUsed", "dataTable")) {
      outId <- multiPlotsGenId(iPlot, type=type)
      output[[outId]] <- NULL
    }
  }
  multiPlotsPrevQuantity(length(multiPlot()))
  gc()

  # Assign the new multiPlots
  for(iPlot0 in seq_along(multiPlot())) {
    local({
      iPlot <- iPlot0
      pName <- multiPlotsGenId(iPlot)
      # Assign plots
      plotOutId <- multiPlotsGenId(iPlot, type="plot")
      if(
        isTRUE(obsmonConfig$general$multiPlotsEnableInteractivity) &&
        plotCanBeMadeInteractive(multiPlot()[[pName]]$obplot)
      ) {
        output[[plotOutId]] <- renderPlotly({
          myPlot <- multiPlot()[[pName]]
          myPlot <- addTitleToPlot(myPlot$obplot, myPlot$title)
          # Convert ggplot object to plotly and customise
          myPlot <- ggplotly(req(myPlot), tooltip = c("x","y")) %>%
            config(
              displaylogo=FALSE, collaborate=FALSE, cloud=FALSE,
              scrollZoom=TRUE
            )
          myPlot
        })
      } else {
        output[[plotOutId]] <- renderPlot({
          myPlot <- multiPlot()[[pName]]
          addTitleToPlot(req(myPlot$obplot), myPlot$title)
        },
           res=96, pointsize=18
        )
      }
      # Assign maps and map titles
      mapId <- multiPlotsGenId(iPlot, type="map")
      mapTitleId <- multiPlotsGenId(iPlot, type="mapTitle")
      output[[mapId]] <- renderLeaflet(req(multiPlot()[[pName]]$obmap))
      output[[mapTitleId]] <- renderText(req(multiPlot()[[pName]]$title))
      # Assign queryUsed and dataTable
      queryUsedId <- multiPlotsGenId(iPlot, type="queryUsed")
      dataTableId <- multiPlotsGenId(iPlot, type="dataTable")
      saveAsTxtId <- paste0(dataTableId, "DownloadAsTxt")
      saveAsCsvId <- paste0(dataTableId, "DownloadAsCsv")
      output[[queryUsedId]] <- renderText(req(multiPlot()[[pName]]$queryUsed))
      output[[dataTableId]] <- renderDataTable(
        req(multiPlot()[[pName]]$plotData), options=list(pageLength=10)
      )
      output[[saveAsTxtId]] <- downloadHandler(
        filename = function() sprintf("multiplot_%d_data.txt", iPlot),
        content = function(file) {
          multiplotData <- multiPlot()[[pName]]$plotData
          dataInfo <- plotExportedDataInfo(multiPlot()[[pName]])
          write.table(multiplotData, file, sep="\t", row.names=FALSE)
          write(paste0("\n", dataInfo), file, append=TRUE)
        }
      )
      output[[saveAsCsvId]] <- downloadHandler(
        filename = function() sprintf("multiplot_%d_data.csv", iPlot),
        content = function(file) {
          multiplotData <- multiPlot()[[pName]]$plotData
          dataInfo <- plotExportedDataInfo(multiPlot()[[pName]])
          write.csv(multiplotData, file, row.names=FALSE)
          write(paste0("\n", dataInfo), file, append=TRUE)
        }
      )
    })
  }
})