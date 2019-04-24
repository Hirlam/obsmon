############################################################################
#                        Handling of multiPlots tab                       #
############################################################################

# Show multiPlots tab if multiPlots are available
if(length(obsmonConfig$multiPlots)>0) {
  shinyjs::show(selector="#appNavbarPage li a[data-value=multiPlotsTab]")
  # Hide plotly output tab, as the multiplot outputs are generated dinamically
  # and can thus be included in the regular tab
  jsSelec <- "#multiPlotsMainArea li a[data-value=multiPlotsPlotlyTab]"
  for(jsFunc in c(shinyjs::hide, shinyjs::disable)) jsFunc(selector=jsSelec)
}

# Populate multiPlot choices in the UI
mpLabelUnavExpts <- "Unavailable multiPlots"
mpChoices <- setNames(vector("list", length=2), c(" ", mpLabelUnavExpts))
for(plotConfig in obsmonConfig$multiPlots) {
  expt <- expts[[plotConfig$experiment]]
  mpName <- plotConfig$displayName
  if(isTRUE(expt$hasData)) mpChoices[[1]] <- c(mpChoices[[1]], mpName)
  else mpChoices[[2]] <- c(mpChoices[[2]], mpName)
}
if(length(mpChoices[[1]])==0) {
  mpChoices[[1]] <- structure(
    " ", names="ERROR: Required experiments not loaded!"
  )
}
updateSelectInput(session, "multiPlotTitle", choices=mpChoices)


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

  pConfig <- getMultiPlotConfig(input$multiPlotTitle)
  db <- req(tryCatch({
      rtn <- expts[[req(pConfig$experiment)]]$dbs[[req(pConfig$database)]]
      req(isTRUE(dir.exists(rtn$dir)))
      rtn
    },
    error=function(e) {
      signalError(title="Cannot produce multiPlot", sprintf(
        'Could not find files for database "%s" of experiment "%s"',
        pConfig$database, pConfig$experiment
      ))
      NULL
    }
  ))

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
  disableShinyInputs(input, pattern="^multiPlots*")
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


  # Using sink to suppress two annoying blank lines that are sent to
  # stdout whenever a multiPlot is cancelled.
  # See the analogous code in the input$doPlot observe
  tmpStdOut <- vector('character')
  tmpStdOutCon <- textConnection('tmpStdOut', 'wr', local=TRUE)
  sink(tmpStdOutCon, type="message")

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
    enableShinyInputs(input, pattern="^multiPlots*")
    # Printing stdout produced during assync plot, if any
    if(isTRUE(trimws(tmpStdOut)!="")) cat(paste0(tmpStdOut, "\n"))
    close(tmpStdOutCon)
  })
  catch(plotCleanup, function(e) {
    # This prevents printing the annoying "Unhandled promise error" msg when
    # plots are cancelled
    if(!multiPlotInterrupted()) flog.error(e)
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
