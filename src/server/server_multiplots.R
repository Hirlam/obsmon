############################################################################
#                        Handling of multiPlots tab                       #
############################################################################

# Show multiPlots tab if multiPlots are available
if(length(obsmonConfig$multiPlots)>0) {
  shinyjs::show(selector="#appNavbarPage li a[data-value=multiPlotsTab]")
}

# Populate multiPlot choices in the UI.
mpChoices <- unlist(lapply(obsmonConfig$multiPlots, function(mpc) {
  mpc$displayName
}))
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

# Keep track of multiPlot async process PID in case user wants to cancel it
multiPlotCurrentPid <- reactiveVal(-1)

# Management of "Cancel multiPlot" button
multiPlotInterrupted <- reactiveVal(FALSE)
observeEvent(input$multiPlotsCancelPlot, {
  showNotification("Cancelling multiPlot", type="warning", duration=1)
  multiPlotInterrupted(TRUE)
  killProcessTree(multiPlotCurrentPid(), warnFail=TRUE)
}, priority=2000, ignoreInit=TRUE)

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

  # Prepare individual plots asyncronously
  multiPlotsAsyncAndOutput <- futureCall(
    FUN=prepareMultiPlotsCapturingOutput,
    args=list(
      plotType=pConfig$plotType,
      inputsForAllPlots=inputsForAllPlots,
      db=db,
      progressFile=multiPlotsProgressFile(),
      modelDomain=sessionDomain()
    )
  )
  multiPlotCurrentPid(multiPlotsAsyncAndOutput$job$pid)

  then(multiPlotsAsyncAndOutput,
    onFulfilled=function(value) {
      multiPlot(value$plots)
    },
    onRejected=function(e) {
      if(!multiPlotInterrupted()) {
        showNotification("Could not produce plot", duration=1, type="error")
        flog.error(e)
      }
      multiPlot(NULL)
    }
  )
  plotCleanup <- finally(multiPlotsAsyncAndOutput, function() {
    # Force-kill eventual zombie forked processes and reset pid
    killProcessTree(multiPlotCurrentPid())
    multiPlotCurrentPid(-1)

    # Reset items related to multiPlot progress bar
    unlink(multiPlotsProgressFile())
    multiPlotsProgressFile(NULL)
    multiPlotsProgressBar()$close()
    multiPlotsProgressBar(NULL)

    # Hide/show and disable/enable relevant inputs
    shinyjs::hide("multiPlotsCancelPlot")
    shinyjs::show("multiPlotsDoPlot")
    enableShinyInputs(input, pattern="^multiPlots*")

    # Printing output produced during async plot, if any
    producedOutput <- value(multiPlotsAsyncAndOutput)$output
    if(length(producedOutput)>0) message(paste0(producedOutput, "\n"))
  })
  catch(plotCleanup, function(e) {
    # This prevents printing the annoying "Unhandled promise error" msg when
    # plots are cancelled
    if(!multiPlotInterrupted()) flog.error(e)
    NULL
  })
  # This NULL is necessary in order to prevent the future from blocking
  NULL
}, priority=2000)

# Dinamically create slots to hold the generated plots
outputsToResetBeforeNewMultiplot <- reactiveVal()
newOutputSlotsHaveBeenCreated <- reactiveVal(NULL)
output$multiPlotTabsetPanelsContainer <- renderUI({
  for(outID in isolate(outputsToResetBeforeNewMultiplot())) {
    output[[outID]] <- NULL
  }
  newOutputSlotsHaveBeenCreated(NULL)
  if(is.null(multiPlot())) return (NULL)

  req(multiPlot())
  notifId <- showNotification(
    "Preparing multiPlot output...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))

  mainPanelsList <- lapply(seq_along(multiPlot()), function(iPlot) {
    mPlot <- multiPlot()[[iPlot]]
    multiplotIdPrefix <- sprintf("multiplot_%d_", iPlot)

    # (i) Tab to hold the chart
    plotOutputId <- paste0(multiplotIdPrefix, "chart")
    tabs <- list()
    if(isTRUE("plotly" %in% class(mPlot$chart))) {
      tabs[[length(tabs) + 1]] <- interactivePlotTabPanel(plotOutputId)
    } else {
      tabs[[length(tabs) + 1]] <- nonInteractivePlotTabPanel(plotOutputId)
    }

    # (ii) Tab to hold the leaflet map
    if(!is.null(mPlot$leafletMap)) {
      leafletMapOutputId <- paste0(multiplotIdPrefix, "map")
      tabs[[length(tabs) + 1]] <- leafletMapTabPanel(leafletMapOutputId)
    }

    # (ii) Query and data tab
    tabs[[length(tabs) + 1]] <- queryAndDataTabPanel(multiplotIdPrefix)

    do.call(
      tabsetPanel,
      c(id=sprintf("tabsetPanelForMultiPlot_%d", iPlot), tabs)
    )
  })
  # Converting the list to a tagList is necessary for the list of items
  # to display properly.
  do.call(tagList, mainPanelsList)
  newOutputSlotsHaveBeenCreated(Sys.time())
  return(mainPanelsList)
})

# Populate the output slots
observeEvent(newOutputSlotsHaveBeenCreated(), {
  req(newOutputSlotsHaveBeenCreated())
  newOutputSlotsHaveBeenCreated(NULL)
  req(multiPlot())
  notifId <- showNotification(
    "Rendering multiPlots...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  for (iPlot0 in seq_along(multiPlot())) {
    local({
      iPlot <- iPlot0
      mPlot <- multiPlot()[[iPlot]]
      multiplotIdPrefix <- sprintf("multiplot_%d_", iPlot)

      plotOutputId <- paste0(multiplotIdPrefix, "chart")
      outputsToResetBeforeNewMultiplot(
        c(outputsToResetBeforeNewMultiplot(), plotOutputId)
      )
      if(isTRUE("plotly" %in% class(mPlot$chart))) {
        output[[plotOutputId]] <- renderPlotly({
          mPlot$chart %>%
            configPlotlyWrapper(
              toImageButtonOptions=list(
                filename=sprintf("obsmon_multiPlot_%s", iPlot)
              )
            )
        })
      } else {
        output[[plotOutputId]] <- renderPlot(mPlot$chart, res=96, pointsize=18)
      }

      # (ii) Tab to hold the leaflet map
      if(!is.null(mPlot$leafletMap)) {
        leafletMapOutputId <- paste0(multiplotIdPrefix, "map")
        outputsToResetBeforeNewMultiplot(
          c(outputsToResetBeforeNewMultiplot(), leafletMapOutputId)
        )
        output[[leafletMapOutputId]] <- renderLeaflet(mPlot$leafletMap)
      }

      # (iii) Query and data tab
      plotDataTableId <- paste0(multiplotIdPrefix, "plotDataTable")
      outputsToResetBeforeNewMultiplot(
        c(outputsToResetBeforeNewMultiplot(), plotDataTableId)
      )
      output[[plotDataTableId]] <- renderDataTable(
        req(mPlot$dataWithUnits),
        options=list(scrollX=TRUE, scrollY="200px")
      )

      queryUsedOutputId <- paste0(multiplotIdPrefix, "queryUsed")
      outputsToResetBeforeNewMultiplot(
        c(outputsToResetBeforeNewMultiplot(), queryUsedOutputId)
      )
      output[[queryUsedOutputId]] <- renderText(req(mPlot$sqliteQuery))

      rawDataTableId <- paste0(multiplotIdPrefix, "rawDataTable")
      outputsToResetBeforeNewMultiplot(
        c(outputsToResetBeforeNewMultiplot(), rawDataTableId)
      )
      output[[rawDataTableId]] <- renderDataTable(
        req(mPlot$rawData),
        options=list(scrollX=TRUE, scrollY="200px")
      )

      # (iv) Data download handlers
      saveAsTxtId <- paste0(plotDataTableId, "DownloadAsTxt")
      saveAsCsvId <- paste0(plotDataTableId, "DownloadAsCsv")
      outputsToResetBeforeNewMultiplot(
        c(outputsToResetBeforeNewMultiplot(), c(saveAsTxtId, saveAsCsvId))
      )
      output[[saveAsTxtId]] <- downloadHandler(
        filename = function() paste0(multiplotIdPrefix, "data.txt"),
        content = function(file) {
          mPlot$exportData(file, format="txt")
        }
      )
      output[[saveAsCsvId]] <- downloadHandler(
        filename = function() paste0(multiplotIdPrefix, "data.csv"),
        content = function(file) {
          mPlot$exportData(file, format="csv")
        }
      )

      saveRawDataAsTxtId <- paste0(rawDataTableId, "DownloadAsTxt")
      saveRawDataAsCsvId <- paste0(rawDataTableId, "DownloadAsCsv")
      outputsToResetBeforeNewMultiplot(
        c(
          outputsToResetBeforeNewMultiplot(),
          c(saveRawDataAsTxtId, saveRawDataAsCsvId)
        )
      )
      output[[saveRawDataAsTxtId]] <- downloadHandler(
        filename = function() paste0(multiplotIdPrefix, "raw_data.txt"),
        content = function(file) {
          mPlot$exportData(file, format="txt", raw=TRUE)
        }
      )
      output[[saveRawDataAsCsvId]] <- downloadHandler(
        filename = function() paste0(multiplotIdPrefix, "raw_data.csv"),
        content = function(file) {
          mPlot$exportData(file, format="csv", raw=TRUE)
        }
      )

    })
  }
})
