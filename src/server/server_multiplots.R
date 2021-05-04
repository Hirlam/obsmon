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
  # Also start with the mapTab disabled. Will be enabled if needed.
  shinyjs::hide(selector="#multiPlotsMainArea li a[data-value=multiPlotsMapTab]")
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
      progressFile=multiPlotsProgressFile()
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


# Hide/show the multiPlot's maps tab as needed
observeEvent(multiPlot(), {
  somePlotHasLeafletMap <- FALSE
  for(individualPlot in multiPlot()) {
    if("maps" %in% tolower(individualPlot$parentType$category)) {
      somePlotHasLeafletMap <- TRUE
      break
    }
  }
  if(somePlotHasLeafletMap) {
    shinyjs::show(selector="#multiPlotsMainArea li a[data-value=multiPlotsMapTab]")
  } else {
    if(input$multiPlotsMainArea=="multiPlotsMapTab") {
      updateTabsetPanel(session, "multiPlotsMainArea", "multiPlotsPlotTab")
    }
    shinyjs::hide(selector="#multiPlotsMainArea li a[data-value=multiPlotsMapTab]")
  }
})

# Prepare the correct output slots for plots, maps and dataTables
# Code adapted from <https://gist.github.com/wch/5436415>
# (i) Plots
output$multiPlotsPlotContainer <- renderUI({
  notifId <- showNotification(
    "Creating multiPlot chart slots...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  plotOutList <- lapply(seq_along(multiPlot()), function(iPlot) {
    if("plotly" %in% class(multiPlot()[[iPlot]]$chart)) {
      plotlyOutputInsideFluidRow(multiPlotsGenId(iPlot, type="plot"))
    } else {
      plotOutputInsideFluidRow(multiPlotsGenId(iPlot, type="plot"))
    }
  })
  # Converting the list to a tagList is necessary for the list of items
  # to display properly.
  do.call(tagList, plotOutList)
})
# (ii) Maps
output$multiPlotsMapAndMapTitleContainer <- renderUI({
  notifId <- showNotification(
    "Creating multiPlot leaflet map slots...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
  mapAndMapTitleOutList <- lapply(seq_along(multiPlot()), function(iPlot) {
    mapId <- multiPlotsGenId(iPlot, type="map")
    mapTitleId <- multiPlotsGenId(iPlot, type="mapTitle")
    mapAndMapTitleOutput(mapId, mapTitleId)
  })
  do.call(tagList, mapAndMapTitleOutList)
})
# (iii) dataTables
output$multiPlotsQueryAndTableContainer <- renderUI({
  notifId <- showNotification(
    "Rendering multiPlot data tables...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))
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
  notifId <- showNotification(
    "Rendering multiPlots...", duration=NULL, type="message"
  )
  on.exit(removeNotification(notifId))

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
      chart <- multiPlot()[[pName]]$chart
      if("plotly" %in% class(chart)) {
        output[[plotOutId]] <- renderPlotly({
          chart %>%
            configPlotlyWrapper(
              toImageButtonOptions=list(
                filename=sprintf("obsmon_multiPlot_%s", iPlot)
              )
            )
        })
      } else {
        output[[plotOutId]] <- renderPlot(
          chart,
          res=96, pointsize=18
        )
      }
      # Assign maps and map titles
      mapId <- multiPlotsGenId(iPlot, type="map")
      mapTitleId <- multiPlotsGenId(iPlot, type="mapTitle")
      output[[mapId]] <- renderLeaflet(req(multiPlot()[[pName]]$leafletMap))
      output[[mapTitleId]] <- renderText(req(multiPlot()[[pName]]$title))
      # Assign queryUsed and dataTable
      queryUsedId <- multiPlotsGenId(iPlot, type="queryUsed")
      dataTableId <- multiPlotsGenId(iPlot, type="dataTable")
      saveAsTxtId <- paste0(dataTableId, "DownloadAsTxt")
      saveAsCsvId <- paste0(dataTableId, "DownloadAsCsv")
      output[[queryUsedId]] <- renderText(req(multiPlot()[[pName]]$sqliteQuery))
      output[[dataTableId]] <- renderDataTable(
        req(multiPlot()[[pName]]$data),
        options=list(scrollX=TRUE, scrollY="200px")
      )
      output[[saveAsTxtId]] <- downloadHandler(
        filename = function() sprintf("multiplot_%d_data.txt", iPlot),
        content = function(file) {
          req(multiPlot()[[pName]])$exportData(file, format="txt")
        }
      )
      output[[saveAsCsvId]] <- downloadHandler(
        filename = function() sprintf("multiplot_%d_data.csv", iPlot),
        content = function(file) {
          req(multiPlot()[[pName]])$exportData(file, format="csv")
        }
      )
    })
  }
})
