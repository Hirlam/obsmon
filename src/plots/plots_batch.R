configFillInBatchModeDefaults <- function(config) {
  for(iConf in seq_along(config$multiPlots)) {
    bmConf <- config$multiPlots[[iConf]]$batchMode
    # This allows users to use batchMode=true/false in the config file
    if(!is.list(bmConf)) bmConf <- list(enable=isTRUE(bmConf))

    if(is.null(bmConf$enable)) bmConf$enable <- TRUE

    # parentDir
    bmConf$parentDir <- normalizePath(trimws(bmConf$parentDir),mustWork=FALSE)
    if(isFALSE(startsWith(bmConf$parentDir, "/"))) {
      bmConf$parentDir <- file.path(dirObsmonWasCalledFrom, bmConf$parentDir)
    }
    if(length(bmConf$parentDir)==0) bmConf$parentDir <- dirObsmonWasCalledFrom
    # dirName. Will be left as NULL if not defined, so that the
    # makeOneMultiPlotInBatch routine can put a timestamp that
    # corresponds to when the plots are actually produced
    bmConf$dirName <- trimws(bmConf$dirName)
    if(length(bmConf$dirName)==0) bmConf$dirName <- NULL
    # Fig resolution, dimentions and type
    if(length(bmConf$dpi)==0) bmConf$dpi <- 300
    if(length(bmConf$figHeight)==0) bmConf$figHeight <- 6
    if(length(bmConf$figWidth)==0) bmConf$figWidth <- 10
    bmConf$fileType <- tolower(bmConf$fileType)
    if(length(bmConf$fileType)==0) bmConf$fileType <- "png"

    # Saving results
    config$multiPlots[[iConf]]$batchMode <- bmConf
  }
  return(config)
}

getExptNamesNeededForBatch <- function(config) {
  # This routine assumes that multiPlot options in the config file have been
  # parsed using the following routines:
  # "multiPlotsValidateConfig" (from plots_multi.R)
  # "configFillInBatchModeDefaults" (from this file)
  exptNames <- character(0)
  for(mpConfig in config$multiPlots) {
    if(!mpConfig$batchMode$enable) next
    exptNames <- c(exptNames, mpConfig$experiment)
  }
  return(exptNames)
}

makeOneMultiPlotInBatch <- function(mpConf, exptDb) {
  bmConf <- mpConf$batchMode

  # Making shiny-like inputs for each individual plot, to be passed to the
  # regular obsmon plotting routines
  inputsForAllPlots <- multiPlotsMakeShinyInputs(mpConf)
  if(length(inputsForAllPlots)==0) {
    flog.warn("  > Selected multiPlot generated no plots")
    return(NULL)
  }

  dirName <- bmConf$dirName
  if(length(dirName)==0) {
    timeStamp <- strftime(Sys.time(), "%Y_%m_%d_%H%M%S")
    dirName <- tolower(sprintf(
      "obsmon_batch_%s_%s",
      slugify(mpConf$displayName), timeStamp
    ))
  }
  dirPath <- file.path(bmConf$parentDir, dirName)
  dirPathCreated <- tryCatch({
      dir.create(dirPath, recursive=TRUE)
      TRUE
    },
    warning=function(w) {
      flog.error(
        '  > Problems creating dir %s: %s. Skipping multiPlot "%s"',
        dirPath, w, mpConf$displayName
      )
      FALSE
    }
  )
  if(!dirPathCreated) return(-1)
  flog.info(
    '  > multiPlot "%s": Plots will be saved into directory\n  %s',
    mpConf$displayName, dirPath
  )

  # Making the plots
  usedPlotType <- mpConf$plotType$copy()
  usedPlotType$interactive <- FALSE
  plots <- prepareMultiPlots(
    plotType=usedPlotType,
    inputsForAllPlots=inputsForAllPlots,
    db=exptDb,
    modelDomain=DOMAIN
  )

  fileType <- bmConf$fileType
  for(iPlt in seq_along(plots)) {
    flog.info(
      '  > multiPlot "%s": Saving plot %d of %d...',
      mpConf$displayName, iPlt, length(plots)
    )
    plot <- plots[[iPlt]]$chart
    fName <- file.path(dirPath, sprintf("plot_%s.%s",iPlt,bmConf$fileType))
    ggsave(
      filename=fName, plot=plot, device=bmConf$fileType,
      dpi=bmConf$dpi, height=bmConf$figHeight, width=bmConf$figWidth,
      units="in"
    )
  }
}

makeBatchPlots <- function() {
  obsmonConfig <<- configFillInBatchModeDefaults(obsmonConfig)

  # Select the subset of multiPlot configs that have been activated
  # for batch mode and save them into bConfigs
  bConfigs <- list()
  for(mpConfig in obsmonConfig$multiPlots) {
    if(isTRUE(mpConfig$batchMode$enable)) {
      bConfigs[[length(bConfigs) + 1]] <- mpConfig
    }
  }
  if(length(bConfigs)==0) {
    flog.warn("makeBatchPlots: Could not detect any batch-enabled multiPlot.")
    return(-1)
  }

  expts <- initExperiments(getExptNamesNeededForBatch(obsmonConfig))
  for(mpConf in bConfigs) {
    cat("\n")
    db <- expts[[mpConf$experiment]]$dbs[[mpConf$database]]
    if(is.null(db)) {
      flog.error(
        'multiPlot "%s"\n  > Could not find %s data for expt "%s". Skipping.',
        mpConf$displayName, mpConf$database, mpConf$experiment
      )
      next
    }
    flog.info('multiPlot "%s": Producing plots...', mpConf$displayName)
    makeOneMultiPlotInBatch(mpConf, exptDb=db)
    flog.info('Done with batch mode for multiPlot "%s"...',mpConf$displayName)
  }
}
