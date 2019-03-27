makeOneMultiPlotInBatch <- function(mpConf) {
    bmConf <- mpConf$batchMode

    # Making shiny-like inputs for each individual plot, to be passed to the
    # regular obsmon plotting routines
    inputsForAllPlots <- multiPlotsMakeShinyInputs(mpConf)
    if(length(inputsForAllPlots)==0) {
      flog.error("  > Selected multiPlot generated no plots")
      return(NULL)
    }

    expt <- experimentsAsPromises[[mpConf$experiment]]
    dbType <- mpConf$database
    db <- expt$dbs[[dbType]]

    plots <- prepareMultiPlots(
      plotter=plotTypesFlat[[mpConf$plotType]],
      inputsForAllPlots=inputsForAllPlots, db=db
    )

    timeStamp <- strftime(Sys.time(), "%Y_%m_%d_%H%M%S")
    dirname <- file.path(
      bmConf$destDir,
      sprintf(
        "obsmon_batch_%s_%s",
        slugify(mpConf$displayName), timeStamp
      )
    )
    destDirCreated <- tryCatch({
        dir.create(dirname, recursive=TRUE)
        TRUE
      },
      warning=function(w) {
        flog.error('  > Problems creating dir "%s": %s', dirname, w)
        FALSE
      }
    )
    if(!destDirCreated) next

    filetype <- bmConf$filetype
    for(iPlt in seq_along(plots)) {
      flog.info(
        '  > multiPlot "%s": Saving plot %d of %d...',
        mpConf$displayName, iPlt, length(plots)
      )
      plot <- addTitleToPlot(plots[[iPlt]]$obplot, plots[[iPlt]]$title)
      fName <- file.path(dirname, sprintf("plot_%s.%s", iPlt, filetype))
      ggsave(
        filename=fName, plot=plot, device=filetype,
        dpi=600, height=6, width=10, units="in"
      )
    }
}

makeBatchPlots <- function(maxAttempts=10) {
  if(length(obsmonConfig$multiPlots)==0) return(NULL)
  nAttempts <- rep(0, length(obsmonConfig$multiPlots))
  finished <- rep(FALSE, length(obsmonConfig$multiPlots))

  for(iConf in seq_along(obsmonConfig$multiPlots)) {
    mpConf <- obsmonConfig$multiPlots[[iConf]]
    bmConf <- mpConf$batchMode
    # This allows users to use batchMode=true/false in the config file
    if(!is.list(bmConf)) bmConf <- list(enable=isTRUE(bmConf))

    # Setting defaults for [multiPlots.batchMode] options
    if(is.null(bmConf$enable)) bmConf$enable <- TRUE
    if(is.null(bmConf$destDir)) bmConf$destDir <- dirObsmonWasCalledFrom
    bmConf$filetype <- tolower(bmConf$filetype)
    if(length(bmConf$filetype)==0) bmConf$filetype <- "png"
    bmConf$destDir <- normalizePath(bmConf$destDir, mustWork=FALSE)

    # Saving results in global obsmonConfig
    obsmonConfig$multiPlots[[iConf]]$batchMode <<- bmConf
  }

  iConf <- 0
  repeat {
    if(all(finished)) break
    iConf <- iConf + 1
    if(iConf > length(obsmonConfig$multiPlots)) iConf <- 1
    if(finished[iConf]) next

    mpConf <- obsmonConfig$multiPlots[[iConf]]
    if(!isTRUE(mpConf$batchMode$enable)) {
      finished[iConf] <- TRUE
      next
    }
    flog.info('Producing plots in multiPlot "%s"...',mpConf$displayName)

    nAttempts[iConf] <- nAttempts[iConf] + 1
    if(!resolved(experimentsAsPromises)[[mpConf$experiment]]) {
      if(nAttempts[iConf]<maxAttempts) {
        flog.info(
          '  > Experiment "%s" not yet initialised. Retrying later',
          mpConf$experiment
        )
        Sys.sleep(1)
      } else {
        flog.error(
          '  > Batch plot failed for multiPlot "%s" after %d attempts',
          mpConf$displayName, nAttempts[iConf]
        )
        finished[iConf] <- TRUE
      }
      next
    }
    makeOneMultiPlotInBatch(mpConf)
    finished[iConf] <- TRUE
    flog.info('Done with batch mode for multiPlot "%s"...',mpConf$displayName)
  }
}
