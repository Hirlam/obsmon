makeBatchPlots <- function() {
  for(mpConf in obsmonConfig$multiPlots) {
    batchModeConf <- mpConf$batchMode
    if(!is.list(batchModeConf)) {
      batchModeConf <- list(enable=isTRUE(batchModeConf))
    }
    if(is.null(batchModeConf$enable)) batchModeConf$enable <- TRUE
    if(!isTRUE(batchModeConf$enable)) next
    if(is.null(batchModeConf$destDir)) {
      batchModeConf$destDir <- dirObsmonWasCalledFrom
    }
    batchModeConf$filetype <- tolower(batchModeConf$filetype)
    if(length(batchModeConf$filetype)==0) batchModeConf$filetype <- "png"
    
    batchModeConf$destDir <- normalizePath(batchModeConf$destDir, mustWork=FALSE)

    if(is.null(batchModeConf) || isFALSE(batchModeConf)) next
    flog.info('Producing plots in multiPlot config "%s"...',mpConf$displayName)
    # Making shiny-like inputs for each individual plot, to be passed to the
    # regular obsmon plotting routines
    inputsForAllPlots <- multiPlotsMakeShinyInputs(mpConf)
    if(length(inputsForAllPlots)==0) {
      flog.error("  > Selected multiPlot generated no plots")
      next
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
      batchModeConf$destDir,
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

    filetype <- batchModeConf$filetype
    for(iPlt in seq_along(plots)) {
      flog.info("  > Saving plot %d of %d...", iPlt, length(plots))
      plot <- addTitleToPlot(plots[[iPlt]]$obplot, plots[[iPlt]]$title)
      fName <- file.path(dirname, sprintf("plot_%s.%s", iPlt, filetype))
      ggsave(
        filename=fName, plot=plot, device=filetype,
        dpi=600, height=6, width=10, units="in"
      )
    }
    flog.info('Done with batch mode for multiPlot "%s"...',mpConf$displayName)
  }
}
