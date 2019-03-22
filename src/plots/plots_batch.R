for(pConfig in obsmonConfig$multiPlots) {
  if(!isTRUE(pConfig$batchMode)) next
  flog.info('Producing plots in multiPlot config "%s"', pConfig$displayName)
  # Making shiny-like inputs for each individual plot, to be passed to the
  # regular obsmon plotting routines
  inputsForAllPlots <- multiPlotsMakeShinyInputs(pConfig)
  if(length(inputsForAllPlots)==0) {
    flog.error("Selected multiPlot generated no plots")
  }

  expt <- experimentsAsPromises[[pConfig$experiment]]
  dbType <- pConfig$database
  db <- expt$dbs[[dbType]]

  multiPlots <- prepareMultiPlots(
      plotter=plotTypesFlat[[pConfig$plotType]],
      inputsForAllPlots=inputsForAllPlots, db=db
  )

  iPlot <- 0
  fType <- "png"
  for(multiPlot in multiPlots) {
    iPlot <- iPlot + 1
    fName <- sprintf("multiPlot_%s.%s", iPlot, fType)
    plot <- addTitleToPlot(multiPlot$obplot, multiPlot$title)
    ggsave(filename=fName, plot=plot, device=fType)
  }

}
