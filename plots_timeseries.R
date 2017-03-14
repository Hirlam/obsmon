registerPlotCategory("Timeseries")

plotCreateTimeseries <- function(name, queryStub,
                                 additionalPlotting) {
  p <- structure(list(), class = "plotTimeseries")
  p$name <- name
  p$queryStub <- queryStub
  p$dateType <- "range"
  p$additionalPlotting <- additionalPlotting
  p
}

doPlot.plotTimeseries <- function(p, plotRequest, plotData) {
  title <- paste(plotRequest$exp$name, ":", p$name,
                 plotRequest$varname)
  obplot <- ggplot(data=plotData) +
    aes(x=DTG) +
    labs(title=title, xlab="DATE") +
    facet_wrap(~ level)
  obplot <- p$additionalPlotting(obplot)
  obplot
}

registerPlotType(
    "Timeseries",
    plotCreateTimeseries("Number of Observations",
                         "SELECT DTG, nobs_total, level FROM obsmon WHERE %s",
                         function(obplot) {
                           obplot +
                             geom_line(aes(y=nobs_total)) +
                             ylab("nrobs")
                         })
)
