library(gridExtra)

registerPlotCategory("Diagnostic")

doPlot.plotDiagnostic <- function(p, plotRequest, plotData) {
  dtg <- formatDtg(plotRequest$criteria$dtg)
  exp <- plotRequest$exp
  db <- plotRequest$db
  obtype <- plotRequest$criteria$obname
  station <- plotRequest$criteria$station
  stationLabel <- exp$stationLabels[[db]][[obtype]][[station]]
  title <- sprintf("%s: %s %s %s",
                   exp$name, p$name, stationLabel, dtg)
  colors <- "black"
  obplot <- ggplot(plotData, aes(DTG), group="") +
    geom_line(aes(y=obsvalue, colour="Obs", group=""))
  if (plotRequest$criteria$varname=="apd") {
    obplot <- obplot +
      geom_line(aes(y=obsvalue+biascrl, colour="Obs raw", group=""))
    colors <- c(colors, "blue")
  }
  colors <- c(colors, "green", "red")
  obplot <- obplot +
    geom_line(aes(y=obsvalue-fg_dep, colour="FG", group="")) +
    geom_line(aes(y=obsvalue-an_dep, colour="AN", group="")) +
    xlab("DATE") +
    scale_colour_manual(values=colors) +
    labs(title=title, ylab=ylab)
  maxval <- max(plotData$fg_dep, plotData$an_dep)
  minval <- min(plotData$fg_dep, plotData$an_dep)
  bw <- (maxval-minval)/20.
  bottom1 <- ggplot(plotData) +
    geom_histogram(aes(x=fg_dep), colour="black", fill="red", binwidth=bw) +
    geom_vline(xintercept = 0.0)
  bottom2 <- ggplot(plotData) +
    geom_histogram(aes(x=an_dep), colour="black", fill="green", binwidth=bw) +
    geom_vline(xintercept = 0.0)
  bottom <- arrangeGrob(bottom1,  bottom2, ncol=2)
  obplot <- grid.arrange(obplot, bottom1, bottom2,  ncol=1)
  obplot
}

registerPlotType(
    "Diagnostic",
    plotCreate("plotDiagnostic", "Station Diagnostics", "range",
               paste("SELECT",
                     "DTG, obsvalue, fg_dep, an_dep, biascrl, statid",
                     "FROM usage WHERE %s"),
               list("station"))
)
