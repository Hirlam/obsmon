library(gridExtra)

registerPlotCategory("Diagnostic")

plotCreateDiagnostic <- plotCreateConstructor("plotDiagnostic", "range")

doPlot.plotDiagnostic <- function(p, plotRequest, plotData) {
  title <- "test"
  obPlot <- ggplot(plotData, aes(x=DTG, y=obsvalue), group="") +
    geom_line(aes(y=obsvalue, colour="Obs", group="")) +
    geom_line(aes(y=obsvalue-fg_dep, colour="FG", group="")) +
    geom_line(aes(y=obsvalue-an_dep, colour="AN", group="")) +
    xlab("DATE") +
    scale_colour_manual(values=c("black", "green", "red")) +
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
  obPlot <- grid.arrange(obPlot, bottom1, bottom2,  ncol=1)
  obPlot
}

registerPlotType(
    "Diagnostic",
    plotCreateDiagnostic("Surface Diagnostic",
                         paste("SELECT",
                               "DTG, obsvalue, fg_dep, an_dep, biascrl, statid",
                               "FROM usage WHERE (statid like '%%02705%%') AND %s"),
                         list(obname="synop", "varname"))
)
