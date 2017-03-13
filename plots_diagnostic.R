registerPlotCategory("Diagnostic")

surfaceDiagnostics <- function(plotRequest) {
  title <- "test"
  query <- paste("SELECT",
                 "dtg, obsvalue, fg_dep, an_dep, biascrl, statid",
                 "FROM usage WHERE",
                 buildWhereClause(plotRequest$criteria))
  range <- list(plotRequest$criteria$dtgMin, plotRequest$criteria$dtgMax)
  plotData <- expQuery(plotRequest$exp, plotRequest$db, query, dtgs=range)
  obPlot <- ggplot(plotData, aes(x=dtg, y=obsvalue), group="") +
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
  bottom <- grid.arrange(bottom1,  bottom2, ncol=2)
  obPlot <- grid.arrange(obPlot, bottom1, bottom2,  ncol=1)
}
registerPlotType("Diagnostic", "Surface Diagnostics", surfaceDiagnostics)
