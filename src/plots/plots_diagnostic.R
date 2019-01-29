registerPlotCategory("Diagnostic")

statisticsPanel <- function(data, column, bw, fill) {
  columnName <- substitute(column)
  eval(substitute({
    hist <- ggplot(data) +
      geom_histogram(aes(x=column), colour="black", fill=fill, binwidth=bw) +
      geom_vline(xintercept = 0.0)
    ecdf <- ggplot(data) +
      aes(x=column) +
      stat_ecdf() +
      aes(y=pnorm(column, sd=sd(column))) +
      geom_line(alpha=.4) +
      ylab("ECDF")
    qq <- ggplot(data) +
      aes(sample=column) +
      stat_qq()
    grid.arrange(hist, ecdf, qq, ncol=3)
  }, list(column=columnName)))
}

plotTitle.plotDiagnostic <- function(p, plotRequest, plotData) {
  dtg <- formatDtg(plotRequest$criteria$dtg)
  expName <- plotRequest$expName
  obtype <- plotRequest$criteria$obname
  station <- plotRequest$criteria$station
  stationLabel <- plotData$statLabel
  title <- sprintf("%s: %s %s %s",
                   expName, p$name, stationLabel, dtg)
  title
}

doPlot.plotDiagnostic <- function(p, plotRequest, plotData) {
  info <- list()
  info$labels <- c("obs"="Observation",
                   "fg"="First Guess",
                   "an"="Analysis",
                   "biascrl"="Bias correction",
                   "rawobs"="Raw observation")
  info$colors <- c("obs"="black",
                   "fg"="red",
                   "an"="green",
                   "biascrl"="blue",
                   "rawobs"="brown")
  dtg <- plotData[["DTG"]]
  obs <- plotData[["obsvalue"]]
  fgDep <- plotData[["fg_dep"]]
  anDep <- plotData[["an_dep"]]
  compDf <- data.frame("Date"=dtg,
                       "obs"=obs,
                       "fg"=obs-fgDep)
  hasMinimization <- plotRequest$dbType %in% c("ecma_sfc", "ccma")
  if (hasMinimization) {
    compDf["an"] <- obs-anDep
  }
  compDf$panel <- "comparison"
  if (plotRequest$criteria$varname=="apd") {
    bias <- plotData[["biascrl"]]
    compDf["rawobs"] <- obs+bias
    biasDf <- data.frame("Date"=dtg,
                         "biascrl"=bias)
    biasDf$panel <- "bias"
    dfs <- list(compDf, biasDf)
  } else {
      dfs <- list(compDf)
  }
  varname <- unique(plotData$varname)
  data <- do.call(rbind, lapply(dfs, partial(melt, id=c("Date", "panel"))))
  data$panel <- factor(data$panel, levels=c("comparison", "bias"))
  comparison <- ggplot(data, aes(Date, value, group=variable, colour=variable)) +
    geom_point() +
    facet_grid(panel~., scales="free_y") +
    scale_color_manual(labels=info$labels, values=info$colors) +
    labs(y=sprintf("%s [%s]", varname, units[[varname]]))
  maxval <- max(plotData$fg_dep, plotData$an_dep)
  minval <- min(plotData$fg_dep, plotData$an_dep)
  bw <- (maxval-minval)/20.
  panels <- list(comparison,
                 statisticsPanel(plotData, fg_dep, bw, info$colors[["fg"]]))
  if (hasMinimization) {
    lay <- rbind(c(1),
                 c(1),
                 c(2),
                 c(3))
    panels <- c(panels,
                list(statisticsPanel(plotData, an_dep, bw, info$colors[["an"]])))
  } else {
    lay <- rbind(c(1),
                 c(2))
  }
  obplot <- grid.arrange(grobs=panels, layout_matrix=lay)
  obplot
}

registerPlotType(
    "Diagnostic",
    plotCreate("plotDiagnostic", "Station Diagnostics", "range",
               paste("SELECT",
                     "DTG, varname, obsvalue, fg_dep, an_dep, biascrl, statid",
                     "FROM usage WHERE %s"),
               list("station"))
)
