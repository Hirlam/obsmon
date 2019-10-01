registerPlotCategory("VerticalProfiles")

plotTitle.plotVerticalObsProfile <- function(p, plotRequest, plotData) {
  crit <- plotRequest$criteria
  stationLabel <- getStationsForPlotTitle(plotRequest, plotData)
  title <- sprintf(
    "%s: %s\nstation=%s\ndb=%s, DTG=%s, obname=%s, varname=%s",
    plotRequest$expName, p$name,
    stationLabel,
    plotRequest$dbType, formatDtg(crit$dtg), crit$obname, crit$varname
  )
}

doPlot.plotVerticalObsProfile <- function(p, plotRequest, plotData) {
  df <- data.frame(level=plotData$level, obsvalue=plotData$obsvalue)
  varname <- plotRequest$criteria$varname
  xlab <- levelsLableForPlots(plotRequest$criteria$obnumber, varname)
  ylab <- sprintf("%s [%s]", varname, units[[varname]])
  obplot <- ggplot(data=df) +
    aes(x=level, y=obsvalue) +
    geom_line(colour="black") +
    geom_point(size=2, colour="black") +
    coord_flip_wrapper(default=TRUE) +
    labs(x=xlab, y=ylab)
  return(obplot)
}

doPlot.plotVerticalAnalysisProfile <- function(p, plotRequest, plotData) {
  localPlotData <- melt(plotData, id=c("level"))
  varname <- plotRequest$criteria$varname
  xlab <- levelsLableForPlots(plotRequest$criteria$obnumber, varname)
  ylab <- sprintf("%s [%s]", varname, units[[varname]])
  obplot <- ggplot(data=localPlotData) +
    aes(x=level, y=value, group=variable, colour=variable, shape=variable) +
    geom_line() +
    geom_point(size=2) +
    scale_colour_manual(values=c("black", "blue", "red")) +
    coord_flip_wrapper(default=TRUE) +
    labs(x=xlab, y=ylab)
  return(obplot)
}

doPlot.plotVerticalBiasProfile <- function(p, plotRequest, plotData) {
  localPlotData <- melt(plotData, id=c("level"))
  varname <- plotRequest$criteria$varname
  xlab <- levelsLableForPlots(plotRequest$criteria$obnumber, varname)
  ylab <- sprintf("%s [%s]", varname, units[[varname]])
  obplot <- ggplot(data=localPlotData) +
    aes(x=level, y=value, group=variable, colour=variable, shape=variable) +
    geom_line() +
    geom_point(size=2) +
    scale_colour_manual(values=c("black", "blue")) +
    coord_flip_wrapper(default=TRUE) +
    labs(x=xlab, y=ylab)
  return(obplot)
}

registerPlotType("VerticalProfiles",
  plotCreate("plotVerticalObsProfile",
    name="Station Vertical Profile: Obsvalue",
    dateType="single",
    queryStub=paste(
      "SELECT DISTINCT level, obsvalue FROM usage WHERE %s",
      "ORDER BY level"
    ),
    requiredFields=list(
      "station", "obnumber", "obname", "varname"
    )
  )
)

registerPlotType("VerticalProfiles",
  plotCreate(c("plotVerticalAnalysisProfile", "plotVerticalObsProfile"),
    name="Station Vertical Profile: Obs, FG & Analysis",
    dateType="single",
    queryStub=paste(
      "SELECT DISTINCT level, obsvalue,",
      "(obsvalue-fg_dep) AS first_guess,",
      "(obsvalue-an_dep) AS analysis",
      "FROM usage WHERE %s ORDER BY level"
    ),
    requiredFields=list(
      "station", "obnumber", "obname", "varname"
    )
  )
)

registerPlotType("VerticalProfiles",
  plotCreate(c("plotVerticalBiasProfile", "plotVerticalObsProfile"),
    name="Station Vertical Profile: Bias",
    dateType="single",
    queryStub=paste(
      "SELECT DISTINCT level,",
      "obsvalue AS obsvalue_corrected,",
      "(obsvalue+biascrl) AS obsvalue_raw",
      "FROM usage WHERE %s ORDER BY level"
    ),
    requiredFields=list(
      "station", "obnumber", "obname", "varname"
    )
  )
)
