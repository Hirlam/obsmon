registerPlotCategory("VerticalProfiles")

plotTitle.plotVerticalProfile <- function(p, plotRequest, plotData) {
  crit <- plotRequest$criteria
  stationLabel <- getStationsForPlotTitle(plotRequest, plotData)
  title <- sprintf(
    "%s: %s\nstation=%s\ndb=%s, DTG=%s, obname=%s, varname=%s",
    plotRequest$expName, p$name,
    stationLabel,
    plotRequest$dbType, formatDtg(crit$dtg), crit$obname, crit$varname
  )
}

doPlot.plotVerticalProfile <- function(p, plotRequest, plotData) {
  scaleColors <- c(
    "obsvalue"="black", "fg_dep"="blue", "an_dep"="red",
    "first_guess"="blue", "analysis"="red",
    "obsvalue_corrected"="black", "obsvalue_raw"="blue"
  )
  localPlotData <- melt(plotData, id=c("level"))
  varname <- plotRequest$criteria$varname
  xlab <- levelsLableForPlots(plotRequest$criteria$obnumber, varname)
  ylab <- sprintf("%s [%s]", varname, units[[varname]])
  obplot <- ggplot(data=localPlotData) +
    aes(x=level, y=value, group=variable, colour=variable, shape=variable) +
    geom_point(size=2) +
    geom_line() +
    scale_colour_manual(values=scaleColors) +
    coord_flip_wrapper(default=TRUE) +
    labs(x=xlab, y=ylab)
  if(length(names(plotData))<3) {
    # Legends are not necessary if there's only one x and one y
    obplot <- obplot + theme(legend.position = "none")
  }
  if(startsWith(tolower(xlab), "pressure")) {
    # Using xlim causes ggplotly to omit either the shape or the line
    # (whichever is added later) from the legend. Not a big deal,
    # but worth pointing out, as this but may be solved in later
    # releases of the plotly package
    obplot <- obplot + xlim(max(refPressures,localPlotData[["level"]]),0)
  } else {
    obplot <- obplot + xlim(0, max(refHeights,localPlotData[["level"]]))
  }
  return(obplot)
}

registerPlotType("VerticalProfiles",
  plotCreate("plotVerticalProfile",
    name="Station Vertical Profile: Obsvalue",
    dateType="single",
    queryStub=paste(
      "SELECT DISTINCT level, obsvalue FROM usage WHERE %s AND",
      "(obsvalue IS NOT NULL)",
      "ORDER BY level"
    ),
    requiredFields=list(
      "station", "obnumber", "obname", "varname"
    )
  )
)

registerPlotType("VerticalProfiles",
  plotCreate(c("plotVerticalProfile"),
    name="Station Vertical Profile: Obs, FG & Analysis",
    dateType="single",
    queryStub=paste(
      "SELECT DISTINCT level, obsvalue,",
      "(obsvalue-fg_dep) AS first_guess,",
      "(obsvalue-an_dep) AS analysis",
      "FROM usage WHERE %s AND",
      "(fg_dep IS NOT NULL) AND (an_dep IS NOT NULL) AND (obsvalue IS NOT NULL)",
      "ORDER BY level"
    ),
    requiredFields=list(
      "station", "obnumber", "obname", "varname"
    )
  )
)

registerPlotType("VerticalProfiles",
  plotCreate(c("plotVerticalProfile"),
    name="Station Vertical Profile: FG & Analysis Departure",
    dateType="single",
    queryStub=paste(
      "SELECT DISTINCT level, fg_dep, an_dep",
      "FROM usage WHERE %s AND",
      "(fg_dep IS NOT NULL) AND (an_dep IS NOT NULL)",
      "ORDER BY level"
    ),
    requiredFields=list(
      "station", "obnumber", "obname", "varname"
    )
  )
)

registerPlotType("VerticalProfiles",
  plotCreate(c("plotVerticalProfile"),
    name="Station Vertical Profile: Bias",
    dateType="single",
    queryStub=paste(
      "SELECT DISTINCT level,",
      "obsvalue AS obsvalue_corrected,",
      "(obsvalue+biascrl) AS obsvalue_raw",
      "FROM usage WHERE %s AND",
      "(biascrl IS NOT NULL) AND (obsvalue IS NOT NULL)",
      "ORDER BY level"
    ),
    requiredFields=list(
      "station", "obnumber", "obname", "varname"
    )
  )
)
