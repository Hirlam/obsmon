registerPlotCategory("StationVerticalProfiles")

plotTitle.plotStationVerticalProfile <- function(p, plotRequest, plotData) {
  crit <- plotRequest$criteria
  stationLabel <- getStationsForPlotTitle(plotRequest, plotData)
  title <- sprintf(
    "%s: %s\nstation=%s\ndb=%s, DTG=%s, obname=%s, varname=%s",
    plotRequest$expName, p$name,
    stationLabel,
    plotRequest$dbType, formatDtg(crit$dtg), crit$obname, crit$varname
  )
}

doPlot.plotStationVerticalProfile <- function(p, plotRequest, plotData) {
  scaleColors <- c(
    "obsvalue"="black", "obsvalue_corrected"="black", "obsvalue_raw"="blue",
    "fg_dep"="blue", "first_guess"="blue",
    "an_dep"="red", "analysis"="red"
  )

  localPlotData <- plotData %>%
    group_by(level) %>%
    melt(id="level", na.rm=TRUE)
  plotDataSummary <- plotData %>%
    group_by(level) %>%
    gather(key="variable", value="value", -level) %>%
    group_by(level, variable) %>%
    summarize_all(list(~mean(.), ~min(.), ~max(.)))

  obplot <- ggplot() +
    aes(x=level, group=variable, colour=variable, shape=variable) +
    scale_colour_manual(values=scaleColors) +
    # Scatter plot with all data points
    geom_point(data=localPlotData, aes(y=value), size=2) +
    # Draw line through mean values instead of actual data point values
    geom_line(data=plotDataSummary, aes(y=mean), size=1.15) +
    # For clarity: Connect groups of points belonging to the same level
    geom_linerange(
      data=plotDataSummary, aes(ymin=min, ymax=max),
      linetype="dashed", alpha=0.25
    )

  # Axes' labels
  varname <- plotRequest$criteria$varname
  xlab <- levelsLableForPlots(plotRequest$criteria$obnumber, varname)
  ylab <- sprintf("%s [%s]", varname, units[[varname]])
  obplot <- obplot + labs(x=xlab, y=ylab)

  # Set x-axis limits and flipping axes
  # The change in xlim causes ggplotly to omit either the shape or
  # the line (whichever is added later) from the legend. Not a big
  # deal but worth pointing out, as this but may be solved in later
  # releases of the plotly package
  if(startsWith(tolower(xlab), "pressure")) {
    xlim <- c(max(refPressures,localPlotData[["level"]]), 0)
    obplot <- obplot + scale_x_reverse()
  } else {
    xlim <- c(0, max(refHeights,localPlotData[["level"]]))
  }
  obplot <- obplot + coord_flip_wrapper(default=TRUE, xlim=xlim)

  # Hide legend if there's only one curve to be plotted
  if(length(unique(localPlotData[["variable"]]))<2) {
    obplot <- obplot + theme(legend.position = "none")
  }

  return(obplot)
}


registerPlotType("StationVerticalProfiles",
  plotCreate("plotStationVerticalProfile",
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

registerPlotType("StationVerticalProfiles",
  plotCreate(c("plotStationVerticalProfile"),
    name="Station Vertical Profile: Obs, FG & Analysis",
    dateType="single",
    queryStub=paste(
      "SELECT DISTINCT level, obsvalue,",
      "(obsvalue-fg_dep) AS first_guess,",
      "(obsvalue-an_dep) AS analysis",
      "FROM usage WHERE %s AND",
      "(fg_dep IS NOT NULL) AND",
      "(an_dep IS NOT NULL) AND (obsvalue IS NOT NULL)",
      "ORDER BY level"
    ),
    requiredFields=list(
      "station", "obnumber", "obname", "varname"
    )
  )
)

registerPlotType("StationVerticalProfiles",
  plotCreate(c("plotStationVerticalProfile"),
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

registerPlotType("StationVerticalProfiles",
  plotCreate(c("plotStationVerticalProfile"),
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
