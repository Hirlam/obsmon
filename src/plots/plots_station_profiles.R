registerPlotCategory("StationProfiles")

plotTitle.plotStationProfile <- function(p, plotRequest, plotData) {
  crit <- plotRequest$criteria
  stationLabel <- getStationsForPlotTitle(plotRequest, plotData)
  title <- sprintf(
    "%s: %s\nstation=%s\ndb=%s, DTG=%s, obname=%s, varname=%s",
    plotRequest$expName, p$name,
    stationLabel,
    plotRequest$dbType, formatDtg(crit$dtg), crit$obname, crit$varname
  )
}

doPlot.plotStationProfile <- function(p, plotRequest, plotData) {
  scaleColors <- c(
    "obsvalue"="black", "obsvalue_corrected"="black", "obsvalue_raw"="blue",
    "fg_dep"="blue", "first_guess"="blue",
    "an_dep"="red", "analysis"="red"
  )

  localPlotData <- plotData %>%
    group_by(level) %>%
    melt(id="level", na.rm=TRUE)
  localPlotDataMean <- plotData %>%
    group_by(level) %>%
    summarize_all(mean, na.rm=TRUE) %>%
    melt(id="level", na.rm=TRUE)

  obplot <- ggplot(localPlotData) +
    aes(x=level, y=value, group=variable, colour=variable, shape=variable) +
    scale_colour_manual(values=scaleColors) +
    geom_point(size=2) +
    # Draw line through mean values instead of actual data point values
    geom_line(data=localPlotDataMean, size=1.15)
  # For clarity: Connect groups of points belonging to the same level
  if(nrow(localPlotData) != nrow(localPlotDataMean)) {
    # ggplot doesn't seem to like this when localPlotData==localPlotDataMean
    # The smaller-sized geom_point added below, for instance, mess up the
    # legends. Also, for whatever reason, this causes the above added
    # geom_line to vanish sometimes.
    obplot <- obplot +
      # This helps plotly show the mean value. Otherwise, plotly's tooltip
      # shows "value=NA"
      geom_point(data=localPlotDataMean, size=0.01) +
      stat_summary(
        fun.y=mean, fun.ymin=min, fun.ymax=max, geom="linerange", alpha=0.25
      )
  }

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


registerPlotType("StationProfiles",
  plotCreate("plotStationProfile",
    name="Station Obsvalues",
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

registerPlotType("StationProfiles",
  plotCreate(c("plotStationProfile"),
    name="Station Obs, FG & Analysis",
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

registerPlotType("StationProfiles",
  plotCreate(c("plotStationProfile"),
    name="Station FG & Analysis Departure",
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

registerPlotType("StationProfiles",
  plotCreate(c("plotStationProfile"),
    name="Station Bias",
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
