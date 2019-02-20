registerPlotCategory("VerticalProfiles")

plotTitle.plotVerticalObsProfile <- function(p, plotRequest, plotData) {
  dtg <- formatDtg(plotRequest$criteria$dtg)
  station <- plotRequest$criteria$station
  title <- sprintf("%s: %s %s %s", plotRequest$expName, p$name, station, dtg)
}

doPlot.plotVerticalObsProfile <- function(p, plotRequest, plotData) {
  df <- data.frame(level=plotData$level, obsvalue=plotData$obsvalue)
  varname <- plotRequest$criteria$varname
  ylab <- sprintf("%s [%s]", varname, units[[varname]])
  obplot <- ggplot(data=df) +
    aes(x=level, y=obsvalue) +
    geom_line(colour="black") +
    geom_point(size=2, colour="black") +
    coord_flip() +
    labs(x="Level", y=ylab)
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

