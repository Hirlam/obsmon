registerPlotCategory("VerticalProfiles")

plotTitle.plotVerticalProfiles <- function(p, plotRequest, plotData) {
  dtg <- formatDtg(plotRequest$criteria$dtg)
  station <- plotRequest$criteria$station
  title <- sprintf("%s: %s %s %s", plotRequest$expName, p$name, station, dtg)
}

doPlot.plotVerticalProfiles <- function(p, plotRequest, plotData) {
  df <- data.frame(level=plotData$level, obsvalue=plotData$obsvalue)
  obplot <- ggplot(data=df) +
    aes(x=level, y=obsvalue) +
    geom_line(colour="black") +
    geom_point(size=2, colour="black") +
    coord_flip() +
    labs(x="Level", y=plotRequest$criteria$varname)
  return(obplot)
}

registerPlotType("VerticalProfiles",
  plotCreate("plotVerticalProfiles",
    name="Vertical Profile",
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

