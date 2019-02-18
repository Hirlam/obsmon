registerPlotCategory("VerticalProfiles")

plotTitle.plotVerticalProfiles <- function(p, plotRequest, plotData) {
  dtg <- formatDtg(plotRequest$criteria$dtg)
  station <- plotRequest$criteria$station
  title <- sprintf("%s: %s %s %s", plotRequest$expName, p$name, station, dtg)
}

doPlot.plotVerticalProfiles <- function(p, plotRequest, plotData,
                                  maskColumns=character(0),
                                  colours=NULL, shapes=NULL) {
# TEST new
  varname <- plotRequest$criteria$varname
  xlab <- "Level"
  ylab <- varname
  df <- data.frame(level=plotData$level, obsvalue=plotData$obsvalue)
  obplot <- ggplot(data=df) +
    aes(x=level, y=obsvalue) +
    geom_line(colour="black") +
    geom_point(size=3, colour="black") +
    coord_flip() +
    labs(x=xlab, y=ylab)
  return(obplot)
# END OF TEST new


  # TEST OLD
#  localPlotData <- melt(plotData[!(colnames(plotData) %in% maskColumns)],
#                        id=c("obsvalue", "level"))
#  obplot <- ggplot() +
#    geom_line(data=localPlotData,
#              aes(x=value, y=level, group=variable),
#              na.rm=TRUE, alpha=.1) +
#    geom_point(data=localPlotData,
#               aes(x=value, y=level, shape=variable, colour=variable, fill=variable),
#               na.rm=TRUE) +
#    labs(x="T (K)") +
#    facet_wrap("level", labeller=label_both)
#  if (!is.null(shapes)) {
#    obplot <- obplot +
#      scale_shape_manual(values=shapes)
#  }
#  if (!is.null(colours)) {
#    obplot <- obplot +
#      scale_colour_manual(values=colours) +
#      scale_fill_manual(values=colours)
#  } else {
#    obplot <- obplot +
#      scale_colour_brewer(palette="Spectral")
#  }
#  obplot
  # END OF TEST OLD
}

registerPlotType(
    "VerticalProfiles",
    plotCreate("plotVerticalProfiles", "Vertical Profile", "single",
               paste(
                 "SELECT DISTINCT level, obsvalue FROM usage WHERE %s",
                 "ORDER BY level"
               ),
               requiredFields=list(
                 "obnumber", "obname", "varname", "station"
               )
  )
)

