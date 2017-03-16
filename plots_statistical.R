registerPlotCategory("Statistical")

plotCreateStatistical <- plotCreateConstructor("plotStatistical", "single")

doPlot.plotStatistical <- function(p, plotRequest, plotData) {
  title <- paste(plotRequest$exp$name, ":", p$name,
                 plotRequest$varname)
  obplot <- ggplot(data=plotData) +
    aes(level) +
    geom_line(aes(y=fg_bias_total,colour="fg_bias_total")) +
    geom_point(aes(y=fg_bias_total,colour="fg_bias_total"),size=4) +
    geom_line(aes(y=an_bias_total,colour="an_bias_total"))+
    geom_point(aes(y=an_bias_total,colour="an_bias_total"),size=4) +
    geom_line(aes(y=fg_rms_total,colour="fg_rms_total")) +
    geom_point(aes(y=fg_rms_total,colour="fg_rms_total"),size=4) +
    geom_line(aes(y=an_rms_total,colour="an_rms_total")) +
    geom_point(aes(y=an_rms_total,colour="an_rms_total"),size=4) +
    coord_flip()+
    scale_x_continuous(breaks=plotData$level)
  obplot
}

registerPlotType(
    "Statistical",
    plotCreateStatistical("First Guess and Analysis Departure",
                          paste("SELECT",
                                "fg_bias_total, an_bias_total,",
                                "fg_rms_total, an_rms_total, nobs_total, level",
                                "FROM obsmon WHERE %s",
                                "ORDER BY level"),
                          list("obnumber", "obname", "levels"))
)
