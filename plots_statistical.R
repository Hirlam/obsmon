registerPlotCategory("Statistical")

fgAnDeparture <- function(plotRequest) {
  dtg <- plotRequest$criteria$dtgMax
  plotRequest$criteria$dtgExact <- dtg
  plotRequest$criteria$dtgMin <- NULL
  plotRequest$criteria$dtgMax <- NULL
  query <- paste("SELECT",
                 "fg_bias_total, an_bias_total,",
                 "fg_rms_total, an_rms_total, nobs_total, level",
                 "FROM obsmon WHERE",
                 buildWhereClause(plotRequest$criteria),
                 "ORDER BY level")
  plotData <- expQuery(plotRequest$exp, plotRequest$db, query, dtgs=dtg)
  obPlot <- ggplot(plotData, aes(level)) +
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
}
registerPlotType("Statistical", "FG+An", fgAnDeparture)
