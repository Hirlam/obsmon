registerPlotCategory("Timeseries")

numberOfObservations <- function(plotRequest) {
  query <- paste("SELECT DTG, nobs_total, level FROM obsmon WHERE",
                 buildWhereClause(plotRequest$criteria))
  range <- list(plotRequest$criteria$dtgMin, plotRequest$criteria$dtgMax)
  res <- expQuery(plotRequest$exp, plotRequest$db, query, dtgs=range)
  obplot <- ggplot(data=res) +
    geom_line(aes(x=DTG, y=nobs_total)) +
    xlab("DATE") +
    ylab("nrobs") +
    labs(title="test") +
    facet_wrap(~ level)
  obplot
}
registerPlotType("Timeseries", "Number of Observations", numberOfObservations)
