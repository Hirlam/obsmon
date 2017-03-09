library(ggplot2)

source("sql.R")

numberOfObservations <- function(plotRequest) {
  query <- paste("SELECT DTG, nobs_total, level FROM obsmon WHERE",
                 buildWhereClause(plotRequest$criteria))
  res <- expQuery(plotRequest$exp, plotRequest$db, query)
  obplot <- ggplot(data=res) +
    geom_line(aes(x=DTG, y=nobs_total)) +
    xlab("DATE") +
    ylab("nrobs") +
    labs(title="test") +
    facet_wrap(~ level)
  obplot
}

initPlotTypes <- function() {
  plotTypes <- list()
  plotTypes[["Number of Observations"]] = numberOfObservations
  plotTypes
}

plotTypes <- initPlotTypes()
