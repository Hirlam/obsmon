library(ggplot2)

source("sql.R")

plotTypesHierarchical <- list("Timeseries"=list())
plotTypesFlat <- list()

registerPlotType <- function(category, name, plotType) {
  id <- deparse(substitute(plotType))
  if (id %in% names(plotTypesFlat)) {
    flog.error("Plottype %s('%s') is already registered. Discarding.", id, name)
    return(NULL)
  }
  categoryList <- plotTypesHierarchical[[category]]
  if (is.null(categoryList)) {
    flog.error("Unknown plottype category %s, discarding plottype %s('%s').",
               category, id, name)
    return(NULL)
  }
  plotTypesFlat[[id]] <<- plotType
  categoryList[[name]] <- id
  plotTypesHierarchical[[category]] <<- categoryList
}

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

registerPlotType("Timeseries", "Number of Observations", numberOfObservations)
