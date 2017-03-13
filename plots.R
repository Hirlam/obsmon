library(ggplot2)

source("sql.R")

plotTypesHierarchical <- list()
plotTypesFlat <- list()

registerPlotCategory <- function(category) {
  if (category %in% plotTypesHierarchical) {
    flog.error("Category %s is already registered. Discarding.", category)
    return(NULL)
  }
  plotTypesHierarchical[[category]] <<- list()
}

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

source("plots_statistical.R")
source("plots_timeseries.R")
source("plots_maps.R")
source("plots_diagnostic.R")
