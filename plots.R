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

registerPlotType <- function(category, plotType) {
  if (plotType$name %in% names(plotTypesFlat)) {
    flog.error("Plottype '%s' is already registered. Discarding.", plotType$name)
    return(NULL)
  }
  categoryList <- plotTypesHierarchical[[category]]
  if (is.null(categoryList)) {
    flog.error("Unknown plottype category %s, discarding plottype '%s'.",
               category, plotType$name)
    return(NULL)
  }
  plotTypesFlat[[plotType$name]] <<- plotType
  categoryList[[plotType$name]] <- plotType$name
  plotTypesHierarchical[[category]] <<- categoryList
}

# Define generics
plotGenerate <- function(p, plotRequest) UseMethod("plotGenerate")
doPlot <- function(p, plotRequest, plotData) UseMethod("doPlot")

# Provide defaults
plotGenerate.default <- function(p, plotRequest) {
  query <- sprintf(p$queryStub, buildWhereClause(plotRequest$criteria))
  plotData <- expQuery(plotRequest$exp, plotRequest$db,
                       query, dtgs=plotRequest$dtg)
  obplot <- doPlot(p, plotRequest, plotData)
  obplot
}

plotCreateConstructor <- function(class, dateType) {
  function(name, queryStub, additionalPlotting=NULL) {
    p <- structure(list(), class = class)
    p$name <- name
    p$queryStub <- queryStub
    p$dateType <- dateType
    p$additionalPlotting <- additionalPlotting
    p
  }
}

source("plots_statistical.R")
source("plots_timeseries.R")
source("plots_maps.R")
source("plots_diagnostic.R")
