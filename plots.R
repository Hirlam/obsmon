library(ggplot2)
library(png)
library(plyr)
library(grid)

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
  categoryList <- as.list(c(categoryList, plotType$name))
  plotTypesHierarchical[[category]] <<- categoryList
}

applicablePlots <- function(criteria) {
  plots <- list()
  for (categoryName in names(plotTypesHierarchical)) {
    category <- unlist(plotTypesHierarchical[[categoryName]], use.names=FALSE)
    choices <- names(Filter(partial(plotIsApplicable,
                                    criteria=criteria),
                            plotTypesFlat[category]))
    plots[[categoryName]] <- as.list(choices)
  }
  plots
}

# Define generics
plotGenerate <- function(p, plotRequest, progressTracker) UseMethod("plotGenerate")
plotIsApplicable <- function(p, criteria) UseMethod("plotIsApplicable")
doPlot <- function(p, plotRequest, plotData) UseMethod("doPlot")

# Provide defaults
plotGenerate.default <- function(p, plotRequest, progressTracker) {
  progressTracker <- addTask(progressTracker, "Querying database")
  progressTracker <- addTask(progressTracker, "Preparing plot")
  progressTracker <- updateTask(progressTracker, "Querying database", 0.)
  query <- sprintf(p$queryStub, buildWhereClause(plotRequest$criteria))
  plotData <- expQuery(plotRequest$exp, plotRequest$db,
                       query, dtgs=plotRequest$criteria$dtg,
                       progressTracker=progressTracker)
  if (nrow(plotData)==0) {
    image <- readPNG("./nodata.png")
    obplot <- rasterGrob(image)
  } else {
    if (plotRequest$criteria$obnumber == 7
        & "level" %in% colnames(plotData)) {
      plotData <- rename(plotData, c("level"="channel"))
    }
    obplot <- doPlot(p, plotRequest, plotData)
  }
  progressTracker <- updateTask(progressTracker, "Preparing plot", 1.)
  list(obplot, progressTracker)
}

plotIsApplicable.default <- function(p, criteria) {
  requiredNames <- names(p$requiredFields)
  if (is.null(requiredNames)) {
    all(p$requiredFields %in% names(criteria))
  } else {
    res <- mapply(function(n, v) ifelse(n == "",
                                        v %in% names(criteria),
                                        criteria[[n]] %in% v),
                  requiredNames, p$requiredFields)
    all(res)
  }
}

plotCreate <- function(class, name, dateType, queryStub, requiredFields) {
  p <- structure(list(), class = class)
  p$dateType <- dateType
  p$name <- name
  p$queryStub <- queryStub
  p$requiredFields <- requiredFields
  p
}

source("plots_statistical.R")
source("plots_timeseries.R")
source("plots_maps.R")
source("plots_diagnostic.R")
