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
plotBuildQuery <- function(p, plotRequest) UseMethod ("plotBuildQuery")
plotGenerate <- function(p, plotRequest,
                         plotData, progressTracker) UseMethod("plotGenerate")
plotIsApplicable <- function(p, criteria) UseMethod("plotIsApplicable")
plotTitle <- function(p, plotRequest, plotData) UseMethod("plotTitle")
doMap <- function(p, plotRequest, plotData) UseMethod("doMap")
doPlot <- function(p, plotRequest, plotData) UseMethod("doPlot")

# Provide defaults
plotBuildQuery.default <- function(p, plotRequest) {
  sprintf(p$queryStub, buildWhereClause(plotRequest$criteria))
}

plotGenerate.default <- function(p, plotRequest, plotData, progressTracker) {
  result <- list()
  if (is.null(plotData) || nrow(plotData)==0) {
    image <- readPNG("./nodata.png")
    result$obplot <- rasterGrob(image)
    result$obmap <- NULL
    result$title <- NULL
  } else {
    if (plotRequest$criteria$obnumber == 7
        && "level" %in% colnames(plotData)) {
      plotData <- rename(plotData, channel=level)
    }
    result$title <- plotTitle(p, plotRequest, plotData)
    result$obplot <- doPlot(p, plotRequest, plotData)
    result$obmap <- doMap(p, plotRequest, plotData)
  }
  result
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

plotTitle.default <- function(p, plotRequest, plotData) {
  dtg <- formatDtg(plotRequest$criteria$dtg)
  titleStub <- sprintf("%s: %s %%s %s", plotRequest$expName, p$name, dtg)
  switch(
      as.character(plotRequest$criteria$obnumber),
      "7"={
        title <- sprintf(titleStub,
                         paste(plotRequest$criteria$obname,
                               plotRequest$criteria$satname))
      },
      {
        title <- sprintf(titleStub,
                         paste(plotRequest$criteria$obname,
                               plotRequest$criteria$varname))
      }
  )
  title
}

doMap.default <- function(p, plotRequest, plotData) {
  NULL
}

plotCreate <- function(clazz, name, dateType, queryStub, requiredFields, ...) {
  p <- list()
  p$dateType <- dateType
  p$name <- name
  p$queryStub <- queryStub
  p$requiredFields <- requiredFields
  p <- c(p, list(...))
  class(p) <- clazz
  p
}


postProcessQueriedPlotData <- 
  function(plotter, plotData) UseMethod("postProcessQueriedPlotData")

postProcessQueriedPlotData.default <- function(plotter, plotData) {
    plotData
}
