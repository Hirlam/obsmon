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
plotGenerate <- function(p, plotRequest, plotData) UseMethod("plotGenerate")
plotIsApplicable <- function(p, criteria) UseMethod("plotIsApplicable")
plotTitle <- function(p, plotRequest, plotData) UseMethod("plotTitle")
doMap <- function(p, plotRequest, plotData) UseMethod("doMap")
doPlot <- function(p, plotRequest, plotData) UseMethod("doPlot")

# Provide defaults
plotBuildQuery.default <- function(p, plotRequest) {
  sprintf(p$queryStub, buildWhereClause(plotRequest$criteria))
}

plotGenerate.default <- function(p, plotRequest, plotData) {
  if (plotRequest$criteria$obnumber==7 && ("level" %in% colnames(plotData))) {
    plotData <- rename(plotData, channel=level)
  }
  result <- list(title=plotTitle(p, plotRequest, plotData))
  if (is.null(plotData) || nrow(plotData)==0) {
    result$obmap=NULL
    if(is.null(plotData)) msg<-"A problem occurred. Please check the logs"
    else msg <- "Query returned no data"
    result$obplot=grobTree(
      rectGrob(gp=gpar(col="black", fill="grey90", alpha=0.5)),
      textGrob(msg)
    )
  } else {
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

# Functions used in in server.R
# Build named list of plot criteria
getPlotDtgCriteriaFromUiInput <- function(input) {
  dtgCrit <- NULL
  dateType <- plotTypesFlat[[input$plottype]]$dateType
  if(!is.null(dateType)) {
    dtgCrit <- switch(dateType,
      "single"=date2dtg(input$date, input$cycle),
      "range"={
        dateRange <- input$dateRange
        list(dateRange[1], dateRange[2], input$cycles)
      }
    )
  }
  return(dtgCrit)
}

plotsBuildCriteria <- function(input) {
  res <- list()
  res$info <- list()
  obname <- input$obname
  res$obnumber <- getAttrFromMetadata('obnumber', obname=obname)
  if (obname == 'satem') {
    sensor <- input$sensor
    res$obname <- sensor
    res$satname <- input$satellite
    levels <- input$channels
  } else {
    res$obname <- obname
    res$varname <- input$variable
    levels <- input$levels

    station <- input$station
    if("" %in% station) station <- ""
    res$station <- station
  }
  res$levels <- list()
  if(length(levels)>0 && levels!="") res$levels <- levels

  res$dtg <- getPlotDtgCriteriaFromUiInput(input)

  return(res)
}
# Perform plotting
preparePlots <- function(plotter, plotRequest, db, stations) {
  tryCatch({
    isWindspeed <- "varname" %in% names(plotRequest$criteria) &&
      plotRequest$criteria$varname %in% c("ff", "ff10m")
    query <- NULL
    if (isWindspeed) {
      plotData <- buildFfData(db, plotter, plotRequest)
    } else {
      query <- plotBuildQuery(plotter, plotRequest)
      plotData <- performQuery(db, query, plotRequest$criteria$dtg)
      # Postprocessing plotData returned by performQuery.
      # This may be useful, e.g., if performing averages over a
      # picked date range.
      plotData <- postProcessQueriedPlotData(plotter, plotData)
    }
    if(!is.null(plotData) && nrow(plotData)>0) {
      statLabels <- c()
      for(statid in plotData$statid) {
        statid <- gsub(" ", "", gsub("'", "", statid))
        statLabels <- c(statLabels, names(stations)[stations==statid])
      }
      if(nrow(plotData)==length(statLabels)) {
        plotData$statLabel <- statLabels
      } else {
        plotData$statLabel <- plotData$statid
      }
    }

    res <- plotGenerate(plotter, plotRequest, plotData)
    res[["queryUsed"]] <- query
    res[["plotData"]] <- plotData
    return(res)
  },
  error=function(e) {flog.error(paste("preparePlots:", e)); NULL}
  )
}
