coord_flip_wrapper <- function(..., default=FALSE) {
  # Adds the argument "default" to the original ggplot's coord_flip.
  # This gets rid of the annoying "Coordinate system already present. Adding
  # new coordinate system, which will replace the existing one" warning which
  # is otherwise issued when trying to modify the x and y limits in plots that
  # use cood_flip
  # Adapted from <https://github.com/tidyverse/ggplot2/issues/2799>
  cf <- coord_flip(...)
  cf$default <- default
  return(cf)
}

plotCanBeMadeInteractive <- function(myPlot) {
 # To be used in the server logic to determine whether to use regular
 # (non-interactive) or plotly (interactive) plot outputs.
 # When using CoordMap in ggplot2, conversion to plotly makes the projections
 # look a bit weird -- hence the restriction on CoordMap below.
 rtn <- is.ggplot(myPlot) && !("CoordMap" %in% class(myPlot$coordinates))
 return(rtn)
}

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

plotSupportsChoosingStations <- function(plottype=NULL, obtype=NULL) {
  if(is.null(plottype) || isTRUE(obtype=="satem")) return(FALSE)
  infoAboutSelectedPlotType <- plotTypesFlat[[plottype]]
  query <- infoAboutSelectedPlotType$queryStub
  # StationIDs are not stored in the "obsmon" table, only in "usage"
  queryIsFromUsage <- grepl("FROM{1}[[:space:]]+usage",query,ignore.case=TRUE)
  return(isTRUE(queryIsFromUsage))
}

plotRequiresSingleStation <- function(plottype=NULL) {
  plotReqFields <- plotTypesFlat[[plottype]]$requiredFields
  return(isTRUE("station" %in% plotReqFields))
}

putLabelsInStations <- function(stations=NULL, obname=NULL) {
  if(length(stations)==0) return(stations)
  if(isTRUE(obname=="synop")) {
    stationLabels <- c()
    for(statID in stations) {
      statName <- synopStations[statID]
      label <- statID
      if(is.character(statName)) label<-sprintf("%s (%s)",statID,statName)
      stationLabels <- c(stationLabels, label)
    }
    names(stations) <- stationLabels
  } else {
    names(stations) <- stations
  }
  return(stations)
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
    names(plotData)[names(plotData)=="level"] <- "channel"
  }
  result <- list(title=plotTitle(p, plotRequest, plotData))
  if(length(result$title)==0) flog.warn("plotGenerate: Empty plot title")
  if (is.null(plotData) || nrow(plotData)==0) {
    result$obmap=NULL
    if(is.null(plotData)) {
      msg <- paste0(
        "Could not produce plot: ",
        "The required data file(s) might be inaccessible.\n"
      )
    } else {
      msg <- "Query returned no data"
    }
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
  dtgCrit <- tryCatch(
    switch(plotTypesFlat[[input$plottype]]$dateType,
      "single"=date2dtg(input$date, input$cycle),
      "range"={
        dateRange <- sort(input$dateRange)
        list(dateRange[1], dateRange[2], input$cycles)
      }
    ),
    error=function(e) NULL
  )
  return(dtgCrit)
}

plotsBuildCriteria <- function(input) {
  res <- list()
  res$info <- list()
  obname <- input$obname
  res$obnumber <- getAttrFromMetadata('obnumber', obname=obname)
  if (isTRUE(obname=='satem')) {
    sensor <- input$sensor
    res$obname <- sensor
    res$satname <- input$satellite
    levels <- input$channels
    excludeLevels <- input$excludeChannels
  } else {
    res$obname <- obname
    res$varname <- input$variable
    levels <- input$levels
    excludeLevels <- input$excludeLevels

    station <- input$station
    if(plotRequiresSingleStation(input$plottype)) station<-input$stationSingle
    if(is.null(station) || "" %in% station) station <- character(0)
    res$station <- station
  }
  res$levels <- list()
  if(length(levels)>0 && levels!="") res$levels <- levels
  res$excludeLevels <- list()
  if(length(excludeLevels)>0 && excludeLevels!="") {
    res$excludeLevels <- excludeLevels
  }

  res$dtg <- getPlotDtgCriteriaFromUiInput(input)

  return(res)
}
# Perform plotting
preparePlots <- function(plotter, plotRequest, db) {
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
      statIds <- c()
      for(statid in plotData$statid) {
        statid <- gsub(" ", "", gsub("'", "", statid))
        statIds <- c(statIds, statid)
      }
      obname <- plotRequest$criteria$obname
      if(isTRUE(plotRequest$criteria$obnumber==7)) obname="satem"
      stations <- putLabelsInStations(statIds, obname)
      plotData$statLabel <- names(stations)
    }

    res <- plotGenerate(plotter, plotRequest, plotData)
    res[["queryUsed"]] <- query
    res[["plotData"]] <- plotData
    return(res)
  },
  error=function(e) {flog.error(paste("preparePlots:", e)); NULL}
  )
}
