plotlySaveAsFigDimensions <- list(height=755, width=1200)

levelsLableForPlots <- function(obnumber, varname=character(0)) {
  strObnumber <- as.character(obnumber)
  obstype <- getAttrFromMetadata("category", obnumber=obnumber)
  quantity <- "Pressure"
  if(obstype=="surface" || (isTRUE(strObnumber=="13") && !isTRUE(varname=="rh"))) {
    quantity <- "Height"
  }
  label <- sprintf("%s [%s]", quantity, units[[tolower(quantity)]])
  return(label)
}

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

plotIsPlotly <- function(myPlot) {
  # To be used in the server logic to determine whether to use regular
  # (non-interactive) or plotly (interactive) plot outputs.
  rtn <- any(c("plotly", "plotlyhtmlwidget") %in% class(myPlot))
  return(rtn)
}

configPlotlyWrapper <- function(...) {
  # Wrapper to plotly's config function, with some useful defaults
  # For a list of all config options, please visit
  # <https://github.com/plotly/plotly.js/blob/master/src/plot_api/plot_config.js>
  # Se allso <https://plotly-r.com/control-modebar.html>
  argList <- list(...)
  argNames <- names(argList)
  if(!("displaylogo" %in% argNames)) argList$displaylogo <- FALSE
  if(!("cloud" %in% argNames)) argList$cloud <- FALSE
  if(!("scrollZoom" %in% argNames)) argList$scrollZoom <- TRUE

  # Defaults for what users are allowed to edit in the plots
  if(!("editable" %in% argNames)) argList$editable <- TRUE
  editsOpts <- list(
    titleText=FALSE,
    shapePosition=FALSE
  )
  if("edits" %in% argNames) {
    for(name in names(argList$edits)) {
      editsOpts[[name]] <- argList$edits[[name]]
    }
  }
  argList$edits <- editsOpts

  # Defaults for saving figures
  toImageButtonOpts <- list(
    filename="obsmon_plot",
    format="png",
    height=plotlySaveAsFigDimensions$height,
    width=plotlySaveAsFigDimensions$width
  )
  if("toImageButtonOptions" %in% argNames) {
    for(name in names(argList$toImageButtonOptions)) {
      toImageButtonOpts[[name]] <- argList$toImageButtonOptions[[name]]
    }
  }
  argList$toImageButtonOptions <- toImageButtonOpts

  return(do.call(config, argList))
}

getStationsForPlotTitle <- function(plotRequest, plotData, limit=5) {
  crit <- plotRequest$criteria
  if(length(crit$station)==0) {
    stations <- character(0)
  } else {
    stations <- paste(sort(unique(plotData$statLabel)), collapse=", ")
    if(stations=="") stations<-paste(sort(unique(crit$station)),collapse=", ")
    if(stations=="") stations <- character(0)
  }
  if(length(stations)>limit) stations <- character(0)
  return(stations)
}

addTitleToPlot <- function(myPlot, title) {
  if(is.null(myPlot) || is.null(title)) return(myPlot)
  newPlot <- tryCatch({
    if(is.ggplot(myPlot)) {
      myPlot + ggtitle(title) + theme(plot.title=element_text(hjust=0.5))
    } else if("plotly" %in% class(myPlot)) {
      yTitle <- attr(myPlot, "yTitle")
      if(is.null(yTitle)) yTitle <- 1.0
      # Use an annotation instead of an actual title, as otherwise plotly
      # will fail to put it in the correct position without overlaps (and
      # it also won't allow users to change the position of the text)
      myPlot %>% add_annotations(
          text=title,
          showarrow=FALSE,
          font=list(size=20),
          xref="paper", xanchor="center", x=0.5,
          # Push y a bit above 1 as, otherwise, the title may overlap with the
          # plot when using ggplotly on a ggplot object containing facet_wraps
          yref="paper", yanchor="bottom", y=yTitle
      ) %>% layout(title=FALSE)
    } else {
      grid.arrange(myPlot, top=textGrob(title, gp=gpar(fontsize=13)))
    }
    },
    error=function(e) {
      flog.error("addTitleToPlot: Problems setting plot title: %s", e)
      myPlot
    }
  )
  return(newPlot)
}

plotExportedDataInfo <- function(plot) {
  header <- paste0(
    paste("# Plot title:", plot$title, "\n"),
    sprintf(
      "# Data retrieved by Obsmon v%s on %s using the following query:\n",
      obsmonVersion, strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
    ),
    paste0("# ", plot$queryUsed, "\n"),
    paste("\n")
  )
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
  if(is.null(plottype)) return(FALSE)
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
      statName <- tryCatch(synopStations[statID], error=function(e) NA)
      label <- statID
      if(!anyNA(statName)) label <- sprintf("%s (%s)", statID, statName)
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
plotGenerate <- function(p, plotRequest, plotData, interactive) UseMethod("plotGenerate")
plotIsApplicable <- function(p, criteria) UseMethod("plotIsApplicable")
plotTitle <- function(p, plotRequest, plotData) UseMethod("plotTitle")
doMap <- function(p, plotRequest, plotData) UseMethod("doMap")
doPlot <- function(p, plotRequest, plotData) UseMethod("doPlot")
doPlotly <- function(p, plotRequest, plotData) UseMethod("doPlotly")

# Provide defaults
doPlotly.default <- function(p, plotRequest, plotData) {
  # Generate a regular ggplot2 plot and then use plotly's
  # ggplotly function to convert it to a plotly object
  ggplotPlot <- doPlot(p, plotRequest, plotData)
  plotlyPlot <- ggplotly(ggplotPlot,
    tooltip=c("x","y")
  ) %>%
    layout(
      margin=list(t=100),
      legend=list(orientation="v", yanchor="center", y=0.5)
    )
  return(plotlyPlot)
}

plotBuildQuery.default <- function(p, plotRequest) {
  sprintf(p$queryStub, buildWhereClause(plotRequest$criteria))
}

plotGenerate.default <- function(p, plotRequest, plotData, interactive) {
  if (plotRequest$criteria$obnumber==7 && ("level" %in% colnames(plotData))) {
    names(plotData)[names(plotData)=="level"] <- "channel"
  }
  result <- list(title=plotTitle(p, plotRequest, plotData))
  if(length(result$title)==0) flog.warn("plotGenerate: Empty plot title")
  if (!isTRUE(nrow(plotData)>0)) {
    result$obmap <- NULL
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
    result$obmap <- doMap(p, plotRequest, plotData)
    result$obplot <- NULL
    if(interactive) {
      result$obplot <- tryCatch(
        doPlotly(p, plotRequest, plotData),
        error=function(e){
          flog.warn('plotGenerate: Failure making plot "%s" interactive: %s',
            p$name, e
          )
          return(NULL)
        }
      )
    }
    if(is.null(result$obplot)) result$obplot <- doPlot(p,plotRequest,plotData)
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
  crit <- plotRequest$criteria
  if(as.character(crit$obnumber)=="7") {
    detail <- sprintf("sensor=%s, satname=%s", crit$obname, crit$satname)
  } else {
    detail <- sprintf("obname=%s, varname=%s", crit$obname, crit$varname)
  }
  title <- sprintf(
    "%s: %s\ndb=%s, DTG=%s, %s",
    plotRequest$expName, p$name,
    plotRequest$dbType, formatDtg(crit$dtg), detail
  )
  return(title)
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
    res$obname <- input$sensor
    res$satname <- input$satellite
    levels <- input$channels
    excludeLevels <- input$excludeChannels
  } else {
    res$obname <- obname
    res$varname <- input$variable
    levels <- input$levels
    excludeLevels <- input$excludeLevels
  }

  res$levels <- list()
  if(length(levels)>0 && levels!="") res$levels <- levels
  res$excludeLevels <- list()
  if(length(excludeLevels)>0 && excludeLevels!="") {
    res$excludeLevels <- excludeLevels
  }

  if(obSupportsStationChoice(obname)) {
    station <- input$station
    if(plotRequiresSingleStation(input$plottype)) station<-input$stationSingle
    if(is.null(station) || "" %in% station) station <- character(0)
    res$station <- station
  }

  res$dtg <- getPlotDtgCriteriaFromUiInput(input)

  return(res)
}
# Perform plotting
preparePlots <- function(plotter, plotRequest, db, interactive=TRUE) {
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
    if(isTRUE(nrow(plotData)>0)) {
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

    res <- plotGenerate(plotter,plotRequest,plotData,interactive=interactive)
    res[["queryUsed"]] <- query
    res[["plotData"]] <- plotData
    return(res)
  },
  error=function(e) {flog.error(paste("preparePlots:", e)); NULL}
  )
}

preparePlotsCapturingOutput <- function(...) {
  # We call preparePlots asyncronously in the shiny app to make it possible
  # to cancel plots. Catching the output is useful in this case because we
  # want to suppress an annoying blank line that is printed out every time a
  # plot is cancelled, but we don't want to prevent any error message from
  # being shown. The output produced by preparePlots will, if not empty, be
  # printed out upon completion of the async task.
  output <- capture.output({
    plots <- preparePlots(...)
  }, type="message")
  output <- trimws(paste(output, collapse="\n"))
  if(output=="") output <- character(0)
  return(list(plots=plots, output=output))
}
