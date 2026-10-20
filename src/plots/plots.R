# SPINNER_CHART just shows a spinner at the middle of an empty plot
EMPTY_CHART <- plotly_empty(type="scatter", mode="markers") %>%
  config(
    displayModeBar=FALSE,
    scrollZoom=FALSE,
    editable=FALSE,
    staticPlot=TRUE
  )
SPINNER_CHART <- EMPTY_CHART %>%
  layout(
    images=list(
      list(
        # Add the spinner gif
        source=SPINNER_IMAGE_PATH,
        x=0.44,
        y=0.4125,
        sizex=0.3,
        sizey = 0.15,
        xref="paper",
        yref="paper",
        xanchor="left",
        yanchor="bottom"
      )
    )
  )
lockBinding("EMPTY_CHART", globalenv())
lockBinding("SPINNER_CHART", globalenv())

.asScalarCharacterOrNa <- function(val) {
  if (
    is.null(val) ||
    inherits(val, "uninitializedField") ||
    isS4(val) ||
    is.environment(val) ||
    is.function(val) ||
    length(val) == 0 ||
    (!is.atomic(val) && !is.factor(val))
  ) {
    return(NA_character_)
  }

  res <- tryCatch(as.character(val), error=function(e) character(0))
  res <- res[!is.na(res) & nzchar(res)]
  if(length(res) == 0) return(NA_character_)
  res[[1]]
}

.plotHasComparison <- function(plot) {
  length(.getComparisonPlots(plot)) > 0
}

.plotUsesJointComparison <- function(plot) {
  .plotHasComparison(plot) &&
    plot$parentType$category %in% c("Statistical", "Timeseries", "StationVerticalProfiles") &&
    !(plot$parentType$category == "Timeseries" &&
      plot$parentType$name %in% c("Hovmöller", "Land-sea departures"))
}

.plotSupportsComparison <- function(plotType) {
  category <- plotType$category
  if(category %in% c("Statistical", "StationVerticalProfiles", "Maps", "Average Maps")) {
    return(TRUE)
  }
  category == "Timeseries"
}

.getComparisonPlots <- function(plot) {
  compPlots <- tryCatch(plot$compPlots, error=function(e) NULL)
  if(!is.list(compPlots) || inherits(compPlots, "uninitializedField")) {
    compPlots <- list()
  }
  Filter(function(compPlot) inherits(compPlot, "obsmonPlot"), compPlots)
}

.updateComparisonPlotSettings <- function(plot, params=list(), modelDomain=NULL) {
  for(experimentPlot in c(list(plot), .getComparisonPlots(plot))) {
    for(name in names(params)) {
      experimentPlot$paramsAsInUiInput[[name]] <- params[[name]]
    }
    if(!is.null(modelDomain)) experimentPlot$modelDomain <- modelDomain
  }
  plot
}

.getPlotExperimentName <- function(plot, fallback) {
  experimentName <- .asScalarCharacterOrNa(plot$primaryExperimentName)
  if(is.na(experimentName)) {
    experimentName <- .asScalarCharacterOrNa(plot$paramsAsInUiInput$experiment)
  }
  if(is.na(experimentName)) fallback else experimentName
}

.getComparisonExperimentNames <- function(plot) {
  compPlots <- .getComparisonPlots(plot)
  if(length(compPlots)==0) return(character(0))
  vapply(
    seq_along(compPlots),
    function(i) .getPlotExperimentName(compPlots[[i]], sprintf("comparison %d", i)),
    character(1)
  )
}

.getMissingComparisonExperimentNames <- function(plot) {
  plots <- c(list(plot), .getComparisonPlots(plot))
  names <- c(.getPlotExperimentName(plot, "primary"), .getComparisonExperimentNames(plot))
  names[vapply(plots, .plotHasNoDataRows, logical(1))]
}

.combinePlotDataByExperiment <- function(plot, maskColumns=character(0)) {
  plots <- c(list(plot), .getComparisonPlots(plot))
  experimentNames <- c(
    .getPlotExperimentName(plot, "primary"),
    .getComparisonExperimentNames(plot)
  )

  dataList <- lapply(plots, function(onePlot) {
    data <- onePlot$data
    data[!(colnames(data) %in% maskColumns)]
  })
  validData <- vapply(dataList, function(data) is.data.frame(data) && nrow(data) > 0, logical(1))
  missingExperiments <- experimentNames[!validData]
  if(length(missingExperiments) > 0) {
    flog.warn("No comparison plot data returned for: %s", paste(missingExperiments, collapse=", "))
  }
  dataList <- dataList[validData]
  experimentNames <- experimentNames[validData]
  if(length(dataList) == 0) {
    result <- data.frame(experiment=character(0))
    attr(result, "missingExperiments") <- missingExperiments
    return(result)
  }
  identifierCols <- c("DTG", "level", "varname", "statid", "latitude", "longitude")
  commonIdentifiers <- Reduce(intersect, lapply(dataList, colnames))
  commonIdentifiers <- commonIdentifiers[commonIdentifiers %in% identifierCols]
  metricColumns <- unique(unlist(lapply(dataList, function(data) {
    setdiff(colnames(data), identifierCols)
  }), use.names=FALSE))
  combinedColumns <- c(commonIdentifiers, setdiff(metricColumns, commonIdentifiers))

  for(i in seq_along(dataList)) {
    absentColumns <- setdiff(combinedColumns, colnames(dataList[[i]]))
    for(col in absentColumns) dataList[[i]][[col]] <- rep(NA_real_, nrow(dataList[[i]]))
    dataList[[i]] <- dataList[[i]][combinedColumns]
    dataList[[i]]$experiment <- experimentNames[[i]]
  }
  result <- do.call(rbind, dataList)
  row.names(result) <- NULL
  attr(result, "missingExperiments") <- missingExperiments
  result
}

.combineAnyDataByExperiment <- function(plot, dataAccessor) {
  plots <- c(list(plot), .getComparisonPlots(plot))
  experimentNames <- c(
    .getPlotExperimentName(plot, "primary"),
    .getComparisonExperimentNames(plot)
  )

  dataList <- lapply(plots, dataAccessor)
  validData <- vapply(dataList, function(data) is.data.frame(data) && nrow(data) > 0, logical(1))
  missingExperiments <- experimentNames[!validData]
  if(length(missingExperiments) > 0) {
    flog.warn("No comparison data returned for: %s", paste(missingExperiments, collapse=", "))
  }
  dataList <- dataList[validData]
  experimentNames <- experimentNames[validData]
  if(length(dataList) == 0) {
    result <- data.frame(experiment=character(0))
    attr(result, "missingExperiments") <- missingExperiments
    return(result)
  }
  dataColumns <- unique(unlist(lapply(dataList, colnames), use.names=FALSE))

  for(i in seq_along(dataList)) {
    missingColumns <- setdiff(dataColumns, colnames(dataList[[i]]))
    for(column in missingColumns) {
      prototypePlot <- which(vapply(dataList, function(data) column %in% colnames(data), logical(1)))[[1]]
      prototype <- dataList[[prototypePlot]][[column]]
      dataList[[i]][[column]] <- prototype[rep(NA_integer_, nrow(dataList[[i]]))]
    }
    dataList[[i]] <- dataList[[i]][dataColumns]
    dataList[[i]]$experiment <- experimentNames[[i]]
  }
  dataList <- lapply(dataList, function(data) data[c("experiment", dataColumns)])
  result <- do.call(rbind, dataList)
  row.names(result) <- NULL
  attr(result, "missingExperiments") <- missingExperiments
  result
}

.getExperimentColourValues <- function(experimentNames) {
  experimentNames <- unique(as.character(experimentNames))
  palette <- c(
    "#0072B2", "#D55E00", "#009E73", "#56B4E9",
    "#CC79A7", "#000000", "#F0E442", "#E69F00"
  )
  setNames(rep(palette, length.out=length(experimentNames)), experimentNames)
}

.getVariableLineTypeValues <- function(variableNames) {
  variableNames <- unique(as.character(variableNames))
  linetypes <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  setNames(rep(linetypes, length.out=length(variableNames)), variableNames)
}

# All mapped aesthetics use the same label, so ggplotly preserves the group
# directly instead of constructing tuples of experiment and metric names.
.comparisonScaleValues <- function(data, values, column="variable") {
  groups <- data[!duplicated(data$legendGroup), ]
  setNames(unname(values[as.character(groups[[column]])]), groups$legendGroup)
}

.withComparisonLegend <- function(plot, data) {
  attr(plot, "comparisonLegend") <- unlist(lapply(unique(data$experiment), function(experiment) {
    unique(data$legendGroup[data$experiment == experiment])
  }), use.names=FALSE)
  plot
}

.applyComparisonLegend <- function(chart, ggplotPlot) {
  groups <- attr(ggplotPlot, "comparisonLegend")
  if(is.null(groups)) return(chart)
  for(i in seq_along(chart$x$data)) {
    if(identical(chart$x$data[[i]]$mode, "text")) {
      chart$x$data[[i]]$showlegend <- FALSE
      next
    }
    rank <- match(chart$x$data[[i]]$name, groups)
    if(length(rank) != 1 || is.na(rank)) next
    chart$x$data[[i]]$legendgroup <- groups[[rank]]
    chart$x$data[[i]]$legendrank <- rank
  }
  layout(chart, legend=list(traceorder="normal", groupclick="togglegroup"))
}

.plotHasNoDataRows <- function(plot) {
  if(length(plot$rawData)==0) plot$fetchRawData()
  rawData <- plot$rawData
  if(is.data.frame(rawData) && nrow(rawData)==0) return(TRUE)

  data <- tryCatch(plot$data, error=function(e) NULL)
  is.data.frame(data) && nrow(data)==0
}

.plotHasAnyComparisonData <- function(plot) {
  plots <- c(list(plot), .getComparisonPlots(plot))
  any(!vapply(plots, .plotHasNoDataRows, logical(1)))
}

.getFirstNonEmptyComparisonPlot <- function(plot) {
  plots <- c(list(plot), .getComparisonPlots(plot))
  nonEmpty <- which(!vapply(plots, .plotHasNoDataRows, logical(1)))
  if(length(nonEmpty) == 0) return(plot)
  plots[[nonEmpty[[1]]]]
}

.generateSingleComparisonPanel <- function(plot, title) {
  originalCompPlots <- tryCatch(plot$compPlots, error=function(e) list())
  plot$compPlots <- list()
  on.exit({
    plot$compPlots <- originalCompPlots
  })

  panel <- tryCatch({
    if(.plotHasNoDataRows(plot)) {
      errorPlot("No data to plot.")
    } else if(class(plot$parentType$plottingFunction) == "uninitializedField") {
      plot$.defaultGenerate()
    } else {
      rtn <- plot$parentType$plottingFunction(plot)
      if(plot$parentType$interactive && !("plotly" %in% class(rtn))) {
        rtn <- plot$parentType$ggplotlyWrapper(rtn)
      }
      rtn
    }
  },
    error=function(e) {
      flog.error(e)
      errorPlot("Could not produce plot: Please check the logs.")
    }
  )

  if(plot$parentType$category == "Timeseries" && "plotly" %in% class(panel)) {
    return(panel)
  }
  addTitleToPlot(panel, title)
}

.prefixComparisonMapLegend <- function(panel, experimentName, experimentRank,
                                       categorical=FALSE) {
  if(!"plotly" %in% class(panel)) return(panel)
  panel <- plotly_build(panel)

  prefixColorbar <- function(colorbar) {
    title <- colorbar$title
    if(is.null(title)) return(colorbar)
    if(is.list(title)) {
      titleText <- .asScalarCharacterOrNa(title$text)
      if(is.na(titleText) || startsWith(titleText, paste0(experimentName, ": "))) {
        return(colorbar)
      }
      title$text <- paste0(experimentName, ": ", titleText)
    } else {
      titleText <- .asScalarCharacterOrNa(title)
      if(is.na(titleText) || startsWith(titleText, paste0(experimentName, ": "))) {
        return(colorbar)
      }
      colorbar$title <- paste0(experimentName, ": ", titleText)
    }
    colorbar
  }

  if(categorical && !is.null(panel$x$data)) {
    categoryRanks <- integer(0)
    for(i in seq_along(panel$x$data)) {
      trace <- panel$x$data[[i]]
      if(isFALSE(trace$showlegend)) next
      traceName <- .asScalarCharacterOrNa(trace$name)
      if(is.na(traceName) || !nzchar(traceName)) next
      categoryKey <- .asScalarCharacterOrNa(trace$key)
      if(is.na(categoryKey)) categoryKey <- .asScalarCharacterOrNa(trace$legendgroup)
      if(is.na(categoryKey)) categoryKey <- traceName

      if(!categoryKey %in% names(categoryRanks)) {
        categoryRanks[[categoryKey]] <- length(categoryRanks) + 1L
      }
      panel$x$data[[i]]$name <- paste0(experimentName, ": ", traceName)
      panel$x$data[[i]]$legendgroup <- paste(experimentName, categoryKey, sep=" :: ")
      panel$x$data[[i]]$legendrank <- experimentRank * 100L + categoryRanks[[categoryKey]]
      panel$x$data[[i]]$showlegend <- TRUE
    }
    if(is.null(panel$x$layout$legend)) panel$x$layout$legend <- list()
    panel$x$layout$legend$traceorder <- "normal"
    panel$x$layout$legend$groupclick <- "togglegroup"
  } else {
    for(i in seq_along(panel$x$data)) {
      marker <- panel$x$data[[i]]$marker
      if(!is.null(marker$colorbar)) {
        panel$x$data[[i]]$marker$colorbar <- prefixColorbar(marker$colorbar)
      }
    }
    coloraxisNames <- grep("^coloraxis[0-9]*$", names(panel$x$layout), value=TRUE)
    for(coloraxisName in coloraxisNames) {
      coloraxis <- panel$x$layout[[coloraxisName]]
      if(!is.null(coloraxis$colorbar)) {
        panel$x$layout[[coloraxisName]]$colorbar <- prefixColorbar(coloraxis$colorbar)
      }
    }
  }
  panel
}

.shareComparisonMapColorScale <- function(panels, experimentNames) {
  colorValues <- unlist(lapply(panels, function(panel) {
    unlist(lapply(panel$x$data, function(trace) {
      color <- trace$marker$color
      if(is.numeric(color)) color else numeric(0)
    }), use.names=FALSE)
  }), use.names=FALSE)
  colorValues <- colorValues[is.finite(colorValues)]
  if(length(colorValues)==0) return(panels)

  colorMap <- .getSuitableColorScale(colorValues)
  dataPal <- colorNumeric(palette=colorMap$palette, domain=colorMap$domain)
  scaleColors <- grDevices::colorRampPalette(colorMap$palette)(25)
  sharedColorscale <- cbind(
    as.character(seq(0, 1, length.out=length(scaleColors))),
    scaleColors
  )

  colorbarLocations <- list()
  for(panelIndex in seq_along(panels)) {
    for(traceIndex in seq_along(panels[[panelIndex]]$x$data)) {
      marker <- panels[[panelIndex]]$x$data[[traceIndex]]$marker
      if(!is.null(marker$colorbar)) {
        colorbarLocations[[length(colorbarLocations)+1]] <- c(panelIndex, traceIndex)
      }
    }
  }
  if(length(colorbarLocations)==0) return(panels)
  showscaleLocation <- which(vapply(colorbarLocations, function(location) {
    isTRUE(panels[[location[[1]]]]$x$data[[location[[2]]]]$marker$showscale)
  }, logical(1)))[1]
  if(is.na(showscaleLocation)) showscaleLocation <- 1L
  showscaleLocation <- colorbarLocations[[showscaleLocation]]

  for(panelIndex in seq_along(panels)) {
    for(traceIndex in seq_along(panels[[panelIndex]]$x$data)) {
      trace <- panels[[panelIndex]]$x$data[[traceIndex]]
      if(identical(trace$fill, "toself")) {
        panels[[panelIndex]]$x$data[[traceIndex]]$fillcolor <- dataPal(trace$customdata$value)
      }
      marker <- trace$marker
      if(!is.numeric(marker$color)) next
      marker$cmin <- colorMap$domain[[1]]
      marker$cmax <- colorMap$domain[[2]]
      marker$colorscale <- sharedColorscale
      if(!is.null(marker$colorbar)) {
        isSharedColorbar <- panelIndex == showscaleLocation[[1]] &&
          traceIndex == showscaleLocation[[2]]
        marker$showscale <- isSharedColorbar
        if(isSharedColorbar) {
          title <- marker$colorbar$title
          if(is.list(title)) {
            titleText <- .asScalarCharacterOrNa(title$text)
            experimentName <- experimentNames[[panelIndex]]
            if(!is.na(titleText) && startsWith(titleText, paste0(experimentName, ": "))) {
              title$text <- substring(titleText, nchar(experimentName) + 3L)
              marker$colorbar$title <- title
            }
          } else {
            titleText <- .asScalarCharacterOrNa(title)
            experimentName <- experimentNames[[panelIndex]]
            if(!is.na(titleText) && startsWith(titleText, paste0(experimentName, ": "))) {
              marker$colorbar$title <- substring(titleText, nchar(experimentName) + 3L)
            }
          }
        }
      }
      panels[[panelIndex]]$x$data[[traceIndex]]$marker <- marker
    }
  }
  panels
}

.generateSideBySideComparisonPlot <- function(plot) {
  plots <- c(list(plot), .getComparisonPlots(plot))
  nonEmpty <- !vapply(plots, .plotHasNoDataRows, logical(1))
  plots <- plots[nonEmpty]
  if(length(plots) == 0) return(errorPlot("No data to plot."))
  experimentNames <- c(
    .getPlotExperimentName(plot, "primary"),
    .getComparisonExperimentNames(plot)
  )
  experimentNames <- experimentNames[nonEmpty]

  if(plot$parentType$name == "Hovmöller") {
    values <- unlist(lapply(plots, function(p) p$data$fg_bias_total))
    colourLimits <- range(values[is.finite(values)])
    panels <- lapply(seq_along(plots), function(i) {
      panel <- hovmollerTimeseriesPlottingFunction(plots[[i]], colourLimits,
        showColorbar=i == length(plots))
      if(!"plotly" %in% class(panel)) panel <- addTitleToPlot(panel, experimentNames[[i]])
      panel
    })
  } else {
    panels <- mapply(
      FUN=.generateSingleComparisonPanel,
      plot=plots,
      title=experimentNames,
      SIMPLIFY=FALSE
    )
  }

  if(all(vapply(panels, function(panel) "plotly" %in% class(panel), logical(1)))) {
    if(plot$parentType$category %in% c("Maps", "Average Maps")) {
      panels <- lapply(seq_along(panels), function(i) {
        .prefixComparisonMapLegend(
          panels[[i]], experimentNames[[i]], i,
          categorical=plot$parentType$name == "Observation Usage"
        )
      })
      if(plot$parentType$name != "Observation Usage") {
        panels <- .shareComparisonMapColorScale(panels, experimentNames)
      }
    }
    if(plot$parentType$name == "Land-sea departures") {
      panels <- lapply(seq_along(panels), function(i) {
        panel <- plotly_build(panels[[i]])
        for(j in seq_along(panel$x$data)) {
          trace <- panel$x$data[[j]]
          label <- paste0(experimentNames[[i]], ": ", trace$name)
          panel$x$data[[j]]$name <- label
          panel$x$data[[j]]$legendgroup <- label
        }
        panel
      })
    }
    combined <- subplot(panels, nrows=1, shareX=FALSE, shareY=FALSE,
      margin=0.04, titleX=TRUE, titleY=TRUE)
    if(plot$parentType$category == "Timeseries") {
      # Add experiment labels after subplot so nested panels retain them.
      for(i in seq_along(panels)) {
        combined <- add_annotations(combined,
          text=experimentNames[[i]], showarrow=FALSE,
          xref="paper", x=(i - 0.5) / length(panels), xanchor="center",
          yref="paper", y=1.02, yanchor="bottom", font=list(size=20)
        )
      }
      combined <- layout(combined, margin=list(t=150))
      attr(combined, "yTitle") <- 1.1
    }
    if(plot$parentType$category %in% c("Maps", "Average Maps")) {
      domains <- lapply(seq_along(panels), function(i) {
        panelCount <- length(panels)
        start <- if(i == 1) 0 else 0.02 + (i - 1) * 0.96 / panelCount
        end <- if(i == panelCount) 1 else 0.02 + i * 0.96 / panelCount
        c(start, end)
      })
      geoLayout <- setNames(lapply(seq_along(domains), function(i) {
        list(domain=list(x=domains[[i]]))
      }), if(length(domains) == 1) "geo" else c("geo", paste0("geo", seq.int(2, length(domains)))))
      combined <- do.call(layout, c(list(p=combined, margin=list(t=150)), geoLayout))
      attr(combined, "yTitle") <- 1.1
    }
    return(combined)
  }

  grid.arrange(grobs=panels, ncol=length(panels))
}

plotTypeClass <- setRefClass(Class="obsmonPlotType",
  fields=list(
    name="character",
    category="character",
    dateType="character",
    dataFieldsInRetrievedPlotData="list",
    dataFieldsInSqliteWhereClause="list",
    extraDataFields="list",
    stationChoiceType="character",
    interactive="logical",
    # dataPostProcessingFunction: A function where the 1st arg is
    # data (data.frame) and the 2nd, if used, is obsmonPlotObj (obsmonPlot)
    dataPostProcessingFunction="ANY",
    plottingFunction="ANY", # A function of 1 arg: plot (an obsmonPlot object)
    leafletPlottingFunction="ANY", # A function of 1 arg: plot (an obsmonPlot object)
    plotTitleFunction="ANY", # A function of 1 arg: plot (an obsmonPlot object)
    ############################
    flattenedDataFieldsInSqliteWhereClause = function(...) {
      dataFieldsInWhereClause <- c()
      for (item in .self$dataFieldsInSqliteWhereClause) {
        if (is.list(item)) {
          dataFieldsInWhereClause <- c(dataFieldsInWhereClause, names(item))
        } else {
          dataFieldsInWhereClause <- c(dataFieldsInWhereClause, item)
        }
      }
      return(dataFieldsInWhereClause)
    },
    supportsStationSelection = function(...) {
      return(isTRUE("statid" %in% .self$getRetrievedSqliteFields()))
    },
    requiresSingleStation = function(...) {
      return(isTRUE(.self$stationChoiceType == "single"))
    },
    queriedDbTable = function(...) {
      return(ifelse(.self$supportsStationSelection, "usage", "obsmon"))
    }
  ),
  methods=list(
    ############################
    initialize = function(...) {
      callSuper(...)

      if(length(.self$interactive)==0) {
        # Using "!isFALSE" to catch the "logical(0)" case and
        # interpret it as "TRUE"
        .self$interactive <- !isFALSE(
          obsmonConfig$general$plotsEnableInteractivity
        )
      }

      # Validate dataFieldsInSqliteWhereClause and extraDataFields entries
      varnameRegex <- "^[a-zA-Z_$][a-zA-Z_$0-9]*$"
      for(field in c(
        "dataFieldsInRetrievedPlotData",
        "dataFieldsInSqliteWhereClause",
        "extraDataFields"
      )) {
        .self$field(field, unique(.self$field(field)))
        if(length(.self$field(field))>0) {
          fieldValues <- .self$field(field)
          sqlColnames <- c()
          for (val in fieldValues) {
            if(is.list(val)) sqlColnames <- c(sqlColnames, names(val))
            else sqlColnames <- c(sqlColnames, val)
          }
          for (name in sqlColnames) {
            if(!isTRUE(grepl(varnameRegex, name))) {
              stop(sprintf("Field '%s' contains invalid column names: %s", field, name))
            }
          }
        }
      }

      # Validate stationChoiceType
      stationType <- .self$stationChoiceType
      if(length(stationType)>0 && !(stationType %in% c("single", "multiple"))) {
        stop("Field 'stationChoiceType', if passed, should be one of: 'single', 'multiple'")
      }

      # Make sure "statid" is listed as one of the wanted fields if
      # stationChoiceType is passed
      if (
        (length(.self$stationChoiceType) > 0) &&
        !("statid" %in% .self$getRetrievedSqliteFields())
      ) {
        .self$dataFieldsInSqliteWhereClause <- c(
          .self$dataFieldsInSqliteWhereClause,
          "statid"
        )
      }

      # Validate fields ending with "Function"
      for (field in names(.self$getRefClass()$fields())) {
        if(!endsWith(field, "Function")) next
        func <- .self$field(field)
        if (class(func) != "uninitializedField" && typeof(func) != "closure") {
          stop(sprintf("Field '%s' is not a function", field))
        }
      }

      # Validate dateType
      if(length(.self$dateType)==0) .self$dateType <- "single"
      if(!isTRUE(.self$dateType %in% c("single", "range"))) {
        stop("Field 'dateType' must be one of: 'single', 'range'")
      }
    },
    ############################
    getRetrievedSqliteFields = function() {
      dbCols <- c(
        .self$dataFieldsInRetrievedPlotData,
        .self$flattenedDataFieldsInSqliteWhereClause,
        .self$extraDataFields
      )
      return(unique(dbCols))
    },
    ############################
    getQueryStub = function() {
      # stationIDs are not stored in the "obsmon" table, only in "usage"
      whereStub <- "WHERE %s"
      if (
        (.self$queriedDbTable == "obsmon") &&
        !("nobs_total" %in% .self$getRetrievedSqliteFields())
      ) {
        whereStub <- paste(whereStub, "AND (nobs_total > 0)")
      }
      stub <- paste(
        "SELECT DISTINCT",
        paste(.self$getRetrievedSqliteFields(), collapse=", "),
        "FROM", .self$queriedDbTable, whereStub
      )
      return (stub)
    },
    ############################
    ggplotlyWrapper = function(ggplotPlot) {
      # Convert ggplot2 object ggplotPlot into a plotly object using
      # plotly's ggplotly function, and then apply some customisations
      plotlyPlot <- tryCatch({
        .applyComparisonLegend(ggplotly(ggplotPlot, tooltip=c("x","y")), ggplotPlot) %>%
          layout(
            margin=list(t=100),
            legend=list(orientation="v", yanchor="center", y=0.5)
          )
        },
        error=function(e){
          flog.warn(
            'ggplotlyWrapper: Failure making plot "%s" interactive: %s',
            .self$name, e
          )
          return(ggplotPlot)
        }
      )
      return(plotlyPlot)
    },
    ############################
    isCompatibleWithUiParams = function(paramsAsInUiInput) {
      # See former "plotIsApplicable"
      sqliteParams <- .self$getSqliteParamsFromUiParams(paramsAsInUiInput)
      for (item in .self$dataFieldsInSqliteWhereClause) {
        if(is.list(item)) {
          paramsAndValues <- item
        } else {
          paramsAndValues <- list(NA)
          names(paramsAndValues) <- item
        }

        for (param in names(paramsAndValues)) {
          # statid is a bit special because, in the server, we use the
          # obsmonPlotType objects themselves to decide whether to show
          # or hide station selection menus
          if (param=="statid") next
          if (length(sqliteParams[[param]])==0) return (FALSE)
          paramValue <- paramsAndValues[[param]]
          if(!is.na(paramValue) && !isTRUE(sqliteParams[[param]]==paramValue)) {
            return (FALSE)
          }
        }
      }
      return (TRUE)
    },
    ############################
    getSqliteParamsFromUiParams = function(paramsAsInUiInput) {
      # Previously called "plotsBuildCriteria"
      # plotRequest <- list()
      # plotRequest$expName <- req(input$experiment)
      # plotRequest$dbType <- db$dbType
      # plotRequest$criteria <- plotsBuildCriteria(input)
      # TODO: Make this more general (in case of future addition of new params)
      res <- list()
      obname <- paramsAsInUiInput$obname
      res$obnumber <- getAttrFromMetadata('obnumber', obname=obname)
      if (isTRUE(obname=='satem')) {
        res$obname <- paramsAsInUiInput$sensor
        res$satname <- paramsAsInUiInput$satellite
        levels <- paramsAsInUiInput$channels
        excludeLevels <- paramsAsInUiInput$excludeChannels
      } else {
        if (isTRUE(obname=='scatt')) {
          res$satname <- paramsAsInUiInput$scatt_satellite
        }
        res$obname <- obname
        res$varname <- paramsAsInUiInput$variable
        levels <- paramsAsInUiInput$levels
        excludeLevels <- paramsAsInUiInput$excludeLevels
      }

      res$level <- list()
      if (!is.null(levels)) {
        validLevels <- !is.na(levels) & nzchar(as.character(levels))
        if (any(validLevels)) res$level <- levels[validLevels]
      }
      res$excludeLevels <- list()
      if (!is.null(excludeLevels)) {
        validExcludeLevels <- !is.na(excludeLevels) & nzchar(as.character(excludeLevels))
        if (any(validExcludeLevels)) res$excludeLevels <- excludeLevels[validExcludeLevels]
      }

      if(obSupportsStationChoice(obname)) {
        stations <- paramsAsInUiInput$station
        if(is.null(stations) || "" %in% stations) stations <- character(0)
        res$statid <- stations
      }

      res$dtg <- tryCatch(
        switch(.self$dateType,
          "single"=date2dtg(paramsAsInUiInput$date, paramsAsInUiInput$cycle),
          "range"={
            dateRange <- sort(paramsAsInUiInput$dateRange)
            list(dateRange[1], dateRange[2], paramsAsInUiInput$cycles)
          }
        ),
        error=function(e) NULL
      )

      return(res)
    }
  )
)

.refClassObjHash <- function(.self) {
  componentsHashes <- c()
  for (fieldName in names(.self$getRefClass()$fields())) {
    fieldClass <- .self$getRefClass()$fields()[[fieldName]]

    # Skip fields that are actually methods under the hood
    if(fieldClass == "activeBindingFunction") next

    # Skip fields that are allowed to change
    if(fieldName %in% c("hash", ".cache")) next

    component <- .self$field(fieldName)
    # Hash just the function code, not environment or other attrs
    if(is.function(component)) component <- deparse(component)

    if(is(component, "envRefClass")) {
      # Parse recursively so that we also handle the special cases listed above
      componentsHash <- .refClassObjHash(component)
    } else if(is.list(component)) {
      componentsHash <- digest::digest(lapply(component, function(item) {
        if(is(item, "envRefClass")) .refClassObjHash(item) else digest::digest(item)
      }))
    } else {
      componentsHash <- digest::digest(component)
    }
    componentsHashes <- c(componentsHashes, componentsHash)
  }
  return(digest::digest(componentsHashes))
}

obsmonPlotClass <- setRefClass(Class="obsmonPlot",
  fields=list(
    parentType="obsmonPlotType",
    db="obsmonDatabase",
    paramsAsInUiInput="list",
    rawData="data.frame",
    modelDomain="domain",
    compPlots="ANY",
    primaryExperimentName="ANY",
    ##############################
    chart = function(newValue) {
      if(missing(newValue)) {
        return (.self$.memoise(FUN=.self$.generate))
      } else {
        flog.debug("Cannot set chart. Ignoring assignment.")
      }
    },
    leafletMap = function(newValue) {
      if(missing(newValue)) {
        return (.self$.memoise(FUN=.self$.generateLeafletMap))
      } else {
        flog.debug("Cannot set leafletMap. Ignoring assignment.")
      }
    },
    # ggplot2 doesn't like units: Remove them from data used in plots
    data = function(newValue) {
      if(missing(newValue)) {
        return(drop_units(.self$dataWithUnits))
      } else {
        flog.debug("Cannot set data. Ignoring assignment.")
      }
    },
    dataWithUnits = function(newValue) {
      if(missing(newValue)) {
        if(length(.self$rawData)==0) .self$fetchRawData()
        return(.self$.memoise(FUN=.self$.getDataFromRawData))
      } else {
        flog.debug("Cannot set dataWithUnits. Ignoring assignment.")
      }
    },
    queryDataWithUnits = function(newValue) {
      if(missing(newValue)) {
        if(!.plotHasComparison(.self)) return(.self$dataWithUnits)
        .combineAnyDataByExperiment(.self, function(plot) plot$dataWithUnits)
      } else {
        flog.debug("Cannot set queryDataWithUnits. Ignoring assignment.")
      }
    },
    queryRawData = function(newValue) {
      if(missing(newValue)) {
        if(!.plotHasComparison(.self)) return(.self$rawData)
        .combineAnyDataByExperiment(.self, function(plot) plot$rawData)
      } else {
        flog.debug("Cannot set queryRawData. Ignoring assignment.")
      }
    },
    sqliteQuery = function(...) {.self$.getSqliteQuery()},
    paramsAsInSqliteDbs = function(...) {
      .self$parentType$getSqliteParamsFromUiParams(.self$paramsAsInUiInput)
    },
    title = function(...) {.self$.getTitle()},
    hash = function(...) .refClassObjHash(.self),
    ##############################
    .cache="list"
  ),
  methods=list(
    initialize = function(...) {
      callSuper(...)
      if (inherits(.self$primaryExperimentName, "uninitializedField") || is.null(.self$primaryExperimentName)) {
        .self$primaryExperimentName <- character(0)
      }
      if (inherits(.self$compPlots, "uninitializedField") || is.null(.self$compPlots)) {
        .self$compPlots <- list()
      }
    },

    fetchRawData = function(...) {
      flog.trace("Fetching raw data for plot '%s'...", .self$title)
      fetchedData <- performQuery(
        db=.self$db,
        query=.self$sqliteQuery,
        dtgs=.self$paramsAsInSqliteDbs$dtg,
        ...
      )
      if(is.null(fetchedData)) {
        colnames <- .self$parentType$getRetrievedSqliteFields()
        fetchedData <- data.frame(matrix(ncol=length(colnames), nrow=0))
        colnames(fetchedData) <- colnames
      }
      fetchedData <- fetchedData[do.call(order, fetchedData),]
      row.names(fetchedData) <- NULL
      .self$rawData <- fetchedData
      flog.trace("Done fetching raw data for plot '%s'", .self$title)
    },

    exportData = function(file, format, raw=FALSE) {
      comparison <- .plotHasComparison(.self)
      if(raw) {
        dataToBeExported <- .self$queryRawData
      } else {
        dataToBeExported <- drop_units(.self$queryDataWithUnits)
      }

      format <- tolower(format)
      if(!format %in% c("csv", "txt")) {
        flog.error("Format '%s' not supported.", format)
        return(NULL)
      }

      dataInfo <- paste0(
        paste("# Plot title:", .self$title, "\n"),
        sprintf(
          "# Data retrieved by Obsmon v%s on %s using the following query:\n",
          obsmonVersion, strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
        ),
        paste0("# ", .self$sqliteQuery, "\n"),
        paste("\n")
      )
      if(!comparison) {
        if(format=="csv") {
          write.csv(dataToBeExported, file, row.names=FALSE)
        } else {
          write.table(dataToBeExported, file, sep="\t", row.names=FALSE)
        }
        write(paste0("\n", dataInfo), file, append=TRUE)
      } else {
        # Comparison downloads put metadata first, followed by the table whose
        # leading experiment column identifies each row.
        writeLines(dataInfo, file)
        if(format=="csv") {
          write.table(dataToBeExported, file, sep=",", row.names=FALSE,
            col.names=TRUE, append=TRUE, qmethod="double")
        } else {
          write.table(dataToBeExported, file, sep="\t", row.names=FALSE,
            col.names=TRUE, append=TRUE)
        }
      }
    },

    ############################
    .generate = function() {

      if(
        .self$modelDomain$grid$hasPoints &&
        isTRUE(grepl("maps", tolower(.self$parentType$category))) &&
        !isTRUE(.self$parentType$interactive)
      ) {
        flog.warn("Ignoring domain info: only supported in interactive maps.")
      }

      plot <- tryCatch({
        if(.plotHasNoDataRows(.self) &&
          (!.plotHasComparison(.self) || !.plotHasAnyComparisonData(.self))) {
          rtn <- errorPlot("Could not produce plot: No data.")
        } else if(.plotHasComparison(.self) && !.plotUsesJointComparison(.self)) {
          rtn <- .generateSideBySideComparisonPlot(.self)
        } else if(class(.self$parentType$plottingFunction) == "uninitializedField") {
          rtn <- .self$.defaultGenerate()
        } else {
          rtn <- .self$parentType$plottingFunction(.self)
          if(.self$parentType$interactive && !("plotly" %in% class(rtn))) {
            rtn <- .self$parentType$ggplotlyWrapper(rtn)
          }
        }
        rtn
      },
        error=function(e) {
          flog.error(e)
          errorPlot("Could not produce plot: Please check the logs.")
        }
      )

      plot <- plot %>% addTitleToPlot(.self$title)
      if("plotly" %in% class(plot)) {
        plot <- plot %>% configPlotlyWrapper()
      }

      return(plot)
    },

    .generateLeafletMap = function() {
      rtn <- tryCatch({
        if(.plotHasComparison(.self)) return(NULL)
        if(class(.self$parentType$leafletPlottingFunction) != "uninitializedField") {
          .self$parentType$leafletPlottingFunction(.self)
        } else {
          .self$.defaultGenerateLeafletMap()
        }
      },
        error=function(e) {
          flog.error("Problems creating leaflet plot: %s", e)
          return(NULL)
        }
      )
      return(rtn)
    },

    .defaultGenerate = function() {
      # melt data so we can plot multiple curves (sharing same x-axis), each
      # with a different color and symbol
      data_colnames <- colnames(.self$data)
      df <- melt(.self$data, id=data_colnames[1])
      graph <- ggplot(data=df) +
        aes_string(
          x=data_colnames[1],
          y="value",
          group="variable",
          colour="variable",
          shape="variable",
          fill="variable"
        ) +
        geom_point(size=4) +
        geom_line()

      if(.self$parentType$interactive) {
        graph <- .self$parentType$ggplotlyWrapper(graph)
      }

      attributes(graph)$createdByDefaultGenerate <- TRUE
      return(graph)
    },

    .defaultGenerateLeafletMap = function() {
      if(!isTRUE(grepl("maps", tolower(parentType$category)))) return(NULL)
      # Use "check.names=FALSE" so that names such as "fg_dep+biascrl"
      # are not modified
      localPlotData <- data.frame(.self$data, check.names=FALSE)

      dataColumn <- unname(attributes(.self$data)$comment["dataColumn"])
      if(is.null(dataColumn)) {
        dataColumn <- colnames(.self$data)[ncol(.self$data)]
      }

      if(is.numeric(localPlotData[[dataColumn]])) {
        cm <- .getSuitableColorScale(localPlotData[dataColumn])
        dataPallete <- colorNumeric(palette=cm$palette, domain=cm$domain)
      } else {
        dataPallete <- colorFactor("RdYlBu", domain = NULL)
      }

      rtnLeafletMap <- leaflet(data=localPlotData) %>%
        addTiles() %>%
        addProviderTiles(
          "Esri.WorldStreetMap",
          options=providerTileOptions(opacity=0.5)
        ) %>%
        addCircleMarkers(
          lng=~longitude,
          lat=~latitude,
          popup=.getLeafletPopupContents(.self$data),
          fillColor=as.formula(sprintf("~dataPallete(`%s`)", dataColumn)),
          fillOpacity=0.5,
          stroke=FALSE,
          weight=1,
          opacity=1,
          color="black",
          clusterOptions=markerClusterOptions(disableClusteringAtZoom=6)
        ) %>%
        addLegend(
          "topright",
          pal=dataPallete,
          values=as.formula(sprintf("~`%s`", dataColumn)),
          opacity=1
        )
      return(rtnLeafletMap)
    },

    .getDataFromRawData = function() {
      if(length(.self$rawData)==0) .self$fetchRawData()
      rtn <- data.frame(.self$rawData, check.names=FALSE)

      # Filter out unwanted cols from the rawData
      if(length(.self$parentType$dataFieldsInRetrievedPlotData)>0) {
        selectedCols <- intersect(
          unlist(.self$parentType$dataFieldsInRetrievedPlotData),
          colnames(rtn)
        )
        rtn <- rtn[selectedCols]
      }

      # Add units
      rtn <- .self$.memoise(
        FUN=fillObsmonDataFrameWithUnits,
        df=rtn,
        # varname & obname are used to get the default units
        varname=.self$paramsAsInUiInput$variable,
        obname=.self$paramsAsInUiInput$obname,
        # These two lines provide info to enable unit conversions
        varUnits=.self$paramsAsInUiInput$variableUnits,
        levelsUnits=.self$paramsAsInUiInput$levelsUnits
      )

      if(
        isTRUE(.self$parentType$queriedDbTable == "usage") &&
        isTRUE(.self$paramsAsInUiInput$groupLevelsIntoStandardSwitch) &&
        ("level" %in% colnames(rtn))
      ) {
        # Group levels into reference/standard levels
        refLevels <- NULL
        if(ud_are_convertible(units(rtn$level), "Pa")) {
          refLevels <- refPressures
        } else if(ud_are_convertible(units(rtn$level), "m")) {
          refLevels <- refHeights
        }

        if(!is.null(refLevels)) {
          units(refLevels) <- units(rtn$level)
          refLevels <- drop_units(refLevels)

          reportedLevels <- rtn$level
          reportedLevel2Level <- Vectorize(function(reportedLevel) {
            refLevelIndex <- which.min(abs(refLevels - reportedLevel))
            return(refLevels[refLevelIndex])
          })
          rtn$level <- reportedLevel2Level(drop_units(reportedLevels))
          units(rtn$level) <- units(reportedLevels)
        }
      }

      # Apply eventual user-defined data post-processing
      if(class(.self$parentType$dataPostProcessingFunction) != "uninitializedField") {
        rtn <- .self$parentType$dataPostProcessingFunction(data=rtn, obsmonPlotObj=.self)
      }

      return(rtn[complete.cases(rtn),])
    },

    .getSqliteQuery = function() {
      # Previously named "plotBuildQuery"
      return(
        sprintf(
          .self$parentType$getQueryStub(),
          buildWhereClause(.self$paramsAsInSqliteDbs)
        )
      )
    },

    .getTitle = function() {
      if(class(.self$parentType$plotTitleFunction) != "uninitializedField") {
        return(.self$parentType$plotTitleFunction(.self))
      }

      valuesList2FormattedStr <- function(vals) {
        if (length(vals) > 1) {
          if(length(vals)>5) {
            vals <- c(vals[1:2], "...", vals[(length(vals)-1):length(vals)])
          }
          vals <- paste0("[", paste(vals, collapse=", "), "]")
        }
        return(vals)
      }

      sqliteParams <- .self$paramsAsInSqliteDbs

      rtn <- ""
      hasComp <- .plotHasComparison(.self)
      if(hasComp) {
        primaryExptName <- .getPlotExperimentName(.self, "primary")
        compExperimentNames <- .getComparisonExperimentNames(.self)
        rtn <- paste0(
          primaryExptName, " vs ",
          paste(compExperimentNames, collapse=", "),
          ": "
        )
      } else if(length(.self$paramsAsInUiInput$experiment) > 0) {
        rtn <- paste0(.self$paramsAsInUiInput$experiment, ": ")
      }

      rtn <- paste0(rtn, .self$parentType$name, "\n")

      if(length(.self$paramsAsInUiInput$station)>0) {
        rtn <- paste0(
          rtn,
          "station=",
          valuesList2FormattedStr(.self$paramsAsInUiInput$station),
          "\n"
        )
      }

      if(length(.self$paramsAsInUiInput$odbBase)>0) {
        rtn <- paste0(rtn, "db=", .self$paramsAsInUiInput$odbBase)
      }

      dtgs <- formatDtg(sqliteParams$dtg)
      if(length(dtgs)>0) {
        rtn <- sprintf("%s, DTG=%s", rtn, dtgs)
      }

      for (param in names(sqliteParams)) {
        if (param %in% c("dtg", "obnumber", "statid")) next
        if (param %in%
            setdiff(
              .self$parentType$dataFieldsInRetrievedPlotData,
              .self$parentType$flattenedDataFieldsInSqliteWhereClause
            )
        ) next
        if (length(sqliteParams[[param]]) == 0) next
        formattedVals <- valuesList2FormattedStr(sqliteParams[[param]])
        rtn <- sprintf("%s, %s=%s", rtn, param, formattedVals)
      }
      return (rtn)
    },

    .memoise = function(FUN, ...) {
      usedHash <- digest::digest(list(.self$hash, deparse(FUN), list(...)))
      if(is.null(.self$.cache[[usedHash]])) .self$.cache[[usedHash]] <- FUN(...)
      return(.self$.cache[[usedHash]])
    }
  )
)

obsmonPlotRegistryClass <- setRefClass(Class="obsmonPlotRegistry",
  fields=list(
    plotTypes="list"
  ),
  methods=list(
    registerPlotType = function(...) {
      args <- list(...)
      if(length(args)==1) {
        plot <- args[[1]]
      } else {
        plot <- plotTypeClass$new(...)
      }
      if(plot$name %in% names(.self$plotTypes)) {
        stop(sprintf(
          'Cannot register plot "%s": Name is already registered.',
          plot$name
        ))
      }
      newEntry <- list(plot)
      names(newEntry) <- plot$name
      .self$plotTypes <- c(.self$plotTypes, newEntry)
    },
    #########################
    getCategorisedPlotTypeNames = function(compatibleWithUiInputParams=NULL) {
      rtn <- list()

      if(is.null(compatibleWithUiInputParams)) {
        pTypes <- .self$plotTypes
      } else {
        pTypes <- list()
        for (pType in .self$plotTypes) {
          if(!pType$isCompatibleWithUiParams(compatibleWithUiInputParams)) next
          pTypes <- c(pTypes, pType)
        }
      }

      for (pType in pTypes) {
        if (pType$category %in% names(rtn)) {
          rtn[[pType$category]] <- c(rtn[[pType$category]], list(pType$name))
        } else {
          rtn[[pType$category]] <- list(pType$name)
        }
      }
      return(rtn)
    }
  )
)

# Create the plot registry that will be used throughout
plotRegistry <- obsmonPlotRegistryClass()

#######################################################
# Some utility functions used when defining the plots #
#######################################################
errorPlot <- function(msg) {
  ggplot() +
    annotate("text", x=0, y=0, size=8, label=msg) +
    theme(
      panel.background = element_rect(fill="grey90"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    )
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
  plotlySaveAsFigDimensions <- list(height=755, width=1200)
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

addTitleToPlot <- function(myPlot, title) {
  if(length(title)==0) {
    flog.warn("addTitleToPlot: Empty title")
    return(myPlot)
  }
  newPlot <- tryCatch({
    if(inherits(myPlot, "ggplot")) {
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

.getSuitableColorScale <- function(plotData) {
  # Use a divergent colormat whenever there's change of sign
  # in the data, but use a sequential colormap otherwise.
  # Red/blue colors will represent +/- values.
  cm <- list(name=NULL, palette=NULL, direction=NULL, domain=NULL)

  if(is.data.frame(plotData)) {
    dataColumnName <- unname(attributes(plotData)$comment["dataColumn"])
    if(is.null(dataColumnName)) {
      dataColumnName <- colnames(plotData)[ncol(plotData)]
    }
    dataRange <- range(plotData[[dataColumnName]], na.rm=TRUE)
  } else {
    dataRange <- range(plotData, na.rm=TRUE)
  }
  # Use integers as default upper/lower limits
  dataRange <- c(floor(dataRange[1]), ceiling(dataRange[2]))

  if (prod(dataRange) >= 0) {
    spread <- diff(dataRange)
    if (sign(sum(dataRange)) < 0) {
      cm$name <- "Blues"
      cm$direction <- -1
      mincol <- dataRange[1]
      snapToZero <- dataRange[2]^2 < spread
      maxcol <- ifelse(snapToZero, 0., dataRange[2])
    } else {
      cm$name <- "Reds"
      cm$direction <- 1
      maxcol <- dataRange[2]
      snapToZero <- dataRange[1]^2 < spread
      mincol <- ifelse(snapToZero, 0., dataRange[1])
    }
  } else {
    cm$name <- "RdBu"
    cm$direction <- -1
    maxcol <- max(abs(dataRange))
    mincol <- -maxcol
  }
  cm$palette <- brewer.pal(brewer.pal.info[cm$name,]$maxcolors, cm$name)
  if(cm$direction < 0) cm$palette <- rev(cm$palette)
  cm$domain <- c(mincol, maxcol)
  return(cm)
}

.getLeafletPopupContents <- function(plotData) {
  plotData$popupContents <- ""
  for (colname in colnames(plotData)) {
    plotData$popupContents <- paste(
      plotData$popupContents,
      sprintf("%s: %s<br>", colname, plotData[[colname]])
    )
  }
  return(plotData[["popupContents"]])
}

fillDataWithQualityControlStatus <- function(data, ...) {
  status <- rep("NA", nrow(data))
  status <- ifelse(data$anflag == 0, "Rejected", status)
  status <- ifelse(data$active  > 0, "Active", status)
  status <- ifelse(data$rejected > 0, "Rejected", status)
  status <- ifelse(data$passive > 0, "Passive", status)
  status <- ifelse(data$blacklisted > 0, "Blacklisted", status)
  status <- ifelse(data$anflag  > 0, "Active(2)", status)
  status <- ifelse(data$anflag == 4, "Rejected", status)
  status <- ifelse(data$anflag == 8, "Blacklisted", status)
  status <- ifelse(data$anflag > 100, "Rejected in TITAN", status)
  status <- ifelse(data$anflag == 100, "TITAN: blacklisted", status)
  status <- ifelse(data$anflag == 101, "TITAN: nometa", status)
  status <- ifelse(data$anflag == 102, "TITAN: plaus", status)
  status <- ifelse(data$anflag == 103, "TITAN: clim", status)
  status <- ifelse(data$anflag == 104, "TITAN: buddy", status)
  status <- ifelse(data$anflag == 105, "TITAN: SCT", status)
  status <- ifelse(data$anflag == 106, "TITAN: DEM", status)
  status <- ifelse(data$anflag == 107, "TITAN: isol", status)
  status <- ifelse(data$anflag == 108, "TITAN: fg", status)
  status <- ifelse(data$anflag == 109, "TITAN: STEVE", status)
  status <- ifelse(data$anflag == 110, "TITAN: fge", status)
  status <- ifelse(data$anflag == 111, "TITAN: ccrrt", status)
  status <- ifelse(data$anflag == 112, "TITAN: puddle", status)
  status <- ifelse(data$anflag == 113, "TITAN: buddy_eve", status)
  status <- ifelse(data$anflag == 114, "TITAN: fg2", status)
  status <- ifelse(data$anflag == 151, "Fraction", status)
  status <- ifelse(data$anflag == 199, "Domain", status)

  data$status <- status
  data <- data[rev(order(data$status)),]
  return(data)
}
