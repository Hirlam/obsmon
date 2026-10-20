.filterOutZeroNobsTotal <- function(data, ...) {
  # Filters out data for which nobs_total==0, so that we don't end up with
  # these showing up in the plots. Returns the data unchanged if it does not
  # contain info on nobs_total
  if(nrow(data)==0) return(data)
  if(!is.null(data$nobs_total)) {
    dataNobsTotal <- aggregate(
      list(nobs_total=data$nobs_total),
      by=list(level=data[["level"]]),
      FUN=sum
    )
    levelsToRm <- dataNobsTotal$level[dataNobsTotal$nobs_total==0]
    data <- data[!(data[["level"]] %in% levelsToRm),]
  }
  return(data)
}

.numberOfActiveObsDataPostProcessingFunction <- function(data, ...) {
  data <- fillDataWithQualityControlStatus(data) %>%
    subset(grepl("active", tolower(status))) %>%
    group_by(DTG, level) %>%
    summarize(n_active_obs=n(), .groups="drop")
  return(data)
}


.obsFitActiveObsDataPostProcessingFunction <- function(data, ...) {
  # Preserve attributes
  # Store original column-level attributes (e.g. units)
  # so we can reattach them later
  oldColAttrs <- lapply(names(data), function(nm) attributes(data[[nm]]))
  names(oldColAttrs) <- names(data)

  # Convert to data.table
  dt <- as.data.table(data)
  dt <- fillDataWithQualityControlStatus(dt)

  # Filter for active
  dt <- dt[grepl("active", tolower(status))]
  hasAnDep <- "an_dep" %in% colnames(dt)

  # Summarize by:
  #  - nobs_total
  #  - keep departures for next group operation
  if(hasAnDep) {
    dt <- dt[, .(
      nobs_total = .N,
      fg_dep     = fg_dep,
      an_dep     = an_dep
    ), by=.(DTG, level, varname)]
  } else {
    dt <- dt[, .(
      nobs_total = .N,
      fg_dep     = fg_dep
    ), by=.(DTG, level, varname)]
  }

  # Now compute rms and bias by groups
  if(hasAnDep) {
    dt <- dt[, .(
      # total number of obs in this group
      nobs_total    = .N,
      fg_rms_total = sqrt(mean(fg_dep^2, na.rm=TRUE)),
      an_rms_total = sqrt(mean(an_dep^2, na.rm=TRUE)),
      fg_bias_total = mean(fg_dep, na.rm=TRUE),
      an_bias_total = mean(an_dep, na.rm=TRUE)
    ), by=.(DTG, level, varname)]
  } else {
    dt <- dt[, .(
      # total number of obs in this group
      nobs_total    = .N,
      fg_rms_total = sqrt(mean(fg_dep^2, na.rm=TRUE)),
      fg_bias_total = mean(fg_dep, na.rm=TRUE)
    ), by=.(DTG, level, varname)]
  }

  # Assign the same units as fg_dep or an_dep
  # (assuming these units existed originally)
  if (!is.null(oldColAttrs[["fg_dep"]][["units"]])) {
    units(dt$fg_rms_total)  <- oldColAttrs[["fg_dep"]][["units"]]
    units(dt$fg_bias_total) <- oldColAttrs[["fg_dep"]][["units"]]
  }
  if (!is.null(oldColAttrs[["an_dep"]][["units"]])) {
    units(dt$an_rms_total)  <- oldColAttrs[["an_dep"]][["units"]]
    units(dt$an_bias_total) <- oldColAttrs[["an_dep"]][["units"]]
  }

  # Convert back to tibble
  out <- as_tibble(dt)

  # Reattach original column-level attributes for columns that still exist
  for (nm in intersect(names(out), names(oldColAttrs))) {
    attributes(out[[nm]]) <- oldColAttrs[[nm]]
  }

  # Now 'out' is a tibble, but with new columns and (restored) units
  return(out)
}

landSeaDeparturesTimeseriesPlotPostProcessingFunction <- function(data, ...) {
  data <- .filterOutZeroNobsTotal(data)
  data <- within(data, rm("nobs_total"))
  seaCols <- c("fg_uncorr_sea", "fg_dep_sea", "an_dep_sea")
  landCols <- c("fg_uncorr_land", "fg_dep_land", "an_dep_land")
  data[data$nobs_sea==0, seaCols] <- NA
  data[data$nobs_land==0, landCols] <- NA
  return(data)
}

.getStaticGenericTimeseriesPlot <- function(
  plot, maskColumns=character(0), colours=NULL, shapes=NULL,
  showVariableLegend=FALSE, comparisonData=NULL,
  comparisonLineAlpha=1, comparisonPointSize=2.6
) {
  hasComp <- .plotHasComparison(plot)

  if (hasComp) {
    if(is.null(comparisonData)) {
      combinedData <- .combinePlotDataByExperiment(plot, maskColumns=maskColumns)
    } else {
      combinedData <- comparisonData
    }
    localPlotData <- melt(combinedData, id=c("DTG", "level", "experiment"))
  } else {
    localPlotData <- melt(
      plot$data[!(colnames(plot$data) %in% maskColumns)], id=c("DTG", "level")
    )
    primaryExperimentName <- .asScalarCharacterOrNa(plot$primaryExperimentName)
    if(is.na(primaryExperimentName)) primaryExperimentName <- "primary"
    localPlotData$experiment <- primaryExperimentName
  }

  if (hasComp) {
    localPlotData$legendGroup <- if(showVariableLegend) {
      metric <- as.character(localPlotData$variable)
      if(plot$parentType$name == "Bias Correction") {
        metric[metric == "fg_bias_total"] <- "fg_corr"
        metric[metric == "fg_uncorr_total"] <- "fg_uncorr"
      }
      paste(localPlotData$experiment, metric, sep=": ")
    } else {
      localPlotData$experiment
    }
    variableOrder <- levels(factor(localPlotData$variable))
    shapeValues <- shapes
    if(is.null(shapeValues)) shapeValues <- setNames(rep(16, length(variableOrder)), variableOrder)
    if(is.null(names(shapeValues))) names(shapeValues) <- variableOrder
    legendColours <- .comparisonScaleValues(localPlotData,
      .getExperimentColourValues(localPlotData$experiment), "experiment")
    legendShapes <- .comparisonScaleValues(localPlotData, shapeValues)
    legendLinetypes <- .comparisonScaleValues(localPlotData,
      .getVariableLineTypeValues(localPlotData$variable))
    obplot <- ggplot(data=localPlotData) +
      aes(x=DTG, y=value, group=legendGroup, colour=legendGroup,
        linetype=legendGroup, shape=legendGroup, fill=legendGroup) +
      geom_line(na.rm=TRUE, alpha=comparisonLineAlpha, size=0.85) +
      geom_point(na.rm=TRUE, size=comparisonPointSize) +
      scale_colour_manual(name=NULL, values=legendColours) +
      scale_fill_manual(name=NULL, values=legendColours) +
      scale_linetype_manual(name=NULL, values=legendLinetypes) +
      scale_shape_manual(name=NULL, values=legendShapes) +
      labs(x="DATE") +
      facet_wrap("level", labeller=label_both)
  } else {
    obplot <- ggplot(data=localPlotData) +
      geom_line(aes(x=DTG, y=value, group=variable), na.rm=TRUE, alpha=.1) +
      geom_point(
        aes(x=DTG, y=value, shape=variable, colour=variable, fill=variable),
        na.rm=TRUE
      ) +
      labs(x="DATE") +
      facet_wrap("level", labeller=label_both) +
      theme(legend.title=element_blank())
  }

  if (!hasComp && !is.null(shapes)) {
    obplot <- obplot +
      scale_shape_manual(
        values=shapes
      )
  }
  if (!hasComp) {
    if (!is.null(colours)) {
      obplot <- obplot +
        scale_colour_manual(values=colours) +
        scale_fill_manual(values=colours)
    } else {
      obplot <- obplot +
        scale_colour_brewer(palette="Spectral")
    }
  }
  if(!hasComp && length(levels(localPlotData$variable))<2) {
    # Only show legend when there are at least 2 different variables plotted
    obplot <- obplot + theme(legend.position = "none")
  }
  if(hasComp) obplot <- .withComparisonLegend(obplot, localPlotData)
  obplot
}

.getInteractiveGenericTimeseriesPlot <- function(plot) {
  ggplotPlot <- plot
  if(!inherits(plot, "ggplot")) ggplotPlot <- .getStaticGenericTimeseriesPlot(plot)

  myPlotly <- .applyComparisonLegend(ggplotly(ggplotPlot, tooltip=c("x","y")), ggplotPlot) %>%
    layout(
      margin=list(
        t=100,
        l=75 # Prevent y-axis label from being cut out
      ),
      legend=list(orientation="v", yanchor="center", y=0.5)
    ) %>%
    config(edits=list(axisTitleText=FALSE))
  # This parameter will be used in addTitleToPlot in order to
  # prevent the title from overlapping with the plot. ggplotly
  # seems to have issues when the original ggplot has facte_wraps.
  attr(myPlotly, "yTitle") <- 1.0375

  return(myPlotly)
}

# Register higher-level plotting functions
genericTimeseriesPlottingFunction <- function(plot) {
  if(!.plotHasAnyComparisonData(plot)) return(errorPlot("No data to plot."))
  if(plot$parentType$interactive) {
    return(.getInteractiveGenericTimeseriesPlot(plot))
  } else {
    return(.getStaticGenericTimeseriesPlot(plot))
  }
}
obsFitTimeseriesPlottingFunction <- function(plot) {
  if(!.plotHasAnyComparisonData(plot)) return(errorPlot("No data to plot."))
  dataPlot <- .getFirstNonEmptyComparisonPlot(plot)

  fgColor <- "blue"
  anColor <- "red"
  fgRmsShape <- 22
  anRmsShape <- 0
  fgBiasShape <- 21
  anBiasShape <- 1

  ylab <- sprintf(
    "%s [%s]",
    unique(dataPlot$data$varname),
    units(dataPlot$dataWithUnits[["fg_bias_total"]])
  )
  if(.plotHasComparison(plot) && plot$parentType$interactive) {
    return(.getInteractiveComparisonObsFitPlot(plot, ylab))
  }

  baseGgplotPlot <- .getStaticGenericTimeseriesPlot(
    plot,
    maskColumns=c("nobs_total", "varname"),
    colours=c(fgColor, anColor, fgColor, anColor),
    shapes=c(fgRmsShape, anRmsShape, fgBiasShape, anBiasShape),
    showVariableLegend=TRUE
  ) + ylab(ylab)

  if(plot$parentType$interactive) {
    return(.getInteractiveGenericTimeseriesPlot(baseGgplotPlot))
  }
  return(baseGgplotPlot)
}

.getInteractiveComparisonObsFitPlot <- function(plot, ylab) {
  comparisonData <- .combinePlotDataByExperiment(
    plot, maskColumns=c("nobs_total", "varname")
  )
  baseGgplotPlot <- .getStaticGenericTimeseriesPlot(
    plot,
    maskColumns=c("nobs_total", "varname"),
    shapes=c(
      fg_rms_total=15, an_rms_total=0,
      fg_bias_total=16, an_bias_total=1
    ),
    showVariableLegend=TRUE,
    comparisonData=comparisonData
  ) + ylab(ylab)
  chart <- .getInteractiveGenericTimeseriesPlot(baseGgplotPlot)
  chart
}

firstGuessTotalTimeseriesPlottingFunction <- function(plot) {
  if(!.plotHasAnyComparisonData(plot)) return(errorPlot("No data to plot."))

  hasComp <- .plotHasComparison(plot)
  dataPlot <- .getFirstNonEmptyComparisonPlot(plot)
  if(hasComp) {
    labelData <- .combinePlotDataByExperiment(plot, maskColumns=c("varname"))
    timeSpan <- diff(range(as.numeric(labelData$DTG), na.rm=TRUE))
    dodgeWidth <- if(is.finite(timeSpan) && timeSpan > 0) timeSpan / max(10, length(unique(labelData$DTG))) * 0.25 else 0.25
    labelData$legendGroup <- paste(labelData$experiment, "fg_uncorr", sep=": ")
    labelLayer <- geom_text(
      data=labelData,
      aes(x=DTG, y=fg_uncorr_total, label=nobs_total,
        group=legendGroup, colour=legendGroup),
      position=position_dodge(width=dodgeWidth),
      show.legend=FALSE
    )
  } else {
    labelLayer <- geom_text(
      data=plot$data, aes(x=DTG, y=fg_uncorr_total, label=nobs_total)
    )
  }
  baseGgplotPlot <- .getStaticGenericTimeseriesPlot(
    plot,
    maskColumns=c("nobs_total", "varname"),
    shapes=c(fg_bias_total=16, fg_uncorr_total=17),
    showVariableLegend=hasComp,
    comparisonLineAlpha=0.3,
    comparisonPointSize=1.5
  ) +
    labelLayer +
    ylab(sprintf(
      "%s [%s]",
      unique(dataPlot$data$varname),
      units(dataPlot$dataWithUnits[["fg_bias_total"]])
    ))

  if(plot$parentType$interactive) {
    return(.getInteractiveGenericTimeseriesPlot(baseGgplotPlot))
  }
  return(baseGgplotPlot)
}

landSeaDeparturesTimeseriesPlottingFunction <- function(plot) {
  if(nrow(plot$data)==0) return(errorPlot("No data to plot."))

  seaColor <- "blue"
  landColor <- "green"
  ncShape <- 2
  fgShape <- 1
  anShape <- 0

  baseGgplotPlotTop <- .getStaticGenericTimeseriesPlot(
    plot,
    maskColumns=c("nobs_land", "nobs_sea"),
    colours=c(seaColor, seaColor, seaColor, landColor, landColor, landColor),
    shapes=c(ncShape, fgShape, anShape, ncShape, fgShape, anShape)
  ) +
    ylab(sprintf(
      "Brightness Temperature [%s]",
      units(plot$dataWithUnits[["fg_dep_sea"]])
    )) +
    theme(legend.title=element_blank())


  bottomPlotData <- melt(
    plot$data[c("DTG", "level", "nobs_land", "nobs_sea")],
    id=c("DTG", "level")
  )
  baseGgplotPlotBottom <- ggplot(bottomPlotData) +
    aes(DTG, value, group=variable, fill=variable) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values=c(seaColor, landColor)) +
    facet_wrap("level", labeller=label_both) +
    labs(x="DATE", y="Number of Observations") +
    theme(legend.title=element_blank())

  if(plot$parentType$interactive) {
    top <- .getInteractiveGenericTimeseriesPlot(baseGgplotPlotTop) %>%
      layout(
        yaxis=list(
          title=sprintf(
            "Brightness Temperature [%s]",
            units(plot$dataWithUnits[["fg_dep_sea"]])
          )
        )
      )
    bottom <- .getInteractiveGenericTimeseriesPlot(baseGgplotPlotBottom) %>%
      layout(yaxis=list(title="Number of Observations"))
    interactivePlot <- subplot(
      top,
      bottom,
      nrows=2,
      margin=0.05,
      titleX=TRUE,
      titleY=TRUE
    ) %>% add_annotations(
      text="DATE",
      x=0.5,
      y=-0.075,
      xref="paper",
      yref="paper",
      textfont=list(size=25),
      showarrow=FALSE
    )
    # Prevent overlap between title and y-axis
    attr(interactivePlot, "yTitle") <- 1.0375
    return(interactivePlot)
  }
  return(grid.arrange(baseGgplotPlotTop, baseGgplotPlotBottom, ncol=1))
}

hovmollerTimeseriesPlottingFunction <- function(plot, colourLimits=NULL, showColorbar=TRUE) {
  if(nrow(plot$data)==0) return(errorPlot("No data to plot."))

  baseGgplotPlot <- ggplot(plot$data) +
    aes(DTG, level, fill=fg_bias_total) +
    geom_tile() +
    labs(x="DATE", y="Channel")

  if(!is.null(colourLimits)) {
    baseGgplotPlot <- baseGgplotPlot + scale_fill_gradient(limits=colourLimits)
    if(!showColorbar) baseGgplotPlot <- baseGgplotPlot + theme(legend.position="none")
  }
  if(plot$parentType$interactive && !is.null(colourLimits)) {
    data <- plot$data
    dates <- sort(unique(data$DTG))
    levels <- sort(unique(data$level))
    values <- matrix(NA_real_, nrow=length(levels), ncol=length(dates))
    values[cbind(match(data$level, levels), match(data$DTG, dates))] <- data$fg_bias_total
    return(plot_ly(x=dates, y=levels, z=values, type="heatmap",
      zmin=colourLimits[[1]], zmax=colourLimits[[2]],
      colorscale=list(c(0, "#132B43"), c(1, "#56B1F7")),
      showscale=showColorbar, colorbar=list(title="fg_bias_total")) %>%
      layout(xaxis=list(title="DATE"), yaxis=list(title="Channel"), margin=list(t=100)))
  }
  if(plot$parentType$interactive) {
    return(.getInteractiveGenericTimeseriesPlot(baseGgplotPlot))
  }
  return(baseGgplotPlot)
}

# Register Timeseries plots
plotRegistry$registerPlotType(
  name="Number of Observations",
  category="Timeseries",
  dateType="range",
  dataFieldsInRetrievedPlotData=list("DTG", "level", "nobs_total"),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  dataPostProcessingFunction=.filterOutZeroNobsTotal,
  plottingFunction=genericTimeseriesPlottingFunction
)

plotRegistry$registerPlotType(
  name="Number of Active Observations",
  category="Timeseries",
  dateType="range",
  dataFieldsInRetrievedPlotData=list(
    "DTG", "statid", "latitude", "longitude", "level",
    "anflag", "active", "rejected", "passive", "blacklisted"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  dataPostProcessingFunction=.numberOfActiveObsDataPostProcessingFunction,
  plottingFunction=genericTimeseriesPlottingFunction
)

plotRegistry$registerPlotType(
  name="Observation Value",
  category="Timeseries",
  dateType="range",
  dataFieldsInRetrievedPlotData=list("DTG", "level", "obsvalue"),
  dataFieldsInSqliteWhereClause=list("statid", "obnumber", "obname", "varname"),
  stationChoiceType="single",
  plottingFunction=genericTimeseriesPlottingFunction
)

plotRegistry$registerPlotType(
  name="First-Guess Departure Timeseries",
  category="Timeseries",
  dateType="range",
  dataFieldsInRetrievedPlotData=list("DTG", "level", "fg_dep"),
  dataFieldsInSqliteWhereClause=list("statid", "obnumber", "obname", "varname"),
  stationChoiceType="single",
  plottingFunction=genericTimeseriesPlottingFunction
)

plotRegistry$registerPlotType(
  name="Analysis Departure Timeseries",
  category="Timeseries",
  dateType="range",
  dataFieldsInRetrievedPlotData=list("DTG", "level", "an_dep"),
  dataFieldsInSqliteWhereClause=list("statid", "obnumber", "obname", "varname"),
  stationChoiceType="single",
  plottingFunction=genericTimeseriesPlottingFunction
)

plotRegistry$registerPlotType(
  name="ObsFit",
  category="Timeseries",
  dateType="range",
  dataFieldsInRetrievedPlotData=list(
    "DTG", "level", "varname", "nobs_total", "fg_rms_total", "an_rms_total",
    "fg_bias_total", "an_bias_total"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  dataPostProcessingFunction=.filterOutZeroNobsTotal,
  plottingFunction=obsFitTimeseriesPlottingFunction
)

# Register the new plot type
plotRegistry$registerPlotType(
  name="ObsFit (Active Observations Only)",
  category="Timeseries",
  dateType="range",
  dataFieldsInRetrievedPlotData=list(
    "DTG", "level", "varname", "fg_dep", "an_dep",
    "statid", "latitude", "longitude", 
    "anflag", "active", "rejected", "passive", "blacklisted"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  dataPostProcessingFunction=.obsFitActiveObsDataPostProcessingFunction,
  # Use the same plotting function as "ObsFit" but with different data
  plottingFunction=obsFitTimeseriesPlottingFunction 
)

plotRegistry$registerPlotType(
  name="Bias Correction",
  category="Timeseries",
  dateType="range",
  dataFieldsInRetrievedPlotData=list(
    "DTG", "level", "varname", "nobs_total", "fg_bias_total", "fg_uncorr_total"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  dataPostProcessingFunction=.filterOutZeroNobsTotal,
  plottingFunction=firstGuessTotalTimeseriesPlottingFunction
)

plotRegistry$registerPlotType(
  name="Land-sea departures",
  category="Timeseries",
  dateType="range",
  dataFieldsInRetrievedPlotData=list(
    "DTG", "level", "nobs_total", "nobs_land", "nobs_sea",
    "fg_uncorr_sea", "fg_dep_sea", "an_dep_sea", "fg_uncorr_land",
    "fg_dep_land", "an_dep_land"
  ),
  dataFieldsInSqliteWhereClause=list(list(obnumber=7), "obname"),
  dataPostProcessingFunction=landSeaDeparturesTimeseriesPlotPostProcessingFunction,
  plottingFunction=landSeaDeparturesTimeseriesPlottingFunction
)

plotRegistry$registerPlotType(
  name="Hovmöller",
  category="Timeseries",
  dateType="range",
  dataFieldsInRetrievedPlotData=list(
    "DTG", "level", "nobs_total", "fg_bias_total"
  ),
  dataFieldsInSqliteWhereClause=list(list(obnumber=7), "obname"),
  dataPostProcessingFunction=.filterOutZeroNobsTotal,
  plottingFunction=hovmollerTimeseriesPlottingFunction
)
