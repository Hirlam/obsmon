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
  plot, maskColumns=character(0), colours=NULL, shapes=NULL
) {

  localPlotData <- melt(
    plot$data[!(colnames(plot$data) %in% maskColumns)], id=c("DTG", "level")
  )

  obplot <- ggplot(data=localPlotData) +
    geom_line(aes(x=DTG, y=value, group=variable), na.rm=TRUE, alpha=.1) +
    geom_point(
      aes(x=DTG, y=value, shape=variable, colour=variable, fill=variable),
      na.rm=TRUE
    ) +
    labs(x="DATE") +
    facet_wrap("level", labeller=label_both) +
    theme(legend.title=element_blank())
  if (!is.null(shapes)) {
    obplot <- obplot +
      scale_shape_manual(values=shapes)
  }
  if (!is.null(colours)) {
    obplot <- obplot +
      scale_colour_manual(values=colours) +
      scale_fill_manual(values=colours)
  } else {
    obplot <- obplot +
      scale_colour_brewer(palette="Spectral")
  }
  if(length(levels(localPlotData$variable))<2) {
    # Only show legend when there are at least 2 different variables plotted
    obplot <- obplot + theme(legend.position = "none")
  }
  obplot
}

.getInteractiveGenericTimeseriesPlot <- function(plot) {
  ggplotPlot <- plot
  if(!is.ggplot(plot)) ggplotPlot <- .getStaticGenericTimeseriesPlot(plot)

  myPlotly <- ggplotly(ggplotPlot, tooltip=c("x","y")) %>%
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
  if(nrow(plot$data)==0) return(errorPlot("No data to plot."))
  if(plot$parentType$interactive) {
    return(.getInteractiveGenericTimeseriesPlot(plot))
  } else {
    return(.getStaticGenericTimeseriesPlot(plot))
  }
}

obsFitTimeseriesPlottingFunction <- function(plot) {
  if(nrow(plot$data)==0) return(errorPlot("No data to plot."))

  fgColor <- "blue"
  anColor <- "red"
  fgRmsShape <- 22
  anRmsShape <- 0
  fgBiasShape <- 21
  anBiasShape <- 1

  baseGgplotPlot <- .getStaticGenericTimeseriesPlot(
    plot,
    maskColumns=c("nobs_total", "varname"),
    colours=c(fgColor, anColor, fgColor, anColor),
    shapes=c(fgRmsShape, anRmsShape, fgBiasShape, anBiasShape)
  ) +
    ylab(sprintf(
      "%s [%s]",
      unique(plot$data$varname),
      units(plot$dataWithUnits[["fg_bias_total"]])
    ))

  if(plot$parentType$interactive) {
    return(.getInteractiveGenericTimeseriesPlot(baseGgplotPlot))
  }
  return(baseGgplotPlot)
}

firstGuessTotalTimeseriesPlottingFunction <- function(plot) {
  if(nrow(plot$data)==0) return(errorPlot("No data to plot."))

  baseGgplotPlot <- .getStaticGenericTimeseriesPlot(
    plot, maskColumns=c("nobs_total", "varname")
  ) +
    geom_text(
      data=plot$data, aes(x=DTG, y=fg_uncorr_total, label=nobs_total)
    ) +
    ylab(sprintf(
      "%s [%s]",
      unique(plot$data$varname),
      units(plot$dataWithUnits[["fg_bias_total"]])
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

hovmollerTimeseriesPlottingFunction <- function(plot) {
  if(nrow(plot$data)==0) return(errorPlot("No data to plot."))

  baseGgplotPlot <- ggplot(plot$data) +
    aes(DTG, level, fill=fg_bias_total) +
    geom_tile() +
    labs(x="DATE", y="Channel")

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
