registerPlotCategory("Timeseries")

doPlot.plotTimeseries <- function(p, plotRequest, plotData,
                                  maskColumns=character(0),
                                  colours=NULL, shapes=NULL) {
  dtg <- formatDtg(plotRequest$criteria$dtg)
  titleStub <- sprintf("%s : %s %%s %s", plotRequest$exp$name, p$name, dtg)
  switch(
      as.character(plotRequest$criteria$obnumber),
      "7"={
        title <- sprintf(titleStub,
                         paste(plotRequest$criteria$obname,
                               plotRequest$criteria$satname))
        wrapVariable <- "channel"
      },
      {
        title <- sprintf(titleStub,
                         paste(plotRequest$criteria$obname,
                               plotRequest$criteria$varname))
        wrapVariable <- "level"
      }
  )
  localPlotData <- melt(plotData[!(colnames(plotData) %in% maskColumns)],
                        id=c("DTG", wrapVariable))
  obplot <- ggplot() +
    geom_point(data=localPlotData,
               aes(x=DTG, y=value, shape=variable, colour=variable),
               na.rm=TRUE) +
    labs(title=title, x="DATE") +
    facet_wrap(wrapVariable, labeller=label_both)
  if (!is.null(shapes)) {
    obplot <- obplot +
      scale_shape_manual(values=shapes)
  }
  if (!is.null(colours)) {
    obplot <- obplot +
      scale_colour_manual(values=colours)
  } else {
    obplot <- obplot +
      scale_colour_brewer(palette="Spectral")
  }
  obplot
}

registerPlotType(
    "Timeseries",
    plotCreate("plotTimeseries", "Number of Observations", "range",
               "SELECT DTG, level, nobs_total FROM obsmon WHERE %s",
               list("obnumber", "obname", "levels"))
)

doPlot.obsFit <- function(p, plotRequest, plotData) {
  fgColor <- "blue"
  anColor <- "red"
  rmsShape <- 0
  biasShape <- 1
  ind <- plotData$nobs_total==0
  cols <- c("fg_rms_total", "an_rms_total", "fg_bias_total", "an_bias_total")
  plotData[ind, cols] <- NA
  varname <- unique(plotData$varname)
  NextMethod(.Generic, maskColumns=c("nobs_total", "varname"),
             colours=c(fgColor, anColor, fgColor, anColor),
             shapes=c(rmsShape, biasShape, rmsShape, biasShape)) +
    ylab(units[[varname]])
}

registerPlotType(
    "Timeseries",
    plotCreate(c("obsFit", "plotTimeseries"), "ObsFit", "range",
               paste("SELECT DTG, level, varname, nobs_total,",
                     "fg_rms_total, an_rms_total,",
                     "fg_bias_total, an_bias_total",
                     "FROM obsmon WHERE %s"),
               list("obnumber", "obname", "levels"))
)

doPlot.biasCorrection <- function(p, plotRequest, plotData) {
  cycle <- plotRequest$criteria$dtg[[1]] %% 100
  ind <- as.POSIXlt(plotData$DTG)$hour==cycle
  plotData <- plotData[ind,]
  varname <- unique(plotData$varname)
  NextMethod(.Generic, maskColumns=c("nobs_total", "varname")) +
    geom_text(data=plotData, aes(x=DTG, y=fg_uncorr_total, label=nobs_total)) +
    ylab(units[[varname]])
}

registerPlotType(
    "Timeseries",
    plotCreate(c("biasCorrection", "plotTimeseries"),
               "Bias Correction", "range",
               paste("SELECT DTG, level, varname, nobs_total,",
                     "fg_bias_total, fg_uncorr_total",
                     "FROM obsmon WHERE %s"),
               list("obnumber"=7, "obname", "levels"))
)

doPlot.landSeaDepartures <- function(p, plotRequest, plotData) {
  seaColor <- "blue"
  landColor <- "green"
  ncShape <- 2
  fgShape <- 1
  anShape <- 0
  seaCols <- c("fg_uncorr_sea", "fg_dep_sea", "an_dep_sea")
  landCols <- c("fg_uncorr_land", "fg_dep_land", "an_dep_land")
  plotData[plotData$nobs_sea==0, seaCols] <- NA
  plotData[plotData$nobs_land==0, landCols] <- NA
  top <- NextMethod(.Generic, maskColumns=c("nobs_land", "nobs_sea"),
                    colours=c(seaColor, seaColor, seaColor,
                              landColor, landColor, landColor),
                    shapes=c(ncShape, fgShape, anShape,
                             ncShape, fgShape, anShape)
                    ) +
    ylab("Brightness Temperature [K]")
  localPlotData <- melt(plotData[c("DTG", "channel", "nobs_land", "nobs_sea")],
                        id=c("DTG", "channel"))
  bottom <- ggplot(localPlotData) +
    aes(DTG, value, group=variable, fill=variable) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values=c(seaColor, landColor)) +
    facet_wrap("channel", labeller=label_both) +
    labs(x="DATE", y="Number of Observations")
  grid.arrange(top, bottom, ncol=1)
}

registerPlotType(
    "Timeseries",
    plotCreate(c("landSeaDepartures", "plotTimeseries"),
               "Land-sea departures", "range",
               paste("SELECT DTG, level, nobs_land, nobs_sea,",
                     "fg_uncorr_sea, fg_dep_sea, an_dep_sea,",
                     "fg_uncorr_land, fg_dep_land, an_dep_land",
                     "FROM obsmon WHERE %s"),
               list("obnumber"=7, "obname", "levels"))
)

doPlot.hovmoller <- function(p, plotRequest, plotData) {
  dtg <- formatDtg(plotRequest$criteria$dtg)
  titleStub <- sprintf("%s : %s %%s %s", plotRequest$exp$name, p$name, dtg)
  title <- sprintf(titleStub,
                   paste(plotRequest$criteria$obname,
                         plotRequest$criteria$satname))
  obplot <- ggplot(plotData) +
    aes(DTG, channel, fill=fg_bias_total) +
    geom_raster() +
    labs(title=title, x="DATE", y="Channels")
}

registerPlotType(
    "Timeseries",
    plotCreate(c("hovmoller", "plotTimeseries"),
               "Hovmöller", "range",
               paste("SELECT DTG, level, fg_bias_total",
                     "FROM obsmon WHERE %s"),
               list("obnumber"=7, "obname", "levels"))
)
