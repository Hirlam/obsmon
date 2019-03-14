registerPlotCategory("Statistical")

plotTitle.plotStatistical <- function(p, plotRequest, plotData) {
  dtg <- formatDtg(plotRequest$criteria$dtg)
  titleStub <- sprintf("%s: %s %%s %s", plotRequest$expName, p$name, dtg)
  switch(
      as.character(plotRequest$criteria$obnumber),
      "1"=,
      "4"=,
      "9"={
        detail <- plotRequest$criteria$varname
      },
      "7"={
        detail <- paste(plotRequest$criteria$obname,
                        plotRequest$criteria$satname)
      },
      {
        detail <- paste(plotRequest$criteria$obname,
                        plotRequest$criteria$varname)
      }
  )
  title <- sprintf(titleStub, detail)
  title
}

doPlot.plotStatistical <- function(p, plotRequest, plotData) {
  switch(
      as.character(plotRequest$criteria$obnumber),
      "1"=,
      "4"=,
      "9"={
        varname <- plotRequest$criteria$varname
        xlab <- varname
        ylab <- sprintf("Bias/RMS (%s)", units[[varname]])
        df <- data.frame(
            params=factor(c("FGBias", "AnBias", "FGRMS",  "AnRMS"),
                          c("FGBias", "AnBias", "FGRMS",  "AnRMS")),
            biasRMSvalues=c(plotData$fg_bias_total, plotData$an_bias_total,
                            plotData$fg_rms_total, plotData$an_rms_total)
        )
        obplot <- ggplot(data=df) +
          aes(x=params, y=biasRMSvalues,
              fill=c("blue", "darkblue", "red", "darkred")) +
          geom_bar(stat="identity") +
          scale_fill_manual(name=NULL, values=c("turquoise2", "coral", "coral2", "turquoise3")) +
          guides(fill=FALSE) +
          labs(x=xlab, y=ylab) +
          theme(legend.position="none")
      },
      "7"={
        xlab <- "Channels"
        ylab <- "Brightness temperature [K]"
        localPlotData <- melt(plotData, id=c("channel"))
        obplot <- ggplot(data=localPlotData) +
          aes(x=channel, y=value,
            group=variable, colour=variable, shape=variable, fill=variable
          ) +
          geom_point(size=4) +
          geom_line() +
          scale_shape_manual(name=NULL, values=c(22,23,22,23)) +
          scale_colour_manual(name=NULL, values=c("blue", "blue4", "red4", "red")) +
          scale_fill_manual(name=NULL, values=c("blue", "blue4", "red4", "red")) +
          coord_flip_wrapper(default=TRUE) +
          scale_x_continuous(breaks=plotData$channel) +
          labs(x=xlab, y=ylab)
      },
      {
        varname <- plotRequest$criteria$varname
        ylab <- sprintf("%s [%s]", varname, units[[varname]])
        xlab <- "Pressure"
        localPlotData <- melt(plotData, id=c("level"))
        obplot <- ggplot(data=localPlotData) +
          aes(x=level, y=value,
            group=variable, colour=variable, shape=variable, fill=variable
          ) +
          # Add geom_point before geom_line so that plotly included the shape
          # in the legend. See comment on xlim below.
          geom_point(size=4) +
          geom_line() +
          scale_shape_manual(name=NULL, values=c(22,23,22,23)) +
          scale_colour_manual(name=NULL, values=c("blue", "blue4", "red4", "red")) +
          scale_fill_manual(name=NULL, values=c("blue", "blue4", "red4", "red")) +
          coord_flip_wrapper(default=TRUE) +
          labs(x=xlab, y=ylab) +
          # Using xlim causes ggplotly to omit either the shape or the line
          # (whichever is not added first) from the legend. Not a big deal,
          # but worth pointing out, as this but may be solved in later
          # releases of the plotly package
          xlim(100000,0)
      }
  )

  obplot
}

registerPlotType(
    "Statistical",
    plotCreate("plotStatistical",
               "First Guess and Analysis Departure", "single",
               paste("SELECT",
                     "fg_bias_total, an_bias_total,",
                     "fg_rms_total, an_rms_total, level",
                     "FROM obsmon WHERE %s",
                     "ORDER BY level"),
               list("obnumber", "obname"))
)
