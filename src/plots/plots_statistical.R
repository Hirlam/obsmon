firstGuessAndAnPlottingFunction <-  function(plot) {
  sqliteParams <- plot$getSqliteParamsFromUiParams()
  plotData <- plot$data

  strObnumber <- as.character(sqliteParams$obnumber)
  switch(
      strObnumber,
      "1"=,
      "4"=,
      "9"={
        varname <- sqliteParams$varname
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
          scale_fill_manual(
            name=NULL, values=c("turquoise2", "coral", "coral2", "turquoise3")
          ) +
          guides(fill=FALSE) +
          labs(x=xlab, y=ylab) +
          theme(legend.position="none")
      },
      {
        if(strObnumber=="7") {
          xVar <- "channel"
          xlab <- "Channels"
          ylab <- "Brightness temperature [K]"
        } else {
          xVar <- "level"
          varname <- sqliteParams$varname
          ylab <- sprintf("%s [%s]", varname, units[[varname]])
          xlab <- levelsLableForPlots(strObnumber, varname)
        }
        localPlotData <- melt(plotData, id=c(xVar))

        shape_colours <- c("blue", "blue4", "red4", "red")
        obplot <- ggplot(data=localPlotData) +
          aes_string(x=xVar, y="value",
            group="variable", colour="variable",
            shape="variable", fill="variable"
          ) +
          # Add geom_point before geom_line so that plotly includes the shapes
          # in the legend. See comment on xlim below.
          geom_point(size=4) +
          geom_line() +
          scale_shape_manual(name=NULL, values=c(22,23,22,23)) +
          scale_colour_manual(name=NULL, values=shape_colours) +
          scale_fill_manual(name=NULL, values=shape_colours) +
          coord_flip_wrapper(default=TRUE) +
          labs(x=xlab, y=ylab)
        if(strObnumber=="7") {
          obplot <- obplot + scale_x_continuous(breaks=plotData$channel)
        } else {
          if(startsWith(tolower(xlab), "pressure")) {
            # Using xlim causes ggplotly to omit either the shape or the line
            # (whichever is added later) from the legend. Not a big deal,
            # but worth pointing out, as this but may be solved in later
            # releases of the plotly package
            obplot <- obplot + xlim(max(refPressures,localPlotData[[xVar]]),0)
          } else {
            obplot <- obplot + xlim(0, max(refHeights,localPlotData[[xVar]]))
          }
        }
      }
  )
  return(obplot)
}

plotRegistry$registerPlotType(plotType(
  name="First Guess and Analysis Departure",
  category="Statistical",
  dataX="level",
  dataY=list("fg_bias_total", "an_bias_total", "fg_rms_total", "an_rms_total"),
  requiredDataFields=list("obnumber", "obname"),
  plottingFunction=firstGuessAndAnPlottingFunction
))
