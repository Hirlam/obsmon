firstGuessAndAnPlottingFunction <-  function(plot) {
  sqliteParams <- plot$paramsAsInSqliteDbs
  plotData <- plot$data

  strObnumber <- as.character(sqliteParams$obnumber)
  switch(
      strObnumber,
      "1"=,
      "4"=,
      "9"={
        varname <- sqliteParams$varname
        xlab <- varname
        ylab <- sprintf("Bias/RMS [%s]", units(plotData$fg_bias_total))
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
        localPlotData <- melt(plotData, id="level")

        if(strObnumber=="7") {
          # Mind that channels appear as "level" in the databases
          xlab <- "Channel"
          ylab <- "Brightness temperature"
        } else {
          xlab <- sprintf("Level [%s]", units(plotData$level))
          ylab <- sqliteParams$varname
        }
        ylab <- tryCatch(
          sprintf("%s [%s]", ylab, units(plotData$fg_bias_total)),
          error=function(e) {flog.warn(e); ylab}
        )

        shape_colours <- c("blue", "blue4", "red4", "red")
        obplot <- ggplot(data=localPlotData) +
          aes_string(x="level", y="value",
            group="variable", colour="variable",
            shape="variable", fill="variable"
          ) +
          # Add geom_point before geom_line so that plotly includes the shapes
          # in the legend. See comment on xlim below.
          geom_point(size=4) +
          geom_line() +
          scale_shape_manual(name=NULL, values=c(22,23,22,23)) +
          scale_colour_manual(name=NULL, values=shape_colours) +
          scale_fill_manual(name=NULL, values=shape_colours)
        if(strObnumber=="7") {
          obplot <- obplot + scale_x_continuous(breaks=plotData$channel)
        } else {
          if(ud_are_convertible(units(plotData$level), "Pa")) {
            # Using xlim causes (i) problems with data frames with units
            # information, and (ii) ggplotly to omit either the shape or the
            # line (whichever is added later) from the legend. Item (ii) is not 
            # a big deal, but worth pointing out, as this but may be solved in
            # later releases of the plotly package.
            obplot <- obplot +
              coord_cartesian(
                xlim=c(
                  max(refPressures, localPlotData[["level"]]),
                  set_units(0, "Pa")
                ),
                default=TRUE
              )
          } else if (ud_are_convertible(units(plotData$level), "meters")) {
            obplot <- obplot + xlim(0, max(refHeights, localPlotData[["level"]]))
            obplot <- obplot +
              coord_cartesian(
                xlim=c(
                  set_units(0, "meters"),
                  max(refHeights, localPlotData[["level"]]),
                ),
                default=TRUE
              )
          }
        }
        obplot <- obplot +
          labs(x=xlab, y=ylab) +
          coord_flip_wrapper(default=TRUE)
      }
  )
  return(obplot)
}

plotRegistry$registerPlotType(
  name="First Guess and Analysis Departure",
  category="Statistical",
  dataFieldsInRetrievedPlotData=list(
    "level", "fg_bias_total", "an_bias_total", "fg_rms_total", "an_rms_total"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  plottingFunction=firstGuessAndAnPlottingFunction
)
