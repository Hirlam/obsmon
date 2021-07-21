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

        if("fg_dep_total" %in% colnames(plotData)) {
          ylab <- sprintf("Departure (%s)", units(plot$dataWithUnits$fg_dep_total))
        } else {
          ylab <- sprintf("Bias/RMS (%s)", units(plot$dataWithUnits$fg_bias_total))
        }

        dataCol2FillColor <- list(
          fg_bias_total="blue", an_bias_total="darkblue",
          fg_rms_total="red", an_rms_total="darkred",
          fg_dep_total="blue", an_dep_total="red"
        )
        dataCol2ScaleFillColor <- list(
          fg_bias_total="turquoise2", an_bias_total="coral",
          fg_rms_total="coral2", an_rms_total="turquoise3",
          fg_dep_total="turquoise2", an_dep_total="coral2"
        )
        dataCol2LineLabels <- list(
          fg_bias_total="FGBias", an_bias_total= "AnBias",
          fg_rms_total="FGRMS", an_rms_total="AnRMS",
          fg_dep_total="FgDep", an_dep_total="AnDep"
        )

        lineLabels <- c()
        plotValues <- c()
        fillColors <- c()
        scale_fill_colors <- c()
        for(colname in colnames(plotData)) {
          if (!(colname %in% names(dataCol2FillColor))) next
          lineLabels <- c(lineLabels, dataCol2LineLabels[[colname]])
          plotValues <- c(plotValues, plotData[[colname]])
          fillColors <- c(fillColors, dataCol2ScaleFillColor[[colname]])
          scale_fill_colors <- c(scale_fill_colors, dataCol2ScaleFillColor[[colname]])
        }
        params <- factor(lineLabels, lineLabels)

        df <- data.frame(params=params,plotValues=plotValues)
        obplot <- ggplot(data=df) +
          aes(x=params, y=plotValues, fill=fillColors) +
          geom_bar(stat="identity") +
          scale_fill_manual(name=NULL, values=scale_fill_colors) +
          guides(fill=FALSE) +
          labs(x=xlab, y=ylab) +
          theme(legend.position="none")
      },
      {
        localPlotData <- melt(plotData, id="level")

        yLabUnits <- units(plot$dataWithUnits[[localPlotData[["variable"]][1]]])
        if(strObnumber=="7") {
          # Mind that channels appear as "level" in the databases
          xlab <- "Channel"
          ylab <- "Brightness Temperature"
        } else {
          varname <- sqliteParams$varname
          xlab <- sprintf("Level [%s]", units(plot$dataWithUnits$level))
          ylab <- varname
        }
        ylab <- sprintf("%s [%s]", ylab, yLabUnits)

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
          scale_fill_manual(name=NULL, values=shape_colours) +
          coord_flip_wrapper(default=TRUE) +
          labs(x=xlab, y=ylab)
        if(strObnumber=="7") {
          obplot <- obplot + scale_x_continuous(breaks=plotData$level)
        } else {
          if(ud_are_convertible(units(plot$dataWithUnits$level), "Pa")) {
            # Using xlim causes ggplotly to omit either the shape or the line
            # (whichever is added later) from the legend. Not a big deal,
            # but worth pointing out, as this but may be solved in later
            # releases of the plotly package
            units(refPressures) <- units(plot$dataWithUnits$level)
            obplot <- obplot +
              xlim(max(drop_units(refPressures), localPlotData[["level"]]), 0)
          } else if(ud_are_convertible(units(plot$dataWithUnits$level), "meters")) {
            units(refHeights) <- units(plot$dataWithUnits$level)
            obplot <- obplot +
              xlim(0, max(drop_units(refHeights), localPlotData[["level"]]))
          }
        }
      }
  )
  return(obplot)
}

.postProcessingFuncAverageOverDTGs <- function(data) {
# Post-process the retrieved data to perform the averages
  originalDataComments <- comment(data)

  nonNumericCols <- names(which(sapply(data, is.numeric)==FALSE))
  groupByCols = c("level")
  colsToDrop <- c("DTG", setdiff(nonNumericCols, groupByCols))
  data <- data[!(colnames(data) %in% colsToDrop)]

  data <- data %>%
    group_by(!!!syms(groupByCols)) %>%
    summarize_all(mean)
  comment(data) <- originalDataComments
  return(data)
}

#######################################################################
plotRegistry$registerPlotType(
  name="First Guess and Analysis Bias/RMS",
  category="Statistical",
  dataFieldsInRetrievedPlotData=list(
    "level", "fg_bias_total", "an_bias_total", "fg_rms_total", "an_rms_total"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  plottingFunction=firstGuessAndAnPlottingFunction
)

plotRegistry$registerPlotType(
  name="First Guess and Analysis Departure",
  category="Statistical",
  dataFieldsInRetrievedPlotData=list(
    "level", "fg_dep_total", "an_dep_total"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  plottingFunction=firstGuessAndAnPlottingFunction
)

plotRegistry$registerPlotType(
  name="Average First Guess and Analysis Bias/RMS",
  category="Statistical",
  dataFieldsInRetrievedPlotData=list(
    "DTG", "level", "fg_bias_total", "an_bias_total",
    "fg_rms_total", "an_rms_total"
  ),
  dateType="range",
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  plottingFunction=firstGuessAndAnPlottingFunction,
  dataPostProcessingFunction=.postProcessingFuncAverageOverDTGs
)

plotRegistry$registerPlotType(
  name="Average First Guess and Analysis Departure",
  category="Statistical",
  dataFieldsInRetrievedPlotData=list(
    "level", "fg_dep_total", "an_dep_total"
  ),
  dateType="range",
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  plottingFunction=firstGuessAndAnPlottingFunction,
  dataPostProcessingFunction=.postProcessingFuncAverageOverDTGs
)
