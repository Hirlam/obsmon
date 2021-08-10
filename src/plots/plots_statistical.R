firstGuessAndAnPlottingFunction <-  function(plot) {
  sqliteParams <- plot$paramsAsInSqliteDbs
  plotData <- plot$data

  dataCol2FillColor <- list(
    fg_bias_total="blue", an_bias_total="darkblue",
    fg_bias_total_mean="blue", an_bias_total_mean="darkblue",
    fg_bias_total_sd="blue", an_bias_total_sd="red",
    fg_rms_total="red", an_rms_total="darkred",
    fg_rms_total_mean="red", an_rms_total_mean="darkred",
    fg_rms_total_sd="blue", an_rms_total_sd="red",
    fg_dep_total="blue", an_dep_total="red",
    fg_dep_total_mean="blue", an_dep_total_mean="red",
    fg_dep_total_sd="blue", an_dep_total_sd="red",
    fg_dep="blue", an_dep="red",
    fg_dep_mean="blue", an_dep_mean="red",
    fg_dep_sd="blue", an_dep_sd="red"
  )
  dataCol2Shape <- list(
    fg_bias_total=22, an_bias_total=23,
    fg_bias_total_mean=22, an_bias_total_mean=23,
    fg_bias_total_sd=22, an_bias_total_sd=23,
    fg_rms_total=22, an_rms_total=23,
    fg_rms_total_mean=22, an_rms_total_mean=23,
    fg_rms_total_sd=22, an_rms_total_sd=23,
    fg_dep_total=22, an_dep_total=23,
    fg_dep_total_mean=22, an_dep_total_mean=23,
    fg_dep_total_sd=22, an_dep_total_sd=23,
    fg_dep=22, an_dep=23,
    fg_dep_mean=22, an_dep_mean=23,
    fg_dep_sd=22, an_dep_sd=23
  )
  dataCol2ScaleFillColor <- list(
    fg_bias_total="turquoise2", an_bias_total="coral",
    fg_bias_total_mean="turquoise2", an_bias_total_mean="coral",
    fg_bias_total_sd="turquoise2", an_bias_total_sd="coral",
    fg_rms_total="coral2", an_rms_total="turquoise3",
    fg_rms_total_mean="coral2", an_rms_total_mean="turquoise3",
    fg_rms_total_sd="coral2", an_rms_total_sd="turquoise3",
    fg_dep_total="turquoise2", an_dep_total="coral2",
    fg_dep_total_mean="turquoise2", an_dep_total_mean="coral2",
    fg_dep_total_sd="turquoise2", an_dep_total_sd="coral2",
    fg_dep="turquoise2", an_dep="coral2",
    fg_dep_mean="turquoise2", an_dep_mean="coral2",
    fg_dep_sd="turquoise2", an_dep_sd="coral2"
  )
  dataCol2LineLabels <- list(
    fg_bias_total="FGBias", an_bias_total= "AnBias",
    fg_rms_total="FGRMS", an_rms_total="AnRMS",
    fg_dep_total="FgDep", an_dep_total="AnDep",
    fg_dep="FgDep", an_dep="AnDep",
    fg_dep_mean="FgDepMean", an_dep_mean="AnDepMean"
  )

  strObnumber <- as.character(sqliteParams$obnumber)
  switch(
      strObnumber,
      "1"=,
      "4"=,
      "9"={
        varname <- sqliteParams$varname
        xlab <- varname

        if(any(startsWith(colnames(plotData), "fg_dep"))) {
          ylab <- "Departure"
          colnameStartToGetUnits <- "fg_dep"
        } else {
          ylab <- "Bias/RMS"
          colnameStartToGetUnits <- "fg_bias"
        }
        for(colname in colnames(plot$dataWithUnits)) {
          if(startsWith(colname, colnameStartToGetUnits)) {
            ylabUnits <- units(plot$dataWithUnits[[colname]])
            ylab <- sprintf("%s (%s)", ylab, ylabUnits)
            break
          }
        }

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
        # TEST
        propertiesWithMeanAndSd <- c()
        for(colname in colnames(plotData)) {
          if(!endsWith(colname, "_mean")) next
          propertyName <- str_remove(colname, "_mean$")
          stdevColname <- paste0(propertyName, "_sd")
          if(!(stdevColname %in% colnames(plotData))) next
          propertiesWithMeanAndSd <- c(propertiesWithMeanAndSd, propertyName)
        }
        # END TEST

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

        shape_numbers <- c(22,23,22,23)
        shape_colours <- c("blue", "blue4", "red4", "red")

        # TEST
        # Remove std dev data from plotData because these will be added later
        # as errorbars
        dataIsStdMask <- endsWith(as.character(localPlotData$variable), "_sd")
        traceLabels <- unique(localPlotData$variable)
        localPlotData <- localPlotData[!dataIsStdMask, ]
        nVariables <- length(unique(localPlotData$variable))
        shape_numbers <- shape_numbers[1:nVariables]
        shape_colours <- shape_colours[1:nVariables]
        iColor <- 0
        for(property in propertiesWithMeanAndSd) {
          iColor <- iColor + 1
          shape_colours <- c(shape_colours, shape_colours[iColor])
          shape_numbers <- c(shape_numbers, shape_numbers[iColor])
        }
        # END TEST

        obplot <- ggplot(data=localPlotData) +
          aes_string(x="level", y="value",
            group="variable", colour="variable",
            shape="variable", fill="variable"
          ) +
          # Add geom_point before geom_line so that plotly includes the shapes
          # in the legend. See comment on xlim below.
          geom_point(size=4) +
          geom_line() +
          scale_shape_manual(name=NULL, values=shape_numbers) +
          scale_colour_manual(name=NULL, values=shape_colours) +
          scale_fill_manual(name=NULL, values=shape_colours) +
          coord_flip_wrapper(default=TRUE) +
          labs(x=xlab, y=ylab)

        # TEST
        if(length(propertiesWithMeanAndSd) > 0) {
          sdBarData <- data.frame(plotData)
          colnames(sdBarData) <- gsub("_sd$", "\\.sd", colnames(sdBarData))
          colnames(sdBarData) <- gsub("_mean$", "\\.mean", colnames(sdBarData))

          # This "sub" call replaces colnames such as "A.B" with "B.A"
          colnames(sdBarData) <-  sub("^(.*)\\.([^\\.]*)$", "\\2.\\1", colnames(sdBarData))

          sdBarData <- reshape(sdBarData,
            varying=2:length(colnames(sdBarData)),
            direction = "long", # towards long
            timevar="property", # the grouping variable
            idvar="level", #identifying variable
            sep="." #separated by dots
          )
          rownames(sdBarData) <- NULL
          sdBarData$property <- paste0(sdBarData$property, "_std")

          obplot <- obplot +
          #geom_errorbar(
          geom_pointrange(
            data=sdBarData,
            inherit.aes=FALSE,
            show.legend=TRUE,
            #width=.2,
            mapping=aes(
              x=level,
              y=mean,
              ymin=mean - sd,
              ymax=mean + sd,
              group=property,
              colour=property,
              shape=property,
              fill=property
            )
          )
        }
        # END TEST

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

.postProcessingFuncAvgDtgsLevels <- function(data) {
  # Post-process the retrieved data to perform the averages
  originalDataComments <- comment(data)

  nonNumericCols <- names(which(sapply(data, is.numeric)==FALSE))
  groupByCols = c("level")
  colsToDrop <- c("DTG", setdiff(nonNumericCols, groupByCols))
  data <- data[!(colnames(data) %in% colsToDrop)]

  population_sd <- function(x) sqrt(sum((x - mean(x))^2)/(length(x)))

  data <- data %>%
    group_by(!!!syms(groupByCols)) %>%
    summarize_all(list(mean=mean, sd=population_sd))
  comment(data) <- originalDataComments

  return(data)
}

#######################################################################
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
  name="First Guess and Analysis Bias/RMS",
  category="Statistical",
  dataFieldsInRetrievedPlotData=list(
    "level", "fg_bias_total", "an_bias_total", "fg_rms_total", "an_rms_total"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  plottingFunction=firstGuessAndAnPlottingFunction
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
  dataPostProcessingFunction=.postProcessingFuncAvgDtgsLevels
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
  dataPostProcessingFunction=.postProcessingFuncAvgDtgsLevels
)

plotRegistry$registerPlotType(
  name="Station Average First Guess and Analysis Departure",
  category="Statistical",
  dataFieldsInRetrievedPlotData=list(
    "level", "fg_dep", "an_dep"
  ),
  dateType="range",
  dataFieldsInSqliteWhereClause=list("statid", "obnumber", "obname"),
  plottingFunction=firstGuessAndAnPlottingFunction,
  dataPostProcessingFunction=.postProcessingFuncAvgDtgsLevels
)
