getSdBarData <- function(plotData) {
    # Identify columns in the data for properties with mean & sd info
    propertiesWithMeanAndSd <- c()
    for(colname in colnames(plotData)) {
      if(!endsWith(colname, "_mean")) next
      propertyName <- str_remove(colname, "_mean$")
      stdevColname <- paste0(propertyName, "_sd")
      if(!(stdevColname %in% colnames(plotData))) next
      propertiesWithMeanAndSd <- c(propertiesWithMeanAndSd, propertyName)
    }

    # Put errorbar data in a separate dataframe formatted conveniently
    sdBarData <- NULL
    if(length(propertiesWithMeanAndSd) > 0) {
      sdBarData <- data.frame(plotData)
      colnames(sdBarData) <- gsub("_sd$", "\\.sd", colnames(sdBarData))
      colnames(sdBarData) <- gsub("_mean$", "\\.mean", colnames(sdBarData))

      # This "sub" call replaces colnames such as "A.B" with "B.A"
      colnames(sdBarData) <-  sub(
        "^(.*)\\.([^\\.]*)$",
        "\\2.\\1",
        colnames(sdBarData)
      )

      sdBarData <- reshape(sdBarData,
        varying=2:length(colnames(sdBarData)),
        direction = "long", # towards long
        timevar="property", # the grouping variable
        idvar="level", #identifying variable
        sep="." #separated by dots
      )
      rownames(sdBarData) <- NULL
      sdBarData$property <- paste0(sdBarData$property, "_sd")

      # Error bars with zero width are just clutter. Removing these.
      sdBarData <- sdBarData[sdBarData$sd > 0, ]
      if(nrow(sdBarData) == 0) sdBarData <- NULL
    }
    return(sdBarData)
}

addErrorbar <- function(obplot, sdBarData, strObnumber) {
  # Return obplot with added error bars
  if(strObnumber %in% c("1", "4", "9")) {
    xvar <- "variable"
    sdBarData$variable <- sub("_sd$", "_mean", sdBarData$property)
    # Reduce opacity of geom_bar to facilitate visualisation of errorbar
    obplot <- obplot + aes(alpha=0.9)
    errorbarWidth <- 0.2
  } else {
    xvar <- "level"
    # Adjust errorbar width. It seems ggplot's width is calculated w.r.t.
    # the value of yvar here. If we don't do this, ranges that are too
    # small will lead to huge-width errorbar endings.
    maxRange <- 0
    for (property in unique(sdBarData[["property"]])) {
      sdBarDataSubset <- sdBarData[sdBarData$property == property, ]
      range <- max(sdBarDataSubset$mean) - min(sdBarDataSubset$mean)
      if(range > maxRange) maxRange <- range
    }
    errorbarWidth <- min(0.25 * maxRange, 0.2)
  }


  obplot <- obplot +
    geom_errorbar(
      data=sdBarData,
      inherit.aes=FALSE,
      show.legend=TRUE,
      width=errorbarWidth,
      mapping=aes_string(
        x=xvar,
        y="mean",
        ymin="mean - sd",
        ymax="mean + sd",
        group="property",
        colour="property",
        # Shape and fill are not used by geom_errorbar, but if we don't
        # add these here then the legend is formatted strangely after
        # making the plot interactive with ggplotly
        shape="property",
        fill="property"
      )
    )
  return(obplot)
}

firstGuessAndAnPlottingFunction <-  function(plot) {
  sqliteParams <- plot$paramsAsInSqliteDbs
  plotData <- plot$data

  # Separate main plot data from data used to draw error bars, as these
  # are formatted differently and handled separately.
  sdBarData <- getSdBarData(plotData)
  mainPlotData <- melt(plotData, id="level")
  dataIsStdMask <- endsWith(as.character(mainPlotData$variable), "_sd")
  mainPlotData <- mainPlotData[!dataIsStdMask, ]

  dataCol2FillColor <- c(
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
  dataCol2Shape <- c(
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
  dataCol2ScaleFillColor <- c(
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

        # Main plot, without stdev data. These will be plotted as errorbars
        obplot <- ggplot(data=mainPlotData) +
          aes(
            x=variable,
            y=value,
            group=variable,
            color=variable,
            fill=variable,
            # shape is not used in geom_bar, but if we don't add this here
            # then the legend becomes strange after using ggplotly
            shape=variable
          ) +
          geom_bar(stat="identity") +
          scale_colour_manual(name=NULL, values=dataCol2ScaleFillColor) +
          scale_fill_manual(name=NULL, values=dataCol2ScaleFillColor) +
          theme(legend.title=element_blank()) +
          labs(x=xlab, y=ylab)
      },
      {
        yLabUnits <- units(plot$dataWithUnits[[mainPlotData[["variable"]][1]]])
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

        # Main data in plot
        obplot <- ggplot(data=mainPlotData) +
          aes_string(x="level", y="value",
            group="variable", colour="variable",
            shape="variable", fill="variable"
          ) +
          # Add geom_point before geom_line so that plotly includes the shapes
          # in the legend. See comment on xlim below.
          geom_point(size=4) +
          geom_line() +
          scale_shape_manual(name=NULL, values=dataCol2Shape) +
          scale_colour_manual(name=NULL, values=dataCol2FillColor) +
          scale_fill_manual(name=NULL, values=dataCol2FillColor) +
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
              xlim(max(drop_units(refPressures), mainPlotData[["level"]]), 0)
          } else if(ud_are_convertible(units(plot$dataWithUnits$level), "meters")) {
            units(refHeights) <- units(plot$dataWithUnits$level)
            obplot <- obplot +
              xlim(0, max(drop_units(refHeights), mainPlotData[["level"]]))
          }
        }
      }
  )

  # Add errorbars to the plot, if applicable
  if(!is.null(sdBarData)) {
    withCallingHandlers(
      # Suppress warnings about the use of shape and fill aesthetics.
      # See comment in the addErrorbar routine.
      obplot <- addErrorbar(obplot, sdBarData, strObnumber),
      warning = function(w) {
        if (startsWith(conditionMessage(w), "Ignoring unknown aesthetics:")) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }
  return(obplot)
}

.postProcessingFuncAvgDtgsLevels <- function(data, ...) {
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
