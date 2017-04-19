library(reshape2)

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
              fill=c("red", "darkblue", "red", "darkblue")) +
          geom_bar(stat="identity") +
          guides(fill=FALSE) +
          labs(x=xlab, y=ylab)
      },
      "7"={
        ylab <- "Channels"
        xlab <- "Brightness temperature [K]"
        obplot <- ggplot(plotData, aes(channel)) +
          geom_line(aes(y=fg_bias_total,colour="fg_bias_total")) +
          geom_point(aes(y=fg_bias_total,colour="fg_bias_total"),size=4) +
          geom_line(aes(y=an_bias_total,colour="an_bias_total"))+
          geom_point(aes(y=an_bias_total,colour="an_bias_total"),size=4) +
          geom_line(aes(y=fg_rms_total,colour="fg_rms_total")) +
          geom_point(aes(y=fg_rms_total,colour="fg_rms_total"),size=4) +
          geom_line(aes(y=an_rms_total,colour="an_rms_total")) +
          geom_point(aes(y=an_rms_total,colour="an_rms_total"),size=4) +
          coord_flip() +
          scale_x_continuous(breaks=plotData$channel) +
          labs(x=xlab, y=ylab)
      },
      {
        varname <- plotRequest$criteria$varname
        xlab <- sprintf("(%s)", units[[varname]])
        ylab <- "Pressure"
        localPlotData <- melt(plotData, id=c("level"))
        obplot <- ggplot(data=localPlotData) +
          aes(x=level, y=value,
              group=variable, colour=variable, shape=variable) +
          geom_line() +
          geom_point(size=4) +
          scale_shape_manual(values=c(16,16,32,32)) +
          scale_colour_manual(values=c("blue", "red", "blue", "red")) +
          coord_flip() +
          labs(x=xlab, y=ylab) +
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
               list("obnumber", "obname", "levels"))
)
