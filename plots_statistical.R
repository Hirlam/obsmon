library(reshape2)

registerPlotCategory("Statistical")

plotCreateStatistical <- plotCreateConstructor("plotStatistical", "single")

doPlot.plotStatistical <- function(p, plotRequest, plotData) {
  dtg <- formatDtg(plotRequest$criteria$dtg)
  titleStub <- sprintf("%s : %s %%s %s", plotRequest$exp$name, p$name, dtg)
  switch(
      as.character(plotRequest$criteria$obnumber),
      "1"=,
      "4"=,
      "9"={
        varname <- plotRequest$criteria$varname
        title <- sprintf(titleStub, varname)
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
          ylab(ylab) +
          xlab(xlab) +
          labs(title=title)
      },
      "7"={
        ylab <- "Channels"
        xlab <- "Brightness temperature [K]"
        title <- sprintf(titleStub,
                         paste(plotRequest$criteria$sensor,
                               plotRequest$criteria$satellite))
        obplot <- ggplot(plotData, aes(level)) +
          geom_line(aes(y=fg_bias_total,colour="fg_bias_total")) +
          geom_point(aes(y=fg_bias_total,colour="fg_bias_total"),size=4) +
          geom_line(aes(y=an_bias_total,colour="an_bias_total"))+
          geom_point(aes(y=an_bias_total,colour="an_bias_total"),size=4) +
          geom_line(aes(y=fg_rms_total,colour="fg_rms_total")) +
          geom_point(aes(y=fg_rms_total,colour="fg_rms_total"),size=4) +
          geom_line(aes(y=an_rms_total,colour="an_rms_total")) +
          geom_point(aes(y=an_rms_total,colour="an_rms_total"),size=4) +
          coord_flip() +
          scale_x_continuous(breaks=plotData$level) +
          ylab(xlab) +
          xlab(ylab) +
          labs(title=title)
      },
      {
        varname <- plotRequest$criteria$varname
        xlab <- sprintf("(%s)", units[[varname]])
        ylab <- "Pressure"
        title <- sprintf(titleStub,
                         paste(plotRequest$criteria$obname, varname))
        localPlotData <- melt(plotData, id=c("level"))
        obplot <- ggplot(data=localPlotData) +
          aes(x=level, y=value,
              group=variable, colour=variable, shape=variable) +
          geom_line() +
          geom_point(size=4) +
          scale_shape_manual(values=c(16,16,32,32)) +
          scale_colour_manual(values=c("blue", "red", "blue", "red")) +
          coord_flip()+
          ylab(xlab) +
          xlab(ylab) +
          xlim(100000,0) +
          labs(title=title)
      }
  )
  obplot
}

registerPlotType(
    "Statistical",
    plotCreateStatistical("First Guess and Analysis Departure",
                          paste("SELECT",
                                "fg_bias_total, an_bias_total,",
                                "fg_rms_total, an_rms_total, level",
                                "FROM obsmon WHERE %s",
                                "ORDER BY level"),
                          list("obnumber", "obname", "levels"))
)
