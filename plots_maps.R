registerPlotCategory("Maps")

plotTitle.plotMap <- function(p, plotRequest, plotData) {
  dtg <- formatDtg(plotRequest$criteria$dtg)
  titleStub <- sprintf("%s: %s %%s %s\n%%s", plotRequest$expName, p$name, dtg)
  levels <- paste(plotRequest$criteria$levels, collapse=", ")
  switch(
      as.character(plotRequest$criteria$obnumber),
      "7"={
        varLabel <- plotRequest$criteria$satname
        levelLabel <- "channels"
      },
      {
        varLabel <- plotRequest$criteria$varname
        levelLabel <- "levels"
      }
  )
  title <- sprintf(titleStub,
                   paste(plotRequest$criteria$obname, varLabel),
                   sprintf("%s: %s", levelLabel, levels))
  title
}


doPlot.plotMap <- function(p, plotRequest, plotData) {
  x1 <- min(plotData$longitude)-2
  x2 <- max(plotData$longitude)+2
  y1 <- min(plotData$latitude)-2
  y2 <- max(plotData$latitude)+2
  ggplot() +
    geom_path(data=map_data(map="world"),
              aes(long, lat, group=group),
              colour="gray50") +
    coord_map("stereographic", xlim=c(x1, x2), ylim=c(y1, y2)) +
    labs(x="lat", y="lon")
}

doMap.plotMap <- function(p, plotRequest, plotData) {
  x1 <- min(plotData$longitude)-2
  x2 <- max(plotData$longitude)+2
  y1 <- min(plotData$latitude)-2
  y2 <- max(plotData$latitude)+2
  zoomLevel <- 4
  zoomLevels <- c(2000, 4000, 6000, 10000, 15000, 45000, 70000, 90000)
  for ( i in 2:length(zoomLevels)){
    if ((length(plotData$plotValues) > zoomLevels[i-1])
        & (length(plotData$plotValues) <= zoomLevels[i])){
      zoomLevel <- i+3
    }
  }
}

myLabelFormat <- function(
  prefix = '', suffix = '', between = ' &ndash; ', digits = 3, big.mark = ',',
  transform = identity, reverseOrder = FALSE
) {

  formatNum <- function(x) {
    format(
      round(transform(x), digits), trim = TRUE, scientific = FALSE,
      big.mark = big.mark
    )
  }

  function(type, ...) {
    switch(
      type,
      numeric = (function(cuts) {
        if (reverseOrder) {
          cuts <- sort(cuts, decreasing=T)
        }
        paste0(prefix, formatNum(cuts), suffix)
      })(...),
      bin = (function(cuts) {
        if (reverseOrder) {
          cuts <- sort(cuts, decreasing=T)
        }
        n = length(cuts)
        paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]), suffix)
      })(...),
      quantile = (function(cuts, p) {
        if (reverseOrder) {
          cuts <- sort(cuts, decreasing=T)
        }
        n = length(cuts)
        p = paste0(round(p * 100), '%')
        cuts = paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
        # mouse over the legend labels to see the values (quantiles)
        paste0(
          '<span title="', cuts, '">', prefix, p[-n], between, p[-1], suffix,
          '</span>'
        )
      })(...),
      factor = (function(cuts) {
        if (reverseOrder) {
          cuts <- sort(cuts, decreasing=T)
        }
        paste0(prefix, as.character(transform(cuts)), suffix)
      })(...)
    )
  }
}

doMap.mapThreshold <- function(p, plotRequest, plotData) {
  x1 <- min(plotData$longitude)-2
  x2 <- max(plotData$longitude)+2
  y1 <- min(plotData$latitude)-2
  y2 <- max(plotData$latitude)+2
  zoomLevel <- 4
  zoomLevels <- c(2000, 4000, 6000, 10000, 15000, 45000, 70000, 90000)
  for ( i in 2:length(zoomLevels)){
    if ((length(plotData$plotValues) > zoomLevels[i-1])
        & (length(plotData$plotValues) <= zoomLevels[i])){
      zoomLevel <- i+3
    }
  }
  plotData$popup <- paste("Statid: ", plotData$statid, "<br>Value: ",
                          signif(plotData$plotValues, digits=5),
                          "<br>Level: ", plotData$level)
  if ( max(plotData$plotValues) > 0 ) {
    plotData$radius <- (abs(plotData$plotValues)/max(abs(plotData$plotValues)))*10
    if ( length(plotData$radius) > 0 ) {
      for (i in 1:length(plotData$radius)) {
        if ( plotData$radius[i] < 3 ){ plotData$radius[i]=3}
      }
    }
  }else{
    plotData$radius=5
  }
  cm <- colormapContinuous(min(plotData$plotValues),
                           max(plotData$plotValues))
  dataPal <- colorNumeric(palette=cm$palette, domain=cm$domain, reverse=cm$reverse)
  legendPal <- colorNumeric(palette=cm$palette, domain=cm$domain, reverse=!cm$reverse)
  clusterOptions <- markerClusterOptions(disableClusteringAtZoom=zoomLevel)
  obMap <- leaflet(plotData) %>%
    addProviderTiles("Esri.WorldStreetMap",
                     options=providerTileOptions(opacity=0.7)) %>%
    fitBounds(x1, y1, x2, y2) %>%
    addCircleMarkers(~longitude, ~latitude, popup=~popup, radius=~radius,
                     stroke=FALSE, weight=1, opacity=1, color="black",
                     fillColor=~dataPal(plotValues), fillOpacity=.7,
                     clusterOptions = clusterOptions) %>%
    addLegend("topright", pal=legendPal, values=cm$domain, opacity=1,
              labFormat=myLabelFormat(reverseOrder=T))
  obMap
}

doMap.mapUsage <- function(p, plotRequest, plotData) {
  status <- rep("NA", nrow(plotData))
  status <- ifelse(plotData$anflag == 0, "Rejected", status)
  status <- ifelse(plotData$active  > 0, "Active", status)
  status <- ifelse(plotData$rejected > 0, "Rejected", status)
  status <- ifelse(plotData$passive > 0, "Passive", status)
  status <- ifelse(plotData$blacklisted > 0, "Blacklisted", status)
  status <- ifelse(plotData$anflag  > 0, "Active(2)", status)
  status <- ifelse(plotData$anflag == 4, "Rejected", status)
  status <- ifelse(plotData$anflag == 8, "Blacklisted", status)
  plotData$status <- status
  x1 <- min(plotData$longitude)-2
  x2 <- max(plotData$longitude)+2
  y1 <- min(plotData$latitude)-2
  y2 <- max(plotData$latitude)+2
  zoomLevel <- 4
  zoomLevels <- c(2000, 4000, 6000, 10000, 15000, 45000, 70000, 90000)
  for ( i in 2:length(zoomLevels)){
    if ((length(plotData$plotValues) > zoomLevels[i-1])
        & (length(plotData$plotValues) <= zoomLevels[i])){
      zoomLevel <- i+3
    }
  }
  pal <- colorFactor(c("green", "blue", "black", "grey", "yellow", "red"),
                     domain=c("Active", "Active(2)",
                              "Blacklisted", "NA", "Passive", "Rejected"))
  plotData$popup <- paste("Statid: ", plotData$statid, "<br>Anflag: ",
                          plotData$anflag, "<br>Status:", plotData$status)
  clusterOptions <- markerClusterOptions(disableClusteringAtZoom=zoomLevel)
  obMap <- leaflet(data=plotData[rev(order(status)),]) %>%
    addProviderTiles("Esri.WorldStreetMap",
                     options=providerTileOptions(opacity=0.5)) %>%
    fitBounds(x1, y1, x2, y2 ) %>%
    addCircleMarkers(~longitude, ~latitude, popup=~popup, radius=8,
                     stroke=FALSE, weight=1, opacity=1, color="black",
                     fillColor=~pal(status), fillOpacity=.5,
                     clusterOptions=clusterOptions) %>%
    addLegend("topright", pal=pal, values=~status, opacity=1)
  obMap
}

doPlot.mapUsage <- function(p, plotRequest, plotData) {
  status <- rep("NA", nrow(plotData))
  status <- ifelse(plotData$anflag == 0, "Rejected", status)
  status <- ifelse(plotData$active  > 0, "Active", status)
  status <- ifelse(plotData$rejected > 0, "Rejected", status)
  status <- ifelse(plotData$passive > 0, "Passive", status)
  status <- ifelse(plotData$blacklisted > 0, "Blacklisted", status)
  status <- ifelse(plotData$anflag  > 0, "Active(2)", status)
  status <- ifelse(plotData$anflag == 4, "Rejected", status)
  status <- ifelse(plotData$anflag == 8, "Blacklisted", status)
  plotData$status <- status
  NextMethod() +
    geom_point(data=plotData[rev(order(status)),],
               aes(x=longitude, y=latitude, colour=status),
               size=2, alpha=.5, stroke=0.) +
    scale_colour_manual(name="Legend",
                        values=c("Active"="green", "Active(2)"="blue",
                                 "Rejected"="red", "Passive"="yellow",
                                 "Blacklisted"="black", "NA"="grey"))
}

registerPlotType(
    "Maps",
    plotCreate(c("mapUsage", "plotMap"),
               "Observation Usage", "single",
               paste("SELECT",
                     "latitude, longitude, statid,",
                     "active, rejected, passive, blacklisted, anflag",
                     "FROM usage WHERE %s"),
               list("obnumber", "obname", "levels"))
)

plotBuildQuery.mapThreshold <- function(p, plotRequest) {
  sprintf(p$queryStub, p$dataColumn, buildWhereClause(plotRequest$criteria))
}

doPlot.mapThreshold <- function(p, plotRequest, plotData) {
  minval <- min(plotData$plotValues)
  maxval <- max(plotData$plotValues)
  cm <- colormapContinuous(minval, maxval)
  NextMethod() +
    geom_point(data=plotData,
               aes(x=longitude, y=latitude, fill=plotValues),
               size=3, shape=21, colour="gray50", alpha=.5, stroke=0.) +
    scale_fill_distiller(p$dataColumn, type=cm$type, palette=cm$palette,
                         direction=cm$direction, limits=cm$domain)
}

registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "First Guess Departure Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid, obsvalue,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s"),
               list("obnumber", "obname", "levels"),
               dataColumn="fg_dep")
)
registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "First Guess Departure + Bias Correction Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s"),
               list("obnumber"=7, "obname", "levels"),
               dataColumn="fg_dep+biascrl")
)
registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "Analysis Departure Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid, obsvalue,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s"),
               list("obnumber", "obname", "levels"),
               dataColumn="an_dep")
)
registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "Analysis Increment Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "obsvalue, fg_dep, an_dep,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s"),
               list("obnumber", "obname", "levels"),
               dataColumn="fg_dep-an_dep")
)
registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "Bias Correction Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s"),
               list("obnumber"=7, "obname", "levels"),
               dataColumn="biascrl")
)
registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "Observations Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s"),
               list("obnumber", "obname", "levels"),
               dataColumn="obsvalue")
)

# The "mapThresholdWithRangeAvgs" class is similar to mapThreshold, except
# that it takes in a date range and supports the selection of multiple cycles.
# The plotValues will represent an average of the selected dataColumn over the
# selectes dates and cycles.
mapThresholdWithRangeAggregateAndApplyFunction <-
  function(plotter, plotData, FUN='mean', aggregateBy=c("statid", "level")) {
  # Grouping data by the colnames specified in aggregateBy, 
  # then applying function FUN within each group
  if(nrow(plotData) > 0) {
    aggregateByList = plotData[, aggregateBy]
    columnsNotToBeAggreg <- which(colnames(plotData) %in% 
                                c("DTG", aggregateBy)
                              )
    plotData <- aggregate(plotData[, -columnsNotToBeAggreg], 
                  by=aggregateByList,
                  FUN=FUN,
                  na.rm=TRUE
                )
  }
  # Returning
  plotData
}

postProcessQueriedPlotData.mapThresholdWithRangeAvgs <-
  function(plotter, plotData) {
    mapThresholdWithRangeAggregateAndApplyFunction(
      plotter, plotData, FUN="mean", aggregateBy=c("statid", "level")
  )
}

registerPlotType(
    "Maps",
    plotCreate(c("mapThresholdWithRangeAvgs", "mapThreshold", "plotMap"),
               "Average Analysis Increment Map", "range",
               paste("SELECT",
                     "DTG, latitude, longitude, level, statid,",
                     "obsvalue, fg_dep, an_dep,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s"),
               list("obnumber", "obname", "levels"),
               dataColumn="fg_dep-an_dep")
)
