registerPlotCategory("Maps")

plotTitle.plotMap <- function(p, plotRequest, plotData) {
  crit <- plotRequest$criteria
  stationLabel <- getStationsForPlotTitle(plotRequest, plotData)
  if(as.character(crit$obnumber)=="7") {
    levelLabel <- "channels"
    detail <- sprintf("sensor=%s, satname=%s", crit$obname, crit$satname)
  } else {
    levelLabel <- "levels"
    detail <- sprintf("obname=%s, varname=%s", crit$obname, crit$varname)
  }

  title <- sprintf("%s: %s", plotRequest$expName, p$name)
  if(length(stationLabel)>0 && stationLabel!="") {
    title <- sprintf("%s\nstation=%s", title, stationLabel)
  }
  title <- sprintf(
    "%s\ndb=%s, DTG=%s, %s",
    title,
    plotRequest$dbType, formatDtg(crit$dtg), detail
  )
  levels <- paste(crit$levels, collapse=", ")
  if(levels!="") title <- sprintf("%s\n%s: %s", title, levelLabel, levels)
  return(title)
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
    labs(x="Longitude", y="Latitude")
}

doPlotly.plotMap <- function(p, plotRequest, plotData) {
  myPlotly <- plot_geo(
    data=plotData, lat=~latitude, lon =~longitude
  ) %>%
    layout(
      margin = list(
        t=100, # To leave space for the title
        b=10, # Looks better when figure is exported
        l=175, r=175 # To prevent legend from ending up too far away from plot
      ),
      showlegend = TRUE,
      legend = list(
        orientation="v", yanchor="center", y=0.5, xanchor="left", x=1.01
      ),
      geo = list(
        # See <https://plot.ly/r/reference/#layout-geo>
        resolution = 50,
        showland = TRUE,
        showlakes = TRUE,
        showcountries=TRUE,
        landcolor = toRGB("grey98"),
        countrycolor = toRGB("grey50"),
        lakecolor = toRGB("white"),
        projection = list(type="stereographic"),
        coastlinewidth = 0.5,
        countrywidth = 0.5,
        lataxis = list(
          range = range(plotData$latitude) + c(-1, 1),
          showgrid = TRUE,
          dtick = 10
        ),
        lonaxis = list(
          range = range(plotData$longitude) + c(-2, 2),
          showgrid = TRUE,
          dtick = 15
        )
      )
    )
  return(myPlotly)
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
  plotData$popup <- paste("Station: ", plotData$statLabel, "<br>Value: ",
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
  cm <- getSuitableColorScale(plotData)
  dataPal <- colorNumeric(palette=cm$palette, domain=cm$domain)
  legendPal <- colorNumeric(palette=rev(cm$palette), domain=cm$domain)
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
  pal <- colorFactor(c("green", "blue", "black", "grey", "magenta3", "red"),
                     domain=c("Active", "Active(2)",
                              "Blacklisted", "NA", "Passive", "Rejected"))
  plotData$popup <- paste(
                      "Station: ", plotData$statLabel,
                      "<br>Level: ", plotData$level,
                      "<br>Anflag: ", plotData$anflag,
                      "<br>Status:", plotData$status
                    )
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
               aes(x=longitude, y=latitude,
                 colour=status, shape=status, fill=status
               ),
               size=2, alpha=.75) +
    scale_shape_manual(name="Legend",
                        values=c("Active"=21, "Active(2)"=21,
                                 "Rejected"=22, "Passive"=23,
                                 "Blacklisted"=24, "NA"=13)) +
    scale_fill_manual(name="Legend",
                        values=c("Active"="green", "Active(2)"="blue",
                                 "Rejected"="red", "Passive"="magenta3",
                                 "Blacklisted"="black", "NA"=NA)) +
    scale_colour_manual(name="Legend",
                        values=c("Active"="green", "Active(2)"="blue",
                                 "Rejected"="red", "Passive"="black",
                                 "Blacklisted"="black", "NA"="grey"))
}

doPlotly.mapUsage <- function(p, plotRequest, plotData) {
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

  colors <- c(
    "Active"="green", "Active(2)"="blue",
    "Rejected"="red", "Passive"="magenta3",
    "Blacklisted"="black", "NA"="grey"
  )

  myPlotly <- NextMethod()
  myPlotly <- myPlotly %>%
    add_markers(
      data=plotData[rev(order(status)),],
      text=~paste(
        paste("Station:", statLabel),
        paste("Level:", level),
        sprintf("Coords: (%.3f\u00B0, %.3f\u00B0)", longitude, latitude),
        paste("Anflag:", anflag),
        paste("Status:", status),
        sep="<br />"
      ),
      marker = list(
        line = list(
          color = 'black',
          width = 0.25,
          opacity=0.1
        )
      ),
      symbol=~status, color=~status, colors=colors, size=2,
      hoverinfo="text"
    )

  return(myPlotly)
}

registerPlotType(
    "Maps",
    plotCreate(c("mapUsage", "plotMap"),
               "Observation Usage", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "active, rejected, passive, blacklisted, anflag, obsvalue",
                     "FROM usage WHERE %s"),
               list("obnumber", "obname"))
)

plotBuildQuery.mapThreshold <- function(p, plotRequest) {
  sprintf(p$queryStub, p$dataColumn, buildWhereClause(plotRequest$criteria))
}

doPlot.mapThreshold <- function(p, plotRequest, plotData) {
  minval <- min(plotData$plotValues)
  maxval <- max(plotData$plotValues)
  cm <- getSuitableColorScale(plotData)
  NextMethod() +
    geom_point(data=plotData,
               aes(x=longitude, y=latitude, fill=plotValues),
               size=3, shape=21, colour="gray50", alpha=.5, stroke=0.) +
    scale_fill_distiller(p$dataColumn, palette=cm$name,
                         direction=cm$direction, limits=cm$domain)
}

doPlotly.mapThreshold <- function(p, plotRequest, plotData) {
  cm <- getSuitableColorScale(plotData)
  myPlotly <- NextMethod()
  myPlotly <- myPlotly %>%
    add_markers(
      text=~paste(
        paste("Station:", statLabel),
        paste("Level:", level),
        sprintf("Coords: (%.3f\u00B0, %.3f\u00B0)", longitude, latitude),
        paste0(p$dataColumn, ": ", signif(plotValues, digits=5)),
        sep="<br />"
      ),
      size=2, color=~plotValues, colors=cm$palette,
      marker = list(
        line = list(
          color = 'black',
          width = 0.25,
          opacity=0.1
        )
      ),
      hoverinfo="text"
    ) %>%
    colorbar(
      limits=cm$domain,
      title=p$dataColumn,
      yanchor="center", y=0.5,
      xanchor="left", x=1.0
    )
  return(myPlotly)
}

registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "First Guess Departure Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid, obsvalue,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s AND (plotValues NOT NULL)"),
               list("obnumber", "obname"),
               dataColumn="fg_dep")
)
registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "First Guess Departure + Bias Correction Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s AND",
                     "fg_dep NOT NULL AND (plotValues NOT NULL)"),
               list("obnumber", "obname"),
               dataColumn="fg_dep+biascrl")
)
registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "Analysis Departure Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid, obsvalue,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s AND (plotValues NOT NULL)"),
               list("obnumber", "obname"),
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
                     "FROM usage WHERE %s AND (plotValues NOT NULL)"),
               list("obnumber", "obname"),
               dataColumn="an_dep-fg_dep")
)
registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "Bias Correction Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s AND (plotValues NOT NULL)"),
               list("obnumber", "obname"),
               dataColumn="biascrl")
)
registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "Observations Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s AND (plotValues NOT NULL)"),
               list("obnumber", "obname"),
               dataColumn="obsvalue")
)

# The "mapThresholdWithRangeAvgs" class is similar to mapThreshold, except
# that it takes in a date range and supports the selection of multiple cycles.
# The plotValues will represent an average of the selected dataColumn over the
# selectes dates and cycles.
registerPlotCategory("AverageMaps")

postProcessQueriedPlotData.mapThresholdWithRangeAvgs <- function(plotData) {
  # Columns we don't want to calculate the average for
  groupBy=c("statid", "latitude", "longitude", "level")
  # Make sure we don't try to calculate means of non-numeric columns
  nonNumericCols <- names(which(sapply(plotData, is.numeric)==FALSE))
  groupBy <- unique(c(groupBy, nonNumericCols))
  # Remove DTG if present. Doesn't make sense to have it after the averages.
  plotData <- within(plotData, rm(DTG))
  # Finally, calculate the averages
  plotData <- plotData %>%
    group_by(.dots=groupBy) %>%
    summarize_all(mean)
  return(plotData)
}

registerPlotType(
    "AverageMaps",
    plotCreate(c("mapThresholdWithRangeAvgs", "mapThreshold", "plotMap"),
               "Average First Guess Departure Map", "range",
               paste("SELECT",
                     "latitude, longitude, level, statid, obsvalue,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s AND (plotValues NOT NULL)"),
               list("obnumber", "obname"),
               dataColumn="fg_dep")
)
registerPlotType(
    "AverageMaps",
    plotCreate(c("mapThresholdWithRangeAvgs", "mapThreshold", "plotMap"),
               "Average First Guess Departure + Bias Correction Map", "range",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s AND (plotValues NOT NULL)"),
               list("obnumber", "obname"),
               dataColumn="fg_dep+biascrl")
)
registerPlotType(
    "AverageMaps",
    plotCreate(c("mapThresholdWithRangeAvgs", "mapThreshold", "plotMap"),
               "Average Analysis Departure Map", "range",
               paste("SELECT",
                     "latitude, longitude, level, statid, obsvalue,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s AND (plotValues NOT NULL)"),
               list("obnumber", "obname"),
               dataColumn="an_dep")
)
registerPlotType(
    "AverageMaps",
    plotCreate(c("mapThresholdWithRangeAvgs", "mapThreshold", "plotMap"),
               "Average Analysis Increment Map", "range",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "obsvalue, fg_dep, an_dep,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s AND (plotValues NOT NULL)"),
               list("obnumber", "obname"),
               dataColumn="an_dep-fg_dep")
)
registerPlotType(
    "AverageMaps",
    plotCreate(c("mapThresholdWithRangeAvgs", "mapThreshold", "plotMap"),
               "Average Bias Correction Map", "range",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s AND (plotValues NOT NULL)"),
               list("obnumber", "obname"),
               dataColumn="biascrl")
)
registerPlotType(
    "AverageMaps",
    plotCreate(c("mapThresholdWithRangeAvgs", "mapThreshold", "plotMap"),
               "Average Observations Map", "range",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "(%s) as plotValues",
                     "FROM usage WHERE %s AND (plotValues NOT NULL)"),
               list("obnumber", "obname"),
               dataColumn="obsvalue")
)
