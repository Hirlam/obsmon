##################################
# helpers for making ggplot maps #
##################################
.getStaticGenericMapPlot <- function(plot) {
  # Former doPlot.plotMap
  x1 <- min(plot$data$longitude)-2
  x2 <- max(plot$data$longitude)+2
  y1 <- min(plot$data$latitude)-2
  y2 <- max(plot$data$latitude)+2
  ggplotMap <- ggplot() +
    geom_path(
      data=map_data(map="world"),
      aes(long, lat, group=group),
      colour="gray50"
    ) +
    coord_map("stereographic", xlim=c(x1, x2), ylim=c(y1, y2)) +
    labs(x="Longitude", y="Latitude")
  return(ggplotMap)
}

.mapUsageStaticPlottingFunction <- function(plot) {
  ggplotMap <- .getStaticGenericMapPlot(plot) +
    geom_point(
      data=plot$data,
      aes(x=longitude, y=latitude, colour=status, shape=status, fill=status),
      size=2, alpha=.75
    ) +
    scale_shape_manual(
      name="Legend",
      values=c(
        "Active"=21, "Active(2)"=21, "Rejected"=22, "Passive"=23,
        "Blacklisted"=24, "NA"=13
      )
    ) +
    scale_fill_manual(
      name="Legend",
      values=c(
        "Active"="green", "Active(2)"="blue", "Rejected"="red",
        "Passive"="magenta3", "Blacklisted"="black", "NA"=NA
      )
    ) +
    scale_colour_manual(
      name="Legend",
      values=c(
        "Active"="green", "Active(2)"="blue", "Rejected"="red",
        "Passive"="black", "Blacklisted"="black", "NA"="grey"
      )
    )
  return(ggplotMap)
}

.mapThresholdStaticPlottingFunction <- function(plot) {
  cm <- .getSuitableColorScale(plot$data)
  dataColumnName <- unname(attributes(plot$data)$comment["dataColumn"])
  if(length(dataColumnName) == 0) {
    flog.error(".mapThresholdStaticPlottingFunction: Empty dataColumnName")
  }

  ggplotMap <- .getStaticGenericMapPlot(plot) +
    geom_point(
      data=plot$data,
      aes_string(x="longitude", y="latitude", fill=dataColumnName),
      size=3,
      shape=21,
      colour="gray50",
      alpha=.5,
      stroke=0.
    ) +
    scale_fill_distiller(
      dataColumnName,
      palette=cm$name,
      direction=cm$direction,
      limits=cm$domain
    )

    return(ggplotMap)
}

##################################
# helpers for making plotly maps #
##################################
.getInteractiveGenericMapPlot <- function(plot) {
  # Former doPlotly.plotMap
  myPlotly <- plot_geo(
    data=plot$data, lat=~jitter(latitude, 1), lon =~jitter(longitude, 1)
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
          range = range(plot$data$latitude) + c(-1, 1),
          showgrid = TRUE,
          dtick = 10
        ),
        lonaxis = list(
          range = range(plot$data$longitude) + c(-2, 2),
          showgrid = TRUE,
          dtick = 15
        )
      )
    )
  return(myPlotly)
}

.mapUsageInteractivePlottingFunction <- function(plot) {
  colors <- c(
    "Active"="green", "Active(2)"="blue",
    "Rejected"="red", "Passive"="magenta3",
    "Blacklisted"="black", "NA"="grey"
  )

  plotlyMap <- .getInteractiveGenericMapPlot(plot) %>%
    add_markers(
      data=plot$data,
      text=~gsub(
        "\\w*:[[:space:]]*<br />", "",
        gsub(
          "(<br />){2,}", "<br />",
          paste(
            if("statid" %in% names(plot$data)) paste("Station:", statid),
            if("level" %in% names(plot$data)) paste("Level:", level),
            if("channel" %in% names(plot$data)) paste("Channel:", channel),
            sprintf("Coords: (%.3f\u00B0, %.3f\u00B0)", longitude, latitude),
            paste("Anflag:", anflag),
            paste("Status:", status),
            sep="<br />"
          )
        )
      ),
      marker=list(
        line=list(
          color = 'black',
          width = 0.25,
          opacity=0.1
        )
      ),
      symbol=~status,
      color=~status,
      colors=colors,
      size=2,
      hoverinfo="text"
    )

  return(plotlyMap)
}

.mapThresholdInteractivePlottingFunction <- function(plot) {
  cm <- .getSuitableColorScale(plot$data)
  dataColumnName <- unname(attributes(plot$data)$comment["dataColumn"])

  .generatePlotlyUpdateColorScaleButton <- function(colorScaleName) {
    rtn <- list(
      label=colorScaleName,
      method="restyle",
      args=list(list(
        marker.colorscale=colorScaleName
      ))
    )
    return(rtn)
  }

  plotlyMap <- .getInteractiveGenericMapPlot(plot) %>%
    add_markers(
      text=~gsub(
        "\\w*:[[:space:]]*<br />", "",
        gsub(
          "(<br />){2,}", "<br />",
          paste(
	    if("statid" %in% names(plot$data)) paste("Station:", statid),
	    if("level" %in% names(plot$data)) paste("Level:", level),
	    if("channel" %in% names(plot$data)) paste("Channel:", channel),
            sprintf("Coords: (%.3f\u00B0, %.3f\u00B0)", longitude, latitude),
            paste0(dataColumnName, ": ", signif(plot$data[[dataColumnName]], digits=5)),
            sep="<br />"
          )
        )
      ),
      size=2,
      color=as.formula(sprintf("~%s", dataColumnName)),
      colors=cm$palette,
      marker=list(
        line=list(
          color='black',
          width=0.25,
          opacity=0.1
        )
      ),
      hoverinfo="text"
    ) %>%
    colorbar(
      limits=cm$domain,
      title=dataColumnName,
      yanchor="center", y=0.5,
      xanchor="left", x=1.0
    ) %>%
    layout(
      updatemenus = list(
        list(
          y = 0.8,
          buttons=lapply(
            plotlyJSColorMapNames,
            .generatePlotlyUpdateColorScaleButton
          )
        )
      )
    )

  return(plotlyMap)
}

###################################
# helpers for making leaflet maps #
###################################
.getLeafletMapZoomLevel <- function(plotData) {
  dataColumn <- unname(attributes(plotData)$comment["dataColumn"])
  if(is.null(dataColumn)) {
    dataColumn <- colnames(plotData)[ncol(plotData)]
  }
  zoomLevel <- 4
  zoomLevels <- c(2000, 4000, 6000, 10000, 15000, 45000, 70000, 90000)
  for ( i in 2:length(zoomLevels)){
    if ((length(plotData[[dataColumn]]) > zoomLevels[i-1])
        & (length(plotData[[dataColumn]]) <= zoomLevels[i])){
      zoomLevel <- i+3
    }
  }
  return(zoomLevel)
}

.fillDataWithLeafletRadiusInfo <- function(plotData) {
  dataColumn <- unname(attributes(plotData)$comment["dataColumn"])
  if (max(plotData[[dataColumn]]) > 0) {
    plotData$radius <- 10.0 * (
      abs(plotData[[dataColumn]])/max(abs(plotData[[dataColumn]]))
    )
    if (length(plotData$radius) > 0) {
      for (i in 1:length(plotData$radius)) {
        if (plotData$radius[i] < 3) { plotData$radius[i]=3}
      }
    }
  } else {
    plotData$radius <- 5
  }
  return(plotData)
}

####################################################
# plotting functions passed when registering plots #
####################################################
# a) combined ggploy/plotly (shown in "plot" tab in the UI)
.mapUsagePlottingFunction <- function(plot) {
  if(nrow(plot$data)==0) return(errorPlot("No data to plot."))
  if(plot$parentType$interactive) {
    return(.mapUsageInteractivePlottingFunction(plot))
  } else {
    return(.mapUsageStaticPlottingFunction(plot))
  }
}

.mapThresholdPlottingFunction <- function(plot) {
  if(nrow(plot$data)==0) return(errorPlot("No data to plot."))
  if(plot$parentType$interactive) {
    return(.mapThresholdInteractivePlottingFunction(plot))
  } else {
    return(.mapThresholdStaticPlottingFunction(plot))
  }
}

# b) leaflet (shown in "map" tab in the UI)
.mapThresholdLeafletPlottingFunction <- function(plot) {
  plotData <- .fillDataWithLeafletRadiusInfo(plot$data)
  dataColumn <- unname(attributes(plotData)$comment["dataColumn"])
  zoomLevel <- .getLeafletMapZoomLevel(plotData)

  cm <- .getSuitableColorScale(plotData)
  dataPal <- colorNumeric(palette=cm$palette, domain=cm$domain)
  legendPal <- colorNumeric(palette=rev(cm$palette), domain=cm$domain)

  leafletMap <- leaflet(data=plotData) %>%
    addProviderTiles(
      "Esri.WorldStreetMap",
      options=providerTileOptions(opacity=0.7)
    ) %>%
    addCircleMarkers(
      lng=~longitude,
      lat=~latitude,
      popup=.getLeafletPopupContents(plot$data),
      radius=~radius,
      fillColor=as.formula(sprintf("~dataPal(`%s`)", dataColumn)),
      fillOpacity=.7,
      stroke=FALSE,
      weight=1,
      opacity=1,
      color="black",
      clusterOptions=markerClusterOptions(disableClusteringAtZoom=zoomLevel)
    ) %>%
    addLegend(
      "topright",
      pal=legendPal,
      title=dataColumn,
      values=cm$domain,
      opacity=1,
      labFormat=labelFormat(transform=function(x) sort(x, decreasing=TRUE))
    )
  return(leafletMap)
}

.mapUsageLeafletPlottingFunction <- function(plot) {
  zoomLevel <- .getLeafletMapZoomLevel(plot$data)
  pallete <- colorFactor(
    c("green", "blue", "black", "grey", "magenta3", "red"),
    domain=c("Active", "Active(2)", "Blacklisted", "NA", "Passive", "Rejected")
  )

  leafletMap <- leaflet(data=plot$data) %>%
    addProviderTiles(
      "Esri.WorldStreetMap",
      options=providerTileOptions(opacity=0.7)
    ) %>%
    addCircleMarkers(
      lng=~longitude,
      lat=~latitude,
      popup=.getLeafletPopupContents(plot$data),
      radius=8,
      stroke=FALSE,
      fillColor=~pallete(status),
      fillOpacity=.5,
      weight=1,
      opacity=1,
      color="black",
      clusterOptions=markerClusterOptions(disableClusteringAtZoom=zoomLevel)
    ) %>%
    addLegend(
      "topright",
      pal=pallete,
      values=~status,
      opacity=1
    )
  return(leafletMap)
}

##################
# Register plots #
##################
plotRegistry$registerPlotType(
  name="Observation Usage",
  category="Maps",
  dateType="single",
  dataFieldsInRetrievedPlotData=list(
    "latitude", "longitude", "level", "statid", "active", "rejected",
    "passive", "blacklisted", "anflag", "obsvalue"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  plottingFunction=.mapUsagePlottingFunction,
  leafletPlottingFunction=.mapUsageLeafletPlottingFunction,
  dataPostProcessingFunction = function(data) {
    status <- rep("NA", nrow(data))
    status <- ifelse(data$anflag == 0, "Rejected", status)
    status <- ifelse(data$active  > 0, "Active", status)
    status <- ifelse(data$rejected > 0, "Rejected", status)
    status <- ifelse(data$passive > 0, "Passive", status)
    status <- ifelse(data$blacklisted > 0, "Blacklisted", status)
    status <- ifelse(data$anflag  > 0, "Active(2)", status)
    status <- ifelse(data$anflag == 4, "Rejected", status)
    status <- ifelse(data$anflag == 8, "Blacklisted", status)
    data$status <- status
    data <- data[rev(order(data$status)),]
    return(data)
  }
)

plotRegistry$registerPlotType(
  name="First Guess Departure Map",
  category="Maps",
  dateType="single",
  dataFieldsInRetrievedPlotData=list(
    "latitude", "longitude", "level", "statid", "obsvalue", "fg_dep"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  plottingFunction=.mapThresholdPlottingFunction,
  leafletPlottingFunction=.mapThresholdLeafletPlottingFunction,
  dataPostProcessingFunction = function(data) {
    comment(data) <- c(dataColumn="fg_dep")
    return(data)
  }
)

plotRegistry$registerPlotType(
  name="First Guess Departure + Bias Correction Map",
  category="Maps",
  dateType="single",
  dataFieldsInRetrievedPlotData=list(
    "latitude", "longitude", "level", "statid", "fg_dep", "biascrl"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  plottingFunction=.mapThresholdPlottingFunction,
  leafletPlottingFunction=.mapThresholdLeafletPlottingFunction,
  dataPostProcessingFunction = function(data) {
    data[["fg_dep+biascrl"]] <- data$fg_dep + data$biascrl
    comment(data) <- c(dataColumn="fg_dep+biascrl")
    return(data)
  }
)

plotRegistry$registerPlotType(
  name="Analysis Departure Map",
  category="Maps",
  dateType="single",
  dataFieldsInRetrievedPlotData=list(
    "latitude", "longitude", "level", "statid", "obsvalue", "an_dep"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  plottingFunction=.mapThresholdPlottingFunction,
  leafletPlottingFunction=.mapThresholdLeafletPlottingFunction,
  dataPostProcessingFunction = function(data) {
    comment(data) <- c(dataColumn="an_dep")
    return(data)
  }
)

plotRegistry$registerPlotType(
  name="Analysis Increment Map",
  category="Maps",
  dateType="single",
  dataFieldsInRetrievedPlotData=list(
    "latitude", "longitude", "level", "statid", "obsvalue", "fg_dep", "an_dep"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  plottingFunction=.mapThresholdPlottingFunction,
  leafletPlottingFunction=.mapThresholdLeafletPlottingFunction,
  dataPostProcessingFunction = function(data) {
    data[["fg_dep-an_dep"]] <- data$fg_dep - data$an_dep
    comment(data) <- c(dataColumn="fg_dep-an_dep")
    return(data)
  }
)

plotRegistry$registerPlotType(
  name="Bias Correction Map",
  category="Maps",
  dateType="single",
  dataFieldsInRetrievedPlotData=list(
    "latitude", "longitude", "level", "statid", "biascrl"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  plottingFunction=.mapThresholdPlottingFunction,
  leafletPlottingFunction=.mapThresholdLeafletPlottingFunction,
  dataPostProcessingFunction = function(data) {
    comment(data) <- c(dataColumn="biascrl")
    return(data)
  }
)

plotRegistry$registerPlotType(
  name="Observations Map",
  category="Maps",
  dateType="single",
  dataFieldsInRetrievedPlotData=list(
    "latitude", "longitude", "level", "statid", "obsvalue"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  plottingFunction=.mapThresholdPlottingFunction,
  leafletPlottingFunction=.mapThresholdLeafletPlottingFunction,
  dataPostProcessingFunction = function(data) {
    comment(data) <- c(dataColumn="obsvalue")
    return(data)
  }
)

plotRegistry$registerPlotType(
  name="First Guess Map",
  category="Maps",
  dateType="single",
  dataFieldsInRetrievedPlotData=list(
    "latitude", "longitude", "level", "statid", "obsvalue", "fg_dep"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname"),
  plottingFunction=.mapThresholdPlottingFunction,
  leafletPlottingFunction=.mapThresholdLeafletPlottingFunction,
  dataPostProcessingFunction = function(data) {
    data[["obsvalue-fg_dep"]] <- data$obsvalue - data$fg_dep
    comment(data) <- c(dataColumn="obsvalue-fg_dep")
    return(data)
  }
)

#################################################
# Register plots of the "Average Maps" category #
#################################################
# These are similar to the ones defined above, except that they take in a date
# range and support the selection of multiple cycles. The plotted values will
# represent an average of the data presented in the the corresponding
# non-average plots over the selected dates and cycles.
for(templatePlotType in plotRegistry$plotTypes) {
  if(templatePlotType$category != "Maps") next
  if(templatePlotType$name == "Observation Usage") next
  avgMapPlotType <- templatePlotType$copy()
  avgMapPlotType$name <- paste("Average", templatePlotType$name)
  avgMapPlotType$category <- paste("Average", templatePlotType$category)
  avgMapPlotType$dateType <- "range"

  # Defining a generating function for the data post-processing function.
  # This is important because of lazy-evaluation: otherwise, the last
  # templatePlotType in the loop will be used in all data post-processing
  # functions defined in the loop.
  gendataPostProcessingFunction <- function(templatePT=templatePlotType) {
    force(templatePT) # Important: Force the evaluation of the func arg.
    function(data) {
      # Calculate averages as a post-process step upon the queried data
      data <- templatePT$dataPostProcessingFunction(data)
      originalDataComments <- comment(data)

      groupByCols = intersect(
        c("statid", "latitude", "longitude", "level"), colnames(data)
      )
      nonNumericCols <- names(which(sapply(data, is.numeric)==FALSE))
      colsToDrop <- c("DTG", setdiff(nonNumericCols, groupByCols))
      data <- data[!(colnames(data) %in% colsToDrop)]

      data <- data %>%
        group_by(!!!syms(groupByCols)) %>%
        summarize_all(mean)
      comment(data) <- originalDataComments
      return(data)
    }
  }
  avgMapPlotType$dataPostProcessingFunction <- gendataPostProcessingFunction()
  plotRegistry$registerPlotType(avgMapPlotType)
}
