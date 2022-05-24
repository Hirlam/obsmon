##################################
# helpers for making ggplot maps #
##################################
domainProj2ggplotProj <- function(domain) {
  rtn <- list(
    name="stereographic",
    params=NULL
  )
  if(!domain$grid$hasPoints) return(rtn)

  rtn$name <- switch(domain$proj$name,
    merc="mercator",
    stere="stereographic",
    lcc="lambert",
    projname
  )

  rtn$params <- switch(domain$proj$name,
    lcc=list(lat0=domain$proj$lat0, lat1=domain$proj$lat0),
    NULL
  )

  return(rtn)
}

.getStaticGenericMapPlot <- function(plot) {
  domain <- plot$modelDomain
  if(!domain$grid$hasPoints) {
    x1 <- min(plot$data$longitude) - 2
    x2 <- max(plot$data$longitude) + 2
    y1 <- min(plot$data$latitude) - 2
    y2 <- max(plot$data$latitude) + 2
  } else {
    x1 <- domain$ezone_minlon - 2
    x2 <- domain$ezone_maxlon + 2
    y1 <- domain$ezone_minlat - 2
    y2 <- domain$ezone_maxlat + 2
  }

  projParams <- domainProj2ggplotProj(domain)
  ggplotMap <- ggplot() +
    geom_path(
      data=map_data(map="world"),
      aes(long, lat, group=group),
      colour="gray50"
    ) +
    coord_map(
      projection=projParams$name,
      parameters=projParams$params,
      xlim=c(x1, x2),
      ylim=c(y1, y2)
    ) +
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
        "Active"=21, "Active(2)"=21,
        "Rejected"=22, "Passive"=23,
        "Blacklisted"=24, "NA"=13,
        "Rejected in TITAN"=22, "Domain"=22,
        "TITAN: blacklisted"=22, "TITAN: nometa"=22,
        "TITAN: plaus"=22, "TITAN: clim"=22,
        "TITAN: buddy"=22, "TITAN: SCT"=22,
        "TITAN: DEM"=22, "TITAN: isol"=22,
        "TITAN: fg"=22, "Fraction"=22
      )
    ) +
    scale_fill_manual(
      name="Legend",
      values=c(
        "Active"="green", "Active(2)"="blue",
        "Rejected"="red", "Passive"="magenta3",
        "Blacklisted"="black", "NA"=NA,
        "Rejected in TITAN"="purple", "Domain"="cadetblue",
        "TITAN: blacklisted"="black", "TITAN: nometa"="lightblue",
        "TITAN: plaus"="lightgreen", "TITAN: clim"="darkblue",
        "TITAN: buddy"="lightgray", "TITAN: SCT"="darkred",
        "TITAN: DEM"="lightgray", "TITAN: isol"="blue",
        "TITAN: fg"="orange", "Fraction"="beige"
      )
    ) +
    scale_colour_manual(
      name="Legend",
      values=c(
        "Active"="green", "Active(2)"="blue",
        "Rejected"="red", "Passive"="black",
        "Blacklisted"="black", "NA"="grey",
        "Rejected in TITAN"="purple", "Domain"="cadetblue",
        "TITAN: blacklisted"="black", "TITAN: nometa"="lightblue",
        "TITAN: plaus"="lightgreen", "TITAN: clim"="darkblue",
        "TITAN: buddy"="lightgray", "TITAN: SCT"="darkred",
        "TITAN: DEM"="lightgray", "TITAN: isol"="blue",
        "TITAN: fg"="orange", "Fraction"="beige"
      )
    )

  return(ggplotMap)
}

.mapThresholdStaticPlottingFunction <- function(plot) {
  domain <- plot$modelDomain
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
domainProj2plotlyProj <- function(domain) {
  rtn <- list(
    name="stereographic",
    params=NULL
  )
  if(!domain$grid$hasPoints) return(rtn)

  rtn$name <- switch(domain$proj$name,
    merc="mercator",
    stere="stereographic",
    lcc="conic conformal",
    projname
  )

  rtn$params <- switch(domain$proj$name,
    lcc=list(lat0=domain$proj$lat0, lat1=domain$proj$lat0),
    NULL
  )

  return(rtn)
}

drawBoundaries <- function(
    fig,
    domain,
    name="Boundaries",
    corners=NULL,
    showlegend=TRUE,
    legendgroup=NULL,
    ...
) {
    # Add to fig line segments connecting the given corners within the domain.
    # Args:
    #     fig (go.Scattergeo): Figure where the boundaries are to be drawn.
    #     domain (netatmoqc.domains.Domain): Model domain.
    #     name (str): Name of the boundaries (shown in legend).
    #         (Default value = "boundaries").
    #     corners: (x, y) coords of he start and end of the corners of the closed
    #         boundary to be drawn. If corners is not passed, then the domain
    #         corners will be used. (Default value = None)
    #     showlegend (bool): (Default value = True)
    #     legendgroup (str): Passed to go.Scattergeo. (Default value = None).
    #     ...: Passed to the "lines" opt of go.Scattergeo.
    #
    # Returns:
    #     go.Scattergeo: Input figure with the passed boundaries drawn.

    if(is.null(corners)) corners <- domain$grid$corners

    # Construct line segments
    # We interpolate using (x, y) instead of (lon, lat) to prevent distorted
    # line segments (segments that do not conform to the used projection).
    segments <- list()
    npts_per_segment <- max(max(domain$grid$nlon, domain$grid$nlat) %/% 100, 5)
    for(istart in seq_along(corners)) {
      start <- corners[[istart]]
      end <- corners[[istart %% length(corners) + 1]]
      segments[[length(segments) + 1]] <- list(
        x=seq(start[1], end[1], length.out=npts_per_segment),
        y=seq(start[2], end[2], length.out=npts_per_segment)
      )
    }

    xvals <- double(3 * length(segments) * npts_per_segment)
    yvals <- double(3 * length(segments) * npts_per_segment)

    ######################################
    # Add all segments as a single trace #
    ######################################

    # Put start points in go.Scattergeo's "single-trace" style
    every3rdIndex <- seq.int(1, length(xvals), 3)
    xvals[every3rdIndex] <- unlist(sapply(segments, function(item) item[1]))
    yvals[every3rdIndex] <- unlist(sapply(segments, function(item) item[2]))
    lonlatData <- domain$proj$xy2lonlat(xvals, yvals)
    lats <- as.vector(lonlatData$lat)
    lons <- as.vector(lonlatData$lon)

    # Same for end points
    # Rolling every third lat/lon
    roll <- function(vec) return(c(vec[2:length(vec)], vec[1]))
    every3rdIndexFromTwo <- seq.int(2, length(xvals), 3)
    lats[every3rdIndexFromTwo] <- roll(lats[every3rdIndex])
    lons[every3rdIndexFromTwo] <- roll(lons[every3rdIndex])

    # Indicate separation between traces
    every3rdIndexFromThree <- seq.int(3, length(xvals), 3)
    lats[every3rdIndexFromThree] <- NA
    lons[every3rdIndexFromThree] <- NA

    fig <- fig %>% add_trace(type="scattergeo", inherit=FALSE,
      name=name,
      lat=lats,
      lon=lons,
      mode="lines",
      line=...,
      legendgroup=legendgroup,
      showlegend=showlegend
    )

    return(fig)
}

drawDomain <- function(plot, domain) {
  if(!domain$grid$hasPoints) return(plot)

  if(domain$ezone_ngrid > 0) {
    plot <- plot %>%
      drawBoundaries(
        domain=domain,
        name="Extension Zone",
        corners=domain$grid$ezone_corners,
        color=I("blue")
      )
  }

  plot <- plot %>%
    drawBoundaries(domain=domain, name="Domain", color=I("red"))

  return(plot)
}

drawGriddedScattergeoTrace <- function(
  fig, data, dataColumnName, cm, domain
) {
  data <- na.omit(data.frame(
    i=data$grid_i,
    j=data$grid_j,
    n_obs=data$n_obs,
    value=data[[dataColumnName]]
  ))

  # Corners of the polygon representing the grid elements
  data$corner.1 <- domain$grid$grid2lonlat(data$i, data$j)
  data$corner.2 <- domain$grid$grid2lonlat(data$i + 1, data$j)
  data$corner.3 <- domain$grid$grid2lonlat(data$i + 1, data$j + 1)
  data$corner.4 <- domain$grid$grid2lonlat(data$i, data$j + 1)

  dataPal <- colorNumeric(palette=cm$palette, domain=cm$domain)
  data$color <- dataPal(data$value)

  # It is in principle possible to define all polygons with a single call to
  # add_trace, by adding all lon, lats & values to vectors with NA vals placed
  # between the data for each polygon. However, there seems to be a bug in
  # plotly that causes the ends of some of the polygons generated with that
  # method to be connected in the wrong way when using 'fill="toself"'. Adding
  # every polygon separately in a loop, like we're doing below, solves this.
  # Probably related to <https://github.com/plotly/plotly.js/issues/2845>.
  for(irow in 1:nrow(data)) {
    row <- data[irow, ]
    fig <- fig %>%
      add_trace(type="scattergeo", inherit=FALSE,
        lon=c(
          row$corner.1$lon,
          row$corner.4$lon,
          row$corner.3$lon,
          row$corner.2$lon,
          row$corner.1$lon
        ),
        lat=c(
          row$corner.1$lat,
          row$corner.4$lat,
          row$corner.3$lat,
          row$corner.2$lat,
          row$corner.1$lat
        ),
        customdata=list(value=row$value),
        fill="toself",
        fillcolor=row$color,
        opacity=0.75,
        mode="none",
        text=gsub(
          "\\w*:[[:space:]]*<br />", "",
          gsub(
            "(<br />){2,}", "<br />",
            paste(
              sprintf("Grid: (%d, %d)", row$i, row$j),
              sprintf("Coords: (%.3f\u00B0, %.3f\u00B0)", row$corner.1$lon, row$corner.1$lat),
              sprintf("n_obs: %d", row$n_obs),
              paste("Value:", row$value),
              sep="<br />"
            )
          )
        ),
        hoverinfo="text",
        # Configure traces so that a single legend item toggles all polygons
        name="Grid elements",
        legendgroup="Grid Elements",
        showlegend=FALSE
      )
  }

  return(fig)
}

.getInteractiveGenericMapPlot <- function(plot) {
  domain <- plot$modelDomain
  if(domain$grid$hasPoints) {
    rangeLat <- c(domain$ezone_minlat, domain$ezone_maxlat)
    rangeLon <- c(domain$ezone_minlon, domain$ezone_maxlon)
    map_center <- list(lon=domain$center_lonlat[1], lat=domain$center_lonlat[2])
  } else {
    rangeLat <- range(plot$data$latitude)
    rangeLon <- range(plot$data$longitude)
    map_center <- list(lon=mean(rangeLon), lat=mean(rangeLat))
  }
  rangeLat <- rangeLat + c(-1, 1)
  rangeLon <- rangeLon + c(-1, 1)

  projParams <- domainProj2plotlyProj(domain=domain)
  myPlotly <- plot_geo(
    data=plot$data,
    lat=~jitter(latitude, amount=1E-6),
    lon =~jitter(longitude, amount=1E-6)
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
        projection = list(
          type=projParams$name,
          parallels=projParams$params
        ),
        coastlinewidth = 0.5,
        countrywidth = 0.5,
        lataxis = list(
          range = rangeLat,
          showgrid = TRUE,
          dtick = 10
        ),
        lonaxis = list(
          range = rangeLon,
          showgrid = TRUE,
          dtick = 15
        ),
        center = map_center
      )
    )
  return(myPlotly)
}

.mapUsageInteractivePlottingFunction <- function(plot) {
  colors <- c(
    "Active"="green", "Active(2)"="green",
    "Rejected"="red", "Passive"="magenta3",
    "Blacklisted"="black", "NA"="grey",
    "Rejected in TITAN"="purple", "Domain"="cadetblue",
    "TITAN: blacklisted"="black", "TITAN: nometa"="lightblue",
    "TITAN: plaus"="lightgreen", "TITAN: clim"="darkblue",
    "TITAN: buddy"="lightgray", "TITAN: SCT"="darkred",
    "TITAN: DEM"="lightgray", "TITAN: isol"="blue",
    "TITAN: fg"="orange", "Fraction"="beige"
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
            if("obsvalue" %in% names(plot$data)) paste("Obsvalue:", signif(obsvalue, digits=5)),
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


.mapThresholdInteractivePlotAddMarkers <- function(
  plotlyMap, plot, dataColumnName, cm, visible=TRUE
) {
  plotlyMap <- plotlyMap %>%
    add_markers(
      name="Data Markers",
      text=~gsub(
        "\\w*:[[:space:]]*<br />", "",
        gsub(
          "(<br />){2,}", "<br />",
          paste(
	    if("statid" %in% names(plot$data)) paste("Station:", statid),
	    if("level" %in% names(plot$data)) paste("Level:", level),
	    if("channel" %in% names(plot$data)) paste("Channel:", channel),
            sprintf("Coords: (%.3f\u00B0, %.3f\u00B0)", longitude, latitude),
            if("n_obs" %in% names(plot$data)) paste("n_obs:", n_obs),
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
      visible=visible,
      showlegend=FALSE,
      hoverinfo="text"
    )

  return(plotlyMap)
}

.mapThresholdInteractivePlottingFunction <- function(plot) {
  plotlyMap <- .getInteractiveGenericMapPlot(plot)

  dataColumnName <- unname(attributes(plot$data)$comment["dataColumn"])

  cm <- .getSuitableColorScale(plot$data)

  isGridAveraged <- "grid_i" %in% colnames(plot$data)
  plotlyMap <- plotlyMap %>%
    # Draw markers even if using gridded data. This adds the colorscale.
    .mapThresholdInteractivePlotAddMarkers(
      plot, dataColumnName, cm,  visible=!isGridAveraged
    ) %>% {
      if(isGridAveraged) {
        drawGriddedScattergeoTrace(., plot$data, dataColumnName, cm, domain=plot$modelDomain)
      } else {
        .
      }
    } %>%
    colorbar(
      limits=cm$domain,
      title=dataColumnName,
      yanchor="center", y=0.5,
      xanchor="left", x=1.0
    ) %>%
    # Prevent overlap between colorbar and eventual extra legend
    layout(
      legend = list(
        orientation="h", xanchor="center", x=0.5, yanchor="bottom", y=-0.01
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
    return(.mapUsageInteractivePlottingFunction(plot) %>% drawDomain(plot$modelDomain))
  } else {
    return(.mapUsageStaticPlottingFunction(plot))
  }
}

.mapThresholdPlottingFunction <- function(plot) {
  if(nrow(plot$data)==0) return(errorPlot("No data to plot."))
  if(plot$parentType$interactive) {
    return(.mapThresholdInteractivePlottingFunction(plot) %>% drawDomain(plot$modelDomain))
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

  colors <- c(
    "Active"="green", "Active(2)"="green",
    "Rejected"="red", "Passive"="magenta3",
    "Blacklisted"="black", "NA"="grey",
    "Rejected in TITAN"="purple", "Domain"="cadetblue",
    "TITAN: blacklisted"="black", "TITAN: nometa"="lightblue",
    "TITAN: plaus"="lightgreen", "TITAN: clim"="darkblue",
    "TITAN: buddy"="lightgray", "TITAN: SCT"="darkred",
    "TITAN: DEM"="lightgray", "TITAN: isol"="blue",
    "TITAN: fg"="orange", "Fraction"="beige"
  )
  domains <- c(
    "Active", "Active(2)", "Blacklisted", "NA",   "Rejected", "Passive",
    "Rejected in TITAN", "Domain",    "TITAN: blacklisted", "TITAN: nometa",
    "TITAN: plaus", "TITAN: clim", "TITAN: buddy", "TITAN: SCT", "TITAN: DEM",
    "TITAN: isol", "TITAN: fg", "Fraction"
  )

  get_color <- function(stat) return(colors[[stat]])
  color <- unname(sapply(plot$data$status, get_color))

  used_colors <- unique(color)
  used_labels <- unique(plot$data$status)
  obsvalue <- signif(plot$data$obsvalue, digits=5)

  myIcons <- makeAwesomeIcon(
    icon="home", library="ion",
    markerColor=color, iconColor="white", spin=FALSE,
    extraClasses=NULL, squareMarker=FALSE, iconRotate=0,
    fontFamily="monospace", text=as.character(obsvalue)
  )

  leafletMap <- leaflet(data=plot$data) %>%
    addProviderTiles(
      "Esri.WorldStreetMap",
      options=providerTileOptions(opacity=0.5)
    ) %>%
    addAwesomeMarkers(
      lng=~longitude,
      lat=~latitude,
      popup=.getLeafletPopupContents(plot$data),
      clusterOptions=markerClusterOptions(disableClusteringAtZoom=zoomLevel),
      icon=myIcons
    ) %>%
    addLegend(
      "topright",
      opacity=1,
      colors=used_colors,
      labels=used_labels
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
  dataPostProcessingFunction=fillDataWithQualityControlStatus
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
  dataPostProcessingFunction = function(data, ...) {
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
  dataPostProcessingFunction = function(data, ...) {
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
  dataPostProcessingFunction = function(data, ...) {
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
  dataPostProcessingFunction = function(data, ...) {
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
  dataPostProcessingFunction = function(data, ...) {
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
  dataPostProcessingFunction = function(data, ...) {
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
  dataPostProcessingFunction = function(data, ...) {
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
    function(data, obsmonPlotObj) {
      # Calculate averages as a post-process step upon the queried data
      data <- templatePT$dataPostProcessingFunction(data, obsmonPlotObj=obsmonPlotObj)
      domain <- obsmonPlotObj$modelDomain
      originalDataComments <- comment(data)

      performGridAverage <- domain$grid$hasPoints
      if(performGridAverage) {
        ijGridData <- domain$grid$lonlat2grid(
          lon=data$longitude,
          lat=data$latitude
        )
        data$grid_i <- ijGridData$i
        data$grid_j <- ijGridData$j

        data <- subset(data, select = -c(statid, level, latitude, longitude))
        groupByCols = intersect(
          c("grid_i", "grid_j", "level"), colnames(data)
        )
      } else {
        groupByCols = intersect(
          c("statid", "latitude", "longitude", "level"), colnames(data)
        )
      }

      nonNumericCols <- names(which(sapply(data, is.numeric)==FALSE))
      colsToDrop <- c("DTG", setdiff(nonNumericCols, groupByCols))
      data <- data[!(colnames(data) %in% colsToDrop)]

      data <- data %>% group_by(!!!syms(groupByCols))
      if(performGridAverage) {
        min_nObs <- obsmonPlotObj$paramsAsInUiInput$minNobsForGriddedAverages
        min_nObs <- max(c(min_nObs, 1))
        data <- data %>% add_tally(name="n_obs") %>% filter(n_obs >= min_nObs)
      }
      data <- data %>% summarize_all(mean)

      if(performGridAverage) {
        lonlatData <- domain$grid$grid2lonlat(i=data$grid_i, j=data$grid_j)
        data$longitude <- lonlatData$lon
        data$latitude <- lonlatData$lat
      }

      columnOrder <- intersect(
        c("grid_i", "grid_j", "longitude", "latitude", "n_obs", colnames(data)),
        colnames(data)
      )
      data <- data[, columnOrder]
      comment(data) <- originalDataComments

      return(data)
    }
  }
  avgMapPlotType$dataPostProcessingFunction <- gendataPostProcessingFunction()
  plotRegistry$registerPlotType(avgMapPlotType)
}
