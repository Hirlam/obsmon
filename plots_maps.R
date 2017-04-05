registerPlotCategory("Maps")

doPlot.plotMap <- function(p, plotRequest, plotData) {
  dtg <- formatDtg(plotRequest$criteria$dtg)
  titleStub <- sprintf("%s: %s %%s %s\n%%s", plotRequest$exp$name, p$name, dtg)
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
  x1 <- min(plotData$longitude)-2
  x2 <- max(plotData$longitude)+2
  y1 <- min(plotData$latitude)-2
  y2 <- max(plotData$latitude)+2
  ggplot() +
    geom_path(data=map_data(map="world"),
              aes(long, lat, group=group),
              colour="gray50") +
    coord_map("stereographic", xlim=c(x1, x2), ylim=c(y1, y2)) +
    labs(title=title, x="lat", y="lon")
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
    geom_point(data=plotData[order(status),],
               aes(x=longitude, y=latitude, colour=status),
               size=2, alpha=.5) +
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

doPlot.mapThreshold <- function(p, plotRequest, plotData) {
  minval <- min(plotData$plotValues)
  maxval <- max(plotData$plotValues)
  if (minval*maxval >= 0) {
    type <- "seq"
    spread <- maxval - minval
    dataSign <- sign(maxval + minval)
    if (dataSign < 0) {
      palette <- "Blues"
      direction <- -1
      mincol <- minval
      snapToZero <- maxval^2 < spread
      maxcol <- ifelse(snapToZero, 0., maxval)
    } else {
      palette <- "Reds"
      direction <- 1
      maxcol <- maxval
      snapToZero <- minval^2 < spread
      mincol <- ifelse(snapToZero, 0., minval)
    }
  } else {
    type <- "div"
    palette <- "RdBu"
    direction <- -1
    col <- max(abs(minval), abs(maxval))
    mincol <- -col
    maxcol <- col
  }
  NextMethod() +
    geom_point(data=plotData,
               aes(x=longitude, y=latitude, fill=plotValues),
               size=3, shape=21, colour="gray50") +
    scale_fill_distiller(type=type, palette=palette,
                         direction=direction, limits=c(mincol, maxcol))
}

registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "First Guess Departure Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "(fg_dep) as plotValues",
                     "FROM usage WHERE %s"),
               list("obnumber", "obname", "levels"))
)
registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "First Guess Departure + Bias Correction Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "(fg_dep+biascrl) as plotValues",
                     "FROM usage WHERE %s"),
               list("obnumber"=7, "obname", "levels"))
)
registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "Analysis Departure Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "(an_dep) as plotValues",
                     "FROM usage WHERE %s"),
               list("obnumber", "obname", "levels"))
)
registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "Analysis Increment Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "(fg_dep-an_dep) as plotValues",
                     "FROM usage WHERE %s"),
               list("obnumber", "obname", "levels"))
)
registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "Bias Correction Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "(biascrl) as plotValues",
                     "FROM usage WHERE %s"),
               list("obnumber"=7, "obname", "levels"))
)
registerPlotType(
    "Maps",
    plotCreate(c("mapThreshold", "plotMap"),
               "Observations Map", "single",
               paste("SELECT",
                     "latitude, longitude, level, statid,",
                     "(obsvalue) as plotValues",
                     "FROM usage WHERE %s"),
               list("obnumber", "obname", "levels"))
)
