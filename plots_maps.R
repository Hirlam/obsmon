registerPlotCategory("Maps")

world<-map_data(map="world")

usageMap <- function(title, plotData, mode) {
  status <- rep("NA", length(plotData$longitude))
  status <- ifelse(plotData$anflag == 0, "Rejected", status)
  status <- ifelse(plotData$active  > 0, "Active", status)
  status <- ifelse(plotData$rejected > 0, "Rejected", status)
  status <- ifelse(plotData$passive > 0, "Passive", status)
  status <- ifelse(plotData$blacklisted > 0, "Blacklisted", status)
  status <- ifelse(plotData$anflag  > 0, "Active(2)", status)
  status <- ifelse(plotData$anflag == 4, "Rejected", status)
  status <- ifelse(plotData$anflag == 8, "Blacklisted", status)
  obPlot <- NULL
  plotData$status <- status
  x1 <- min(plotData$longitude)-2
  x2 <- max(plotData$longitude)+2
  y1 <- min(plotData$latitude)-2
  y2 <- max(plotData$latitude)+2
  obPlot <- ggplot(world, aes(long, lat))
  obPlot <- obPlot + geom_path(data=world, aes (group = group), colour="black") +
    coord_map("stereographic", xlim=c(x1, x2), ylim=c(y1, y2)) +
    geom_point(data=plotData[rev(order(status)),],
               aes(x=longitude, y=latitude, colour=status),
               size=3) +
    ylab("lat") +
    xlab("lon") +
    labs(title = title) +
    scale_colour_manual(name="Legend",
                        values=c("Active"="green", "Active(2)"="blue",
                                 "Rejected"="red", "Passive"="yellow",
                                 "Blacklisted"="black", "NA"="grey"))
  return(obPlot)
}

observationUsage <- function(plotRequest) {
  dtg <- plotRequest$criteria$dtgMax
  plotRequest$criteria$dtgExact <- dtg
  plotRequest$criteria$dtgMin <- NULL
  plotRequest$criteria$dtgMax <- NULL
  query <- paste("SELECT",
                 "latitude, longitude,",
                 "active, rejected, passive, blacklisted, anflag",
                 "FROM usage WHERE",
                 buildWhereClause(plotRequest$criteria))
  plotData <- expQuery(plotRequest$exp, plotRequest$db, query, dtgs=dtg)
  title <- paste(plotRequest$exp$displayName,
                 ": Observation Usage")
  obplot <- usageMap(title, plotData, "plot")
}
registerPlotType("Maps", "Observation Usage", observationUsage)
