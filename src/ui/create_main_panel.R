
createMainPanel <- function(prependToIds=NULL) {
  if(is.null(prependToIds)) {
    tabsetPanelId <- "mainArea"
    uiOutputIdForPlot <- "plotContainer"
    uiOutputIdForQueryAndTable <- "queryAndTableContainer"
    uiOutputIdForMapAndTitle <- "mapAndMapTitleContainer"
  } else {
    tabsetPanelId <- paste0(prependToIds, "MainArea")
    uiOutputIdForPlot <- paste0(prependToIds, "PlotContainer")
    uiOutputIdForQueryAndTable <- paste0(prependToIds,"QueryAndTableContainer")
    uiOutputIdForMapAndTitle <- paste0(prependToIds, "MapAndMapTitleContainer")
  }

  rtn <- mainPanel(
    width=9,
    tabsetPanel(
      id=tabsetPanelId,
      tabPanel("Plot", value="plotTab",
        uiOutput(uiOutputIdForPlot)
      ),
      tabPanel("Map", value="mapTab",
        uiOutput(uiOutputIdForMapAndTitle)
      ),
      tabPanel(value="dataTab", "Query and data",
        uiOutput(uiOutputIdForQueryAndTable)
      )
    )
  )
  return(rtn)
}
