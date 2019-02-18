
createMainPanel <- function(prependToIds=NULL) {
  if(is.null(prependToIds)) {
    tabsetPanelId <- "mainArea"
    tabPanelValueMapTab <- "mapTab"
    tabPanelValuePlotTab <- "plotTab"
    tabPanelValueDataTab <- "dataTab"
    uiOutputIdForPlot <- "plotContainer"
    uiOutputIdForQueryAndTable <- "queryAndTableContainer"
    uiOutputIdForMapAndTitle <- "mapAndMapTitleContainer"
  } else {
    tabsetPanelId <- paste0(prependToIds, "MainArea")
    tabPanelValueMapTab <- paste0(prependToIds, "MapTab")
    tabPanelValuePlotTab <- paste0(prependToIds, "PlotTab")
    tabPanelValueDataTab <- paste0(prependToIds, "DataTab")
    uiOutputIdForPlot <- paste0(prependToIds, "PlotContainer")
    uiOutputIdForQueryAndTable <- paste0(prependToIds,"QueryAndTableContainer")
    uiOutputIdForMapAndTitle <- paste0(prependToIds, "MapAndMapTitleContainer")
  }

  rtn <- mainPanel(
    width=9,
    tabsetPanel(
      id=tabsetPanelId,
      tabPanel("Plot", value=tabPanelValuePlotTab,
        uiOutput(uiOutputIdForPlot)
      ),
      tabPanel("Map", value=tabPanelValueMapTab,
        uiOutput(uiOutputIdForMapAndTitle)
      ),
      tabPanel("Query and data", value=tabPanelValueDataTab,
        uiOutput(uiOutputIdForQueryAndTable)
      )
    )
  )
  return(rtn)
}
