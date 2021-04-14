
createMainPanel <- function(prependToIds=NULL) {
  if(is.null(prependToIds)) {
    tabsetPanelId <- "mainArea"
    tabPanelValueMapTab <- "mapTab"
    tabPanelValuePlotTab <- "plotTab"
    tabPanelValuePlotlyTab <- "plotlyTab"
    tabPanelValueDataTab <- "dataTab"
    uiOutputIdForPlot <- "plotContainer"
    uiOutputIdForPlotly <- "plotlyContainer"
    uiOutputIdForQueryAndTable <- "queryAndTableContainer"
    uiOutputIdForMapAndTitle <- "mapAndMapTitleContainer"
  } else {
    tabsetPanelId <- paste0(prependToIds, "MainArea")
    tabPanelValueMapTab <- paste0(prependToIds, "MapTab")
    tabPanelValuePlotTab <- paste0(prependToIds, "PlotTab")
    tabPanelValuePlotlyTab <- paste0(prependToIds, "PlotlyTab")
    tabPanelValueDataTab <- paste0(prependToIds, "DataTab")
    uiOutputIdForPlot <- paste0(prependToIds, "PlotContainer")
    uiOutputIdForPlotly <- paste0(prependToIds, "PlotlyContainer")
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
      tabPanel("Plot", value=tabPanelValuePlotlyTab,
        uiOutput(uiOutputIdForPlotly)
      ),
      tabPanel("Map", value=tabPanelValueMapTab,
        uiOutput(uiOutputIdForMapAndTitle)
      ),
      tabPanel("Query and Data", value=tabPanelValueDataTab,
        uiOutput(uiOutputIdForQueryAndTable)
      )
    )
  )
  return(rtn)
}
