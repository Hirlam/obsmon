createMainPanel <- function(prependToIds=NULL) {
  if(is.null(prependToIds)) {
    tabsetPanelId <- "mainArea"
    tabPanelValueMapTab <- "mapTab"
    tabPanelValuePlotTab <- "plotTab"
    tabPanelValuePlotlyTab <- "plotlyTab"
    tabPanelValueDataTab <- "dataTab"
    tabPanelValueQueryAndRawDataTab <- "queryAndRawDataTab"
    uiOutputIdForPlot <- "plotContainer"
    uiOutputIdForPlotly <- "plotlyContainer"
    uiOutputIdForPlotDataTable <- "plotDataTableContainer"
    uiOutputIdForQueryAndRawDataTable <- "queryAndRawDataTableContainer"
    uiOutputIdForMapAndTitle <- "mapAndMapTitleContainer"
  } else {
    tabsetPanelId <- paste0(prependToIds, "MainArea")
    tabPanelValueMapTab <- paste0(prependToIds, "MapTab")
    tabPanelValuePlotTab <- paste0(prependToIds, "PlotTab")
    tabPanelValuePlotlyTab <- paste0(prependToIds, "PlotlyTab")
    tabPanelValueDataTab <- paste0(prependToIds, "DataTab")
    tabPanelValueQueryAndRawDataTab <- paste0(prependToIds, "QueryAndRawDataTab")
    uiOutputIdForPlot <- paste0(prependToIds, "PlotContainer")
    uiOutputIdForPlotly <- paste0(prependToIds, "PlotlyContainer")
    uiOutputIdForPlotDataTable <- paste0(prependToIds,"PlotDataTableContainer")
    uiOutputIdForQueryAndRawDataTable <- paste0(prependToIds,"QueryAndRawDataTableContainer")
    uiOutputIdForMapAndTitle <- paste0(prependToIds, "MapAndMapTitleContainer")
  }

  rtn <- mainPanel(
    width=9,
    tabsetPanel(
      id=tabsetPanelId,
      tabPanel("Plot", value=tabPanelValuePlotTab,
        uiOutput(uiOutputIdForPlot) %>% withSpinner(color="#0dc5c1")
      ),
      tabPanel("Plot", value=tabPanelValuePlotlyTab,
        div(
          style="display:flex; align-items:center; justify-items: center;",
          div(
            style="flex-grow:1; overflow:auto;",
            uiOutput(uiOutputIdForPlotly) %>% withSpinner(color="#0dc5c1")
          ),
          div(
            style="float:right;",
            uiOutput(paste0(tabsetPanelId, "PlotEditingOptions"))
          )
        )
      ),
      tabPanel("Map", value=tabPanelValueMapTab,
        uiOutput(uiOutputIdForMapAndTitle)
      ),
      tabPanel("Data", value=tabPanelValueDataTab,
        uiOutput(uiOutputIdForPlotDataTable)
      ),
      tabPanel("Query & Raw Data", value=tabPanelValueQueryAndRawDataTab,
        uiOutput(uiOutputIdForQueryAndRawDataTable)
      )
    )
  )
  return(rtn)
}
