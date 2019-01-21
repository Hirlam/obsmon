
createMainPanel <- function(prependToIds=NULL) {
  if(is.null(prependToIds)) {
    tabsetPanelId <- "mainArea"
    uiOutputIdForPlot <- "plotContainer"
    mapTitleOutputId <- "mapTitle"
    mapOutputId <- "map"
    queryUsedOutputId <- "queryUsed"
    dataTableOutputId <- "dataTable"
  } else {
    tabsetPanelId <- paste0(prependToIds, "MainArea")
    uiOutputIdForPlot <- paste0(prependToIds, "PlotContainer")
    mapTitleOutputId <- paste0(prependToIds, "MapTitle")
    mapOutputId <- paste0(prependToIds, "Map")
    queryUsedOutputId <- paste0(prependToIds, "QueryUsed")
    dataTableOutputId <- paste0(prependToIds, "DataTable")
  }

  rtn <- mainPanel(
    width=9,
    tabsetPanel(
      id=tabsetPanelId,
      tabPanel("Plot", value="plotTab",
        fluidRow(
          column(12, align="center",
            tags$head(tags$style("#plot{height:80vh !important;}")),
            uiOutput(uiOutputIdForPlot),
            tags$style(type="text/css", "body { overflow-y: scroll; }")
          )
        )
      ),
      tabPanel("Map", value="mapTab",
        fluidRow(
          column(12,
            textOutput(mapTitleOutputId),
            tags$style(type="text/css", ".shiny-text-output { text-align: center; }")
          )
        ),
        fluidRow(
          column(12, align="center",
            tags$head(tags$style("#map{height:80vh !important;}")),
            leafletOutput(outputId=mapOutputId, height="auto", width="auto")
              %>% withSpinner(color="#0dc5c1"),
            tags$style(type="text/css", "body { overflow-y: scroll; }")
          )
        )
      ),
      tabPanel(value="dataTab", "Query and data",
        wellPanel(h5("Query used:"), textOutput(queryUsedOutputId)),
        h5("Data:"),
        dataTableOutput(dataTableOutputId) %>% withSpinner(color="#0dc5c1")
      )
    )
  )
  return(rtn)
}
