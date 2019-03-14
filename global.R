# This file is used by shiny. Whatever is put in here becomes visible
# to both ui.R and server.R
if(!exists("initFileSourced")) source("src/init.R")

defaultMenuLabels <- list(
  experiment="Experiment",
  odbBase="Data Assimilation Category/Database",
  obtype="Observation Type",
  obname="Observation Name",
  sensor="Sensor",
  satellite="Satellite",
  channels="Channels",
  variable="Variable",
  levels="Levels",
  plottype="Type of Plot",
  station="Station"
)

multiPlotsGenId <- function(iPlot, type=NULL) {
  # Generate output IDs for the dinamically generated outpus
  qpName <- sprintf("multiPlot_%d", iPlot)
  if(is.null(type)) return(qpName)
  recogOutTypes <- c("plot", "map", "mapTitle", "queryUsed", "dataTable")
  if(!(type %in%  recogOutTypes)) {
    stop(sprintf(
      "multiPlotsGenId: Choose type from: %s",
      paste(recogOutTypes, collapse=", ")
    ))
  }
  return(sprintf("%s_%s", qpName, type))
}

getDefLabel <- function(inputId) {
  rtn <- defaultMenuLabels[[inputId]]
  if(is.null(rtn)) rtn <- inputId
  return(rtn)
}

plotOutputInsideFluidRow <- function(plotOutputId) {
  fluidPage(
    fluidRow(
      column(12, align="center",
        plotOutput(plotOutputId, height="755px", width="100%")
      )
    ) %>% withSpinner(color="#0dc5c1")
  )
}

plotlyOutputInsideFluidRow <- function(plotlyOutputId) {
  fluidPage(
    fluidRow(
      column(12, align="center",
        plotlyOutput(plotlyOutputId, height="755px", width="100%")
      )
    ) %>% withSpinner(color="#0dc5c1")
  )
}

mapAndMapTitleOutput <- function(mapOutputId, mapTitleOutputId) {
  fluidPage(
    fluidRow(
      column(12,
        textOutput(mapTitleOutputId),
        tags$style(type="text/css", ".shiny-text-output { text-align: center; }")
      )
    ),
    fluidRow(
      column(12, align="center",
        tags$head(tags$style("#map{height:80vh !important;}")),
        leafletOutput(outputId=mapOutputId, width="auto"),
        tags$style(type="text/css", "body { overflow-y: scroll; }")
      )
    )
  )
}

queryUsedAndDataTableOutput <- function(queryUsedOutputId, dataTableOutputId){
  downloadButtonCsvId <- paste0(dataTableOutputId, "DownloadAsCsv")
  downloadButtonTxtId <- paste0(dataTableOutputId, "DownloadAsTxt")
  fluidPage(
    fluidRow(
      column(12, align="center",
        wellPanel(h5("Query used:"), textOutput(queryUsedOutputId)),
        downloadButton(downloadButtonTxtId, "Download as TXT"),
        downloadButton(downloadButtonCsvId, "Download as CSV"),
        dataTableOutput(dataTableOutputId)
      )
    )
  )
}

source("src/ui/create_main_panel.R")
source("src/ui/tabs/main_tab.R")
source("src/ui/tabs/multi_plots_tab.R")
source("src/ui/tabs/doc_tab.R")
