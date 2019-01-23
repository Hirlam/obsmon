
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

quickPlotsGenId <- function(iPlot, type=NULL) {
  # Generate output IDs for the dinamically generated outpus
  qpName <- sprintf("quickPlot_%d", iPlot)
  if(is.null(type)) return(qpName)
  recogOutTypes <- c("plot", "map", "mapTitle", "queryUsed", "dataTable")
  if(!(type %in%  recogOutTypes)) {
    stop(sprintf(
      "quickPlotsGenId: Choose type from: %s",
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
        tags$head(tags$style("#plot{height:80vh !important;}")),
        plotOutput(plotOutputId),
        tags$style(type="text/css", "body { overflow-y: scroll; }")
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
        leafletOutput(outputId=mapOutputId, height="auto", width="auto"),
        tags$style(type="text/css", "body { overflow-y: scroll; }")
      )
    )
  )
}

queryUsedAndDataTableOutput <- function(queryUsedOutputId, dataTableOutputId){
  fluidPage(
    fluidRow(
      column(12, align="center",
        wellPanel(h5("Query used:"), textOutput(queryUsedOutputId)),
        h5("Data:"),
        dataTableOutput(dataTableOutputId)
      )
    )
  )
}

source("src/ui/create_main_panel.R")
source("src/ui/tabs/main_tab.R")
source("src/ui/tabs/quick_plots_tab.R")

