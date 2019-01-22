
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
        plotOutput(plotOutputId) %>% withSpinner(color="#0dc5c1"),
        tags$style(type="text/css", "body { overflow-y: scroll; }")
      )
    )
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
        leafletOutput(outputId=mapOutputId, height="auto", width="auto")
          %>% withSpinner(color="#0dc5c1"),
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
        dataTableOutput(dataTableOutputId) %>% withSpinner(color="#0dc5c1")
      )
    )
  )
}

source("src/ui/create_main_panel.R")
source("src/ui/tabs/main_tab.R")
source("src/ui/tabs/quick_plots_tab.R")

