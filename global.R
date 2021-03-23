# This file is used by shiny. Whatever is put in here becomes visible
# to both ui.R and server.R
if(!exists("initFileSourced")) source("src/init.R")

# Code to enable timing sessions out
# Adapted from
# <https://stackoverflow.com/questions/33839543/shiny-server-session-time-out-doesnt-work>
timeoutSeconds <- abs(obsmonConfig$general$sessionTimeout)
timeoutWarnInSec <- 60 # Warn users at least 60s prior to scheduled timeout
inactivity <- sprintf("function idleTimer() {
  var tInSec = %s;
  var t = setTimeout(logout, 1000*tInSec);
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  //catches keyboard actions

  function logout() {
    Shiny.setInputValue('timeOut', tInSec)
  }

  function resetTimer() {
    clearTimeout(t);
    Shiny.setInputValue('timeoutTimerReset', 'true', {priority: 'event'});
    t = setTimeout(logout, 1000*tInSec);  // time is in milliseconds;
  }
}
idleTimer();",
  timeoutSeconds
)


defaultMenuLabels <- list(
  experiment="Experiment",
  odbBase="Data Assimilation Category/Database",
  obtype="Observation Type",
  obname="Observation Name",
  sensor="Sensor",
  satellite="Satellite",
  scatt_satellite="Satellite",
  channels="Channels",
  variable="Variable",
  levels=tags$div(
    "Levels", "(Select", actionLink("levelsSelectStandard", "standard)")
  ),
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
        plotOutput(plotOutputId, height="755px", width="auto")
      )
    ) %>% withSpinner(color="#0dc5c1")
  )
}

plotlyOutputInsideFluidRow <- function(plotlyOutputId) {
  fluidPage(
    fluidRow(
      column(12, align="center",
        plotlyOutput(plotlyOutputId, height="755px", width="auto")
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

