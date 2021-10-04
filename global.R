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
  odbBase="Data Assimilation Category/Database",
  obtype="Observation Type",
  obname="Observation Name",
  scatt_satellite="Satellite",
  plottype="Type of Plot",
  stationSingle="Station"
)

getDefLabel <- function(inputId) {
  rtn <- defaultMenuLabels[[inputId]]
  if(is.null(rtn)) rtn <- stringr::str_to_title(inputId)
  return(rtn)
}

plotOutputInsideFluidRow <- function(plotOutputId) {
  fluidPage(
    fluidRow(
      column(12, align="center",
        plotOutput(plotOutputId, height="755px", width="auto") %>%
          withSpinnerWrapper()
      )
    )
  )
}

plotlyOutputInsideFluidRow <- function(plotlyOutputId) {
  fluidPage(
    fluidRow(
      column(12, align="center",
        plotlyOutput(plotlyOutputId, height="755px", width="auto") %>%
          withSpinnerWrapper()
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
        leafletOutput(outputId=mapOutputId, width="auto") %>%
          withSpinnerWrapper(),
        tags$style(type="text/css", "body { overflow-y: scroll; }")
      )
    )
  )
}

queryUsedAndRawDataTableOutput <- function(queryUsedOutputId, dataTableOutputId){
  downloadButtonCsvId <- paste0(dataTableOutputId, "DownloadAsCsv")
  downloadButtonTxtId <- paste0(dataTableOutputId, "DownloadAsTxt")
  fluidPage(
    fluidRow(
      column(12, align="center",
        wellPanel(
          h5("Raw data retrived using the following query:"),
          tags$div(HTML("<b>"), textOutput(queryUsedOutputId), HTML("</b>")),
        ),
        downloadButton(downloadButtonTxtId, "Download as TXT"),
        downloadButton(downloadButtonCsvId, "Download as CSV"),
        dataTableOutput(dataTableOutputId) %>%
          withSpinnerWrapper()
      )
    )
  )
}

plotDataTableOutput <- function(dataTableOutputId){
  downloadButtonCsvId <- paste0(dataTableOutputId, "DownloadAsCsv")
  downloadButtonTxtId <- paste0(dataTableOutputId, "DownloadAsTxt")
  fluidPage(
    fluidRow(
      column(12, align="center",
        wellPanel(
          h5(
            shiny::icon("info-circle", class="query_info_icon") %>%
              bs_embed_tooltip(
                title=paste(
                  "Obtained by post-processing the raw queried data. Among",
                  "other things, (i) rows with incomplete entries are removed,",
                  "(ii) units are attached, and (iii) data columns may differ."
                ),
                trigger="hover"
              ),
            "Data used in the plot:",
          )
        ),
        downloadButton(downloadButtonTxtId, "Download as TXT"),
        downloadButton(downloadButtonCsvId, "Download as CSV"),
        dataTableOutput(dataTableOutputId) %>%
          withSpinnerWrapper()
      )
    )
  )
}

source("src/ui/tabs/main_tab.R")
source("src/ui/tabs/multi_plots_tab.R")
source("src/ui/tabs/model_domain_tab.R")
source("src/ui/tabs/doc_tab.R")

