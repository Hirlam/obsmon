runAppHandlingBusyPort <- function(
  # Wrapper to shiny's runApp
  appDir=getwd(), defaultPort=getOption("shiny.port"),
  launch.browser=getOption("shiny.launch.browser", interactive()),
  host = getOption("shiny.host", "127.0.0.1"),
  maxNAtt=10,
  ...
) {
  port <- defaultPort
  success <- FALSE
  nAtt <- 0
  lisOnMsgStart <- 'Listening on '
  lisOnMsgMarker <- "------------------------------------"
  error_msg <- NULL
  while (!success & (nAtt<maxNAtt)) {
    tryCatch(
      {
        cat("\n")
        cat(paste(lisOnMsgMarker, "\n", sep=""))
        lisOnMsg <- paste(lisOnMsgStart,"http://",host,":",port,"\n", sep='')
        cat(lisOnMsg)
        cat(paste(lisOnMsgMarker, "\n", sep=""))
        cat("\n")

        runApp(appDir, launch.browser=launch.browser, port=port, ...)
        success <- TRUE
      },
      error=function(e) {
        error_msg <<- e
        flog.warn(
          'Failed to create server using port %s. Issued error msg: %s',
          port, e
        )
        port <<- sample(1024:65535, 1)
        lisOnMsgStart <<- "Port updated: Listening on "
        lisOnMsgMarker <<- "-------------------------------------------------"
      }
    )
    nAtt <- nAtt + 1
  }

  if(!success) {
    msg <- paste("\nFailed to create server after", nAtt, "attempts.\n")
    msg <- paste(msg, "Possible reason: ", error_msg, sep="\n")
    msg <- paste(msg, "Stopping now.", sep="\n")
    stop(msg)
  }
}

toggleTab <- function(..., condition) {
  if(isTRUE(condition)) showTab(...)
  else hideTab(...)
}

# Wrapper to shinycssloaders::withSpinner with useful defaults
withSpinnerWrapper <- function(...) {
  return(withSpinner(...,
    image=SPINNER_IMAGE_PATH,
    image.width=100,
    image.height=100
  ))
}

# Showing messages in the UI
signalError <- function(message, title="Error") {
  showModal(modalDialog(
      title=title,
      message,
      easyClose=TRUE
  ))
}
confirmationModal <- function(title, msg, inputId) {
  modalDialog(
    span(msg),
    title=title,
    footer = tagList(
      modalButton("Cancel"),
      actionButton(inputId, "Delete")
    ),
    easyClose=TRUE
  )
}
showConfirmationDialog <- function(
  title="Please confirm action",
  msg="Are you sure?",
  inputId="confirmationDialog"
) {
  showModal(confirmationModal(title, msg, inputId))
}

# Helpers to update and keep track of UI choices and seletions
getSelection <- function(session, inputId, choices, select=c("NORMAL", "ALL", "NONE")) {
  select <- match.arg(select)
  switch(select,
         "NORMAL"={
           oldSelection <- isolate(session$input[[inputId]])
           validChoices <- unlist(choices, use.names=FALSE)
           validSelections <- oldSelection %in% validChoices
           if (is.null(oldSelection)) {
             NULL
           } else if (any(validSelections)) {
             oldSelection[validSelections]
           } else {
             NULL
           }
         },
         "ALL"={
           choices
         },
         "NONE"={
           c()
         })
}

updateInputWrapper <- function(
  updateFunc, session, inputId, label=NULL, choices=NULL, selected=NULL, ...
){
  # Update an input using "updateFunc" while preserving the selected options(s)
  currentChoices <- session$userData$UiChoices[[inputId]]
  if(isTRUE(all.equal(choices, currentChoices))) return(NULL)
  session$userData$UiChoices[[inputId]] <- choices

  # Show/hide "sync" UI icon
  cssSelector<-sprintf('.control-label[for="%s"] .updating_info_icon', inputId)
  shinyjs::show(selector=cssSelector, anim=TRUE, animType="fade")
  on.exit(shinyjs::hide(selector=cssSelector, anim=TRUE, animType="fade"))

  if(is.null(selected)) selected <- getSelection(session, inputId, choices)
  updateFunc(
    session, inputId, label=label, choices=choices, selected=selected, ...
  )
}

updateSelectInputWrapper <- function(...) {
  updateInputWrapper(updateFunc=updateSelectInput, ...)
}

updatePickerInputWrapper <- function(...) {
  updateInputWrapper(updateFunc=updatePickerInput, ...)
}


# Enabling/disabling UI elements
grepFilter <- function(x, pattern=NULL, except=NULL) {
  rtn <- x
  if(length(pattern)>0) {
    for(patt in pattern) rtn <- grep(patt, rtn, value=TRUE)
  }
  if(length(except)>0) {
    for(patt in except) rtn <- rtn[!(rtn %in% grep(patt, rtn, value=TRUE))]
  }
  return(rtn)
}

disableShinyInputs <- function(input, pattern=NULL, except=NULL) {
  inpNames <- isolate(names(reactiveValuesToList(input)))
  inpNames <- grepFilter(inpNames, pattern, except)
  for(inp in inpNames) shinyjs::disable(inp)
  # We need the code below for pickerInputs. See
  # <https://github.com/dreamRs/shinyWidgets/issues/341>
  # <https://stackoverflow.com/a/27317528>
  shinyjs::runjs("$('.selectpicker').prop('disabled', true);")
  shinyjs::runjs("$('.selectpicker').selectpicker('refresh');")
}
enableShinyInputs <- function(input, pattern=NULL, except=NULL) {
  inpNames <- isolate(names(reactiveValuesToList(input)))
  inpNames <- grepFilter(inpNames, pattern, except)
  for(inp in inpNames) shinyjs::enable(inp)
  shinyjs::runjs("$('.selectpicker').prop('disabled', false);")
  shinyjs::runjs("$('.selectpicker').selectpicker('refresh');")
}

interactivePlotTabPanel <- function(plotOutputId) {
  tabPanel("Plot", value="plotlyTab",
    div(
      style="display:flex; align-items:center; justify-items: center;",
      div(
        style="flex-grow:1; overflow:auto;",
        plotlyOutputInsideFluidRow(plotOutputId) %>% withSpinnerWrapper()
      ),
      div(
        style="float:right;",
        uiOutput(paste0(plotOutputId, "PlotEditingOptions"))
      )
    )
  )
}

nonInteractivePlotTabPanel <- function(plotOutputId) {
  tabPanel("Plot", value="plotTab",
    plotOutputInsideFluidRow(plotOutputId) %>% withSpinnerWrapper()
  )
}

leafletMapTabPanel <- function(mapOutputId="map") {
  tabPanel("Map", value="mapTab",
    mapAndMapTitleOutput(mapOutputId, sprintf("%sTitle", mapOutputId))
  )
}

queryAndDataTabPanel <- function(idPrefix=character(0)) {
  navbarMenu(title="Query & Data",
    tabPanel(
      title="Data Used in the Plot",
      plotDataTableOutput(paste0(idPrefix, "plotDataTable"))
    ),
    tabPanel(
      title="Query & Retrieved Raw Data",
      queryUsedAndRawDataTableOutput(
        paste0(idPrefix, "queryUsed"),
        paste0(idPrefix, "rawDataTable")
      )
    )
  )
}
