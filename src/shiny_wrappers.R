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
      error=function(w) {
        flog.warn(paste('Failed to create server using port', port, sep=" "))
        port <<- sample(1024:65535, 1)
        lisOnMsgStart <<- "Port updated: Listening on "
        lisOnMsgMarker <<- "-------------------------------------------------"
      }
    )
    nAtt <- nAtt + 1
  }

  if(!success) {
    msg <- paste("Failed to create server after", nAtt, "attempts.\n",sep=" ")
    msg <- paste(msg, "Stopping now.\n", sep=" ")
    stop(msg)
  }
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
  updateFunc, session, inputId, label=NULL, choices=NULL, selected=NULL,
  ..., finishedCaching=NULL
){
  # Update an input using "updateFunc" while preserving the selected options(s)
  # (if any) as well as keeping track of current choices.

  # First, update label
  if(!is.null(finishedCaching)) {
    if(is.null(label)) label <- getDefLabel(inputId)
    cachingExtraInfo <- NULL
    if (!finishedCaching) {
      cachingExtraInfo <- "(caching ongoing)"
    } else if(length(choices)==0) {
      cachingExtraInfo <- "(cache info not available)"
    }
    label <- paste(label, cachingExtraInfo)
  }
  updateFunc(session, inputId, label=label)

  # Now, update choices and selected items (if needed)
  currentChoices <- session$userData$UiChoices[[inputId]]
  if(is.null(choices) || isTRUE(all.equal(choices, currentChoices))) {
    return(NULL)
  }

  selection <- getSelection(session, inputId, choices)
  updateFunc(session, inputId, choices=choices, selected=selection, ...)
  session$userData$UiChoices[[inputId]] <- choices
}

updateSelectInputWrapper <- function(...) {
  updateInputWrapper(updateFunc=updateSelectInput, ...)
}

updatePickerInputWrapper <- function(...) {
  updateInputWrapper(updateFunc=updatePickerInput, ...)
}

updateCheckboxGroup <- function(session, inputId, choices, select="NORMAL") {
    if (is.null(choices)) {
      return(NULL)
    }
    selection <- getSelection(session, inputId, choices, select)
    updateCheckboxGroupInput(session, inputId,
                             choices=choices, selected=selection, inline=TRUE)
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
  for(inp in inpNames) {
    shinyjs::disable(inp)
    if(inp %in% c("channels", "levels", "station", "stationSingle")) {
      # We need to to this for pickerInputs. See issue
      # <https://github.com/dreamRs/shinyWidgets/issues/341>
      shinyjs::runjs(sprintf("$('#%s').selectpicker('refresh');", inp))
    }
  }
}
enableShinyInputs <- function(input, pattern=NULL, except=NULL) {
  inpNames <- isolate(names(reactiveValuesToList(input)))
  inpNames <- grepFilter(inpNames, pattern, except)
  for(inp in inpNames) {
    shinyjs::enable(inp)
    if(inp %in% c("channels", "levels", "station", "stationSingle")) {
      shinyjs::runjs(sprintf("$('#%s').selectpicker('refresh');", inp))
    }
  }
}
