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
             choices
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

updateSelectInputWrapper <- function(
  session, inputId, label=NULL, choices=NULL, selected=NULL,
  choicesFoundIncache=TRUE, ...
){
  # Update a selectInputs while preserving the selected options(s)
  # (if any) as well as keeping track of current choices and labels

  # Attaching new lists "userData$UiLabels" and "userData$UiChoices" to
  # session if this is the 1st time this routine is run in the session.
  # These lists will keep track of the current labels and choices in the
  # UI menus. It seems shiny doesn't have a native method to return those.
  if(is.null(session$userData$UiLabels)) session$userData$UiLabels <- list()
  if(is.null(session$userData$UiChoices)) session$userData$UiChoices <- list()

  # First, update label
  notCachedLabelMsg <- "(no cache info, using defaults)"
  currentLabel <- session$userData$UiLabels[[inputId]]
  if(is.null(currentLabel)) currentLabel <- getDefLabel(inputId)

  currLabelFlaggedAsNotCached <- isTRUE(grepl(notCachedLabelMsg,currentLabel))
  needsLabelChange <- {
    isTRUE(label!=currentLabel) ||
    (choicesFoundIncache && currLabelFlaggedAsNotCached) ||
    (!choicesFoundIncache && !currLabelFlaggedAsNotCached)
  }

  if(needsLabelChange) {
    if(is.null(label)) label <- currentLabel
    label <- gsub(notCachedLabelMsg, "", label, fixed=TRUE)
    if(!choicesFoundIncache) label <- paste(label, notCachedLabelMsg)
    updateSelectInput(session, inputId, label=label)
    session$userData$UiLabels[[inputId]] <- label
  }

  # Now, update items and choices
  currentChoices <- session$userData$UiChoices[[inputId]]
  if(is.null(choices) || isTRUE(all.equal(choices,currentChoices)))return(NULL)

  selection <- getSelection(session, inputId, choices)
  updateSelectInput(
    session, inputId, choices=choices, selected=selection, label=NULL, ...
  )
  session$userData$UiChoices[[inputId]] <- choices
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
  for(inp in inpNames) shinyjs::disable(inp)
}
enableShinyInputs <- function(input, pattern=NULL, except=NULL) {
  inpNames <- isolate(names(reactiveValuesToList(input)))
  inpNames <- grepFilter(inpNames, pattern, except)
  for(inp in inpNames) shinyjs::enable(inp)
}
