
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
# allMenuLabels and allMenuChoices will keep track of the current
# labels and choices in the UI menus. It seems shiny doesn't have
# a method to return those.
allMenuLabels <- list()
allMenuChoices <- list()
# Updates a selectInput, preserving the selected
# option(s) if available
updateSelectInputWrapper <- function(
  session, inputId, label=NULL, choices=NULL, selected=NULL,
  choicesFoundIncache=TRUE, ...
){

  # First, update label
  notCachedLabelMsg <- "(cache info not available)"
  currentLabel <- allMenuLabels[[inputId]]
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
    allMenuLabels[[inputId]] <<- label
  }

  # Now, update items and choices
  currentChoices <- allMenuChoices[[inputId]]
  if(is.null(choices) || isTRUE(all.equal(choices,currentChoices)))return(NULL)

  selection <- getSelection(session, inputId, choices)
  updateSelectInput(
    session, inputId, choices=choices, selected=selection, label=NULL, ...
  )
  allMenuChoices[[inputId]] <<- choices
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
disableShinyInputs <- function(input, except=c()) {
  allInputs <- names(input)
  if(is.null(allInputs)) allInputs <- input
  inputsToDisable <- allInputs[!(allInputs %in% except)]
  for(inp in inputsToDisable) shinyjs::disable(inp)
}
enableShinyInputs <- function(input, except=c()) {
  allInputs <- names(input)
  if(is.null(allInputs)) allInputs <- input
  inputsToEnable <- allInputs[!(allInputs %in% except)]
  for(inp in inputsToEnable) shinyjs::enable(inp)
}
