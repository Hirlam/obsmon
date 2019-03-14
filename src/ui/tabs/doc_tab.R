docTab <- function(docPath) {
  # Using addResourcePath is necessary so that tags$iframe can find files
  # outside the www directory
  addResourcePath('docDir', dirname(docPath))
  docUrl <- sprintf(
    "docDir/%s#view=FitH&pagemode=bookmarks",
    basename(docPath
  ))
  fluidPage(
    # Embed PDF file to show it using the browser's viewer
    tags$iframe(
      src=docUrl,
      style="position: absolute; height:90%; width:98.5%;"
    )
  )
}
