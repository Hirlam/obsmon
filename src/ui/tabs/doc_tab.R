docTab <- function(docPath) {
  # Using addResourcePath is necessary so that tags$iframe can find files
  # outside the www directory
  addResourcePath('docDir', dirname(docPath))
  docUrl <- sprintf(
    "docDir/%s#view=FitH&pagemode=bookmarks",
    basename(docPath)
  )
  # Try to embedded the pdf doc in the page itself, and offer a download
  # option if this fails.
  bootstrapPage(
    tags$object(
      HTML(
        "Could not open documentation. Your browser may not have a PDF",
        "reader plugin, or it may have been disabled.<br><br>",
        "Please download the documentation file instead.<br>"
      ),
      downloadButton("downloadDoc", "Download doc file"),
      data=docUrl,
      type="application/pdf",
      border=3,
      style="position:absolute; height:90%; width:98.5%;"
    )
  )
}
