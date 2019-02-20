############################################################################
#            Add user guide tab if pdf file for guide exists               #
############################################################################
docPath <- file.path("docs", "obsmon_documentation.pdf")
if(file.exists(docPath)) {
  appendTab("appNavbarPage",
    tabPanel(
      title="Documentation",
      value="docTab",
      docTab(docPath)
    )
  )
}
