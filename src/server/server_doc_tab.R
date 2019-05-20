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

  # The downloadDoc output is used when the documentation fails
  # to be embedded in the doc tab
  output$downloadDoc <- downloadHandler(
    filename = basename(docPath),
    content = function(file) file.copy(docPath, file)
  )

}
