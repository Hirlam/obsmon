docTab <- function(docPath) {
  # Using addResourcePath is necessary so that tags$iframe can find files
  # outside the www directory
  addResourcePath('docDir', dirname(docPath))
  docUrl <- sprintf(
    "docDir/%s#view=FitH&pagemode=bookmarks",
    basename(docPath
  ))
  # Show the pdf file on embedded in the page if the user has a working pdf
  # reader plugin. Otherwise, show a message explaining what happened and
  # giving instructions on how to find doc elsewhere.
  # Not showing docPath to the users, as some of them may access obsmon
  # remotely and it is not a good idea to expose these paths to everyone.
  fluidPage(
    tags$object(
      HTML(
        paste(
          "Could not open documentation. Your browser may not have a PDF",
          "reader plugin, or it may have been disabled.<br>",
          'Please open the doc file directly. It is located in the "doc"',
          "directory under obsmon's installation dir.<br><br>",
          "If you cannot access that (e.g., if you are accessing obsmon from",
          "a remote browser), then you can clone the code's repo using<br>",
          "<code>git clone https://git.hirlam.org/Obsmon</code><br><br>",
          "A copy of the doc file is also",
          sprintf('<a href="%s">available online</a>,',
            "https://drive.google.com/file/d/1J4bSjOFBKMVJClXKg-11VU0azWu0dX7r/preview"
          ),
          "but it is not guaranteed to be up-do-date."
        )
      ),
      data=docUrl,
      type="application/pdf",
      style="position: absolute; height:90%; width:98.5%;"
    )
  )
}
