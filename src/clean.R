removeDirs <- function(args) {
  dirList <- args$output_dirs
  if("all" %in% args$clean) {
    cat("Removing dir", args$output_rootdir, "\n")
    unlink(args$output_rootdir, recursive=TRUE)
  } else {
    for (dirType in names(dirList)) {
      if(!dirType %in% args$clean) next
      cat("Removing dir", dirList[[dirType]], "\n")
      unlink(dirList[[dirType]], recursive=TRUE)
    }
  }
}
