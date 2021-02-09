.getThisFilePath <- function() {
  argv <- commandArgs(trailingOnly = FALSE)
  thisFilePath <- normalizePath(sub("--file=", "", argv[grep("--file=", argv)]))

  # Account for the fact that the script may be executed via a symlink
  rtn <- Sys.readlink(thisFilePath)
  if(rtn=="" || anyNA(rtn)) rtn <- thisFilePath

  return(rtn) 
}

.filesAreR <- function(fPaths) {
  rtn <- c()
  for(fPath in fPaths) {
    if(!file.exists(fPath)) rtn <- c(rtn, FALSE)
    else if(tools::file_ext(fPath)=="R") rtn <- c(rtn, TRUE)
    else if(tools::file_ext(fPath)=="") {
      con = file(fPath, "r")
      line = readLines(con, n=1, warn=FALSE)
      close(con)
      if(length(line)>0) {
        rtn <- c(rtn, grepl("Rscript", line, fixed=TRUE, useBytes=TRUE))
      } else {
        rtn <- c(rtn, FALSE)
      }
    }
    else rtn <- c(rtn, FALSE)
  }
  return(rtn)
}

.locateRSources <- function(path, ignore_regex=NULL) {
  # Locate R files (descend recursively through directories)
  allFiles <- list.files(path=path, recursive=TRUE, full.names=TRUE)
  # Keep only R files
  allRFiles <- normalizePath(allFiles[.filesAreR(allFiles)])
  # Make sure we don't include ignored patterns
  for(patt in ignore_regex) {
    allRFiles <- allRFiles[!grepl(patt, allRFiles)]
  }
  return(allRFiles)
}
