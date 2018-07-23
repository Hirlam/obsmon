

filesAreR <- function(fPaths) {
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
