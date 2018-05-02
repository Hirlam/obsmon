libLoc=Sys.getenv("R_LIBS_USER")
if(libLoc=="") libLoc=file.path(Sys.getenv("HOME"), "R", "library")
libLoc <- path.expand(libLoc)
Sys.setenv(R_LIBS_USER=libLoc)

# Getting the directory where the obsmon files are located
# The env var OBSMON_SRC_DIR is exported in the obsmon bash script
obsmonSrcDir <- Sys.getenv('OBSMON_SRC_DIR')
# Changing work dir to obsmonSrcDir, so all needed files can be found
setwd(obsmonSrcDir)

# Initialising
source("init.R")

shinydir <- obsmonSrcDir

runAppHandlingBusyPort <- function(
  callAndErrorMsg=NULL, defaultPort=5391, recDepth=0, maxNAtt=10
) {
  if(recDepth==0) {
    tryCatch(
      runApp(shinydir, launch.browser=FALSE, port=defaultPort),
      error=function(w) runAppHandlingBusyPort(w, recDepth=recDepth+1)
    )
  } else if (recDepth+1>maxNAtt) {
    msg <- paste("Failed to create server after",maxNAtt,"attempts.",sep=" ")
    msg <- paste(msg, "\n", "Stopping now.\n", sep=" ")
    stop(msg)
  } else {
      msg <- callAndErrorMsg[["message"]]
      cat(msg, "\n")
      cat("Trying again with a different TCP port:\n")
      tryCatch(
        runApp(shinydir, launch.browser=FALSE),
        error=function(w) runAppHandlingBusyPort(w, recDepth=recDepth+1)
      )
  }
}

runAppHandlingBusyPort(defaultPort=5391)
