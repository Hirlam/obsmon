# Getting the directory where the obsmon files are located
# The env var OBSMON_SRC_DIR is exported in the obsmon bash script
obsmonSrcDir <- Sys.getenv('OBSMON_SRC_DIR')
# Changing work dir to obsmonSrcDir, so all needed files can be found
setwd(obsmonSrcDir)

# Initialising
source("init.R")

shinydir <- obsmonSrcDir
runApp(shinydir, launch.browser=FALSE, port=5391)
