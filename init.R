# Flagging that this file has been sourced
initFileSourced <- TRUE

obsmonVersion <- "2.2.0-beta0"

getUserName <- function() {

  userName <- Sys.info()[["user"]]
  if (is.null(userName) | userName == "") userName <- Sys.getenv("USER")
  if (is.null(userName) | userName == "") userName <- Sys.getenv("LOGNAME")
  return(userName)
}

userName <- getUserName()
# Configuring library paths
localRLibDir <- normalizePath(file.path("utils", "build", "local_R_library"), mustWork=FALSE)
localInstallRLib <- file.path(localRLibDir, "locally_installed_R_library")
userRLib <- Sys.getenv("R_LIBS_USER")
if(userRLib=="") userRLib <- file.path(Sys.getenv("HOME"), "R", "library")
userRLib <- normalizePath(userRLib, mustWork=FALSE)
.libPaths(unique(c(localInstallRLib, userRLib, .libPaths())))

libMsg <- "Directories in the R library search path (in order of priority):\n"
for(dir in .libPaths()) {
  libMsg <- paste(libMsg, " >", dir, "\n")
}

tryCatch(
  {
    library(Cairo)
    library(DBI)
    library(dplyr)
    library(dbplyr)
    library(flock)
    library(futile.logger)
    library(future)
    library(ggplot2)
    library(grid)
    library(gridExtra)
    library(leaflet)
    library(methods)
    library(pbapply)
    library(png)
    library(pryr)
    library(RcppTOML)
    library(reshape2)
    library(shiny)
    library(shinyjs)
    library(stringi)

    flog.info(libMsg)
    flog.info(paste('Running as user "', userName, '"', sep=""))
  },
  error=function(e) stop(paste(e, libMsg, sep="\n"))
)

# Creating some default config and cache dirs
systemConfigDir <- file.path("", "etc", "obsmon", userName)
systemCacheDirPath <- file.path("", "var", "cache", "obsmon", userName)
for(dir in c(systemConfigDir, systemCacheDirPath)) {
  # These dir.create commands fails silently if user has no permission or if
  # the directories already exist. That is OK.
  # All users can rwx to dirname(systemCacheDirPath), but no one modifies what
  # someone else has created
  dir.create(dirname(dir), recursive=TRUE, showWarnings=FALSE, mode="1777")
  # Access to a user's own cache files only
  dir.create(dir, recursive=FALSE, showWarnings=FALSE, mode="0755")
}

runAppHandlingBusyPort <- function(
  callAndErrorMsg=NULL,appDir=getwd(),defaultPort=5391,recDepth=0,maxNAtt=10
) {
  if(recDepth==0) {
    tryCatch(
      runApp(appDir, launch.browser=FALSE, port=defaultPort),
      error=function(w) runAppHandlingBusyPort(w, appDir=appDir,recDepth=recDepth+1)
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
        runApp(appDir, launch.browser=FALSE),
        error=function(w) runAppHandlingBusyPort(w, appDir=appDir, recDepth=recDepth+1)
      )
  }
}

setPackageOptions <- function(config) {
  options(shiny.usecairo=TRUE)
  pboptions(type="timer")
  pdf(NULL)
  flog.appender(appender.file(stderr()), 'ROOT')
  flog.threshold(parse(text=config$general$logLevel)[[1]])
  plan(multiprocess)
}

sourceObsmonFiles <- function() {
  source("colors.R")
  source("utils.R")
  source("sql.R")
  source("database.R")
  source("experiments.R")
  source("plots.R")
  source("plots_statistical.R")
  source("plots_timeseries.R")
  source("plots_maps.R")
  source("plots_diagnostic.R")
  source("progress.R")
  source("windspeed.R")
}

fillInDefault <- function(config, key, default) {
  if (is.null(config$general[[key]])) {
    config$general[[key]] <- default
  }
  if(key=="cacheDir") {
    config$general[[key]] <- normalizePath(config$general[[key]], mustWork=FALSE)
  }
  config
}

getSuitableCacheDirDefault <- function() {

  cacheDirPath <- NA
  homeCacheDirPath <- file.path(Sys.getenv("HOME"), ".obsmon", "experiments_cache")

  cacheDirPrio <- c(systemCacheDirPath, homeCacheDirPath)
  for(dirPath in cacheDirPrio) {
    dirCreated <- dir.create(dirPath, recursive=TRUE, showWarnings=FALSE, mode="0755")
    if(!(dirCreated | dir.exists(dirPath))) next
    if(file.access(dirPath, mode=2)==0) cacheDirPath <- dirPath
    if(dirCreated) unlink(dirPath, recursive=TRUE)
    if(!is.na(cacheDirPath)) break
  }

  return(cacheDirPath)
}

fillInDefaults <- function(config) {
  config <- fillInDefault(config, "cacheDir", getSuitableCacheDirDefault())
  config <- fillInDefault(config, "logLevel", "WARN")
  config
}

getValidConfigFilePath <- function(verbose=FALSE) {
  
  configFileDefBasename <- "config.toml"
  exampleConfigFilePath <- normalizePath("config.toml.example", mustWork=FALSE)

  userEnvConfigPath <- Sys.getenv("OBSMON_CONFIG_FILE")
  obsmonSrcDirConfigPath <- normalizePath(configFileDefBasename, mustWork=FALSE)
  sysDirConfigPath <- file.path(systemConfigDir, configFileDefBasename)
  confOrder <- c(userEnvConfigPath, obsmonSrcDirConfigPath, sysDirConfigPath)

  configPath <- NA
  for (fPath in confOrder) {
    if(file.exists(fPath)) {
      configPath <- normalizePath(fPath)
      break
    }
  }

  if(is.na(configPath)) {
    msg <- paste('Config file"', configFileDefBasename, '"not found.\n')
    msg <- paste(msg, "\n")
    msg <- paste(msg, "Please put such file in one of the following dirs:\n")
    msg <- paste(msg, "  >", dirname(exampleConfigFilePath), "\n")
    msg <- paste(msg, "  >", systemConfigDir, "\n")
    msg <- paste(msg, "\n")
    msg <- paste(msg, "Alternatively, you can specify the full path to your")
    msg <- paste(msg, "config file by exporting the\n")
    msg <- paste(msg, "envvar OBSMON_CONFIG_FILE\n")
    msg <- paste(msg, "\n")
    msg <- paste(msg, "Please check the following config file template:\n")
    msg <- paste(msg, "  >", exampleConfigFilePath, "\n")
    stop(msg)
  } 

  if(verbose) flog.info(paste("Config file found:", configPath, "\n"))
  return(configPath)
}

readConfig <- function() {
  configPath <- getValidConfigFilePath(verbose=TRUE)
  config <- fillInDefaults(parseTOML(configPath))
  config
}

assertCacheDirWritable <- function(config, verbose=FALSE) {

  cacheDirPath <- config$general[["cacheDir"]]

  dir.create(cacheDirPath, recursive=TRUE, showWarnings=FALSE, mode="0755")
  writable <- tryCatch(
    file.access(cacheDirPath, mode=2)==0,
    error=function(e) FALSE
  )

  if(!writable) {
    msg <- paste("Cannot write to cacheDir", cacheDirPath, "\n")
    msg <- paste(msg, "Please specify a valid cacheDir value under the\n")
    msg <- paste(msg, '"[general]" section in your config file.\n')
    stop(msg)
  }

  if(verbose) flog.info(paste("cacheDir set to:", cacheDirPath, "\n"))

}

configure <- function() {
  if (!exists("obsmonConfig")) {
    config <- readConfig()
    assertCacheDirWritable(config, verbose=TRUE)
    setPackageOptions(config)
    obsmonConfig <<- config
    sourceObsmonFiles()
  }
}

configure()
