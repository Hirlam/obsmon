# Flagging that this file has been sourced
initFileSourced <- TRUE

thisAppDir <- getwd()

if(!exists("runningAsStandalone") || runningAsStandalone==FALSE) {
  source("src/src_info_obsmon.R")
  source('src/lib_paths_config.R')
  runningAsStandalone <- FALSE
}

if(!runningAsStandalone) {
  # This info is already printed in a banner when runningAsStandalone
  cat(obsmonBanner)
}

getUserName <- function() {
  userName <- Sys.info()[["user"]]
  if (is.null(userName) | userName == "") userName <- Sys.getenv("USER")
  if (is.null(userName) | userName == "") userName <- Sys.getenv("LOGNAME")
  if (is.null(userName) | userName == "") userName <- "Unknown username"
  return(userName)
}
userName <- getUserName()

tryCatch(
  {
    suppressPackageStartupMessages(library(bsplus))
    suppressPackageStartupMessages(library(Cairo))
    suppressPackageStartupMessages(library(DBI))
    suppressPackageStartupMessages(library(dbplyr))
    suppressPackageStartupMessages(library(dplyr))
    suppressPackageStartupMessages(library(flock))
    suppressPackageStartupMessages(library(futile.logger))
    suppressPackageStartupMessages(library(future))
    suppressPackageStartupMessages(library(ggplot2))
    suppressPackageStartupMessages(library(grid))
    suppressPackageStartupMessages(library(gridExtra))
    suppressPackageStartupMessages(library(leaflet))
    suppressPackageStartupMessages(library(methods))
    suppressPackageStartupMessages(library(plotly))
    suppressPackageStartupMessages(library(png))
    suppressPackageStartupMessages(library(pryr))
    suppressPackageStartupMessages(library(promises))
    suppressPackageStartupMessages(library(RcppTOML))
    suppressPackageStartupMessages(library(reshape2))
    suppressPackageStartupMessages(library(shiny))
    suppressPackageStartupMessages(library(shinycssloaders))
    suppressPackageStartupMessages(library(shinyjs))
    suppressPackageStartupMessages(library(stringi))
    suppressPackageStartupMessages(library(V8))
  },
  error=function(e) stop(paste(e, libPathsMsg[['error']], sep="\n"))
)
flog.info(sprintf("Main process PID: %d", Sys.getpid()))
flog.info(paste('Running as user "', userName, '"', sep=""))
flog.info(libPathsMsg[['success']])

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

setPackageOptions <- function(config) {
  options(shiny.usecairo=TRUE)
  pdf(NULL)
  flog.appender(appender.file(stderr()), 'ROOT')
  logLevel <- parse(text=config$general$logLevel)[[1]]
  if(exists("cmdLineArgs") && isTRUE(cmdLineArgs$debug)) logLevel <- DEBUG
  flog.threshold(logLevel)
  # Options controlling parallelism
  maxExtraParallelProcs <- as.integer(config$general$maxExtraParallelProcs)
  if(is.na(maxExtraParallelProcs) || maxExtraParallelProcs<0) {
    maxExtraParallelProcs <- .Machine$integer.max
  } else {
    flog.info(sprintf("Limiting maxExtraParallelProcs to %s",
      maxExtraParallelProcs
    ))
  }
  plan(multiprocess, workers=maxExtraParallelProcs)
}

sourceObsmonFiles <- function() {
  source("src/observation_definitions.R")
  source("src/utils.R")
  source("src/sqlite/sqlite_wrappers.R")
  source("src/sqlite/cache_routines.R")
  source("src/experiments.R")
  source("src/plots/colors.R")
  source("src/plots/plots.R")
  source("src/plots/plots_statistical.R")
  source("src/plots/plots_diagnostic.R")
  source("src/plots/plots_timeseries.R")
  source("src/plots/plots_maps.R")
  source("src/plots/plots_vertical_profiles.R")
  source("src/plots/windspeed.R")
  source("src/plots/plots_multi.R")
  source("src/shiny_wrappers.R")
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
  config <- fillInDefault(config, "initCheckDataExists", FALSE)
  config <- fillInDefault(config, "maxExtraParallelProcs",
    Sys.getenv("OBSMON_MAX_N_EXTRA_PROCESSES")
  )
  config <- fillInDefault(config, "showCacheOptions", FALSE)
  config <- fillInDefault(config, "multiPlotsEnableInteractivity", FALSE)
  config
}

getValidConfigFilePath <- function(verbose=FALSE) {
  
  configFileDefBasename <- "config.toml"
  exampleConfigFilePath <- file.path(thisAppDir,"docs","config.toml.example")

  userEnvConfigPath <- Sys.getenv("OBSMON_CONFIG_FILE")
  obsmonSrcDirConfigPath <- file.path(thisAppDir, configFileDefBasename)
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
    msg <- paste0(
      'Config file "', configFileDefBasename, '" not found.\n\n',
      "Please create and put such a file under one of the following dirs:\n",
      "  > ", dirname(obsmonSrcDirConfigPath), "\n",
      "  > ", systemConfigDir, "\n\n",
      "Alternatively, you can specify the full path to your config \n",
      "file by exporting the envvar OBSMON_CONFIG_FILE\n\n",
      "A config file template can be found at:\n",
      "  > ", exampleConfigFilePath, "\n\n"
    )
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