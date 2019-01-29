# Flagging that this file has been sourced
initFileSourced <- TRUE

if(!exists("runningAsStandalone") || runningAsStandalone==FALSE) {
  source("src_info_obsmon.R")
  source('lib_paths_config.R')
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
    suppressPackageStartupMessages(library(Cairo))
    suppressPackageStartupMessages(library(DBI))
    suppressPackageStartupMessages(library(dplyr))
    suppressPackageStartupMessages(library(dbplyr))
    suppressPackageStartupMessages(library(flock))
    suppressPackageStartupMessages(library(futile.logger))
    suppressPackageStartupMessages(library(promises))
    suppressPackageStartupMessages(library(future))
    suppressPackageStartupMessages(library(ggplot2))
    suppressPackageStartupMessages(library(grid))
    suppressPackageStartupMessages(library(gridExtra))
    suppressPackageStartupMessages(library(leaflet))
    suppressPackageStartupMessages(library(methods))
    suppressPackageStartupMessages(library(png))
    suppressPackageStartupMessages(library(pryr))
    suppressPackageStartupMessages(library(RcppTOML))
    suppressPackageStartupMessages(library(reshape2))
    suppressPackageStartupMessages(library(shiny))
    suppressPackageStartupMessages(library(shinyjs))
    suppressPackageStartupMessages(library(shinycssloaders))
    suppressPackageStartupMessages(library(stringi))
    suppressPackageStartupMessages(library(bsplus))
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

runAppHandlingBusyPort <- function(
  appDir=getwd(), defaultPort=getOption("shiny.port"),
  launch.browser=getOption("shiny.launch.browser", interactive()),
  host = getOption("shiny.host", "127.0.0.1"),
  maxNAtt=10,
  ...
) {

  exitMsg <- paste(
               "===============",
               "Exiting Obsmon.",
               "===============",
               "",
               sep="\n"
             )
  on.exit(cat(exitMsg), add=TRUE)

  port <- defaultPort
  success <- FALSE
  nAtt <- 0
  lisOnMsgStart <- 'Listening on '
  lisOnMsgMarker <- "------------------------------------"
  while (!success & (nAtt<maxNAtt)) {
    tryCatch(
      {
        cat("\n")
        cat(paste(lisOnMsgMarker, "\n", sep=""))
        lisOnMsg <- paste(lisOnMsgStart,"http://",host,":",port,"\n", sep='')
        cat(lisOnMsg)
        cat(paste(lisOnMsgMarker, "\n", sep=""))
        cat("\n")

        runApp(appDir, launch.browser=launch.browser, port=port, ...)
        success <- TRUE
      },
      error=function(w) {
        flog.warn(paste('Failed to create server using port', port, sep=" "))
        port <<- sample(1024:65535, 1)
        lisOnMsgStart <<- "Port updated: Listening on "
        lisOnMsgMarker <<- "-------------------------------------------------"
      }
    )
    nAtt <- nAtt + 1
  }

  if(!success) {
    msg <- paste("Failed to create server after", nAtt, "attempts.\n",sep=" ")
    msg <- paste(msg, "Stopping now.\n", sep=" ")
    stop(msg)
  }
}

setPackageOptions <- function(config) {
  options(shiny.usecairo=TRUE)
  pdf(NULL)
  flog.appender(appender.file(stderr()), 'ROOT')
  logLevel <- parse(text=config$general$logLevel)[[1]]
  if(exists("args") && isTRUE(args$debug)) logLevel <- DEBUG
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
  source("observation_definitions.R")
  source("utils.R")
  source("sql.R")
  source("database.R")
  source("src/cache_routines.R")
  source("experiments.R")
  source("src/plots/colors.R")
  source("src/plots/plots.R")
  source("src/plots/plots_statistical.R")
  source("src/plots/plots_timeseries.R")
  source("src/plots/plots_maps.R")
  source("src/plots/plots_diagnostic.R")
  source("src/plots/windspeed.R")
  source("src/plots/plots_multi.R")
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
