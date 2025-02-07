SPINNER_IMAGE_PATH <- base64enc::dataURI(file="data/loading.gif")

# Flagging that this file has been sourced
initFileSourced <- TRUE

# Define isFALSE function, which is not available for R<3.5
isFALSE <- tryCatch(
  isFALSE,
  error=function(e) {function(x) identical(x, FALSE)}
)

thisAppDir <- getwd()

if(!exists("runningAsStandalone") || !isTRUE(runningAsStandalone)) {
  source("src/src_info_obsmon.R")
  runningAsStandalone <- FALSE
  cmdLineArgs <- list()
  dirObsmonWasCalledFrom <- thisAppDir
  # Write code info to the log for every session when using a shiny server
  # This info is already printed in a banner when running standalone
  message(obsmonBanner)
}


getUserName <- function() {
  userName <- Sys.info()[["user"]]
  if (is.null(userName) | userName == "") userName <- Sys.getenv("USER")
  if (is.null(userName) | userName == "") userName <- Sys.getenv("LOGNAME")
  if (is.null(userName) | userName == "") userName <- "Unknown username"
  return(userName)
}
userName <- getUserName()

# Load packages
cat(paste(
  "Looking for R-libs in the following directories",
  "(listed in order of priority):\n"
))
for(path in .libPaths()) {
  cat(paste("    >", normalizePath(path), "\n"))
}
cat("\n")
suppressPackageStartupMessages(library(bsplus))
suppressPackageStartupMessages(library(Cairo))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(dbplyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(flock))
suppressPackageStartupMessages(library(futile.logger))
suppressPackageStartupMessages(library(future))
suppressPackageStartupMessages(library(future.apply))
suppressPackageStartupMessages(library(ggforce))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(mapproj))
suppressPackageStartupMessages(library(maps))
suppressPackageStartupMessages(library(methods))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(png))
suppressPackageStartupMessages(library(pryr))
suppressPackageStartupMessages(library(promises))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(RcppTOML))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(stringi))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(testthat)) # For tests
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(units))
suppressPackageStartupMessages(library(usethis)) # For tests
suppressPackageStartupMessages(library(V8))

flog.info(sprintf("Main process PID: %d", Sys.getpid()))
flog.info(paste('Running as user "', userName, '"', sep=""))

# homeAuxDir: Used for storage of information such as config files,
# cache, etc.
homeAuxDir <- file.path(Sys.getenv("HOME"), ".obsmon")
dir.create(homeAuxDir, recursive=TRUE, showWarnings=FALSE)
# Creating some default config and cache dirs
systemConfigDir <- file.path("", "etc", "obsmon", userName)
systemCacheDirPath <- file.path("", "var", "cache", "obsmon", userName)
for(dir in c(systemConfigDir, systemCacheDirPath)) {
  # These dir.create commands fails silently if user has no permission or if
  # the directories already exist. That is OK.
  # All users can rwx to dirname(systemCacheDirPath), but no one modifies what
  # someone else has created
  dir.create(dirname(dir), recursive=TRUE, showWarnings=FALSE, mode="1777")
  # Only the user can write to their own cache files
  dir.create(dir, recursive=FALSE, showWarnings=FALSE, mode="0755")
}

setPackageOptions <- function(config) {
  options(shiny.usecairo=TRUE)
  pdf(NULL)
  flog.appender(appender.file(stderr()), 'ROOT')
  flog.threshold(parse(text=config$general$logLevel)[[1]])
  # ggplot2 theme: set globally. theme_bw sets figure backgrounds to white
  theme_set(theme_bw())
  # Options controlling parallelism
  maxNWorkers <- 1 + config$general$maxExtraParallelProcs
  if(maxNWorkers > availableCores()) maxProcsSecLayer <- availableCores()
  else maxProcsSecLayer <- 1
  maxProcsFirstLayer <- floor(maxNWorkers / maxProcsSecLayer)
  if (future::supportsMulticore()) {
    future::plan(list(
      tweak(future::multicore, workers=maxProcsFirstLayer),
      tweak(future::multicore, workers=maxProcsSecLayer)
    ))
  } else {
    future::plan(list(
      tweak(future::multisession, workers=maxProcsFirstLayer),
      tweak(future::multisession, workers=maxProcsSecLayer)
    ))
  }
}

sourceObsmonFiles <- function() {
  source("src/units.R")
  source("src/domains.R")
  source("src/observation_definitions.R")
  source("src/utils.R")
  source("src/sqlite/sqlite_wrappers.R")
  source("src/sqlite/cache_routines.R")
  source("src/experiments.R")
  source("src/plots/plots.R")
  source("src/plots/plots_statistical.R")
  source("src/plots/plots_diagnostic.R")
  source("src/plots/plots_timeseries.R")
  source("src/plots/plots_maps.R")
  source("src/plots/plots_vertical_profiles.R")
  source("src/plots/plots_multi.R")
  source("src/plots/plots_batch.R")
  source("src/shiny_wrappers.R")
}

configGeneralFillInDefault <- function(config, key, default) {
  if (is.null(config$general[[key]])) config$general[[key]] <- default

  currentVal <- config$general[[key]]
  config$general[[key]] <- switch(key,
    "appTimeout"={
      currentVal <- as.numeric(currentVal)
      if(anyNA(currentVal) || currentVal<0) {
        flog.warn("Invalid appTimeout. Resetting to %s s.", default)
        currentVal <- default
      }
      currentVal
    },
    "cacheDir"={
      # Append code version information to the cacheDir path. Useful to
      # prevent conflict when updating the code. Discarding patch, though,
      # as changes to cache cannot be considered as patches. This avoids the
      # creation of too many cache directories as obsmon gets updated.
      obsmonVersionMajorMinor <- paste(
        unlist(strsplit(obsmonVersion, "\\."))[1:2],
        collapse="."
      )
      normalizePath(
        file.path(currentVal, paste0("obsmon_v", obsmonVersionMajorMinor)),
        mustWork=FALSE)
    },
    "logLevel"={
      if(isTRUE(cmdLineArgs$debug)) "DEBUG"
      else currentVal
    },
    "maxAvgQueriesPerProc"={
      currentVal <- round(as.numeric(currentVal))
      if(anyNA(currentVal) || currentVal<1) {
        flog.warn("Resetting maxAvgQueriesPerProc to %s", default)
        currentVal <- default
      }
      currentVal
    },
    "maxExtraParallelProcs"={
      currentVal <- round(as.numeric(currentVal))
      if(anyNA(currentVal) || currentVal<0) {
        currentVal <- 4 * availableCores()
      } else {
        flog.info("Limiting maxExtraParallelProcs to %s", currentVal)
      }
      currentVal
    },
    "sessionTimeout"={
      currentVal <- as.numeric(currentVal)
      if(anyNA(currentVal) || currentVal<0) {
        flog.warn("Invalid sessionTimeout. Resetting to %s s.", default)
        currentVal <- default
      }
      if(currentVal<60) {
        flog.warn("sessionTimeout is set to only %ss!", currentVal)
      }
      currentVal
    },
    currentVal
  )
  config
}

getSuitableCacheDirDefault <- function() {
  cacheDirPath <- NA
  homeCacheDirPath <- file.path(homeAuxDir, "experiments_cache")

  cacheDirPrio <- c(homeCacheDirPath, systemCacheDirPath)
  for(dirPath in cacheDirPrio) {
    dirCreated <- dir.create(dirPath, recursive=TRUE, showWarnings=FALSE)
    if(!(dirCreated | dir.exists(dirPath))) next
    if(file.access(dirPath, mode=2)==0) cacheDirPath <- dirPath
    if(dirCreated) unlink(dirPath, recursive=TRUE)
    if(!anyNA(cacheDirPath)) break
  }

  return(cacheDirPath)
}

configGeneralFillInDefaults <- function(config) {
  if(length(config)==0) config <- list(general=list())
  config <- configGeneralFillInDefault(config, "appTimeout", Inf)
  config <- configGeneralFillInDefault(
    config, "cacheDir", getSuitableCacheDirDefault()
  )
  config <- configGeneralFillInDefault(config, "initCheckDataExists", FALSE)
  config <- configGeneralFillInDefault(config, "logLevel", "WARN")
  config <- configGeneralFillInDefault(config, "maxAvgQueriesPerProc", Inf)
  config <- configGeneralFillInDefault(config, "maxExtraParallelProcs",
    Sys.getenv("OBSMON_MAX_N_EXTRA_PROCESSES")
  )
  config <- configGeneralFillInDefault(
    config, "multiPlotsEnableInteractivity", FALSE
  )
  config <- configGeneralFillInDefault(
    config, "plotsEnableInteractivity", TRUE
  )
  config <- configGeneralFillInDefault(config, "sessionTimeout", Inf)
  config <- configGeneralFillInDefault(config, "showCacheOptions", FALSE)
  config
}

getValidConfigFilePath <- function(verbose=FALSE) {

  configFileDefBasename <- "config.toml"
  exampleConfigFilePath <- file.path(thisAppDir,"docs","config.toml.example")

  userEnvConfigPath <- Sys.getenv("OBSMON_CONFIG_FILE")
  if(userEnvConfigPath=="") userEnvConfigPath <- NULL
  homeAuxDirConfigPath <- file.path(homeAuxDir, configFileDefBasename)
  obsmonSrcDirConfigPath <- file.path(thisAppDir, configFileDefBasename)
  sysDirConfigPath <- file.path(systemConfigDir, configFileDefBasename)
  confOrder <- c(
    userEnvConfigPath,
    homeAuxDirConfigPath,
    sysDirConfigPath,
    obsmonSrcDirConfigPath
  )

  configPath <- NA
  for (fPath in confOrder) {
    if(file.exists(fPath)) {
      configPath <- normalizePath(fPath)
      break
    }
  }

  if(anyNA(configPath)) {
    msg <- paste0(
      'Config file "', configFileDefBasename, '" not found!\n',
      "  Please put the config file under one of the following dir(s):\n"
    )
    for (fPath in confOrder) {
      fDir <- dirname(fPath)
      if(file.access(dirname(fPath), 2) != 0) next
      msg <- paste0(msg, "    > ", fDir, "\n")
    }
    msg <- paste0(msg,
      "  or use the environment variable OBSMON_CONFIG_FILE to provide the\n",
      "  full path (including file name) to an existing configuration file.",
      "\n\n  A config file template can be found at:\n",
      "  > ", exampleConfigFilePath, "\n\n"
    )
    flog.error("getValidConfigFilePath: %s", msg)
    return(character(0))
  }

  if(verbose) flog.info(paste("Config file found:", configPath, "\n"))
  return(configPath)
}

readConfig <- function() {
  configPath <- getValidConfigFilePath(verbose=TRUE)
  config <- NULL
  if(length(configPath)>0) {
    config <- tryCatch(
      parseTOML(configPath),
      warning=function(w) {flog.error(w); NULL}
    )
  }
  return(configGeneralFillInDefaults(config))
}

createCacheDir <- function(config, verbose=FALSE) {
  cacheDirPath <- config$general[["cacheDir"]]
  dir.create(cacheDirPath, recursive=TRUE, showWarnings=FALSE)
  writable <- tryCatch(
    file.access(cacheDirPath, mode=2)==0,
    error=function(e) FALSE
  )
  if(!writable) {
    msg <- paste("Cannot write to cacheDir", cacheDirPath, " !!!!!\n")
    msg <- paste(msg, "Please specify a valid cacheDir value under the\n")
    msg <- paste(msg, '"[general]" section in your config file.\n')
    msg <- paste(msg, ">> Caching will not work!!!!\n")
    warning(msg, immediate.=TRUE)
  }
  if(verbose) flog.info("cacheDir set to: %s\n", cacheDirPath)
}

configure <- function() {
  if(!exists("obsmonConfig")) {
    config <- readConfig()
    if(!isTRUE(cmdLineArgs$batch)) createCacheDir(config, verbose=TRUE)
    setPackageOptions(config)
    flog.debug('Temp dir for session: %s\n', tempdir())
    obsmonConfig <<- config
    confExpts <- obsmonConfig$experiments
    exptNamesInConfig <<- unlist(lapply(confExpts, function(x) x$displayName))
    if(length(exptNamesInConfig)==0) flog.error("No experiment configured!")
    if(length(obsmonConfig$general$configName)>0) {
      flog.info('Config name found: "%s"\n', obsmonConfig$general$configName)
    }
    sourceObsmonFiles()
  }
}

runObsmonStandAlone <- function(cmdLineArgs) {
  exitMsg <- paste(
    "",
    "===============",
    "Exiting Obsmon.",
    "===============",
    "",
    sep="\n"
  )
  on.exit(cat(exitMsg))

  if(cmdLineArgs$batch) {
    makeBatchPlots()
  } else {
    # Running the shinny app. The runAppHandlingBusyPort routine is defined in
    # the file src/shiny_wrappers.R
    displayMode <- NULL
    if(cmdLineArgs$debug) {
      Rprof()
      options(shiny.reactlog=TRUE, error=traceback, shiny.error=browser)
      displayMode <- "showcase"
    }
    runAppHandlingBusyPort(
      appDir=obsmonSrcDir, defaultPort=cmdLineArgs$port,
      launch.browser=cmdLineArgs$launch, quiet=TRUE,
      maxNAtt=max(cmdLineArgs$maxTcpRetries+1, 1),
      display.mode=displayMode
    )
  }
}

configure()
