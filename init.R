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
  config
}

fillInDefaults <- function(config) {
  config <- fillInDefault(config, "cacheDir",
                          file.path("", "var", "cache", "obsmon"))
  config <- fillInDefault(config, "logLevel", "WARN")
  config
}

readConfig <- function() {
  configFile <- "config.toml"
  if (file.exists(configFile)) {
    configPath <- configFile
  } else {
    systemConfigDir <- file.path("", "etc", "obsmon")
    configPath <- file.path(systemConfigDir, configFile)
  }
  config <- fillInDefaults(parseTOML(configPath))
  config
}

configure <- function() {
  if (!exists("obsmonConfig")) {
    config <- readConfig()
    setPackageOptions(config)
    obsmonConfig <<- config
    sourceObsmonFiles()
  }
}
