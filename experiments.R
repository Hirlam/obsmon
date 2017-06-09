# This defines S3 classes to deal with the database connections for experiments

library(DBI)
library(futile.logger)
library(pbapply)
library(pryr)
library(R.cache)

pboptions(type="timer")

source("utils.R")
source("database.R")

setCacheRootPath(path="./.Rcache")

flog.appender(appender.file(stderr()), 'ROOT')
# flog.threshold(TRACE)


expCreateSqliteShardedDtg <- function(name, isProduction,
                                      baseDir, experiment,
                                      ecmaDir, ecmaSfcDir, ccmaDir) {
  flog.info("Initialization of experiment %s...", name)
  x <- structure(list(), class = "sqliteShardedDtg")
  x$name <- name
  x$isProduction <- isProduction
  x$isConnected <- F
  x$dbs$ecma <- createDb(file.path(baseDir, experiment, ecmaDir),
                         "ecma", "ecma.db")
  x$dbs$ecmaSfc <- createDb(file.path(baseDir, experiment, ecmaSfcDir),
                            "ecmaSfc", "ecma.db")
  x$dbs$ccma <- createDb(file.path(baseDir, experiment, ccmaDir),
                         "ccma", "ccma.db")
  flog.info("Finished initialization of experiment %s.", name)
  x
}

initExperiments <- function() {
  configs <- yaml.load_file("config.yml")
  experiments <- list()
  experimentChoices <- list()
  for(config in configs) {
    name <- config$displayName
    experiments[[name]] <-
      expCreateSqliteShardedDtg(name, config$productionSite,
                                config$baseDir, config$experiment,
                                config$ecmaDir, config$ecmaSfcDir,
                                config$ccmaDir)
    experimentChoices <- append(experimentChoices, name)
  }
  experiments
}

experiments <- initExperiments()
