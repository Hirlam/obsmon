expCreateSqliteShardedDtg <- function(name,
                                      baseDir, experiment) {
  flog.info("Initialization of experiment %s...", name)
  x <- structure(list(), class = "sqliteShardedDtg")
  x$name <- name
  x$dbs$ecma <- createDb(file.path(baseDir, experiment, "ecma"),
                         name, "ecma", "ecma.db")
  x$dbs$ecmaSfc <- createDb(file.path(baseDir, experiment, "ecma_sfc"),
                            name, "ecmaSfc", "ecma.db")
  x$dbs$ccma <- createDb(file.path(baseDir, experiment, "ccma"),
                         name, "ccma", "ccma.db")
  flog.info("Finished initialization of experiment %s.", name)
  x
}

initExperiments <- function() {
  experiments <- list()
  experimentChoices <- list()
  for(config in obsmonConfig$experiments) {
    name <- config$displayName
    experiments[[name]] <-
      expCreateSqliteShardedDtg(name, config$baseDir, config$experiment)
    experimentChoices <- append(experimentChoices, name)
  }
  experiments
}

experiments <- initExperiments()
