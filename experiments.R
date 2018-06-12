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

emptyExperiment <- function(name) {
  x <- structure(new.env(), class = "sqliteShardedDtg")
  x$name <- name
  x$dbs$ecma <- NULL
  x$dbs$ecmaSfc <- NULL
  x$dbs$ccma <- NULL
  x
}

initExperimentsAsPromises <- function() {
  # Using new.env(), as lists cannot be used with %<-%
  experiments <- new.env()
  experimentChoices <- list()
  # Making sure experiments don't block each other
  originalNumberAvailableWorkers <- length(availableWorkers())
  plan(multiprocess, workers=2*length(obsmonConfig$experiments))
  on.exit(plan(multiprocess, workers=originalNumberAvailableWorkers))
  for(config in obsmonConfig$experiments) {
    name <- config$displayName
    # Using %<-% (library "future") to have experiments caching asynchronously.
    # This makes it possible to start working with experiments as soon as they
    # are cached, without having to wait for the others to finish.
    experiments[[name]] %<-%
      expCreateSqliteShardedDtg(name, config$baseDir, config$experiment)
    experimentChoices <- append(experimentChoices, name)
  }
  experiments
}

experimentsAsPromises <- initExperimentsAsPromises()
