getDtgs <- function(path) {
  dtgs <- tryCatch({
      foundDtgs <- as.integer(dir(path=path, pattern="[0-9]{10}"))
      foundDtgs <- sort(unique(foundDtgs))
      foundDtgs
    },
    error=function(e) {flog.error(e$message); NULL},
    warning=function(w) {flog.warn(w$message); foundDtgs}
  )
  if(length(dtgs)==0) dtgs <- NULL
  return(dtgs)
}

expCreateSqliteShardedDtg <- function(name,
                                      baseDir, experiment) {
  flog.info("Initializing experiment %s...", name)
  x <- structure(list(), class = "sqliteShardedDtg")
  x$name <- name
  x$dbs$ecmaSfc <- createDb(file.path(baseDir, experiment, "ecma_sfc"),
                            name, "ecmaSfc", "ecma.db")
  x$dbs$ccma <- createDb(file.path(baseDir, experiment, "ccma"),
                         name, "ccma", "ccma.db")
  x$dbs$ecma <- createDb(file.path(baseDir, experiment, "ecma"),
                         name, "ecma", "ecma.db")
  nullExp <- is.null(x$dbs$ecma) & is.null(x$dbs$ecmaSfc) & is.null(x$dbs$ccma)
  if(nullExp) {
    flog.warn("Could not find any data for experiment %s. Skipping it.", name)
    x <- NULL
  } else {
    flog.info("Finished initialization of experiment %s.", name)
  }
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

# Creating and initialising variables that will help keep track of the caching
# progress for each experiment
exptsCachingProgress <- new.env(parent=globalenv())
exptsCacheProgLogFilePath <- new.env(parent=globalenv())
for(config in obsmonConfig$experiments) {
  name <- config$displayName
  exptsCachingProgress[[name]] <- list("ecma"=0.0, "ecmaSfc"=0.0, "ccma"=0.0)
  exptsCacheProgLogFilePath[[name]] <- file.path(
    obsmonConfig$general[["cacheDir"]],
    paste('caching_status_', name, '.tmp.RData', sep='')
  )
  unlink(exptsCacheProgLogFilePath[[name]])
  file.create(exptsCacheProgLogFilePath[[name]])
}

removeExptCachingStatusFiles <- function() {
  for(config in obsmonConfig$experiments) {
    name <- config$displayName
    unlink(exptsCacheProgLogFilePath[[name]])
  }
}

experimentsAsPromises <- initExperimentsAsPromises()
