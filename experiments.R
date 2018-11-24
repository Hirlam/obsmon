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

pathToDataFileForDtg <- function(dtg, db) {
  dbpath <- tryCatch({
      fname <- gsub('_sfc', '', paste0(db$dbType, '.db'), fixed=TRUE)
      file.path(db$dir, dtg, fname)
    },
    error=function(e) NULL,
    warning=function(w) NULL
  )
  return(dbpath)
}

expCreateSqliteShardedDtg <- function(name, baseDir, experiment) {
  flog.info("Initializing experiment %s...", name)
  x <- structure(list(), class = "sqliteShardedDtg")
  x$name <- name
  x$path <- file.path(baseDir, experiment)
  x$cacheDir <- file.path(obsmonConfig$general[["cacheDir"]], slugify(name))
  x$dbs <- list(ccma=NULL, ecma=NULL, ecma_sfc=NULL)
  for(dbType in names(x$dbs)) {
    availableDtgs <- getDtgs(file.path(x$path, dbType))
    if(is.null(availableDtgs)) next
    x$dbs[[dbType]] <- list(
      dbType=dbType,
      dtgs=availableDtgs,
      dir=file.path(x$path, dbType),
#      obtypes=list(
#        aircraft=list(
#          v=list(
#            levelsObsmon=c(NULL),
#            levelsUsage=c(NULL)
#          )
#        )
#      ), # TEMP
      #stations=list(aircraft=c(NULL)), # TEMP
#      stations=NULL, # TEMP
      cachePaths=list(
        obsmon=file.path(x$cacheDir, sprintf('%s_obsmon.db', dbType)),
        usage=file.path(x$cacheDir, sprintf('%s_usage.db', dbType))
      )
    )
  }

  if(is.null(x$dbs$ecma) & is.null(x$dbs$ecma_sfc) & is.null(x$dbs$ccma)){
    flog.warn("Could not find any data for experiment %s. Skipping it.", name)
    return(NULL)
  }

  for(dbType in names(x$dbs)) {
    db <- x$dbs[[dbType]]
    if(is.null(db$dtgs)) next
    #paths <- future_lapply(db$dtgs, partial(pathToDataFileForDtg, db=db))
    #paths <- lapply(db$dtgs, partial(pathToDataFileForDtg, db=db))
    paths <- c()
    for(dtg in db$dtgs){
      paths <- c(paths, pathToDataFileForDtg(dtg, db))
    }
    names(paths) <- db$dtgs
    x$dbs[[dbType]]$paths=paths

    cycles <- lapply(sort(unique(db$dtgs %% 100)), partial(sprintf, "%02d"))
    x$dbs[[dbType]]$cycles <- cycles
  }

  for(dbType in names(x$dbs)) {
    if(is.null(x$dbs[[dbType]])) next
    dtgs <- x$dbs[[dbType]]$dtgs
    # The dtgs returned by getDtgs are sorted in ascending order
    x$dbs[[dbType]]$maxDateRange <- dtg2date(c(dtgs[1], dtgs[length(dtgs)]))
  }

  flog.info("Finished initialization of experiment %s.", name)
  return(x)
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
