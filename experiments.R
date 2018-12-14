getDtgs <- function(path) {
  dtgs <- tryCatch({
      foundDtgs <- as.integer(dir(path=path, pattern="[0-9]{10}"))
      foundDtgs <- sort(unique(foundDtgs))
      foundDtgs
    },
    error=function(e) {flog.error(e); NULL},
    warning=function(w) {flog.warn(w); foundDtgs}
  )
  if(length(dtgs)==0) dtgs <- NULL
  return(dtgs)
}


getAvailableCycles <- function(db, dates) {
  foundDtgs <- c()
  for(date in dates) {
    searchPattern <- sprintf("%s{1}[0-9]{2}", date)
    newDtgs <- as.integer(dir(path=db$dir, pattern=searchPattern))
    foundDtgs <- c(foundDtgs, newDtgs)
  }
  cycles <- c()
  for(dtg in foundDtgs) cycles <- c(cycles, sprintf("%02d", dtg %% 100))
  return(sort(unique(cycles)))
}

pathToDataFileForDtg <- function(dtg, db) {
  dbpath <- tryCatch({
      fname <- gsub('_sfc', '', paste0(db$dbType, '.db'), fixed=TRUE)
      fpath <- file.path(db$dir, dtg, fname)
      fpath
    },
    error=function(e) {flog.error(e); NULL},
    warning=function(w) {flog.warn(w); fpath}
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
      exptName=name,
      dbType=dbType,
      dtgs=availableDtgs,
      dir=file.path(x$path, dbType),
      cacheDir=x$cacheDir,
      cachePaths=list(
        obsmon=normalizePath(
          file.path(x$cacheDir, sprintf('%s_obsmon.db', dbType)),
          mustWork=FALSE
        ),
        usage=normalizePath(
          file.path(x$cacheDir, sprintf('%s_usage.db', dbType)),
          mustWork=FALSE
        )
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

exptNamesinConfig <- c()
for(config in obsmonConfig$experiments) {
  exptNamesinConfig <- c(exptNamesinConfig, config$displayName)
}

experimentsAsPromises <- initExperimentsAsPromises()
