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

pathToDataFileForDtg <- function(exptDir, dtg, dbType) {
  dbpath <- tryCatch({
      fname <- gsub('_sfc', '', paste0(dbType, '.db'), fixed=TRUE)
      fpath <- file.path(exptDir, dtg, fname)
      fpath
    },
    error=function(e) {flog.error(e); NULL},
    warning=function(w) {flog.warn(w); fpath}
  )
  return(dbpath)
}

emptyExperiment <- function(name) {
  x <- list()
  x$name <- name
  x$dbs$ecma <- NULL
  x$dbs$ecmaSfc <- NULL
  x$dbs$ccma <- NULL
  x
}

initExperiment <- function(name, baseDir, experiment) {

  flog.debug("Initializing experiment %s...", name)
  x <- list()
  x$name <- name
  x$path <- file.path(baseDir, experiment)
  x$cacheDir <- file.path(obsmonConfig$general[["cacheDir"]], slugify(name))
  x$dbs <- list(ccma=NULL, ecma=NULL, ecma_sfc=NULL)
  for(dbType in names(x$dbs)) {
    # The dtgs returned by getDtgs are sorted in ascending order
    dtgs <- getDtgs(file.path(x$path, dbType))
    if(is.null(dtgs)) next
    exptDir <- file.path(x$path, dbType)
    # Set paths to experiment data files
    x$dbs[[dbType]] <- list(
      exptName=name,
      dbType=dbType,
      dtgs=dtgs,
      maxDateRange=dtg2date(c(dtgs[1], dtgs[length(dtgs)])),
      dir=file.path(x$path, dbType),
      # Set dirs where obsmon expects to find experiment data for each dtg
      paths=structure(
        lapply(dtgs, partial(pathToDataFileForDtg, exptDir=exptDir, dbType=dbType)),
        names=dtgs
      ),
      # Paths related to caching
      cacheDir=x$cacheDir,
      cachePaths=list(
        obsmon=file.path(x$cacheDir, sprintf('%s_obsmon.db', dbType)),
        usage=file.path(x$cacheDir, sprintf('%s_usage.db', dbType))
      )
    )
  }

  if(is.null(x$dbs$ecma) & is.null(x$dbs$ecma_sfc) & is.null(x$dbs$ccma)){
    flog.warn("Could not find data for experiment %s. Skipping.", name)
    return(NULL)
  }
  flog.debug("Finished initialization of experiment %s.", name)

  return(x)
}

initExperimentsAsPromises <- function() {
  # Using new.env(), as lists cannot be used with %<-%
  experiments <- new.env()
  for(config in obsmonConfig$experiments) {
    name <- config$displayName
    # Using %<-% (library "future") to init experiments asynchronously
    experiments[[name]] %<-%
      initExperiment(name, config$baseDir, config$experiment)
  }
  experiments
}

exptNamesinConfig <- c()
for(config in obsmonConfig$experiments) {
  exptNamesinConfig <- c(exptNamesinConfig, config$displayName)
}

experimentsAsPromises <- initExperimentsAsPromises()
