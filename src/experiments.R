getDtgs <- function(path) {
  dtgs <- tryCatch({
      foundDtgs <- as.integer(dir(path=path, pattern="[0-9]{10}"))
      foundDtgs <- sort(unique(foundDtgs))
      foundDtgs
    },
    error=function(e) {flog.error(e); integer(0)},
    warning=function(w) {flog.warn(w); foundDtgs}
  )
  if(length(dtgs)==0) dtgs <- integer(0)
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

pathToDataFileForDtg <- function(exptDir, dbType, dtg) {
  dbpath <- tryCatch({
      fname <- gsub('_sfc', '', paste0(dbType, '.db'), fixed=TRUE)
      fpath <- file.path(exptDir, dbType, dtg, fname)
      fpath
    },
    error=function(e) {flog.error(e); NULL},
    warning=function(w) {flog.warn(w); fpath}
  )
  return(dbpath)
}

getDataFilePaths <- function(exptDir, dbType, assertExists=FALSE) {
  dtgs <- getDtgs(file.path(exptDir, dbType))
  fPaths <- NULL
  validDtgs <- NULL
  for(dtg in dtgs) {
    fPath <- pathToDataFileForDtg(exptDir, dbType, dtg)
    if(is.null(fPath)) next
    if(assertExists && !file.exists(fPath)) next
    fPaths <- c(fPaths, fPath)
    validDtgs <- c(validDtgs, dtg)
  }
  if(is.null(fPaths)) return(NULL)
  else return(structure(fPaths, names=validDtgs))
}

dbType2DbDescription <- list(
  "ecma"="Upper Air (3D/4D-VAR) - Screening",
  "ccma"="Upper Air (3D/4D-VAR) - Minimization",
  "ecma_sfc"="Surface (CANARI)"
)
dbTypesRecognised <- names(dbType2DbDescription)

emptyExperiment <- function(name) {
  x <- list()
  x$name <- name
  x$dbs <- list()
  for(dbType in dbTypesRecognised) x$dbs[[dbType]] <- NULL
  x
}

initExperiment <- function(name, path, checkFilesExist) {

  flog.debug("Initializing experiment %s...", name)
  x <- list()
  x$name <- name
  x$path <- path
  x$cacheDir <- file.path(obsmonConfig$general[["cacheDir"]], slugify(name))
  x$dbs <- list()
  for(dbType in dbTypesRecognised) {
    x$dbs[[dbType]] <- NULL
    # Making sure to only store dtgs that correspond to existing data files
    dataFilePaths<-getDataFilePaths(x$path,dbType,assertExists=checkFilesExist)
    dtgs <- sort(names(dataFilePaths))
    if(is.null(dtgs)) next
    x$dbs[[dbType]] <- list(
      dbType=dbType,
      dir=file.path(x$path, dbType),
      dtgs=dtgs,
      maxDateRange=dtg2date(c(dtgs[1], dtgs[length(dtgs)])),
      # Set paths where obsmon expects to find experiment data for each dtg
      paths=dataFilePaths,
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
    x <- NULL
  } else {
    flog.debug("Finished initialization of experiment %s.", name)
  }
  return(x)
}


initExperimentsAsPromises <- function(exptNames=NULL) {
  if(!is.null(exptNames)) {
    flog.debug(
      "initExperimentsAsPromises: Only initialising requested experiments"
    )
    exptNames <- slugify(exptNames)
  }
  # Using new.env(), as lists cannot be used with %<-%
  experiments <- new.env()
  simplifiedExptNames <- c()
  for(config in obsmonConfig$experiments) {
    name <- config$displayName
    simplifiedName <- slugify(name)
    if(!is.null(exptNames) && !(simplifiedName %in% exptNames)) {
      flog.debug(
        'initExperimentsAsPromises: Skipping expt "%s": Not in exptNames.',
        name
      )
      next
    }
    if(simplifiedName %in% simplifiedExptNames) {
      flog.error(
        'Conflicting name for experiment "%s". Skipping additional entry.',
        name
      )
      next
    }
    simplifiedExptNames <- c(simplifiedExptNames, simplifiedName)
    # Using %<-% (library "future") to init experiments asynchronously
    experiments[[name]] %<-%
      initExperiment(name, config$path,
        checkFilesExist=obsmonConfig$general[["initCheckDataExists"]]
      )
  }
  experiments
}

flagNotReadyExpts <- function(experiments) {
  # Checks whether experiments have been initialised. Those that are still
  # initialising will be flagged and replaced by empty ones (placeholders).
  # This allows using experiments that are ready even if there are others
  # that are not.
  resolvedStatus <- resolved(experiments)

  readyExpts <- list()
  notReadyExpts <- list()
  exptNames <- exptNamesinConfig[exptNamesinConfig %in% ls(experiments)]
  for (exptName in exptNames) {
    if(resolvedStatus[[exptName]]) {
      readyExpts[[exptName]] <- experiments[[exptName]]
    } else {
      newName <- paste0(exptName, ': Loading experiment...')
      notReadyExpts[[newName]] <- emptyExperiment(newName)
    }
  }
  return(c(readyExpts, notReadyExpts))
}

exptNamesinConfig <- c()
for(config in obsmonConfig$experiments) {
  exptNamesinConfig <- c(exptNamesinConfig, config$displayName)
}
