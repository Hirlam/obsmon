getDtgs <- function(path, date=character(0)) {
  searchPattern <- "[0-9]{10}"
  if(length(date)>0) searchPattern <- sprintf("%s{1}[0-9]{2}", date)
  dtgs <- tryCatch(
    as.integer(dir(path=path, pattern=searchPattern)),
    warning=function(w) integer(0)
  )
  return(sort(dtgs))
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
      file.path(exptDir, dbType, dtg, fname)
    },
    warning=function(w) character(0)
  )
  return(dbpath)
}

dbType2DbDescription <- list(
  "ecma"="Upper Air (3D/4D-VAR) - Screening",
  "ccma"="Upper Air (3D/4D-VAR) - Minimization",
  "ecma_sfc"="Surface (CANARI)"
)
dbTypesRecognised <- names(dbType2DbDescription)

obsmonDatabaseClass <- setRefClass("obsmonDatabase",
  fields=list(
    dbType="character",
    dir="character",
    cacheDir="character",
    exptDir=function() {dirname(dir)},
    cachePaths=function() {
      list(
        obsmon=file.path(cacheDir, sprintf('%s_obsmon.db', dbType)),
        usage=file.path(cacheDir, sprintf('%s_usage.db', dbType))
      )
    },
    dtgsPrivate="numeric",
    dtgsLastUpdated="POSIXt",
    dtgs=function() {
      tDiffSec <- Sys.time() - dtgsLastUpdated
      rtn <- dtgsPrivate
      # Update dtgs once every 60 seconds at most
      if(length(dtgsPrivate)==0 || tDiffSec>60) {
        rtn <- getDtgs(dir)
        dtgsPrivate <<- rtn
        dtgsLastUpdated <<- Sys.time()
      }
      return(rtn)
    },
    dateRange=function() {dtg2date(c(dtgs[1], dtgs[length(dtgs)]))},
    hasData=function() {length(dtgs)>0}
  ),
  methods=list(
    getDataFilePaths=function(selectedDtgs=NULL, assertExists=FALSE) {
       if(is.null(selectedDtgs)) selectedDtgs <- .self$dtgs

       # The filter below normally runs very quickly
       selectedDtgs <- selectedDtgs[selectedDtgs %in% .self$dtgs]

       rtn <- pathToDataFileForDtg(exptDir, dbType, selectedDtgs)
       # The file existence check can take a very long time depending on how
       # many there are or where they are located
       if(assertExists) rtn <- Filter(file.exists, rtn)
       return(rtn)
    }
  )
)

experimentClass <- setRefClass("experiment",
  fields=list(
    name="character",
    path="character",
    dbs="list",
    slugName=function() {slugify(name)},
    hasData=function() {!is.null(unlist(.self$dbs))},
    guiName=function() {
      nameCompl <- character(0)
      if(!.self$hasData) nameCompl <- "(no data found)"
      return(trimws(paste(.self$name, nameCompl)))
    },
    cacheDir=function() {
      file.path(obsmonConfig$general[["cacheDir"]], slugName)
    }
  ),
  methods=list(
    initialize=function(name, path, dbTypes=dbTypesRecognised) {
      .self$name <- name
      .self$path <- path
      .self$dbs <- sapply(dbTypes, function(dbType) {
        dbDir <- file.path(path, dbType)
        if(!isTRUE(file.access(dbDir, 4)==0)) {
          flog.debug("Cannot read dir %s", dbDir)
          return(NULL)
        }
        obsmonDatabaseClass(dbType=dbType, dir=dbDir, cacheDir=.self$cacheDir)
      })
    }
  )
)

initExperiments <- function(exptNames=NULL) {
  if(!is.null(exptNames)) exptNames <- slugify(exptNames)
  experiments <- list()
  slugExptNames <- c()
  for(config in obsmonConfig$experiments) {
    newExpt <- experimentClass(name=config$displayName, path=config$path)
    if(!is.null(exptNames) && !(newExpt$slugName %in% exptNames)) next
    if(newExpt$slugName %in% slugExptNames) {
      flog.error(
        'Conflicting name for experiment "%s". Skipping additional entry.',
        newExpt$slugName
      )
      next
    }
    slugExptNames <- c(slugExptNames, newExpt$slugName)
    experiments[[newExpt$name]] <- newExpt
  }
  experiments
}
