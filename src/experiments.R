dbTypesRecognised <- c("ecma", "ccma", "ecma_sfc")
dbType2DbDescription <- function(dbType) {
  dbType <- as.character(dbType)
  rtn <- switch(dbType,
    "ecma"="Upper Air (3D/4D-VAR) - Screening",
    "ccma"="Upper Air (3D/4D-VAR) - Minimization",
    "ecma_sfc"="Surface (CANARI)",
    dbType
  )
  return(rtn)
}

obsmonDatabaseClass <- setRefClass("obsmonDatabase",
  fields=list(
    dbType="character",
    dir="character",
    cacheDir="character",
    exptName="character",
    # Attributes that use accessor functions
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
      tDiffSec <- Sys.time() - .self$dtgsLastUpdated
      rtn <- .self$dtgsPrivate
      # Update dtgs at most once every 1 second to avoid unnecessary
      # repeated function calls (since other methods also use dtgs)
      if(length(.self$dtgsPrivate)==0 || isTRUE(tDiffSec>1)) {
        rtn <- .self$getDtgs()
        .self$dtgsPrivate <- rtn
        .self$dtgsLastUpdated <- Sys.time()
      }
      return(rtn)
    },
    dateRange=function() {dtg2date(c(dtgs[1], dtgs[length(dtgs)]))},
    hasDtgs=function() {length(dtgs)>0}
  ),
  methods=list(
    initialize=function(dbType, dir, cacheDir, exptName=character(0)) {
      .self$dbType <- dbType
      .self$dir <- dir
      .self$cacheDir <- cacheDir
      .self$exptName <- exptName
    },
    getDataFilePaths=function(selectedDtgs=NULL, assertExists=FALSE) {
      if(is.null(selectedDtgs)) selectedDtgs <- .self$dtgs
      # The filter below normally runs very quickly
      selectedDtgs <- selectedDtgs[selectedDtgs %in% .self$dtgs]

      dbPaths <- tryCatch({
        fname <- gsub('_sfc', '', paste0(.self$dbType, '.db'), fixed=TRUE)
        file.path(.self$exptDir, .self$dbType, selectedDtgs, fname)
        },
        warning=function(w) character(0)
      )

      # The file existence check can take a very long time depending on how
      # many there are or where they are located
      if(assertExists) dbPaths <- Filter(file.exists, dbPaths)
      return(dbPaths)
    },
    getDtgs=function(dates=character(0)) {
      searchPatts <- "[0-9]{10}"
      if(length(dates)>0) searchPatts <- sprintf("%s{1}[0-9]{2}", dates)
      rtn <- lapply(searchPatts, function(pt) dir(path=.self$dir, pattern=pt))
      return(sort(as.integer(unlist(rtn))))
    },
    getAvailableCycles=function(dates) {
      selecDtgs <- .self$getDtgs(dates)
      cycles <- lapply(selecDtgs, function(dtg) sprintf("%02d", dtg %% 100))
      return(sort(unique(unlist(cycles))))
    }
  )
)

experimentClass <- setRefClass("experiment",
  fields=list(
    name="character",
    path="character",
    dbs="list",
    slugName=function() {slugify(name)},
    hasValidDbDirs=function() {
      for(db in .self$dbs) {
        if(dir.exists(db$dir)) return(TRUE)
      }
      return(FALSE)
    },
    guiName=function() {
      nameCompl <- character(0)
      if(!.self$hasValidDbDirs) nameCompl <- "(could not read data)"
      return(trimws(paste(.self$name, nameCompl)))
    },
    cacheDir=function() {
      file.path(obsmonConfig$general[["cacheDir"]], slugName)
    }
  ),
  methods=list(
    initialize=function(name=NULL, path=NULL, dbTypes=dbTypesRecognised) {
      .self$name <- ifelse(length(name)>0, as.character(name), character(0))
      .self$path <- ifelse(length(path)>0, as.character(path), character(0))
      .self$dbs <- sapply(dbTypes, function(dbType) {
        dbDir <- file.path(.self$path, dbType)
        obsmonDatabaseClass(
          dbType=dbType, dir=dbDir, cacheDir=.self$cacheDir,
          exptName=.self$name
        )
      })
    }
  )
)

initExperiments <- function(exptNames=NULL) {
  if(!is.null(exptNames)) exptNames <- slugify(exptNames)
  experiments <- list()
  slugExptNames <- c()
  for(config in obsmonConfig$experiments) {
    slugName <- slugify(config$displayName)
    if(!is.null(exptNames) && !(slugName %in% exptNames)) next
    if(slugName %in% slugExptNames) {
      flog.error(
        'Conflicting name for experiment "%s". Skipping additional entry.',
        slugName
      )
      next
    }
    slugExptNames <- c(slugExptNames, slugName)
    newExpt <- experimentClass(name=config$displayName, path=config$path)
    experiments[[newExpt$name]] <- newExpt
  }
  experiments
}
