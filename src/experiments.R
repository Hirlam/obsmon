dbTypesRecognised <- c("ecma", "ccma", "ecma_sfc")
dbType2DbDescription <- function(dbType) {
  dbType <- as.character(dbType)
  rtn <- switch(dbType,
    "ecma"="Upper Air (3D/4D-VAR) - Screening",
    "ccma"="Upper Air (3D/4D-VAR) - Minimization",
    "ecma_sfc"="Surface",
    dbType
  )
  return(rtn)
}

obsmonDatabaseClass <- setRefClass("obsmonDatabase",
  fields=list(
    dbType="character",
    dir="character",
    cacheDir="character",
    exptName="character", # exptName is optional
    # dtgCache, dtgCacheExpiry and dtgCacheLastUpdated are auxiliary
    # attributes for use in the getDtgs method.
    # dtgCache is a (conveniently organised) copy of the last results returned
    # by the "dir" command called inside the getDtgs method. These cached DTGs
    # expire and are renewed if they get older than dtgCacheExpiry seconds.
    # N.B.: This internal DTG cache has nothing to do with obsmon's sql cache.
    dtgCache="list",
    dtgCacheExpiry="numeric", # In seconds
    dtgCacheLastUpdated="POSIXct",
    # Attributes that use accessor functions
    exptDir=function(...) {dirname(.self$dir)},
    cachePaths=function(...) {
      list(
        obsmon=file.path(.self$cacheDir, sprintf('%s_obsmon.db', .self$dbType)),
        usage=file.path(.self$cacheDir, sprintf('%s_usage.db', .self$dbType))
      )
    },
    dtgs=function(...) {.self$getDtgs()},
    dateRange=function(...) {dtg2date(c(.self$dtgs[1], .self$dtgs[length(.self$dtgs)]))},
    hasDtgs=function(...) {length(.self$dtgs)>0}
  ),
  methods=list(
    initialize=function(...) {
      callSuper(...)
      if(length(.self$dtgCacheLastUpdated)==0) {
        .self$dtgCacheLastUpdated <- as.POSIXct(0, origin="1970-01-01")
      }
      if(length(.self$exptName)==0) .self$exptName <- "Unnamed Experiment"
      if(length(.self$dtgCacheExpiry)==0) .self$dtgCacheExpiry <- Inf
      .self$dtgCacheExpiry <- abs(as.double(.self$dtgCacheExpiry))
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
    getDtgs=function(dates=character(0), cacheExpiry=NULL) {
      # Keep a cache of the dtgs to avoid unnecessary successive calls
      # to the "dir" function, but update the cache if it is older than
      # cacheExpiry seconds.
      if(is.null(cacheExpiry)) cacheExpiry <- .self$dtgCacheExpiry
      tDiffSec <- Sys.time() - .self$dtgCacheLastUpdated
      if(length(.self$dtgCache)==0 || isTRUE(tDiffSec>abs(cacheExpiry))) {
        flog.debug("Getting %s DTGs for %s", .self$dbType, .self$exptName)
        # There is no much gain in running "dir" for only selected dates in
        # comparison to just retrieving all available DTGs.
        allDtgs <- sort(dir(path=.self$dir, pattern="[0-9]{10}"))
        .self$dtgCache <- list()
        for(dtg in allDtgs) {
          date <- substr(dtg, 1, 8)
          .self$dtgCache[[date]] <- c(.self$dtgCache[[date]], as.integer(dtg))
        }
        .self$dtgCacheLastUpdated <- Sys.time()
        flog.debug("Done getting %s DTGs for %s\n",.self$dbType,.self$exptName)
      }

      if(length(dates)==0) rtn <- .self$dtgCache
      else rtn <- .self$dtgCache[as.character(dates)]
      if(length(rtn)==0) rtn <- integer(0)
      return(unlist(rtn, use.names=FALSE))
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
    slugName=function(...) {slugify(.self$name)},
    hasValidDbDirs=function(...) {
      for(db in .self$dbs) {
        if(dir.exists(db$dir)) return(TRUE)
      }
      return(FALSE)
    },
    guiName=function(...) {
      nameCompl <- character(0)
      if(!.self$hasValidDbDirs) nameCompl <- "(could not read data)"
      return(trimws(paste(.self$name, nameCompl)))
    },
    cacheDir=function(...) {
      file.path(obsmonConfig$general[["cacheDir"]], .self$slugName)
    }
  ),
  methods=list(
    initialize=function(
      name=NULL, path=NULL, dbTypes=dbTypesRecognised, dtgCacheExpiry=Inf
    ) {
      .self$name <- ifelse(length(name)>0, as.character(name), character(0))
      .self$path <- ifelse(length(path)>0, as.character(path), character(0))
      .self$dbs <- sapply(dbTypes, function(dbType) {
        dbDir <- file.path(.self$path, dbType)
        obsmonDatabaseClass(
          dbType=dbType, dir=dbDir, cacheDir=.self$cacheDir,
          exptName=.self$name, dtgCacheExpiry=dtgCacheExpiry
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
