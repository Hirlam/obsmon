##################################################
# Routines that are related to caching in obsmon #
##################################################

# Define attributes that will be cached and which will be used, along with
# a DTG, to identify the observations
obsKeyAttributes <- list(
  upper_air=c('statid', 'obname', 'varname', 'level'),
  surface=c('statid', 'obname', 'varname'),
  satem=c('satname', 'obname', 'level'),
  scatt=c('statid', 'varname'),
  radar=c('statid', 'varname', 'level'),
  unknown_type=c('statid', 'obname', 'varname', 'satname', 'level')
)
# Cache file locking
cacheFileLocks <- list()
lockSignalingFpath <- function(fpath) sprintf("%s.lock", fpath)
lockCacheFile <- function(fpath) {
  cacheFileLocks[[fpath]] <<- lock(lockSignalingFpath(fpath))
}
unlockCacheFile <- function(fpath) {
  unlock(cacheFileLocks[[fpath]])
  cacheFileLocks[[fpath]] <<- NULL
}
cacheFileIsLocked <- function(fpath) {
  cacheFileLock <- cacheFileLocks[[fpath]]
  if(is.null(cacheFileLock)) isLocked <- FALSE
  else isLocked <- is.locked(cacheFileLock)
  return(isLocked)
}

createCacheFiles <- function(cacheDir, dbType=dbTypesRecognised, reset=FALSE){
  # Routine to initialise cache files
  rtn <- tryCatch({
      if(reset) {
        flog.warn(paste0("createCacheFiles: Cache reset requested. ",
          sprintf("Recursively removing cache directory\n%s\n", cacheDir),
          "The directory will be recreated and cache files will be restarted."
        ))
        unlink(cacheDir, recursive=TRUE)
      }
      dir.create(cacheDir, recursive=TRUE, showWarnings=FALSE)

      cacheTemplateFilesDir <- file.path("src", "sqlite", "cache_template")
      cacheTemplate <- file.path(cacheTemplateFilesDir, 'cache_db_template.db')
      dbTables <- c('obsmon', 'usage')
      cacheFilePaths <- file.path(cacheDir, sprintf("%s.db",
        apply(expand.grid(dbType,dbTables),1,function(x)paste(x,collapse="_"))
      ))
      file.copy(cacheTemplate, cacheFilePaths, overwrite=FALSE)
      0
    },
    error=function(e) {flog.error(e); -1}
  )
  return(rtn)
}

cacheFilesLatestMdate <- function(db) {
  # Latest modified date of cache files for a given db
  mtimes <- c(-1)
  for(cacheFilePath in db$cachePaths) {
    mtime <- tryCatch(
      file.mtime(cacheFilePath),
      error=function(e) NULL,
      warning=function(w) NULL
    )
    mtimes <- c(mtimes, mtime)
  }
  return(max(mtimes))
}

getFilePathsToCache <- function(db, dtgs) {
  # Returns a selection of data file paths to be screened to info to be put
  # in the cache, as a function of the passed active db and dtgs
  validDtgs <- NULL
  for(dtg in dtgs) {
    fPath <- db$paths[dtg]
    if(is.null(fPath) || is.na(fPath) || length(fPath)==0) next
    validDtgs <- c(validDtgs, dtg)
  }
  fPathsToCache <- tryCatch(
    db$paths[validDtgs],
    error=function(e) {flog.error(e); NULL}
  )
  if(length(fPathsToCache)==0) fPathsToCache <- NULL
  return(fPathsToCache)
}

putObservationsInCache <- function(sourceDbPath, cacheDir, replaceExisting=FALSE) {
  ###################################
  # Main routine to perform caching #
  ###################################
  sourceDbPath <- normalizePath(sourceDbPath, mustWork=FALSE)
  anyFailedCachingAttmpt <- FALSE
  cacheDbConsCreatedHere <- c()
  allDbConsCreatedHere <- c()
  fPathsLockedHere <- c()

  on.exit({
      if(anyFailedCachingAttmpt) {
        errMsg <- sprintf("Problems caching file %s\n", sourceDbPath)
        errMsg <- paste(errMsg, "  > Removing incomplete cache entries\n")
        flog.warn(errMsg)
        for(con_cache in cacheDbConsCreatedHere) {
          # The foreign key contraints+triggers in con_cache will make sure all
          # eventual data cached for this (date, cycle) combination is removed
          dbExecute(con_cache, sprintf(
            "DELETE FROM cycles WHERE date=%d AND cycle=%d", date, cycle
          ))
        }
      } else {
        flog.debug(sprintf("Caching: Done with file %s\n", sourceDbPath))
      }
      allDbConsCreatedHere <- c(allDbConsCreatedHere, cacheDbConsCreatedHere)
      for(dbCon in allDbConsCreatedHere) dbDisconnect(dbCon)
      for(lockedFName in fPathsLockedHere) unlockCacheFile(lockedFName)
    }
  )

  con <- dbConnectWrapper(sourceDbPath, read_only=TRUE)
  if(is.null(con)) {
    flog.error(sprintf("Could not connect to file %s. Not caching.", sourceDbPath))
    return(-1)
  }
  allDbConsCreatedHere <- c(allDbConsCreatedHere, con)

  db_type <- basename(dirname(dirname(sourceDbPath)))
  dtg <- basename(dirname(sourceDbPath))
  date <- as.integer(substr(dtg, start=1, stop=8))
  cycle <- as.integer(substr(dtg, start=9, stop=10))

  createCacheStatus <- createCacheFiles(cacheDir, db_type)
  if(createCacheStatus!=0) {
    flog.error(paste0(
      sprintf("Problems creating %s cache in dir %s.\n", db_type, cacheDir),
      "Not caching"
    ))
    return(-1)
  }

  for(db_table in c('obsmon', 'usage')) {
    cacheFileName <- sprintf('%s_%s.db', db_type, db_table)
    cacheFilePath <- file.path(cacheDir, cacheFileName)

    hadToWait <- FALSE
    while(cacheFileIsLocked(cacheFilePath)) {
      hadToWait <- TRUE
      msg <- sprintf("Caching (%s): Waiting for cache file %s to be unlocked",
        sourceDbPath, cacheFilePath
      )
      flog.debug(msg)
      Sys.sleep(5)
    }
    if(hadToWait) {
      msg <- sprintf("Caching (%s): Cache file %s UNLOCKED. Proceeding.",
        sourceDbPath, cacheFilePath
      )
      flog.debug(msg)
    }

    lockCacheFile(cacheFilePath)
    fPathsLockedHere <- c(fPathsLockedHere, cacheFilePath)

    con_cache <- dbConnectWrapper(cacheFilePath)
    cacheDbConsCreatedHere <- c(cacheDbConsCreatedHere, con_cache)

    # The user may want to re-cache observation (e.g., if they think that the cached
    # information is incomplete)
    if(replaceExisting) {
      flog.debug(sprintf(
        "Recaching (%s). Removing DTG=%d%02d from %s cache.",
        sourceDbPath, date, cycle, cacheFileName
      ))
      removalSuccess <- tryCatch({
          dbExecute(con_cache, sprintf(
            "DELETE FROM cycles WHERE date=%d AND cycle=%d", date, cycle
          ))
          TRUE
        },
        error=function(e) {flog.debug(e); FALSE},
        warning=function(w) {flog.debug(w); FALSE}
      )
      if(!removalSuccess) {
        flog.debug(sprintf(
          "Recaching warning (%s): DTG=%d%02d may not have been removed from %s cache.",
          sourceDbPath, date, cycle, cacheFileName
        ))
      }
    }

    # Do not attempt to cache stuff that has already been cached
    alreadyCached <- tryCatch({
        nOccurenciesThisDTG <- dbGetQuery(con_cache,
          paste('SELECT count(1) as nOcc FROM cycles WHERE',
          sprintf('date=%d AND cycle=%d', date, cycle)
        ))
        nOccurenciesThisDTG$nOcc[1] != 0
      },
      warning=function(w) FALSE,
      error=function(e) FALSE
    )
    if(alreadyCached) {
      flog.debug(sprintf("Already cached %s table of file %s\n", db_table, sourceDbPath))
      next
    } else {
      flog.debug(sprintf("Caching %s table of file %s\n", db_table, sourceDbPath))
    }

    # Register experiment as existing, if not previously done
    exptDir <- dirname(dirname(dirname(sourceDbPath)))
    tryCatch(
      dbExecute(con_cache, sprintf(
        "INSERT INTO experiment(basedir,db,db_table) VALUES('%s','%s','%s')",
        exptDir, db_type, db_table
        )
      ),
      error=function(e) NULL,
      warning=function(w) NULL
    )

    # Warning user if file to be cached seems to come from a different
    # directory than the one originally rigistered
    exptCachedDir <- tryCatch({
        exptCachedDiriQueryResult <- dbGetQuery(con_cache,
          'SELECT basedir FROM experiment ORDER BY mdate_utc DESC LIMIT 1'
        )
        normalizePath(exptCachedDiriQueryResult$basedi[1])
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    if(is.null(exptCachedDir) || !(startsWith(sourceDbPath, exptCachedDir))) {
      errMsg <- "putObservationsInCache: File path and cached exptDir differ:\n"
      errMsg <- paste0(errMsg, '    > Cached exptDir: ', exptCachedDir, '\n')
      errMsg <- paste0(errMsg, '    > File path: ', sourceDbPath, '\n')
      errMsg <- paste0(errMsg, ' You may want to double-check that this is OK.', '\n')
      flog.warn(errMsg)
    }

    # Register date and cycle as existing, if not previously done:
    # (i) Date
    tryCatch(
      dbExecute(con_cache, sprintf(
        'INSERT OR IGNORE INTO dates(date) values(%d);', date
      )),
      warning=function(w) NULL,
      error=function(e) {flog.error(e$message); NULL}
    )
    # (ii) Cycle
    tryCatch(
      dbExecute(con_cache, sprintf(
        'INSERT OR IGNORE INTO cycles(date, cycle) VALUES (%d, %d)',date,cycle
      )),
      warning=function(w) NULL,
      error=function(e) {flog.error(e$message); NULL}
    )

    for(obCategory in names(obsKeyAttributes)) {
      columns <- paste0(obsKeyAttributes[[obCategory]], collapse=', ')
      cache_table <- paste(obCategory, '_obs', sep='')

      if(db_table=='obsmon') {
        usedCols <- gsub('statid', 'NULL as statid', columns, fixed=TRUE)
      } else {
        # statid values in usage contain quotes, which makes querying difficult
        substStr <- paste('TRIM(statid, "', "' ", '") as statid', sep="")
        usedCols <- gsub('statid', substStr, columns, fixed=TRUE)
      }

      if(obCategory=='unknown_type') {
        obnumbers <- getAttrFromMetadata('obnumber')
        queryNewObs <- paste(
          'SELECT DISTINCT', usedCols, 'FROM', db_table,
          'WHERE', sprintf('dtg="%s"', dtg), 'AND',
          'obnumber NOT IN (', paste0(obnumbers, collapse=', '), ')'
        )
      } else {
        obnumbers <- getAttrFromMetadata('obnumber', category=obCategory)
        queryNewObs <- paste(
          'SELECT DISTINCT', usedCols, 'FROM', db_table,
          'WHERE', sprintf('dtg="%s"', dtg), 'AND',
          'obnumber IN (', paste0(obnumbers, collapse=', '), ')'
        )
      }
      newObs <- dbGetQuery(con, queryNewObs)
      if(nrow(newObs)==0) next

      if(!is.null(newObs$statid)) {
        # The join ops in dplyr complain about statid=NULL (obsmon table)
        # They cannot join these with the character-type ones from usage
        newObs$statid <- as.character(newObs$statid)
      }

      queryCachedObs <- paste(
        'SELECT DISTINCT', usedCols, 'FROM', cache_table,
        'WHERE', sprintf("date=%d AND cycle=%d", date, cycle)
      )
      if(!is.null(newObs$obname)) {
        commaSepObnames <- paste0(sprintf("'%s'", unique(newObs$obname)), collapse = ", ")
        queryCachedObs <- paste(queryCachedObs, 'AND', 'obname in (', commaSepObnames, ')')
      }
      cachedObs <- dbGetQuery(con_cache, queryCachedObs)

      if(nrow(cachedObs)==0) {
        rowsToAdd <- newObs
      } else {
        if(!is.null(cachedObs$statid)) {
          cachedObs$statid <- as.character(cachedObs$statid)
        }
        rowsToAdd <- anti_join(newObs, cachedObs, by=colnames(newObs))
      }
      if(nrow(rowsToAdd)>0) {
        cacheSuccess <- tryCatch({
            newColNames <- c('date', 'cycle', colnames(rowsToAdd))
            rowsToAdd$date <- date
            rowsToAdd$cycle <- cycle
            rowsToAdd <- rowsToAdd[newColNames]
            dbWriteTable(con_cache, cache_table, rowsToAdd, append=TRUE)
            TRUE
          },
          warning=function(w) {
            errMsg <- paste0(w, '\n',' > Not caching ', sourceDbPath)
            errMsg <- paste0(errMsg, '\n')
            flog.warn(errMsg)
            return(FALSE)
          },
          error=function(e) {
            errMsg <- paste0(e, '\n',' > Not caching ', sourceDbPath)
            errMsg <- paste0(errMsg, '\n')
            flog.warn(errMsg)
            return(FALSE)
          }
        )
        if(!cacheSuccess) {
          anyFailedCachingAttmpt <- TRUE
          return(-1)
        }
      }
    }
  }
  return(0)
}


# Useful wrappers
assyncPutObsInCache <- function(sourceDbPaths, cacheDir, replaceExisting=FALSE) {
  return(future({
    for(sourceDbPath in sourceDbPaths) {
      tryCatch(
        putObservationsInCache(sourceDbPath, cacheDir=cacheDir, replaceExisting=replaceExisting),
        warning=function(w) flog.warn(w$message),
        error=function(e) flog.error(e$message)
      )
    }
  }))
}

getDateQueryString <- function(dates=NULL) {
  if(is.null(dates)) return(NULL)
  if(length(dates)==1) rtn <- sprintf("date=%s", dates)
  else rtn <- sprintf("date IN (%s)", paste(dates, collapse=", "))
  return(rtn)
}

getCycleQueryString <- function(cycles=NULL) {
  if(is.null(cycles)) return(NULL)
  if(length(cycles)==1) rtn <- sprintf("cycle=%s", cycles)
  else rtn <- sprintf("cycle IN (%s)", paste(cycles, collapse=", "))
  return(rtn)
}


dtgsAreCached <- function(db, dtgs) {
  # Get all cached DTGs
  cachedDtgs <- NULL
  for(cacheFilePath in db$cachePaths) {
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) return(FALSE)
    newCachedDtgs <- tryCatch({
        dbGetQuery(con,
          "SELECT DISTINCT date, cycle FROM cycles"
        )
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    if(is.null(cachedDtgs)) {
      cachedDtgs <- newCachedDtgs
    } else {
      cachedDtgs <- intersect(cachedDtgs, newCachedDtgs)
    }
    dbDisconnect(con)
    if(is.null(newCachedDtgs)) break
  }
  if(is.null(cachedDtgs) || ncol(cachedDtgs)==0) return(FALSE)

  cachedDtgsAsInt <- c()
  for(iRow in seq_len(nrow(cachedDtgs))) {
    dtg <- sprintf("%d%02d",cachedDtgs[iRow,"date"],cachedDtgs[iRow,"cycle"])
    cachedDtgsAsInt <- c(cachedDtgsAsInt, as.integer(dtg))
  }

  cachedDtgsAsInt <- sort(unique(cachedDtgsAsInt))
  dtgs <- sort(unique(as.integer(dtgs)))
  for(dtg in dtgs) {
    if(!(dtg %in% cachedDtgsAsInt)) return(FALSE)
  }
  return(TRUE)
}

getObnamesFromCache <- function(db, category, dates, cycles) {
  rtn <- c()
  dateQueryString <- getDateQueryString(dates)
  cycleQueryString <- getCycleQueryString(cycles)

  for(cacheFilePath in db$cachePaths) {
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    tryCatch({
        queryResult <- dbGetQuery(con, sprintf(
          "SELECT DISTINCT obname FROM %s_obs WHERE %s AND %s",
          category, dateQueryString, cycleQueryString
        ))
        rtn <- c(rtn, queryResult[['obname']])
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    dbDisconnect(con)
  }
  if(length(rtn)==0) rtn <- NULL
  return(sort(unique(rtn)))
}

getObtypesFromCache <- function(db, dates, cycles) {
  rtn <- c()
  dateQueryString <- getDateQueryString(dates)
  cycleQueryString <- getCycleQueryString(cycles)

  for(cacheFilePath in db$cachePaths) {
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    tryCatch({
        dbTables <- dbListTables(con)
        for(dbTable in dbTables) {
          if(!endsWith(dbTable, '_obs')) next
          if(dbTable %in% rtn) next
          queryResult <- dbGetQuery(con, sprintf(
            "SELECT * FROM %s WHERE %s AND %s LIMIT 1",
            dbTable, dateQueryString, cycleQueryString
          ))
          if(nrow(queryResult)>0) rtn <- c(rtn, gsub("_obs", "", dbTable))
        }
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    dbDisconnect(con)
  }
  if(length(rtn)==0) rtn <- NULL
  return(sort(unique(rtn)))
}

getVariablesFromCache <- function(db, dates, cycles, obname) {
  rtn <- c()
  dateQueryString <- getDateQueryString(dates)
  cycleQueryString <- getCycleQueryString(cycles)

  category <- getAttrFromMetadata('category', obname=obname)
  tableName <- sprintf("%s_obs", category)

  for(cacheFilePath in db$cachePaths) {
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    tryCatch({
        tableCols <- dbListFields(con, tableName)
        query <- sprintf("SELECT DISTINCT varname FROM %s WHERE %s AND %s",
          tableName, dateQueryString, cycleQueryString
        )
        if("obname" %in% tableCols) {
          query <- sprintf("%s AND obname='%s'", query, obname)
        }
        queryResult <- dbGetQuery(con, query)
        rtn <- c(rtn, queryResult[['varname']])
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    dbDisconnect(con)
  }
  if(length(rtn)==0) rtn <- NULL
  return(sort(unique(rtn)))
}

getLevelsFromCache <- function(db, dates, cycles, obname, varname) {
  rtn <- list(obsmon=NULL, usage=NULL, all=NULL)

  dateQueryString <- getDateQueryString(dates)
  cycleQueryString <- getCycleQueryString(cycles)
  category <- getAttrFromMetadata('category', obname=obname)
  tableName <- sprintf("%s_obs", category)

  for(odbTable in c("obsmon", "usage")) {
    cacheFilePath <- db$cachePaths[[odbTable]]
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    rtn[[odbTable]] <- tryCatch({
        tableCols <- dbListFields(con, tableName)
        query <- sprintf("SELECT DISTINCT level FROM %s WHERE %s AND %s AND varname='%s'",
          tableName, dateQueryString, cycleQueryString, varname
        )
        if("obname" %in% tableCols) {
          query <- sprintf("%s AND obname='%s'", query, obname)
        }
        query <- sprintf("%s ORDER BY level", query)
        queryResult <- dbGetQuery(con, query)
        queryResult[['level']]
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    dbDisconnect(con)
  }

  rtn[["all"]] <- unique(c(rtn$obsmon, rtn$usage))
  return(rtn)
}

getChannelsFromCache <- function(db, dates, cycles, satname, sensorname) {
  rtn <- c()

  dateQueryString <- getDateQueryString(dates)
  cycleQueryString <- getCycleQueryString(cycles)

  for(odbTable in c("obsmon", "usage")) {
    cacheFilePath <- db$cachePaths[[odbTable]]
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    tryCatch({
        query <- sprintf(
          "SELECT DISTINCT level FROM satem_obs WHERE
           %s AND %s AND satname='%s' AND obname='%s'
           ORDER BY level",
          dateQueryString, cycleQueryString, satname, sensorname
        )
        queryResult <- dbGetQuery(con, query)
        rtn <- c(rtn, queryResult[['level']])
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    dbDisconnect(con)
  }
  return(rtn)
}

getSensornamesFromCache <- function(db, dates, cycles) {
  rtn <- c()

  dateQueryString <- getDateQueryString(dates)
  cycleQueryString <- getCycleQueryString(cycles)

  for(odbTable in c("obsmon", "usage")) {
    cacheFilePath <- db$cachePaths[[odbTable]]
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    tryCatch({
        query <- sprintf(
          "SELECT DISTINCT obname FROM satem_obs WHERE %s AND %s
           ORDER BY obname",
          dateQueryString, cycleQueryString
        )
        queryResult <- dbGetQuery(con, query)
        rtn <- c(rtn, queryResult[['obname']])
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    dbDisconnect(con)
  }
  if(length(rtn)==0) rtn <- NULL
  return(sort(unique(rtn)))
}

getSatnamesFromCache <- function(db, dates, cycles, sensorname) {
  rtn <- c()

  dateQueryString <- getDateQueryString(dates)
  cycleQueryString <- getCycleQueryString(cycles)

  for(odbTable in c("obsmon", "usage")) {
    cacheFilePath <- db$cachePaths[[odbTable]]
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    tryCatch({
        query <- sprintf(
          "SELECT DISTINCT satname FROM satem_obs WHERE %s AND %s
           AND obname='%s'
           ORDER BY satname",
          dateQueryString, cycleQueryString, sensorname
        )
        queryResult <- dbGetQuery(con, query)
        rtn <- c(rtn, queryResult[['satname']])
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    dbDisconnect(con)
  }
  if(length(rtn)==0) rtn <- NULL
  return(sort(unique(rtn)))
}

getStationsFromCache <- function(db, dates, cycles, obname, variable) {
  rtn <- c()

  dateQueryString <- getDateQueryString(dates)
  cycleQueryString <- getCycleQueryString(cycles)

  category <- getAttrFromMetadata('category', obname=obname)
  tableName <- sprintf("%s_obs", category)

  for(cacheFilePath in db$cachePaths) {
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    tryCatch({
        tableCols <- dbListFields(con, tableName)
        query <- sprintf("SELECT DISTINCT statid FROM %s WHERE %s AND %s",
          tableName, dateQueryString, cycleQueryString
        )
        if("obname" %in% tableCols) {
          query <- sprintf("%s AND obname='%s'", query, obname)
        }
        query <- sprintf("%s AND varname='%s'", query, variable)
        queryResult <- dbGetQuery(con, query)
        rtn <- c(rtn, queryResult[['statid']])
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    dbDisconnect(con)
  }
  if(length(rtn)==0) rtn <- NULL
  return(sort(unique(rtn)))
}

# Routines to be used in server.R to get cached values if available
# or return defaults otherwise
getObnames <- function(db, category, dates, cycles) {
  rtn <- list(cached=NULL, general=NULL)
  rtn$cached <- getObnamesFromCache(db, category, dates, cycles)
  rtn$general <- getAttrFromMetadata('obname', category=category)
  return(rtn)
}

getObtypes <- function(db, dates, cycles) {
  rtn <- list(cached=NULL, general=NULL)
  rtn$cached <- getObtypesFromCache(db, dates, cycles)
  rtn$general <- getAttrFromMetadata('category')
  return(rtn)
}

getVariables <- function(db, dates, cycles, obname) {
  rtn <- list(cached=NULL, general=NULL)
  rtn$cached <- getVariablesFromCache(db, dates, cycles, obname)
  rtn$general <- getAttrFromMetadata('variables', obname=obname)
  return(rtn)
}

getAvailableChannels <- function(db, dates, cycles, satname, sensorname) {
  rtn <- getChannelsFromCache(db, dates, cycles, satname, sensorname)
  return(rtn)
}

getAvailableLevels <- function(db, dates, cycles, obname, varname) {
  rtn <- getLevelsFromCache(db, dates, cycles, obname, varname)
  return(rtn)
}

getAvailableSensornames <- function(db, dates, cycles) {
  rtn <- list(cached=NULL, general=NULL)
  rtn$cached <- getSensornamesFromCache(db, dates, cycles)
  sens.sats <- getAttrFromMetadata('sensors.sats', category="satem")
  rtn$general <- gsub('\\.{1}.*', '', sens.sats)
  return(rtn)
}

getAvailableSatnames <- function(db, dates, cycles, sensorname) {
  rtn <- list(cached=NULL, general=NULL)
  rtn$cached <- getSatnamesFromCache(db, dates, cycles, sensorname)
  sens.sats <- getAttrFromMetadata('sensors.sats', category="satem")
  sens.sats <- sens.sats[startsWith(sens.sats, paste0(sensorname, '.'))]
  rtn$general <- gsub(paste0(sensorname, '.'), '', sens.sats, fixed=TRUE)
  return(rtn)
}
