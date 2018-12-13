dbConnectWrapper <- function(dbpath, read_only=FALSE, showWarnings=TRUE) {
  con <- tryCatch({
      if(read_only) {
          newCon<-dbConnect(RSQLite::SQLite(),dbpath,flags=RSQLite::SQLITE_RO)
          dbExecute(newCon, "PRAGMA  journal_mode=OFF")
          dbExecute(newCon, "PRAGMA synchronous=off")
      } else {
          newCon<-dbConnect(RSQLite::SQLite(),dbpath,flags=RSQLite::SQLITE_RW)
          dbExecute(newCon, "PRAGMA foreign_keys=ON")
      }
      dbExecute(newCon, sprintf("PRAGMA  mmap_size=%s", 1024**3))
      dbExecute(newCon, sprintf("PRAGMA  cache_size=%s", 1024**3))
      newCon
    },
    error=function(e) {
      if(showWarnings)flog.error("Error accessing %s: %s", dbpath, e$message)
      NULL
    },
    warning=function(w) {
      if(showWarnings)flog.error("Warning accessing %s: %s", dbpath, w$message)
      NULL
    }
  )
  return(con)
}

makeSingleQuery <- function(query) {
  function(dbpath) {
    con <- dbConnectWrapper(dbpath, read_only=TRUE)
    res <- tryCatch(dbGetQuery(con, query),
                    error=function(e) {
                      flog.warn("Ignoring error querying %s: %s", dbpath, e)
                      NULL
                    })
    dbDisconnect(con)
    res
  }
}

performQuery <- function(db, query, dtgs=NULL, expandRange=TRUE,
                         convertDTG=TRUE, progressTracker=NULL) {
  if (is.null(dtgs)) {
    dbpaths <- db$paths
  } else {
    if (expandRange) {
      n = length(dtgs)
      if (n==1) {
        dbpaths <- db$paths[as.character(dtgs)]
      } else if (n==3) {
        range <- expandDtgRange(dtgs)
        dbpaths <- db$paths[range[[1]] <= db$dtgs & db$dtgs <= range[[2]]]
      } else {
        flog.error("Invalid combination of expandRange and dtgs.")
      }
    } else {
      dbpaths <- db$paths[as.character(dtgs)]
    }
  }
  dbpaths[sapply(dbpaths, is.null)] <- NULL
  if (length(dbpaths)==0) {
    flog.warn("No usable database found.")
    return(NULL)
  }
  singleQuery <- makeSingleQuery(query)
  if (is.null(progressTracker)) {
    res <- pblapply(dbpaths, singleQuery)
  } else {
    nout <- 100
    split <- splitpb(length(dbpaths), 1L, nout)
    b <- length(split)
    res <- vector("list", b)
    for (i in seq_len(b)) {
      res[i] <- list(lapply(dbpaths[split[[i]]], singleQuery))
      updateTask(progressTracker, "Querying database", i/b)
    }
    res <- do.call(c, res, quote=TRUE)
  }
  res <- do.call(rbind, res)
  if(convertDTG & "DTG" %in% names(res)) {
    res$DTG <- as.POSIXct(as.character(res$DTG), format="%Y%m%d%H")
  }
  res
}

readSynopStations <- function() {
  raw <- read.table("allsynop.list.csv",
                    sep=";", quote="",
                    col.names=c("statids", "designation"),
                    colClasses="character", encoding="UTF-8")
  synopStations <- raw$designation
  names(synopStations) <- raw$statids
  synopStations
}
synopStations <- readSynopStations()

### New caching stuff starts here ####
#
# These are attributes that will be cached and which will be used, along with
# a DTG, to identify the observations
obsKeyAttributes <- list(
  upper_air=c('statid', 'obname', 'varname', 'level'),
  surface=c('statid', 'obname', 'varname'),
  satem=c('satname', 'obname', 'level'),
  scatt=c('statid', 'varname'),
  radar=c('statid', 'varname', 'level'),
  unknown_type=c('statid', 'obname', 'varname', 'satname', 'level')
)

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

putObservationsInCache <- function(sourceDbPath, cacheDir, replaceExisting=FALSE) {
  sourceDbPath <- normalizePath(sourceDbPath, mustWork=FALSE)
  anyFailedCachingAttmpt <- FALSE
  dbConnectsCreatedHere <- c()
  fPathsLockedHere <- c()

  on.exit({
      if(anyFailedCachingAttmpt) {
        errMsg <- sprintf("Problems caching file %s\n", sourceDbPath)
        errMsg <- paste(errMsg, "  > Removing incomplete cache entries\n")
        flog.error(errMsg)
        # The foreign key contraints+triggers in con_cache will make sure all
        # eventual data cached for this (date, cycle) combination is removed
        dbExecute(con_cache, sprintf(
          "DELETE FROM cycles WHERE date=%d AND cycle=%d", date, cycle
        ))
      } else {
        flog.debug(sprintf("Caching: Done with file %s\n", sourceDbPath))
      }
      for(dbCon in dbConnectsCreatedHere) dbDisconnect(dbCon)
      for(lockedFName in fPathsLockedHere) unlockCacheFile(lockedFName)
    }
  )

  con <- dbConnectWrapper(sourceDbPath, read_only=TRUE)
  if(is.null(con)) {
    flog.error(sprintf("Could not connect to file %s. Not caching.", sourceDbPath))
    return(NULL)
  }
  dbConnectsCreatedHere <- c(dbConnectsCreatedHere, con)
  dir.create(cacheDir, recursive=TRUE, showWarnings=FALSE)

  db_type <- basename(dirname(dirname(sourceDbPath)))
  dtg <- basename(dirname(sourceDbPath))
  date <- as.integer(substr(dtg, start=1, stop=8))
  cycle <- as.integer(substr(dtg, start=9, stop=10))

  for(db_table in c('obsmon', 'usage')) {
    cacheFileName <- sprintf('%s_%s.db', db_type, db_table)
    cacheFilePath <- file.path(cacheDir, cacheFileName)
    if(!file.exists(cacheFilePath)) {
      cacheTemplate <- file.path('sqlite', 'cache_db_template.db')
      file.copy(cacheTemplate, cacheFilePath)
    }

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
    dbConnectsCreatedHere <- c(dbConnectsCreatedHere, con_cache)

    # The user may want to re-cache observation (e.g., if they think that the cached
    # information is incomplete)
    if(replaceExisting) {
      flog.debug(sprintf(
        "Recaching (%s). Removing DTG=%d%d from %s cache.",
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
          "Recaching warning (%s): DTG=%d%d may not have been removed from %s cache.",
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
      flog.debug(sprintf("Caching file %s\n", sourceDbPath))
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
        tryCatch({
            newColNames <- c('date', 'cycle', colnames(rowsToAdd))
            rowsToAdd$date <- date
            rowsToAdd$cycle <- cycle
            rowsToAdd <- rowsToAdd[newColNames]
            dbWriteTable(con_cache, cache_table, rowsToAdd, append=TRUE)
          },
          warning=function(w) {
            anyFailedCachingAttmpt <- TRUE
            errMsg <- paste0(w$message, '\n',' > Not caching ', sourceDbPath)
            errMsg <- paste0(errMsg, '\n')
            flog.error(errMsg)
            return(NULL)
          },
          error=function(e) {
            anyFailedCachingAttmpt <- TRUE
            errMsg <- paste0(e$message, '\n',' > Not caching ', sourceDbPath)
            errMsg <- paste0(errMsg, '\n')
            flog.error(errMsg)
            return(NULL)
          }
        )
      }
    }
  }
}

assyncPutObsInCache <- function(sourceDbPaths, cacheDir, replaceExisting=FALSE) {
  suppressWarnings(future({
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


datesAreCached <- function(db, dates) {

  dateQueryString <- getDateQueryString(dates)
  for(cacheFilePath in db$cachePaths) {
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) return(FALSE)
    cachedDates <- tryCatch({
        queryResult <- dbGetQuery(con, sprintf(
          "SELECT DISTINCT date FROM dates WHERE %s",
          dateQueryString
        ))
        queryResult[['date']]
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    if(is.null(cachedDates)||length(cachedDates)!=length(dates)) return(FALSE)
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

# Wrappers
getObnames <- function(db, category, dates, cycles) {
  rtn <- list(cached=NULL, general=NULL)
  rtn$cached <- getObnamesFromCache(db, category, dates, cycles)
  if(is.null(rtn$cached)) {
    rtn$general <- getAttrFromMetadata('obname', category=category)
  }
  return(rtn)
}

getObtypes <- function(db, dates, cycles) {
  rtn <- list(cached=NULL, general=NULL)
  rtn$cached <- getObtypesFromCache(db, dates, cycles)
  if(is.null(rtn$cached)) rtn$general <- getAttrFromMetadata('category')
  return(rtn)
}

getVariables <- function(db, dates, cycles, obname) {
  rtn <- list(cached=NULL, general=NULL)
  rtn$cached <- getVariablesFromCache(db, dates, cycles, obname)
  if(is.null(rtn$cached)) rtn$general <- getAttrFromMetadata('variables', obname=obname)
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
  if(is.null(rtn$cached)) {
    sens.sats <- getAttrFromMetadata('sensors.sats', category="satem")
    rtn$general <- gsub('\\.{1}.*', '', sens.sats)
  }
  return(rtn)
}

getAvailableSatnames <- function(db, dates, cycles, sensorname) {
  rtn <- list(cached=NULL, general=NULL)
  rtn$cached <- getSatnamesFromCache(db, dates, cycles, sensorname)
  if(is.null(rtn$cached)) {
    sens.sats <- getAttrFromMetadata('sensors.sats', category="satem")
    sens.sats <- sens.sats[startsWith(sens.sats, paste0(sensorname, '.'))]
    rtn$general <- gsub(paste0(sensorname, '.'), '', sens.sats, fixed=TRUE)
  }
  return(rtn)
}
