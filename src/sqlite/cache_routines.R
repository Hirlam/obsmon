##################################################
# Routines that are related to caching in obsmon #
##################################################

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
    error=function(e) {flog.error("createCacheFiles: %s", e); -1}
  )
  return(rtn)
}


removeCacheEntries <- function(sourceDbPath, cacheDir) {
  db_type <- basename(dirname(dirname(sourceDbPath)))
  dtg <- basename(dirname(sourceDbPath))
  date <- as.integer(substr(dtg, start=1, stop=8))
  cycle <- as.integer(substr(dtg, start=9, stop=10))

  cacheDbConsCreatedHere <- c()
  on.exit(for(dbCon in cacheDbConsCreatedHere) dbDisconnectWrapper(dbCon))

  for(db_table in c('obsmon', 'usage')) {
    cacheFileName <- sprintf('%s_%s.db', db_type, db_table)
    flog.trace(
      'Reset cache (%s): Erasing data for DTG=%d%02d in cache file %s.',
      sourceDbPath, date, cycle, cacheFileName
    )
    err_msg <- sprintf(
      "Recache (%s): DTG=%d%02d may not have been reset in cache file %s",
      sourceDbPath, date, cycle, cacheFileName
    )

    cacheFilePath <- file.path(cacheDir, cacheFileName)
    tryCatch({
      con_cache <- dbConnectWrapper(cacheFilePath)
      cacheDbConsCreatedHere <- c(cacheDbConsCreatedHere, con_cache)
      dbExecute(con_cache, sprintf(
        "DELETE FROM cycles WHERE date=%d AND cycle=%d", date, cycle
      ))
    },
      error=function(e) {flog.warn(err_msg); flog.debug(e)},
      warning=function(w) {flog.warn(err_msg); flog.debug(w)}
    )
  }
}


cacheObsFromFile <- function(sourceDbPath, cacheDir, replaceExisting=FALSE) {
  ###################################
  # Main routine to perform caching #
  ###################################
  sourceDbPath <- normalizePath(sourceDbPath, mustWork=FALSE)
  anyFailedCachingAttmpt <- FALSE
  cacheDbConsCreatedHere <- c()
  allDbConsCreatedHere <- c()

  on.exit({
    if(anyFailedCachingAttmpt) {
      errMsg <- sprintf("Problems caching file %s\n", sourceDbPath)
      errMsg <- paste(errMsg, "  > Removing incomplete cache entries\n")
      flog.warn("cacheObsFromFile: %s", errMsg)
      for(con_cache in cacheDbConsCreatedHere) {
        # The foreign key contraints+triggers in con_cache will make sure all
        # eventual data cached for this (date, cycle) combination is removed
        dbExecute(con_cache, sprintf(
          "DELETE FROM cycles WHERE date=%d AND cycle=%d", date, cycle
        ))
      }
    } else {
      flog.trace("Caching: Done with file %s\n", sourceDbPath)
    }
    allDbConsCreatedHere <- c(allDbConsCreatedHere, cacheDbConsCreatedHere)
    for(dbCon in allDbConsCreatedHere) dbDisconnectWrapper(dbCon)
  })

  con <- dbConnectWrapper(sourceDbPath, read_only=TRUE)
  if(is.null(con)) {
    flog.error("Could not connect to file %s. Not caching.", sourceDbPath)
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

    con_cache <- dbConnectWrapper(cacheFilePath)
    cacheDbConsCreatedHere <- c(cacheDbConsCreatedHere, con_cache)

    # Resetting cache information if any of the following applyes:
    # (1) The user has requested it via the replaceExisting argument
    # (2) The cache entry is out-of-date, i.e., the last-modified date of the
    #     source DB file is more recent that the date when the corresponding
    #     cache entry was created
    mtimeSourceDb <- as.numeric(strftime(
      file.info(sourceDbPath)$mtime, format="%Y%m%d%H%M%S", tz="UTC"
    ))
    ctimeDTGEntry <- dbGetQuery(con_cache,
      paste('SELECT cdate_utc as cdate FROM cycles WHERE',
      sprintf('date=%d AND cycle=%d', date, cycle)
    ))
    ctimeDTGEntry <- as.numeric(ctimeDTGEntry$cdate[1])
    entryExpired <- mtimeSourceDb > ctimeDTGEntry
    overwriteCacheForDTG <- replaceExisting || isTRUE(entryExpired)
    if(overwriteCacheForDTG) removeCacheEntries(sourceDbPath, cacheDir)

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
      flog.trace("Already cached %s table of file %s\n",db_table,sourceDbPath)
      next
    } else {
      flog.trace("Caching %s table of file %s\n", db_table, sourceDbPath)
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
      errMsg <- "cacheObsFromFile: File path and cached exptDir differ:\n"
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

    # Get the observation-related tables present in the cache file
    # NB.: This assumes that the names of such tables end with "_obs"
    cache_tab_names <- dbListTables(con_cache)
    cache_obs_tab_names <- cache_tab_names[endsWith(cache_tab_names, "_obs")]

    for(cache_table in cache_obs_tab_names) {
      obCategory <- gsub("_obs$", "", cache_table)

      # Get, from the cache file, the non-DTG-related attributes
      # that should be cached
      cols <- dbListFields(con_cache, cache_table)
      non_dtg_cols <- cols[!(cols %in% c("date", "cycle"))]

      # Account for differences between usage and obsmon tables w.r.t. statid
      columns <- paste0(non_dtg_cols, collapse=', ')
      if(db_table=='obsmon') {
        usedCols <- gsub('statid', 'NULL as statid', columns, fixed=TRUE)
      } else {
        # statid values in usage contain quotes, which makes querying difficult
        substStr <- paste('TRIM(statid, "', "' ", '") as statid', sep="")
        usedCols <- gsub('statid', substStr, columns, fixed=TRUE)
      }

      # We'll now query the source DB for new observations to be cached
      queryNewObs <- sprintf(
        'SELECT DISTINCT %s FROM %s WHERE dtg="%s"',
        usedCols, db_table, dtg
      )
      if('nobs_total' %in% dbListFields(con, db_table)) {
        # Obsmon-backend sometimes writes obs data in the "obsmon" table
        # even when they have nobs_total==0. Do not add these to cache.
        queryNewObs <- paste(queryNewObs, "AND nobs_total>0")
      }
      if(obCategory=='unknown_type') {
        # Separate obs that have been registered using registerObservation
        # (see observation_definitions.R file) from those that have not
        obnumbers <- getAttrFromMetadata('obnumber')
        queryNewObs <- paste(queryNewObs, 'AND obnumber NOT IN (',
          paste0(obnumbers, collapse=', '), ')'
        )
      } else {
        obnumbers <- getAttrFromMetadata('obnumber', category=obCategory)
        queryNewObs <- paste(queryNewObs, 'AND obnumber IN (',
          paste0(obnumbers, collapse=', '), ')'
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
putObsInCache <- function(sourceDbPaths, cacheDir, replaceExisting=FALSE) {
  for(sourceDbPath in sourceDbPaths) {
    tryCatch(
      cacheObsFromFile(
        sourceDbPath, cacheDir=cacheDir, replaceExisting=replaceExisting
      ),
      warning=function(w) flog.warn("putObsInCache (%s): %s",sourceDbPath, w),
      error=function(e) flog.error("putObsInCache (%s): %s", sourceDbPath, e)
    )
  }
}

rmObsFromCache <- function(sourceDbPaths, cacheDir) {
  for(sourceDbPath in sourceDbPaths) {
    tryCatch(
      removeCacheEntries(sourceDbPath, cacheDir=cacheDir),
      warning=function(w) flog.warn("rmObsFromCache (%s): %s",sourceDbPath, w),
      error=function(e) flog.error("rmObsFromCache (%s): %s", sourceDbPath, e)
    )
  }
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

getDtgQueryString <- function(dates=NULL, cycles=NULL) {
  date_str = getDateQueryString(dates)
  cycles_str = getCycleQueryString(cycles)
  return(paste(c(date_str, cycles_str), collapse=" AND "))
}

dtgsAreCached <- function(db, dtgs) {
  # Determine whether a set of dtgs is present in the cached data for a
  # particular database db belonging to an obsmon experiment.
  # Only consider a DTG to be cached if it is present in both
  # "usage" and "obsmon" cache files
  if(is.null(db) || is.null(dtgs)) return(FALSE)

  cachedDtgs <- NULL
  allDbConsCreatedHere <- c()
  on.exit({
    for(dbCon in allDbConsCreatedHere) dbDisconnectWrapper(dbCon)
  })
  for(cacheFilePath in db$cachePaths) {
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) return(FALSE)
    allDbConsCreatedHere <- c(allDbConsCreatedHere, con)
    dtgsCachedInThisTable <- tryCatch(
      dbGetQuery(con, "SELECT DISTINCT date, cycle FROM cycles"),
      error=function(e) {
        flog.trace("dtgsAreCached: %s (%s)", e, cacheFilePath)
        NULL
      },
      warning=function(w) {
        flog.trace("dtgsAreCached: %s (%s)", w, cacheFilePath)
        NULL
      }
    )
    if(is.null(dtgsCachedInThisTable)) return(FALSE)
    if(is.null(cachedDtgs)) {
      cachedDtgs <- dtgsCachedInThisTable
    } else {
      cachedDtgs <- tryCatch(
        intersect(cachedDtgs, dtgsCachedInThisTable),
        error=function(e) {flog.error("dtgsAreCached: %s", e); NULL}
      )
    }
  }
  if(is.null(cachedDtgs) || ncol(cachedDtgs)==0) return(FALSE)

  cachedDtgsAsStr <- c()
  for(iRow in seq_len(nrow(cachedDtgs))) {
    dtg <- sprintf("%d%02d",cachedDtgs[iRow,"date"],cachedDtgs[iRow,"cycle"])
    cachedDtgsAsStr <- c(cachedDtgsAsStr, dtg)
  }
  cachedDtgsAsStr <- sort(unique(cachedDtgsAsStr))

  for(dtg in sort(unique(dtgs), decreasing=TRUE)) {
    # If a DTG corresponds to a file that doesn't exist (e.g., a cycle 21
    # on the current day when it's still 16:00), then ignore it
    if(!isTRUE(file.exists(db$getDataFilePaths(dtg)))) next
    if(!(dtg %in% cachedDtgsAsStr)) return(FALSE)
  }
  return(TRUE)
}

getObnamesFromCache <- function(db, category, dates, cycles) {
  rtn <- c()
  dtgsQueryString <-  getDtgQueryString(dates, cycles)
  for(cacheFilePath in db$cachePaths) {
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    tryCatch({
        queryResult <- dbGetQuery(con, sprintf(
          "SELECT DISTINCT obname FROM %s_obs WHERE %s",
          category, dtgsQueryString
        ))
        rtn <- c(rtn, queryResult[['obname']])
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    dbDisconnectWrapper(con)
  }
  if(length(rtn)==0) rtn <- character(0)
  return(sort(unique(rtn)))
}

getObtypesFromCache <- function(db, dates, cycles) {
  rtn <- c()
  dtgsQueryString <-  getDtgQueryString(dates, cycles)
  for(cacheFilePath in db$cachePaths) {
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    tryCatch({
        dbTables <- dbListTables(con)
        for(dbTable in dbTables) {
          if(!endsWith(dbTable, '_obs')) next
          if(dbTable %in% rtn) next
          queryResult <- dbGetQuery(con, sprintf(
            "SELECT * FROM %s WHERE %s LIMIT 1",
            dbTable, dtgsQueryString
          ))
          if(nrow(queryResult)>0) rtn <- c(rtn, gsub("_obs", "", dbTable))
        }
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    dbDisconnectWrapper(con)
  }
  if(length(rtn)==0) rtn <- character(0)
  return(sort(unique(rtn)))
}

getCacheTableNameForObname <- function(db, obname) {
  # First, try to builkd the name of the table from the
  # data registered in the observation_definitions.R file
  category <- getAttrFromMetadata('category', obname=obname)
  if(!is.na(category)) {
    rtn <- sprintf("%s_obs", category)
    return(rtn)
  }

  # If the obname is not registered in the observation_definitions.R
  # file, then the above won't work. in this case, try to get the name
  # of the table from the cache database files themselves. Not as efficient
  # as the first approach, but at least the user will be able to access
  # most new obs of usual types (surface, upper air, satellite, etc) even
  # if the new observation is still not registered.
  flog.debug(
    'obname="%s" not registered in observation_definitions.R', obname
  )
  rtn <- NULL
  for(cacheFilePath in db$cachePaths) {
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    for(tableName in dbListTables(con)) {
      tryCatch({
          query = sprintf(
            "SELECT COUNT(*) from %s WHERE obname='%s'",
             tableName, obname
          )
          queryResult <- dbGetQuery(con, query)[['COUNT(*)']]
          if(queryResult>0) {
            rtn <- tableName
            break
          }
        },
        error=function(e) {flog.trace("getCacheTableNameForObname: %s", e); NULL},
        warning=function(w) {flog.trace("getCacheTableNameForObname: %s", w); NULL}
      )
    }
    dbDisconnectWrapper(con)
  }
  return(rtn)
}

getVariablesFromCache <- function(db, dates, cycles, obname, satname=NULL) {
  rtn <- c()
  dtgsQueryString <-  getDtgQueryString(dates, cycles)
  tableName <- getCacheTableNameForObname(db, obname)
  for(cacheFilePath in db$cachePaths) {
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    # Try to determine the appropriate cache table for the obname
    tryCatch({
        tableCols <- dbListFields(con, tableName)
        query <- sprintf("SELECT DISTINCT varname FROM %s WHERE %s",
          tableName, dtgsQueryString
        )
        if("obname" %in% tableCols) {
          query <- sprintf("%s AND obname='%s'", query, obname)
        }
        if(!is.null(satname)) {
          query <- sprintf("%s AND satname='%s'", query, satname)
        }
        queryResult <- dbGetQuery(con, query)
        rtn <- c(rtn, queryResult[['varname']])
      },
      error=function(e) {flog.trace("getVariablesFromCache: %s", e); NULL},
      warning=function(w) {flog.trace("getVariablesFromCache: %s", w); NULL}
    )
    dbDisconnectWrapper(con)
  }
  if(length(rtn)==0) rtn <- character(0)
  return(sort(unique(rtn)))
}

getLevelsFromCache <- function(
  db, dates, cycles, obname, varname, stations=NULL
) {
  rtn <- list(obsmon=NULL, usage=NULL, all=NULL)
  dtgsQueryString <-  getDtgQueryString(dates, cycles)
  tableName <- getCacheTableNameForObname(db, obname)
  for(odbTable in c("obsmon", "usage")) {
    if(odbTable=="obsmon" && !is.null(stations)) next
    cacheFilePath <- db$cachePaths[[odbTable]]
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    rtn[[odbTable]] <- tryCatch({
        tableCols <- dbListFields(con, tableName)
        query <- sprintf(
          "SELECT DISTINCT level FROM %s WHERE %s AND varname='%s'",
          tableName, dtgsQueryString, varname
        )
        if(("statid" %in% tableCols) && length(stations)>0) {
            statidQueryPart <- paste0("statid like '%%%", stations, "%%%'")
            statidQueryPart <- paste(statidQueryPart, collapse=" OR ")
            query <- sprintf("%s AND (%s)", query, statidQueryPart)
        }
        if("obname" %in% tableCols) {
          query <- sprintf("%s AND obname='%s'", query, obname)
        }
        query <- sprintf("%s ORDER BY level", query)
        queryResult <- dbGetQuery(con, query)
        queryResult[['level']]
      },
      error=function(e) character(0),
      warning=function(w) character(0)
    )
    dbDisconnectWrapper(con)
  }

  rtn[["all"]] <- unique(c(rtn$obsmon, rtn$usage))
  return(rtn)
}

getChannelsFromCache <- function(db, dates, cycles, satname, sensorname) {
  rtn <- c()
  dtgsQueryString <-  getDtgQueryString(dates, cycles)
  for(odbTable in c("obsmon", "usage")) {
    cacheFilePath <- db$cachePaths[[odbTable]]
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    tryCatch({
        query <- sprintf(
          "SELECT DISTINCT level FROM satem_obs WHERE
           %s AND satname='%s' AND obname='%s'
           ORDER BY level",
          dtgsQueryString, satname, sensorname
        )
        queryResult <- dbGetQuery(con, query)
        rtn <- c(rtn, queryResult[['level']])
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    dbDisconnectWrapper(con)
  }
  if(length(rtn)==0) rtn <- character(0)
  return(sort(unique(rtn)))
}

getSensornamesFromCache <- function(db, dates, cycles) {
  rtn <- c()
  dtgsQueryString <-  getDtgQueryString(dates, cycles)
  for(odbTable in c("obsmon", "usage")) {
    cacheFilePath <- db$cachePaths[[odbTable]]
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    tryCatch({
        query <- sprintf(
          "SELECT DISTINCT obname FROM satem_obs WHERE %s ORDER BY obname",
          dtgsQueryString
        )
        queryResult <- dbGetQuery(con, query)
        rtn <- c(rtn, queryResult[['obname']])
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    dbDisconnectWrapper(con)
  }
  if(length(rtn)==0) rtn <- character(0)
  return(sort(unique(rtn)))
}

getSatnamesFromCache <- function(db, dates, cycles, sensorname) {
  rtn <- c()
  dtgsQueryString <-  getDtgQueryString(dates, cycles)
  for(odbTable in c("obsmon", "usage")) {
    cacheFilePath <- db$cachePaths[[odbTable]]
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    tryCatch({
        query <- sprintf(
          "SELECT DISTINCT satname FROM satem_obs WHERE %s AND obname='%s'
           ORDER BY satname",
          dtgsQueryString, sensorname
        )
        queryResult <- dbGetQuery(con, query)
        rtn <- c(rtn, queryResult[['satname']])
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    dbDisconnectWrapper(con)
  }
  if(length(rtn)==0) rtn <- character(0)
  return(sort(unique(rtn)))
}

getScattSatnamesFromCache <- function(db, dates, cycles) {
  rtn <- c()
  dtgsQueryString <-  getDtgQueryString(dates, cycles)
  for(odbTable in c("obsmon", "usage")) {
    cacheFilePath <- db$cachePaths[[odbTable]]
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    tryCatch({
        query <- sprintf(
          "SELECT DISTINCT satname FROM scatt_obs WHERE %s ORDER BY satname",
          dtgsQueryString
        )
        queryResult <- dbGetQuery(con, query)
        rtn <- c(rtn, queryResult[['satname']])
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )
    dbDisconnectWrapper(con)
  }
  if(length(rtn)==0) rtn <- character(0)
  return(sort(unique(rtn)))
}

getStationsFromCache <- function(
    db, dates, cycles, obname, variable, satname=NULL
) {
  rtn <- c()
  dtgsQueryString <-  getDtgQueryString(dates, cycles)
  tableName <- getCacheTableNameForObname(db, obname)
  for(cacheFilePath in db$cachePaths) {
    con <- dbConnectWrapper(cacheFilePath, read_only=TRUE, showWarnings=FALSE)
    if(is.null(con)) next
    tryCatch({
        tableCols <- dbListFields(con, tableName)
        query <- sprintf(
          "SELECT DISTINCT statid FROM %s WHERE %s",
          tableName, dtgsQueryString
        )
        if("obname" %in% tableCols) {
          query <- sprintf("%s AND obname='%s'", query, obname)
        }
        if(!is.null(satname)) {
          query <- sprintf("%s AND satname='%s'", query, satname)
        }
        query <- sprintf("%s AND varname='%s'", query, variable)
        queryResult <- dbGetQuery(con, query)
        rtn <- c(rtn, queryResult[['statid']])
      },
      error=function(e) {flog.trace("getStationsFromCache: %s", e); NULL},
      warning=function(w) NULL
    )
    dbDisconnectWrapper(con)
  }
  if(length(rtn)==0) rtn <- character(0)
  return(sort(unique(rtn)))
}

# Routines to be used in server.R to get cached values if available
# or return defaults otherwise
combineCachedAndGeneralChoices <- function(attr) {
  combinedChoices <- c(attr$cached, attr$general)
  if(is.null(names(combinedChoices))) {
    combinedChoices <- unique(combinedChoices)
  } else {
    combinedChoices <- combinedChoices[!duplicated(names(combinedChoices))]
  }
  return(combinedChoices)
}

getObnames <- function(db, category, dates, cycles) {
  cached <- getObnamesFromCache(db, category, dates, cycles)
  general <- getAttrFromMetadata('obname', category=category)
  if(length(general)>0) names(general) <- lapply(general, strLowDashToTitle)
  if(length(cached)>0) names(cached) <- lapply(cached, strLowDashToTitle)
  return(list(cached=cached, general=general))
}

getObtypes <- function(db, dates, cycles) {
  # Sort in decreasing order so that, for instance, "Upper air" will be shown
  # before "Radar"
  cached <- sort(getObtypesFromCache(db, dates, cycles), decreasing=TRUE)
  general <- sort(getAttrFromMetadata('category'), decreasing=TRUE)
  if(!is.null(general)) names(general) <- lapply(general, strLowDashToTitle)
  if(!is.null(cached)) names(cached) <- lapply(cached, strLowDashToTitle)
  return(list(cached=cached, general=general))
}

getVariables <- function(db, dates, cycles, obname, satname=NULL) {
  cached <- getVariablesFromCache(db, dates, cycles, obname, satname)
  general <- getAttrFromMetadata('variables', obname=obname)
  return(list(cached=cached, general=general))
}

getAvailableSensornames <- function(db, dates, cycles) {
  cached <- getSensornamesFromCache(db, dates, cycles)
  general <- getSensorNamesFromMetadata()
  return(list(cached=cached, general=general))
}

getAvailableSatnames <- function(db, dates, cycles, sensorname) {
  cached <- getSatnamesFromCache(db, dates, cycles, sensorname)
  general <- getSatelliteNamesFromMetadata(sensorname)
  return(list(cached=cached, general=general))
}

getAvailableScattSatnames <- function(db, dates, cycles) {
  cached <- getScattSatnamesFromCache(db, dates, cycles)
  general <- getScattSatnamesFromMetadata()
  return(list(cached=cached, general=general))
}
