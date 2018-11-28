dbConnectWrapper <- function(dbpath, read_only=FALSE) {
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
      flog.error("Error accessing %s: %s", dbpath, e$message)
      NULL
    },
    warning=function(w) {
      flog.error("Warning accessing %s: %s", dbpath, w$message)
      NULL
    }
  )
  return(con)
}

prepareConnections <- function(db) {
  makePath <- function(dtg) {
    dbpath <- file.path(db$dir, dtg, db$file)
#    if(file.size(dbpath) %in% c(NA, 0.)) {
#      flog.debug("Missing or empty DB file: %s", dbpath)
#      dbpath <- NULL
#    } 
    return(dbpath)
  }
  db$paths <- future_lapply(db$dtgs, makePath)
  names(db$paths) <- db$dtgs
  db
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
        range <- expandDateRange(dtgs)
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

updateDates <- function(db) {
  earliestDtg <- head(db$dtgs, 1)
  latestDtg <- tail(db$dtgs, 1)
  maxDateRange <- c(dtg2date(earliestDtg),
                    dtg2date(latestDtg))
  db$maxDateRange <- maxDateRange
  db$cycles <- lapply(sort(unique(db$dtgs %% 100)), partial(sprintf, "%02d"))
  db
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

slugify <- function(string) {
  normalized <- stri_trans_general(string, "nfkd; Latin-ASCII; lower")
  slugified <- stri_replace_all_charclass(normalized, "\\p{WHITE SPACE}", "_")
  slugified
}

setPragmas <- function(connection) {
  dbExecute(connection, "PRAGMA journal_mode=OFF")
  dbExecute(connection, "PRAGMA synchronous=OFF")
  dbExecute(connection, "PRAGMA foreign_keys=ON")
}

initNewCache <- function(cachePath) {
  dbLock <- lock(cachePath)
  # dbConnect will create the DB file if it doesn't yet exist, but the
  # directory must exist
  if(!dir.exists(dirname(cachePath))) {
    dir.create(dirname(cachePath), recursive = TRUE)
  }
  # Making sure we are dealing with a new file
  cacheFileDelStatus <- unlink(cachePath)
  if(cacheFileDelStatus==1 && file.exists(cachePath)) {
    flog.error(sprintf("Could not initialise cache on file %s", cachePath))
    unlock(dbLock)
    return(NULL)
  }

  cache <- dbConnect(RSQLite::SQLite(), cachePath)
  setPragmas(cache)
  dbExecute(cache, paste("CREATE TABLE obtype (",
                         "  obtype_id INTEGER PRIMARY KEY,",
                         "  obnumber INTEGER NOT NULL,",
                         "  obtype VARCHAR(20) NOT NULL,",
                         "  variable VARCHAR(20),",
                         "  sensor VARCHAR(20),",
                         "  satellite VARCHAR(20),",
                         "  division INTEGER NOT NULL,",
                         "  fromDbTable VARCHAR(20) NOT NULL,",
                         "  UNIQUE(obnumber, obtype, variable, division, fromDbTable),",
                         "  UNIQUE(obnumber, obtype, sensor, satellite, division, fromDbTable)",
                         ")", sep=""))
  dbExecute(cache, paste("CREATE TABLE dtg (",
                         "  dtg INTEGER PRIMARY KEY",
                         ")", sep=""))
  dbExecute(cache, paste("CREATE TABLE dtg_obtype (",
                         "  dtg INTEGER NOT NULL REFERENCES dtg(dtg),",
                         "  obtype_id INTEGER NOT NULL REFERENCES obtype(obtype_id)",
                         ")", sep=""))
  dbExecute(cache, "CREATE INDEX dtg_obtype_dtg_idx ON dtg_obtype(dtg)")
  dbExecute(cache, paste("CREATE TABLE station (",
                         "  station_id INTEGER PRIMARY KEY,",
                         "  obname VARCHAR(20) NOT NULL,",
                         "  statid VARCHAR(20) NOT NULL,",
                         "  label VARCHAR(20),",
                         "  UNIQUE(obname, statid)",
                         ")", sep=""))
  unlock(dbLock)
  cache
}

openCache <- function(db) {
  if (file.exists(db$cachePath)) {
    flog.debug('Cache path "%s" exists. Connecting.', db$cachePath)
    cache <- dbConnect(RSQLite::SQLite(), db$cachePath)
    setPragmas(cache)
    if(!("obtype" %in% dbListTables(cache)) ||
       !("fromDbTable" %in% dbListFields(cache, "obtype"))) {
      # Fixing obsmon cache files created before the "missing data" bugfix.
      # Before that bugfix, only data from the "obsmon" table was not taken
      # into account when building the cache, and "usage" was ignored.
      # The "fromDbTable" field was then introduced in the cache's obtype table
      # so that one knows which table the cached data was extracted from.
      backup_fname <- paste(
        db$cachePath, '.auto_backup_missing_data_bugfix_', Sys.Date(),
        sep=""
      )
      warnMsg <- paste(
        "The cache file",
        paste("  >>>", db$cachePath),
        'was generated prior to "missing data" bugfix and will be reset.',
        "This may cause caching to take longer than usual this time,",
        "but part of the available data may become unselectable otherwise.",
        'A backup of the old file will be created as:',
        paste("  >>>", backup_fname),
        sep="\n"
      )
      flog.warn(warnMsg)
      dbDisconnect(cache)
      file.copy(db$cachePath, backup_fname)
      cache <- initNewCache(db$cachePath)
    }
  } else {
    flog.debug('Cache path "%s" does not exist. Initialising.', db$cachePath)
    cache <- initNewCache(db$cachePath)
  }
  cache
}

updateCachingStatusFile <- function(db, exptsCachingProgress) {
  thisExptCachingProgress <- exptsCachingProgress[[db$basename]]
  save(
    'thisExptCachingProgress',
    file=exptsCacheProgLogFilePath[[db$basename]]
  )
}

updateCache <- function(db) {
  cachedDtgs <- dbGetQuery(db$cache, "SELECT dtg FROM dtg")[["dtg"]]
  oldDtgIdx <- db$dtgs %in% cachedDtgs
  if (!setequal(db$dtgs[oldDtgIdx], cachedDtgs)) {
    flog.warn(paste("Some old dtgs are no longer available.",
                    "If desired, purge cache manually."))
  }
  newDtgs <- db$dtgs[!oldDtgIdx]
  ingestShard <- function(dtg) {
    path <- db$paths[[dtg]]
    if (is.null(path)) return(NULL)
    else if (file.access(path, mode=4)!=0) {
      flog.warn('User "%s" does NOT have read access to db file "%s"!', userName, path)
      return(NULL)
    } else {
      dbExecute(db$cache, sprintf("ATTACH '%s' AS 'shard'", path))
      tables <- dbGetQuery(db$cache, "SELECT name FROM shard.sqlite_master WHERE type='table'")
      if (all(c("obsmon", "usage") %in% tables$name)) {
        dbWithTransaction(
            db$cache,
            {
              dbExecute(db$cache, sprintf("INSERT INTO dtg VALUES(%s)", dtg))
              dbExecute(db$cache, paste("CREATE TEMP TABLE temp.obtype (",
                                        "  obnumber INTEGER NOT NULL,",
                                        "  obtype VARCHAR(20) NOT NULL,",
                                        "  variable VARCHAR(20),",
                                        "  sensor VARCHAR(20),",
                                        "  satellite VARCHAR(20),",
                                        "  division INTEGER NOT NULL,",
                                        "  fromDbTable VARCHAR(20) NOT NULL,",
                                        "  UNIQUE(obnumber, obtype, variable, division, fromDbTable),",
                                        "  UNIQUE(obnumber, obtype, sensor, satellite, division, fromDbTable)",
                                        ")", sep=""))
              dbExecute(db$cache, paste("INSERT INTO temp.obtype ",
                                        "  (obnumber, obtype, variable, division, fromDbTable)",
                                        "  SELECT DISTINCT obnumber, obname, varname, level, 'obsmon' ",
                                        "FROM shard.obsmon WHERE obnumber!=7 ",
                                        "UNION ",
                                        "  SELECT DISTINCT obnumber, obname, varname, level, 'usage' ",
                                        "FROM shard.usage WHERE obnumber!=7 ",
                                        sep=""))
              dbExecute(db$cache, paste("INSERT INTO temp.obtype ",
                                        "  (obnumber, obtype, sensor, satellite, division, fromDbTable)",
                                        "  SELECT DISTINCT obnumber, 'satem', obname, satname, level, 'obsmon' ",
                                        "FROM shard.obsmon WHERE obnumber==7 ",
                                        "UNION ",
                                        "  SELECT DISTINCT obnumber, 'satem', obname, satname, level, 'usage' ",
                                        "FROM shard.usage WHERE obnumber==7 ",
                                        sep=""))
              dbExecute(db$cache, paste("INSERT OR IGNORE INTO main.obtype (",
                                        "  obnumber, obtype, variable, sensor, satellite, division, fromDbTable",
                                        ") SELECT * FROM temp.obtype"))
              dbExecute(db$cache, paste(sprintf("INSERT INTO main.dtg_obtype SELECT %s, ", dtg),
                                        "m.obtype_id FROM main.obtype m ",
                                        "JOIN temp.obtype t ",
                                        "USING (obnumber, obtype, variable, division, fromDbTable) ",
                                        "WHERE t.variable IS NOT NULL",
                                        sep=""))
              dbExecute(db$cache, paste(sprintf("INSERT INTO main.dtg_obtype SELECT %s, ", dtg),
                                        "m.obtype_id FROM main.obtype m ",
                                        "JOIN temp.obtype t ",
                                        "USING (obnumber, obtype, sensor, satellite, division, fromDbTable) ",
                                        "WHERE t.variable IS NULL",
                                        sep=""))
              dbExecute(db$cache, "DROP TABLE temp.obtype")
              dbExecute(db$cache, paste("INSERT OR IGNORE INTO station (obname, statid)",
                                        " SELECT DISTINCT obname, trim(statid, \"' \") statid",
                                        " FROM shard.usage WHERE obnumber!=7", sep=""))
            }
        )
      }
      dbExecute(db$cache, "DETACH 'shard'")
    }
  }
  if(length(newDtgs)>0) {
    thresholdUpdtCacheLogFile <- 0
    ingestShardUpdatingStatus <- function(dtg) {

      ingestShard(dtg)

      exptsCachingProgress[[db$basename]][[db$name]] <<- (
        exptsCachingProgress[[db$basename]][[db$name]] +
        100.0/length(newDtgs)
      )

      perc <- floor(mean(unlist(exptsCachingProgress[[db$basename]])))
      if(perc >= thresholdUpdtCacheLogFile) {
        updateCachingStatusFile(db, exptsCachingProgress)
        thresholdUpdtCacheLogFile <<- floor(perc) + 1
      }
    }

    lapply(as.character(newDtgs), ingestShardUpdatingStatus)

  } else {
    exptsCachingProgress[[db$basename]][[db$name]] <<- 100.0
    updateCachingStatusFile(db, exptsCachingProgress)
  }
  db
}

initObtypes <- function(db) {

  getNonSatObs <- function(obtypes, dbTable=NA) {
    res <- obtypes %>%
      filter(!is.na(variable)) %>%
      filter(if(!is.na(dbTable)) fromDbTable==dbTable else !is.na(fromDbTable)) %>%
      select(obtype, variable, division) %>%
      group_by(obtype, variable) %>%
      summarize(levelChoices=list(sort(as.integer(division)))) %>%
      group_by(obtype) %>%
      summarize(
        variable={
          lc <- levelChoices
          names(lc) <- variable
          list(lc)
        }) %>%
      summarize(obtype={
        v <- variable
        names(v) <- obtype
        list(v)
      })
    return(res[[1, 1]])
  }

  getSatObs <- function(obtypes, dbTable=NA) {
    res <- obtypes %>%
      filter(is.na(variable)) %>%
      filter(if(!is.na(dbTable)) fromDbTable==dbTable else !is.na(fromDbTable)) %>%
      select(obtype, sensor, satellite, division) %>%
      group_by(obtype, sensor, satellite) %>%
      summarize(channelChoices=list(sort(as.integer(division)))) %>%
      group_by(obtype, sensor) %>%
      summarize(satellite={
        cc <- channelChoices
        names(cc) <- satellite
        list(cc)
      }) %>%
      group_by(obtype) %>%
      summarize(sensor={
        sat <- satellite
        names(sat) <- sensor
        list(sat)
      }) %>%
      summarize(obtype={
        s <- sensor
        names(s) <- obtype
        list(s)
      })
    return(res[[1, 1]])
  }

  obtypes <- collect(tbl(db$cache, "obtype"))
  obsAllTables <- c(getSatObs(obtypes), getNonSatObs(obtypes))
  obsObsmonTable <- c(getSatObs(obtypes, 'obsmon'), getNonSatObs(obtypes, 'obsmon'))
  obsUsageTable <- c(getSatObs(obtypes, 'usage'), getNonSatObs(obtypes, 'usage'))

  db$obtypes <- obsAllTables[sort(names(obsAllTables))]
  db$obtypesObsmonTable <- obsObsmonTable[sort(names(obsObsmonTable))]
  db$obtypesUsageTable <- obsUsageTable[sort(names(obsUsageTable))]

  res <- dbGetQuery(db$cache, paste("SELECT DISTINCT obnumber, ",
                                    "CASE obtype ",
                                    "WHEN 'satem' THEN sensor ",
                                    "ELSE obtype ",
                                    "END obtype FROM main.obtype"))
  db$obnumbers <- res$obnumber
  names(db$obnumbers) <- res$obtype
  db
}

initStations <- function(db) {
  res <- dbGetQuery(db$cache, paste("SELECT station_id, statid ",
                                 "FROM station ",
                                 "WHERE obname='synop' AND label IS NULL",
                                 sep=""))
  statids <- res$statid
  labels <- synopStations[statids]
  res$label <- ifelse(!is.na(labels), labels, statids)
  dbExecute(db$cache, "UPDATE station SET label = :label WHERE station_id = :station_id", params=res)
  stations <- within(collect(tbl(db$cache, "station")), {
    label <- ifelse(!is.na(label), label, statid)
  })
  res <- stations %>%
    select(obname, statid, label) %>%
    group_by(obname) %>%
    summarize(statid=list(statid), label=list(label)) %>%
    summarize(obname={
      l <- mapply(list, statid=statid, label=label, SIMPLIFY=F)
      names(l) <- obname
      list(l)
    })
  stations <- res[[1,1]]
  lapply(names(stations), function(obname) {
    s <- stations[[obname]]
    names(s$statid) <- s$label
    names(s$label) <- s$statid
    db$stations[[obname]] <<- c("Any"="Any", s$statid)
    db$stationLabels[[obname]] <<- c("Any"="Any", s$label)
  })
  db
}

createDb <- function(dir, basename, name, file) {
  flog.debug("...%s: creating %s db...", basename, name)
  db <- structure(list(), class="sqliteShardedDtgDb")
  db$dir <- dir
  db$basename <- basename
  db$name <- name
  db$file <- file

  cacheFileName <- slugify(paste(basename, name, "db", sep="."))
  db$cachePath <- file.path(obsmonConfig$general$cacheDir, cacheFileName)

  flog.debug("......%s: finding %s dtgs......", basename, name)
  db$dtgs <- as.integer(dir(path=dir, pattern="[0-9]{10}"))
  if (length(db$dtgs)==0) {
    flog.debug("%s: Db %s contains no dtgs. Aborting.", basename, name)
    warning("%s: Db %s contains no dtgs. Aborting.", basename, name)
    exptsCachingProgress[[db$basename]][[db$name]] <<- 100.0
    updateCachingStatusFile(db, exptsCachingProgress)
    NULL
  } else {
    flog.debug("......%s: done finding %s dtgs......", basename, name)
    flog.debug("......%s: Opening %s cache db", basename, name)
    db$cache <- openCache(db)
    flog.debug("......%s: checking %s dtgs......", basename, name)
    db <- prepareConnections(db)
    flog.debug("......%s: done checking %s dtgs......", basename, name)
    flog.debug("......%s: updating %s db info......", basename, name)
    flog.debug(
      "......%s: Updating %s cache db. This may take some time.",basename,name
    )
    db <- updateCache(db)
    #flog.debug("......%s: Initialising %s observation types", basename, name)
    #db <- initObtypes(db)
    flog.debug("......%s: Initialising %s stations", basename, name)
    db <- initStations(db)
    flog.debug("......%s: Updating %s dates", basename, name)
    db <- updateDates(db)
    flog.debug("......%s: done updating %s db info......", basename, name)
    flog.debug("...%s: finished %s db...", basename, name)
    db
  }
}

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

putObservationsInCache <- function(sourceDbPath, cacheDir) {
  sourceDbPath <- normalizePath(sourceDbPath, mustWork=FALSE)
  flog.debug(sprintf("Caching file %s\n", sourceDbPath))
  anyFailedCachingAttmpt <- FALSE

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
        flog.debug(sprintf("Done caching file %s\n", sourceDbPath))
      }
      dbDisconnect(con_cache)
      dbDisconnect(con)
      unlock(cacheDbLock)
    }
  )

  con <- dbConnectWrapper(sourceDbPath, read_only=TRUE)
  if(is.null(con)) {
    anyFailedCachingAttmpt <- TRUE
    return(NULL)
  }
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

    # Making sure con_cache is not associated with any other file
    # The final dbDisconnect will be handled inside the "on.exit" statements
    tryCatch(
      dbDisconnect(con_cache),
      error=function(e) NULL,
      warning=function(w) NULL
    )
    con_cache <- dbConnectWrapper(cacheFilePath)
    cacheDbLock <- lock(cacheFilePath)

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
    if(alreadyCached) next

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
      anyFailedCachingAttmpt <- TRUE
      errMsg <- "putObservationsInCache: File path not consistent with cached expt dir:\n"
      errMsg <- paste0(errMsg, '    > Cached expt path: ', exptCachedDir, '\n')
      errMsg <- paste0(errMsg, '    > File path: ', sourceDbPath, '\n')
      errMsg <- paste0(errMsg, ' Not caching this particular file.', '\n')
      flog.error(errMsg)
      return(NULL)
    }

    # Register date and cycle as existing, if not previously done:
    # (i) Date
    tryCatch(
      dbExecute(con_cache, sprintf(
        'INSERT OR IGNORE INTO dates(date) values(%d);', date
      )),
      warning=function(w) NULL,
      error=function(e) {print(e$message); NULL}
    )
    # (ii) Cycle
    tryCatch(
      dbExecute(con_cache, sprintf(
        'INSERT OR IGNORE INTO cycles(date, cycle) VALUES (%d, %d)',date,cycle
      )),
      warning=function(w) NULL,
      error=function(e) {print(e$message); NULL}
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
