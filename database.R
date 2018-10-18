sqliteConnect <- function(dbpath) {
  flog.debug("Connecting to path %s", dbpath)
  con <- dbConnect(RSQLite::SQLite(), dbpath,
                   flags=RSQLite::SQLITE_RO, synchronous=NULL)
  tryCatch(dbExecute(con, "PRAGMA synchronous=off"),
           error=function(e) {
             flog.error("Error accessing %s: %s", dbpath, e)
             con <- NULL
           })
  con
}

prepareConnections <- function(db) {
  makePath <- function(dtg) {
    dbpath <- file.path(db$dir, dtg, db$file)
    if (!file.exists(dbpath)) {
      flog.debug("Missing file %s", dbpath)
      NULL
    } else if(file.info(dbpath)[[1,"size"]] == 0.) {
      flog.debug("Empty file %s", dbpath)
      NULL
    } else {
      dbpath
    }
  }
  db$paths <- future_lapply(db$dtgs, makePath)
  names(db$paths) <- db$dtgs
  db
}

makeSingleQuery <- function(query) {
  function(dbpath) {
    con <- sqliteConnect(dbpath)
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

initCache <- function(cachePath) {
  dbLock <- lock(cachePath)
  # dbConnect will create the DB file if it doesn't yet exist, but the
  # directory must exist
  if(!dir.exists(dirname(cachePath))) {
    dir.create(dirname(cachePath), recursive = TRUE)
  }
  cache <- dbConnect(RSQLite::SQLite(), cachePath)
  if (!dbExistsTable(cache, "dtg")) {
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
  }
  unlock(dbLock)
  cache
}

openCache <- function(db) {
  cacheFile <- slugify(paste(db$basename, db$name, "db", sep="."))
  cachePath <- file.path(obsmonConfig$general$cacheDir, cacheFile)
  if (file.exists(cachePath)) {
    flog.debug('Cache path "%s" exists. Connecting.', cachePath)
    cache <- dbConnect(RSQLite::SQLite(), cachePath)
    setPragmas(cache)
  } else {
    flog.debug('Cache path "%s" does not exist. Initialising.', cachePath)
    cache <- initCache(cachePath)
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
    else if (file.exists(path) && file.access(path, mode=4)==0) {
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
    } else if(file.access(path, mode=4)!=0) {
      flog.warn('User "%s" does NOT have read access to db file "%s"!', userName, path)
      return(NULL)
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
  obtypes <- collect(tbl(db$cache, "obtype"))

  getNonSatObs <- function(obtypes, dbTable) {
    res <- obtypes %>%
      filter(!is.na(variable)) %>%
      filter(fromDbTable==dbTable) %>%
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

  getSatObs <- function(obtypes, dbTable) {
    res <- obtypes %>%
      filter(is.na(variable)) %>%
      filter(fromDbTable==dbTable) %>%
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


  nonSatObs <- getNonSatObs(obtypes, 'obsmon')
  satObs <- getSatObs(obtypes, 'obsmon')

  obs <- c(satObs, nonSatObs)
  db$obtypes <- obs[sort(names(obs))]
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
    flog.debug("......%s: checking %s dtgs......", basename, name)
    db <- prepareConnections(db)
    flog.debug("......%s: done checking %s dtgs......", basename, name)
    flog.debug("......%s: updating %s db info......", basename, name)
    flog.debug("%s: Opening %s cache db", basename, name)
    db$cache <- openCache(db)
    flog.debug(
      "%s: Updating %s cache db. This may take some time.", basename, name
    )
    db <- updateCache(db)
    flog.debug("%s: Initialising %s observation types", basename, name)
    db <- initObtypes(db)
    flog.debug("%s: Initialising %s stations", basename, name)
    db <- initStations(db)
    flog.debug("%s: Updating %s dates", basename, name)
    db <- updateDates(db)
    flog.debug("......%s: done updating %s db info......", basename, name)
    flog.debug("...%s: finished %s db...", basename, name)
    db
  }
}
