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
  db$paths <- lapply(db$dtgs, makePath)
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

getRawObtypes <- function(db, dtgs) {
  query <- paste("SELECT DISTINCT",
                 "obnumber, obname, satname, varname, level",
                 "FROM obsmon WHERE nobs_total>0")
  allObs <- performQuery(db, query, dtgs, expandRange=FALSE)
  allObs <- unique(allObs)
  rownames(allObs) <- NULL
  allObs
}

formatRawObtypes <- function(allObs) {
  obnames <- unique(allObs[1:2])
  obnumbers <- obnames[["obnumber"]]
  names(obnumbers) <- obnames[["obname"]]
  satInd <- allObs$obnumber == 7
  satObs <- allObs[satInd,]
  nonSatObs <- allObs[!satInd, !(names(allObs) %in% "satname")]
  obtypes <- list()
  for (obn in sort(unique(nonSatObs$obname))) {
    obtypes[[obn]] <- list()
    obs <- nonSatObs[nonSatObs$obname == obn,]
    for (var in sort(unique(obs$varname))) {
      obtypes[[obn]][[var]] <- list()
      lvls <- obs[obs$varname == var,]
      for (lvl in sort(unique(lvls$level))) {
        entry <- lvls[lvls$level == lvl,]
        stopifnot(nrow(entry)==1)
        obtypes[[obn]][[var]][[as.character(lvl)]] <- entry
      }
    }
    uNames <- c("u", "u10m")
    ffNames <- c("ff", "ff10m")
    varNames <- names(obtypes[[obn]])
    uPresent <- uNames %in% varNames
    for (i in seq_len(length(uPresent))) {
      if (uPresent[i]) {
        obtypes[[obn]][[ffNames[i]]] <- obtypes[[obn]][[uNames[i]]]
      }
    }
  }
  obtypes$satem <- list()
  for (sens in sort(unique(satObs$obname))) {
    obtypes$satem[[sens]] <- list()
    sats <- satObs[satObs$obname == sens,]
    for (sat in sort(unique(sats$satname))) {
      obtypes$satem[[sens]][[sat]] <- list()
      chans <- sats[sats$satname == sat,]
      for (chan in sort(unique(chans$level))) {
        entry <- chans[chans$level == chan,]
        stopifnot(nrow(entry)==1)
        obtypes$satem[[sens]][[sat]][[as.character(chan)]] <- entry
      }
    }
  }
  if (length(obtypes$satem) == 0) {
    obtypes$satem <- NULL
  }
  list(obnumbers, obtypes)
}

updateObtypes <- function(db, cachedDtgs, newDtgs) {
  flog.info("...updating obtypes for %s...", db$name)
  flog.info("......loading obtypes cache......")
  obnumbersKey <- list(db$masterKey, cachedDtgs, "obnumbers")
  oldObnumbersFile <- findCache(obnumbersKey)
  obtypesKey <- list(db$masterKey, cachedDtgs, "obtypes")
  oldObtypesFile <- findCache(obtypesKey)
  db$obnumbers <- loadCache(obnumbersKey)
  db$obtypes <- loadCache(obtypesKey)
  flog.info("......done loading obtypes cache......")
  if (length(newDtgs)>0) {
    flog.info("......database has changed, updating......")
    rawObtypesKey <- list(db$masterKey, cachedDtgs, "rawobtypes")
    oldRawObtypesFile <- findCache(rawObtypesKey)
    cachedRawObtypes <- loadCache(rawObtypesKey)
    flog.info("......scrubbing new db files......")
    newRawObtypes <- getRawObtypes(db, newDtgs)
    flog.info("......done scrubbing, merging new data......")
    rawObtypes <- unique(rbind(cachedRawObtypes, newRawObtypes))
    rownames(rawObtypes) <- NULL
    rawObtypesKey <- list(db$masterKey, db$dtgs, "rawobtypes")
    saveCache(rawObtypes, rawObtypesKey)
    if (!is.null(oldRawObtypesFile)) {
      file.remove(oldRawObtypesFile)
    }
    res <- formatRawObtypes(rawObtypes)
    db$obnumbers <- res[[1]]
    db$obtypes <- res[[2]]
    obnumbersKey <- list(db$masterKey, db$dtgs, "obnumbers")
    obtypesKey <- list(db$masterKey, db$dtgs, "obtypes")
    saveCache(db$obnumbers, obnumbersKey)
    if (!is.null(oldObnumbersFile)) {
      file.remove(oldObnumbersFile)
    }
    saveCache(db$obtypes, obtypesKey)
    if (!is.null(oldObtypesFile)) {
      file.remove(oldObtypesFile)
    }
  }
  flog.info("...done updating obtypes for %s...", db$name)
  db
}

getRawStations <- function(db, dtgs) {
  query <- "SELECT DISTINCT obname, statid FROM usage"
  allObs <- performQuery(db, query, dtgs, expandRange=FALSE)
  allObs <- unique(allObs)
  rownames(allObs) <- NULL
  allObs
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

formatRawStations <- function(db, rawStations) {
  stations <- list()
  stationLabels <- list()
  for (obtype in names(db$obtypes)) {
    matchingStatids <- rawStations[rawStations$obname==obtype,]$statid
    statids <- trimws(sort(unique(matchingStatids)))
    ends <- nchar(statids)
    statids <- ifelse(substr(statids, 1, 1)=="'"
                      & substr(statids, ends, ends)=="'",
                      trimws(substr(statids, 2, ends-1)),
                      statids)
    if (obtype == 'synop') {
      designations <- ifelse(statids %in% names(synopStations),
                             synopStations[statids],
                             "Unknown")
      names(statids) <- sprintf("%s (%s)", designations, statids)
    } else {
      names(statids) <- statids
    }
    statids <- c(list("Any"="Any"), statids)
    stations[[obtype]] <- statids
    stationLabels[[obtype]] <- names(statids)
    names(stationLabels[[obtype]]) <- statids
  }
  list(stations, stationLabels)
}

updateStations <- function(db, cachedDtgs, newDtgs) {
  flog.info("...updating stations for %s...", db$name)
  flog.info("......loading stations cache......")
  stationsKey <- list(db$masterKey, cachedDtgs, "stations")
  oldStationsFile <- findCache(stationsKey)
  stationLabelsKey <- list(db$masterKey, cachedDtgs, "stationLabels")
  oldStationLabelsFile <- findCache(stationLabelsKey)
  db$stations <- loadCache(stationsKey)
  db$stationLabels <- loadCache(stationLabelsKey)
  flog.info("......done loading stations cache......")
  if (length(newDtgs)>0) {
    flog.info("......database has changed, updating......")
    rawStationsKey <- list(db$masterKey, cachedDtgs, "rawstations")
    oldRawStationsFile <- findCache(rawStationsKey)
    cachedRawStations <- loadCache(rawStationsKey)
    flog.info("......scrubbing new db files......")
    newRawStations <- getRawStations(db, newDtgs)
    flog.info("......done scrubbing, merging new data......")
    rawStations <- unique(rbind(cachedRawStations, newRawStations))
    rownames(rawStations) <- NULL
    rawStationsKey <- list(db$masterKey, db$dtgs, "rawstations")
    saveCache(rawStations, rawStationsKey)
    if (!is.null(oldRawStationsFile)) {
      file.remove(oldRawStationsFile)
    }
    res <- formatRawStations(db, rawStations)
    db$stations <- res[[1]]
    db$stationLabels <- res[[2]]
    stationsKey <- list(db$masterKey, db$dtgs, "stations")
    stationLabelsKey <- list(db$masterKey, db$dtgs, "stationLabels")
    saveCache(db$stations, stationsKey)
    if (!is.null(oldStationsFile)) {
      file.remove(oldStationsFile)
    }
    saveCache(db$stationLabels, stationLabelsKey)
    if (!is.null(oldStationLabelsFile)) {
      file.remove(oldStationLabelsFile)
    }
  }
  flog.info("...done updating stations for %s...", db$name)
  db
}

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
                           "  UNIQUE(obnumber, obtype, variable, division),",
                           "  UNIQUE(obnumber, obtype, sensor, satellite, division)",
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
  flog.info("Cache path: %s", cachePath)
  if (file.exists(cachePath)) {
    cache <- dbConnect(RSQLite::SQLite(), cachePath)
    setPragmas(cache)
  } else {
    cache <- initCache(cachePath)
  }
  cache
}


updateDb <- function(db) {
  cache <- openCache(db)
  cachedDtgs <- dbGetQuery(cache, "SELECT dtg FROM dtg")[["dtg"]]
  oldDtgIdx <- db$dtgs %in% cachedDtgs
  if (!setequal(db$dtgs[oldDtgIdx], cachedDtgs)) {
    flog.warn(paste("Some old dtgs are no longer available.",
                    "If desired, purge cache manually."))
  }
  newDtgs <- db$dtgs[!oldDtgIdx]
  ingestShard <- function(dtg) {
    dbExecute(cache, sprintf("ATTACH '%s' AS 'shard'", db$paths[dtg]))
    tables <- dbGetQuery(cache, "SELECT name FROM shard.sqlite_master WHERE type='table'")
    if (all(c("obsmon", "usage") %in% tables$name)) {
      dbWithTransaction(
          cache,
          {
            dbExecute(cache, sprintf("INSERT INTO dtg VALUES(%s)", dtg))
            dbExecute(cache, paste("CREATE TEMP TABLE temp.obtype (",
                                   "  obnumber INTEGER NOT NULL,",
                                   "  obtype VARCHAR(20) NOT NULL,",
                                   "  variable VARCHAR(20),",
                                   "  sensor VARCHAR(20),",
                                   "  satellite VARCHAR(20),",
                                   "  division INTEGER NOT NULL,",
                                   "  UNIQUE(obnumber, obtype, variable, division),",
                                   "  UNIQUE(obnumber, obtype, sensor, satellite, division)",
                                   ")", sep=""))
            dbExecute(cache, paste("INSERT INTO temp.obtype (",
                                   "  obnumber, obtype, variable, division",
                                   ") SELECT DISTINCT obnumber, obname, varname, level ",
                                   "FROM shard.obsmon WHERE obnumber!=7", sep=""))
            dbExecute(cache, paste("INSERT INTO temp.obtype (",
                                   "  obnumber, obtype, sensor, satellite, division",
                                   ") SELECT DISTINCT obnumber, 'satem', obname, satname, level ",
                                   "FROM shard.obsmon WHERE obnumber==7", sep=""))
            dbExecute(cache, paste("INSERT OR IGNORE INTO main.obtype (",
                                   "  obnumber, obtype, variable, sensor, satellite, division",
                                   ") SELECT * FROM temp.obtype"))
            dbExecute(cache, paste(sprintf("INSERT INTO main.dtg_obtype SELECT %s, ", dtg),
                                   "m.obtype_id FROM main.obtype m ",
                                   "JOIN temp.obtype t ",
                                   "USING (obnumber, obtype, variable, division) ",
                                   "WHERE t.variable IS NOT NULL",
                                   sep=""))
            dbExecute(cache, paste(sprintf("INSERT INTO main.dtg_obtype SELECT %s, ", dtg),
                                   "m.obtype_id FROM main.obtype m ",
                                   "JOIN temp.obtype t ",
                                   "USING (obnumber, obtype, sensor, satellite, division) ",
                                   "WHERE t.variable IS NULL",
                                   sep=""))
            dbExecute(cache, "DROP TABLE temp.obtype")
            dbExecute(cache, paste("INSERT OR IGNORE INTO station (obname, statid)",
                                   " SELECT DISTINCT obname, trim(statid, \"' \") statid",
                                   " FROM shard.usage WHERE obnumber!=7", sep=""))
          }
      )
    }
    dbExecute(cache, "DETACH 'shard'")
  }
  if(length(newDtgs)>0) {
    pblapply(as.character(newDtgs), ingestShard)
  }
  obtypes <- collect(tbl(cache, "obtype"))
  res <- obtypes %>%
    filter(!is.na(variable)) %>%
    select(obtype, variable, division) %>%
    group_by(obtype, variable) %>%
    summarize(levelChoices=list(sort(as.integer(division)))) %>%
    group_by(obtype) %>%
    summarize(variable={
      lc <- levelChoices
      names(lc) <- variable
      list(lc)
    }) %>%
    summarize(obtype={
      v <- variable
      names(v) <- obtype
      list(v)
    })
  db$obtypes <- res[[1,1]]
  res <- dbGetQuery(cache, paste("SELECT station_id, statid ",
                                 "FROM station ",
                                 "WHERE obname='synop' AND label IS NULL",
                                 sep=""))
  statids <- res$statid
  labels <- synopStations[statids]
  res$label <- ifelse(!is.na(labels), labels, statids)
  dbExecute(cache, "UPDATE station SET label = :label WHERE station_id = :station_id", params=res)
  stations <- within(collect(tbl(cache, "station")), {
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
    stations[[obname]] <<- c("Any"="Any", s)
  })
  db$stations <- stations
  res <- dbGetQuery(cache, paste("SELECT DISTINCT obnumber, ",
                                 "CASE obtype ",
                                 "WHEN 'satem' THEN sensor ",
                                 "ELSE obtype ",
                                 "END obtype FROM main.obtype"))
  db$obnumbers <- res$obnumber
  names(db$obnumbers) <- res$obtype
  dbDisconnect(cache)
  db <- updateDates(db)
  db
}

createDb <- function(dir, basename, name, file) {
  flog.info("...creating %s db...", name)
  db <- structure(list(), class="sqliteShardedDtgDb")
  db$dir <- dir
  db$basename <- basename
  db$name <- name
  db$file <- file
  db$masterKey <- getChecksum(list(dir, file))
  flog.info("......finding dtgs......")
  db$dtgs <- as.integer(dir(path=dir, pattern="[0-9]{10}"))
  if (length(db$dtgs)==0) {
    flog.warn("Db contains no dtgs. Aborting.")
    warning("Db contains no dtgs. Aborting.")
    NULL
  } else {
    flog.info("......done finding dtgs......")
    flog.info("......checking dtgs......")
    db <- prepareConnections(db)
    flog.info("......done checking dtgs......")
    flog.info("......updating db info......")
    db <- updateDb(db)
    flog.info("......done updating db info......")
    saveCache(db$dtgs, list(db$masterKey))
    flog.info("...finished %s db...", name)
    db
  }
}
