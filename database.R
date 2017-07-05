sqliteConnect <- function(dbpath) {
  flog.debug("Connecting to path %s", dbpath)
  con <- dbConnect(RSQLite::SQLite(), dbpath,
                   flags=RSQLite::SQLITE_RO, synchronous=NULL)
  tryCatch(dbGetQuery(con, "PRAGMA synchronous=off"),
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

updateDates <- function(db, cachedDtgs, newDtgs) {
  earliestDtg <- head(db$dtgs, 1)
  latestDtg <- tail(db$dtgs, 1)
  db$maxDtgRange <- c(earliestDtg, latestDtg)
  maxDateRange <- c(dtg2date(earliestDtg),
                    dtg2date(latestDtg))
  db$maxDateRange <- maxDateRange
  db$dateRange <- maxDateRange
  db$date <- maxDateRange[[2]]
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

updateDb <- function(db) {
  cachedDtgs <- loadCache(list(db$masterKey))
  oldDtgs <- db$dtgs[db$dtgs %in% cachedDtgs]
  if (!setequal(oldDtgs, cachedDtgs)) {
    flog.warn(paste("Some old dtgs are no longer available.",
                    "If desired, purge cache manually."))
  }
  newDtgs <- db$dtgs[!db$dtgs %in% cachedDtgs]
  db <- updateDates(db, cachedDtgs, newDtgs)
  db <- updateObtypes(db, cachedDtgs, newDtgs)
  db <- updateStations(db, cachedDtgs, newDtgs)
  db
}

slugify <- function(string) {
  normalized <- stri_trans_general(string, "nfkd; Latin-ASCII; lower")
  slugified <- stri_replace_all_charclass(normalized, "\\p{WHITE SPACE}", "_")
  slugified
}

createDb <- function(dir, name, file) {
  flog.info("...creating %s db...", name)
  db <- structure(list(), class="sqliteShardedDtgDb")
  db$dir <- dir
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
