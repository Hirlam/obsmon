# This defines S3 classes to deal with the database connections for experiments

library(DBI)
library(futile.logger)
library(pbapply)
library(pryr)
library(R.cache)

pboptions(type="timer")

source("utils.R")

setCacheRootPath(path="./.Rcache")

# Define generics
expConnect <- function(x) UseMethod("expConnect")
expDisconnect <- function(x) UseMethod("expDisconnect")
expQuery <- function(x, db, query,
                     dtgs=NULL, convertDTG=TRUE) UseMethod("expQuery")

# Provide defaults
expConnect.sqliteShardedDtg <- function(x) {
  drv <- RSQLite::SQLite()
  connect <- function(dtgs, dir, db) {
    conns <- pblapply(dtgs, function(dtg) dbConnect(drv, file.path(dir, dtg, db)))
    names(conns) <- dtgs
    conns
  }
  x$conns$ecma <- connect(x$dtgs, x$dirs$ecma, "ecma.db")
  x$conns$ecmaSfc <- connect(x$dtgs, x$dirs$ecmaSfc, "ecma.db")
  x$conns$ccma <- connect(x$dtgs, x$dirs$ccma, "ccma.db")
  x
}

expQuery.sqliteShardedDtg <- function(x, db, query,
                                      dtgs=NULL, convertDTG=TRUE) {
  if (is.null(dtgs)) {
    conns <- x$conns[[db]]
  } else {
    switch(length(dtgs),
           conns <- list(x$conns[[db]][[as.character(dtgs)]]),
           conns <- x$conns[[db]][dtgs[1] <= x$dtgs & x$dtgs <= dtgs[2]]
           )
  }
  res <- pblapply(conns, function(conn) dbGetQuery(conn, query))
  res <- do.call(rbind, res)
  if(convertDTG & "DTG" %in% names(res)) {
    res$DTG <- as.POSIXct(as.character(res$DTG), format="%Y%m%d%H")
  }
  res
}

sqliteShardedDtgInitDates <- function(x) {
  earliestDtg <- head(x$dtgs, 1)
  latestDtg <- tail(x$dtgs, 1)
  x$maxDtgRange <- c(earliestDtg, latestDtg)
  maxDateRange <- c(dtg2date(earliestDtg),
                    dtg2date(latestDtg))
  x$maxDateRange <- maxDateRange
  x$dateRange <- maxDateRange
  x$date <- maxDateRange[[2]]
  x$cycles <- lapply(sort(unique(x$dtgs %% 100)), partial(sprintf, "%02d"))
  x
}

sqliteShardedDtgGetObtypes <- function(conns) {
  query <- paste("SELECT DISTINCT",
                 "obnumber, obname, satname, varname, level",
                 "FROM obsmon WHERE nobs_total>0")
  res <- lapply(conns, function(conn) dbGetQuery(conn, query))
  allObs <- do.call(rbind, res)
  rownames(allObs) <- NULL
  allObs <- unique(allObs)
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

sqliteShardedDtgInitObtypes <- function(x) {
  x$obtypes <- list()
  for (db in c("ecma", "ecmaSfc", "ccma")) {
    cacheKeyObnumbers <- list(x$cacheHash, db, "obnumbers")
    cacheKeyObtypes <- list(x$cacheHash, db, "obtypes")
    x$obnumbers[[db]] <- loadCache(cacheKeyObnumbers)
    x$obtypes[[db]] <- loadCache(cacheKeyObtypes)
    if (!is.null(x$obnumbers[[db]])
        && !is.null(x$obtypes[[db]])) {
      flog.info("......cache found for %s......", db)
    } else {
      flog.info("......no cache found for %s......", db)
      res <- sqliteShardedDtgGetObtypes(x$conns[[db]])
      x$obnumbers[[db]] <- res[[1]]
      x$obtypes[[db]] <- res[[2]]
      saveCache(x$obnumbers[[db]], cacheKeyObnumbers)
      saveCache(x$obtypes[[db]], cacheKeyObtypes)
    }
  }
  x
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


sqliteShardedDtgInitStations <- function(x) {
  query <- "SELECT DISTINCT obname, statid FROM usage"
  x$stations <- list()
  for (db in c("ecma", "ecmaSfc", "ccma")) {
    cacheKeyStations <- list(x$cacheHash, db, "stations")
    cacheKeyStationLabels <- list(x$cacheHash, db, "stationLabels")
    x$stations[[db]] <- loadCache(cacheKeyStations)
    x$stationLabels[[db]] <- loadCache(cacheKeyStationLabels)
    if (!is.null(x$stations[[db]])
        && !is.null(x$stationLabels[[db]])) {
      flog.info("......cache found for %s......", db)
    } else {
      flog.info("......no cache found for %s......", db)
      x$stations[[db]] <- list()
      flog.info("......querying......")
      raw <- expQuery(x, db, query)
      flog.info("......finished......")
      for (obtype in names(x$obtypes[[db]])) {
        statids <- trimws(sort(unique(raw[raw$obname==obtype,]$statid)))
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
        x$stations[[db]][[obtype]] <- statids
        x$stationLabels[[db]][[obtype]] <- names(statids)
        names(x$stationLabels[[db]][[obtype]]) <- statids
      }
      saveCache(x$stations[[db]], cacheKeyStations)
      saveCache(x$stationLabels[[db]], cacheKeyStationLabels)
    }
  }
  x
}

expCreateSqliteShardedDtg <- function(name, isProduction,
                                      baseDir, experiment,
                                      ecmaDir, ecmaSfcDir, ccmaDir) {
  flog.info("Initialization of experiment %s...", name)
  x <- structure(list(), class = "sqliteShardedDtg")
  x$name <- name
  x$isProduction <- isProduction
  x$isConnected <- F
  x$dirs$ecma <- file.path(baseDir, experiment, ecmaDir)
  x$dbs$ecma <- "ecma.db"
  x$dirs$ecmaSfc <- file.path(baseDir, experiment, ecmaSfcDir)
  x$dbs$ecmaSfc <- "ecma.db"
  x$dirs$ccma <- file.path(baseDir, experiment, ccmaDir)
  x$dbs$ccma <- "ccma.db"
  x$dtgs <- as.integer(dir(path=x$dirs$ecma, pattern="[0-9]{10}"))
  flog.info("...finding dates...")
  x <- sqliteShardedDtgInitDates(x)
  x$cacheHash <- getChecksum(x)
  flog.info("...opening connections...")
  x <- expConnect(x)
  flog.info("...initializing obtypes...")
  x <- sqliteShardedDtgInitObtypes(x)
  flog.info("...initializing stations...")
  x <- sqliteShardedDtgInitStations(x)
  flog.info("...complete.")
  x
}

initExperiments <- function() {
  configs <- yaml.load_file("config.yml")
  experiments <- list()
  experimentChoices <- list()
  for(config in configs) {
    name <- config$displayName
    experiments[[name]] <-
      expCreateSqliteShardedDtg(name, config$productionSite,
                                config$baseDir, config$experiment,
                                config$ecmaDir, config$ecmaSfcDir,
                                config$ccmaDir)
    experimentChoices <- append(experimentChoices, name)
  }
  experiments
}

experiments <- initExperiments()
