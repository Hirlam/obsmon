# This defines S3 classes to deal with the database connections for experiments

library(DBI)
library(futile.logger)
library(pryr)

source("utils.R")

# Define generics
expConnect <- function(x) UseMethod("expConnect")
expDisconnect <- function(x) UseMethod("expDisconnect")
expQuery <- function(x, db, query,
                     dtgs=NULL, convertDTG=TRUE) UseMethod("expQuery")

# Provide defaults
expConnect.sqliteShardedDtg <- function(x) {
  drv <- RSQLite::SQLite()
  connect <- function(dtgs, dir, db) {
    conns <- lapply(dtgs, function(dtg) dbConnect(drv, file.path(dir, dtg, db)))
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
  res <- lapply(conns, function(conn) dbGetQuery(conn, query))
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
    res <- sqliteShardedDtgGetObtypes(x$conns[[db]])
    x$obnumbers[[db]] <- res[[1]]
    x$obtypes[[db]] <- res[[2]]
  }
  x
}

expCreateSqliteShardedDtg <- function(name, isProduction,
                                      baseDir, experiment,
                                      ecmaDir, ecmaSfcDir, ccmaDir) {
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
  x <- sqliteShardedDtgInitDates(x)
  x <- expConnect(x)
  x <- sqliteShardedDtgInitObtypes(x)
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
