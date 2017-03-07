# This defines S3 classes to deal with the database connections for experiments

library(DBI)
library(futile.logger)

source("utils.R")

# Define generics
expConnect <- function(x) UseMethod("expConnect")
expDisconnect <- function(x) UseMethod("expDisconnect")
expQuery <- function(x, query, onlySelected=TRUE) UseMethod("expQuery")
expSetDateRange <- function(x, dateRange) {
  UseMethod("expSetDateRange")
}

# Provide defaults
expSetDateRange.default <- function(x, dateRange) {
  if (dateRange[1] < x$maxDateRange[1]) {
    dateRange[1] <- x$maxDateRange[1]
    flog.warn("Date range exceeds available data. Restricting.")
  }
  if (dateRange[2] > x$maxDateRange[2]) {
    dateRange[2] <- x$maxDateRange[2]
    flog.warn("Date range exceeds available data. Restricting.")
  }
  x$dateRange <- dateRange
  x$selectedDtgs <- x$dtgs[x$dtgs >= date2dtg(dateRange[1])
                         & x$dtgs < date2dtg(dateRange[2])+100]
  x
}

expConnect.sqliteShardedDtg <- function(x) {
  drv <- RSQLite::SQLite()
  x$ecmaConnections <-
    sapply(x$dtgs, function(dtg) {
      dbConnect(drv, file.path(x$ecmaDir, dtg, "ecma.db"))
    }, simplify=FALSE)
  x$ecmaSfcConnections <-
    sapply(x$dtgs, function(dtg) {
      dbConnect(drv, file.path(x$ecmaSfcDir, dtg, "ecma.db"))
    }, simplify=FALSE)
  x$ccmaConnections <-
    sapply(x$dtgs, function(dtg) {
      dbConnect(drv, file.path(x$ccmaDir, dtg, "ccma.db"))
    }, simplify=FALSE)
  x
}

expQuery.sqliteShardedDtg <- function(x, query, onlySelected=TRUE) {
}

sqliteShardedDtgInitDates <- function(x) {
  earliestDtg <- head(x$dtgs, 1)
  latestDtg <- tail(x$dtgs, 1)
  x$maxDtgRange <- c(as.integer(earliestDtg),
                     as.integer(latestDtg))
  maxDateRange <- c(dtg2date(earliestDtg),
                    dtg2date(latestDtg))
  x$maxDateRange <- maxDateRange
  x <- expSetDateRange(x, maxDateRange)
  x$cycles <- sort(unique(substr(x$dtgs, 9, 10)))
  x
}

sqliteShardedDtgGetObtypes <- function(conns) {
  query <- paste("SELECT DISTINCT",
                 "obnumber, obname, satname, varname, level",
                 "FROM obsmon WHERE nobs_total>0")
  res <- lapply(conns, function(conn) dbGetQuery(conn, query))
  allObs <- unique(do.call(rbind, res))
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
  obtypes
}

sqliteShardedDtgInitObtypes <- function(x) {
  x$obtypes <- list()
  x$obtypes$ecma <- sqliteShardedDtgGetObtypes(x$ecmaConnections)
  x$obtypes$ecmaSfc <- sqliteShardedDtgGetObtypes(x$ecmaSfcConnections)
  x$obtypes$ccma <- sqliteShardedDtgGetObtypes(x$ccmaConnections)
  x
}

expCreateSqliteShardedDtg <- function(name, isProduction,
                                      baseDir, experiment,
                                      ecmaDir, ecmaSfcDir, ccmaDir) {
  x <- structure(list(), class = "sqliteShardedDtg")
  x$name <- name
  x$isProduction <- isProduction
  x$isConnected <- F
  x$ecmaDir <- file.path(baseDir, experiment, ecmaDir)
  x$ecmaSfcDir <- file.path(baseDir, experiment, ecmaSfcDir)
  x$ccmaDir <- file.path(baseDir, experiment, ccmaDir)
  x$dtgs <- dir(path=x$ecmaDir, pattern="[0-9]{10}")
  x <- sqliteShardedDtgInitDates(x)
  x <- expConnect(x)
  x <- sqliteShardedDtgInitObtypes(x)
}
