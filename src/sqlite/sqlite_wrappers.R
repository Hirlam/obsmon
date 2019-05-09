########################################################################
# Utility routines to create, format and validate queries to be passed #
# to sqlite, as well as convenience wrappers to facilitate the use of  #
# sqlite throughout the code                                           #
########################################################################

dtgClause <- function(val) {
  n = length(val)
  if (n==1) {
    sprintf("(DTG = %d)", val)
  } else if (n==3) {
    range <- summariseDtgRange(val)
    clause <- sprintf("(%d <= DTG) AND (DTG <= %d)", range[[1]], range[[2]])
    if(length(range[[3]])>0) {
      clause <- paste(clause,
        sprintf("AND (DTG %% 100 IN (%s))", paste(range[[3]], collapse=", "))
      )
    }
    clause
  } else {
    NULL
  }
}

criterion2clause <- function(name, criteria) {
  val <- criteria[[name]]
  switch(
    name,
    "dtg"=dtgClause(val),
    "levels"=sprintf("(level IN (%s))",
                     do.call(partial(paste, sep=", "), as.list(val))),
    "excludeLevels"=sprintf("(level NOT IN (%s))",
                     do.call(partial(paste, sep=", "), as.list(val))),
    "station"={
      if(all(val=="")) {
        NULL
      } else {
        stationQueryStr <- paste0("statid like '%%%",val,"%%'",collapse=" OR ")
        sprintf("(%s)", stationQueryStr)
      }
    },
    switch(
        typeof(val),
        "integer"=sprintf("(%s = %d)", name, val),
        "character"=sprintf("(%s = '%s')", name, val)
    )
  )
}

notValidCrit <- function(crit) {
  rtn <- is.null(crit) || anyNA(crit) || length(crit)==0 || crit==''
  return(rtn)
}

buildWhereClause <- function(criteria) {
  subclauses <- list()
  criteriaNames <- names(criteria)
  crits <- lapply(criteriaNames[criteriaNames!="info"],
                  partial(criterion2clause, criteria=criteria))
  crits <- crits[!(is.null(crits))]
  # Removing NULL, NA or empty values from the crits list.
  # This avoids, e.g., building invalid queries with dangling ANDs
  # (e.g., "WHERE AND", "AND AND", etc)
  crits[sapply(crits, notValidCrit)] <- NULL
  do.call(partial(paste, sep=" AND "), crits)
}

dbConnectWrapper <- function(dbpath, read_only=FALSE, showWarnings=TRUE) {
  con <- tryCatch({
      if(read_only) {
          newCon <- dbConnect(
            RSQLite::SQLite(),dbpath,flags=RSQLite::SQLITE_RO,
            synchronous=NULL
           )
          tryCatch({
              dbExecute(newCon, "PRAGMA synchronous=off")
              dbExecute(newCon, "PRAGMA  journal_mode=OFF")
            },
            error=function(e) {flog.debug(e); NULL}
          )
      } else {
          newCon<-dbConnect(RSQLite::SQLite(),dbpath,flags=RSQLite::SQLITE_RW)
          dbExecute(newCon, "PRAGMA foreign_keys=ON")
      }
      tryCatch({
          dbExecute(newCon, sprintf("PRAGMA  mmap_size=%s", 1024**3))
          dbExecute(newCon, sprintf("PRAGMA  cache_size=%s", 1024**3))
          # Time in milliseconds to wait before signalling that a DB is busy
          dbExecute(newCon, "PRAGMA  busy_timeout=1000")
        },
        error=function(e) {flog.debug(e); NULL}
      )
      newCon
    },
    error=function(e) {
      if(showWarnings) flog.error("dbConnectWrapper: %s (%s)", e, dbpath)
      NULL
    },
    warning=function(w) {
      if(showWarnings) flog.error("dbConnectWrapper: %s (%s)", w, dbpath)
      NULL
    }
  )
  return(con)
}

dbDisconnectWrapper <- function(con) {
  if(is.null(con)) return(NULL)
  tryCatch(dbDisconnect(con),
    error=function(e) flog.error("dbDisconnectWrapper: %s", e),
    warn=function(w) flog.warn("dbDisconnectWrapper: %s", w)
  )
}

singleFileQuerier <- function(dbpath, query) {
  con <- dbConnectWrapper(dbpath, read_only=TRUE)
  # If con if NULL then we could not connect. Returning NULL silently here,
  # as the dbConnectWrapper will have a better error message in this case.
  if(is.null(con)) return(NULL)

  res <- tryCatch(
    dbGetQuery(con, query),
    error=function(e) {
      flog.warn(
        "singleFileQuerier: Error querying %s:\n%s\nIgnoring.",
        dbpath, e
      )
      NULL
    }
  )
  dbDisconnectWrapper(con)
  res
}

performQuery <- function(db, query, dtgs, queryChunkSize=24) {
  selectedDtgs <- dtgs
  if(length(dtgs)>1) {
    range <- summariseDtgRange(dtgs)
    selectedDtgs <- expandDtgRange(range)
  }
  dbpaths <- db$getDataFilePaths(selectedDtgs)
  if(length(dbpaths)>1) {
    # Queries will be split in newNWorkers batches which will be processed
    # simultaneously (in parallel). The number of queries in each batch will
    # be approx queryChunkSize
    originalNbrOfWorkers <- nbrOfWorkers()
    newNWorkers <- 1 + ceiling(length(dbpaths)/abs(queryChunkSize))
    plan(multiprocess, workers=newNWorkers)
    on.exit({
      plan(sequential)
      plan(multiprocess, workers=originalNbrOfWorkers)
    })
  }

  res <- future_lapply(dbpaths,
    partial(singleFileQuerier, query=query),
    future.chunk.size=queryChunkSize
  )
  res <- do.call(rbind, res)
  if("DTG" %in% names(res)) {
    # Convert DTGs from integers to POSIXct
    res$DTG <- as.POSIXct(as.character(res$DTG), format="%Y%m%d%H")
  }
  res
}
