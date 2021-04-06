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
    "level"=sprintf("(level IN (%s))",
                     do.call(partial(paste, sep=", "), as.list(val))),
    "excludeLevels"=sprintf("(level NOT IN (%s))",
                     do.call(partial(paste, sep=", "), as.list(val))),
    "statid"={
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
          RSQLite::SQLite(), dbpath, flags=RSQLite::SQLITE_RO,
          synchronous=NULL
        )
        tryCatch(
          dbExecute(newCon, "PRAGMA  journal_mode=OFF"),
          error=function(e) NULL
        )
      } else {
        newCon <- dbConnect(
          RSQLite::SQLite(), dbpath, flags=RSQLite::SQLITE_RW,
          synchronous=NULL
        )
        # We really need foreign_keys
        dbExecute(newCon, "PRAGMA foreign_keys=ON")
        # But we can live without journal_mode=WAL. The only risk is to have
        # more "database is locked" errors when trying to read from dbs that
        # are being written to
        tryCatch(
          dbExecute(newCon, "PRAGMA journal_mode=WAL"),
          error=function(e) NULL
        )
      }
      # Some non-critical PRAGMAs for both RW and RO modes
      tryCatch({
          dbExecute(newCon, "PRAGMA synchronous=off")
          dbExecute(newCon, sprintf("PRAGMA  mmap_size=%s", 1024**3))
          dbExecute(newCon, sprintf("PRAGMA  cache_size=%s", 1024**3))
          # Time in milliseconds to wait before signalling that a DB is busy
          dbExecute(newCon, "PRAGMA  busy_timeout=1000")
        },
        error=function(e) NULL
      )
      # No critical errors have occured at this point. Returning the new con.
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
  on.exit(dbDisconnectWrapper(con))
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
  res
}

performSingleQuery <- function(iPath, dbpaths, query, progressFile=NULL) {
  # Using a file to get update on progress of plots
  # Unfortunately there was no other way to do this from within a future
  # at the time this code was written
  if(!is.null(progressFile)) {
    tryCatch({
      progressNow <- readPlotProgressFile(progressFile)
      if(is.null(progressNow$current) || progressNow$current<iPath) {
        write(c(iPath, length(dbpaths)), progressFile, append=FALSE)
      }
    },
      error=function(e) {flog.debug(e); unlink(progressFile); NULL},
      warn=function(w) {flog.debug(w); unlink(progressFile); NULL}
    )
  }

  singleFileQuerier(dbpaths[iPath], query)
}

performQuery <- function(
  db, query, dtgs,
  maxAvgQueriesPerProc=obsmonConfig$general$maxAvgQueriesPerProc,
  progressFile=NULL
) {
  startTime <- Sys.time() # For debug purposes
  selectedDtgs <- dtgs
  if(length(dtgs)>1) {
    range <- summariseDtgRange(dtgs)
    selectedDtgs <- expandDtgRange(range)
  }
  dbpaths <- db$getDataFilePaths(selectedDtgs)

  res <- tryCatch({
      queryResult <- future_lapply(seq_along(dbpaths),
        partial(
          performSingleQuery, dbpaths=dbpaths,
          query=query, progressFile=progressFile
        ),
        future.seed=TRUE, # Just gets rid of a very annoying warning
        future.chunk.size=structure(maxAvgQueriesPerProc, ordering=seq_along(dbpaths))
      )
      do.call(rbind, queryResult)
    },
    error=function(e) {flog.error("preformQuery: %s", e); NULL}
  )
  if("DTG" %in% names(res)) {
    # Convert DTGs from integers to POSIXct
    res$DTG <- as.POSIXct(as.character(res$DTG), format="%Y%m%d%H")
  }

  nQueries <- length(dbpaths)
  avgQueryTime <- Inf
  elapsed <- Sys.time() - startTime
  if(nQueries>0) avgQueryTime <- elapsed / nQueries

  flog.debug(
    paste(
      "preformQuery: Split %d queries into %d processes.\n",
      "  > Total elapsed time: %.3f sec. Avg time per query: %.3f sec."
    ),
    nQueries, max(1, ceiling(nQueries/maxAvgQueriesPerProc)),
    elapsed, avgQueryTime
  )

  return(res)
}
