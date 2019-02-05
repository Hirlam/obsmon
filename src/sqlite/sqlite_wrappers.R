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
    range <- expandDtgRange(val)
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
  rtn <- is.null(crit) || is.na(crit) || length(crit)==0 || crit==''
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
          newCon<-dbConnect(RSQLite::SQLite(),dbpath,flags=RSQLite::SQLITE_RO)
          dbExecute(newCon, "PRAGMA  journal_mode=OFF")
          dbExecute(newCon, "PRAGMA synchronous=off")
      } else {
          newCon<-dbConnect(RSQLite::SQLite(),dbpath,flags=RSQLite::SQLITE_RW)
          dbExecute(newCon, "PRAGMA foreign_keys=ON")
      }
      dbExecute(newCon, sprintf("PRAGMA  mmap_size=%s", 1024**3))
      dbExecute(newCon, sprintf("PRAGMA  cache_size=%s", 1024**3))
      # Time in milliseconds to wait before signalling that a DB is busy
      dbExecute(newCon, "PRAGMA  busy_timeout=1000")
      newCon
    },
    error=function(e) {
      if(showWarnings) flog.error("dbConnectWrapper (file %s): %s", dbpath, e)
      NULL
    },
    warning=function(w) {
      if(showWarnings) flog.error("dbConnectWrapper (file %s): %s", dbpath, w)
      NULL
    }
  )
  return(con)
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

performQuery <- function(
  db, query, dtgs=NULL, expandRange=TRUE, convertDTG=TRUE)
{
  if (is.null(dtgs)) {
    dbpaths <- db$paths
  } else {
    if (expandRange) {
      n = length(dtgs)
      if (n==1) {
        dbpaths <- db$paths[as.character(dtgs)]
      } else if (n==3) {
        range <- expandDtgRange(dtgs)
        dbpaths <- db$paths[range[[1]] <= db$dtgs & db$dtgs <= range[[2]]]
      } else {
        flog.error("Invalid combination of expandRange and dtgs.")
      }
    } else {
      dbpaths <- db$paths[as.character(dtgs)]
    }
  }
  dbpaths <- dbpaths[!is.null(dbpaths)]
  dbpaths <- dbpaths[file.exists(dbpaths)]
  if (length(dbpaths)==0) {
    flog.error("performQuery: No usable database found. Please check paths.")
    return(NULL)
  }
  singleQuery <- makeSingleQuery(query)
  res <- lapply(dbpaths, singleQuery)
  res <- do.call(rbind, res)
  if(convertDTG & "DTG" %in% names(res)) {
    res$DTG <- as.POSIXct(as.character(res$DTG), format="%Y%m%d%H")
  }
  res
}
