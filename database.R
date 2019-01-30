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
