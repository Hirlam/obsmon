clamp <- function(value, min, max, default=max) {
  if (anyNA(value) || length(value)==0) {
    default
  } else if (isTRUE(value<min)) {
    min
  } else if (isTRUE(value>max)) {
    max
  } else {
    value
  }
}

slugify <- function(string) {
  normalized <- stri_trans_general(string, "nfkd; Latin-ASCII; lower")
  slugified <- stri_replace_all_charclass(normalized, "\\p{WHITE SPACE}", "_")
  slugified <- gsub("_+", "_", gsub("\\W", '_', slugified))
  slugified
}

strLowDashToTitle <- function(str) {
  # Replace lowDash by space and convert to title case.
  # If conversion fails, the value of the passed argument is returned.
  # Eg.: strLowDashToTitle("my_var_name") == "My Var Name"
  rtn <- tryCatch(
    paste(tools::toTitleCase(unlist(strsplit(str, "_"))),collapse=" "),
    error=function(e) str
  )
  return(rtn)
}

toLowerTrimAndSingleSpaces <- function(str) {
  trimws(gsub(' +',' ',tolower(str)))
}

getChildPIDs <- function(pid) {
  cmd <- sprintf('pstree -pn %s', pid)
  patt <- "\\([[:digit:]]*\\)"
  childPIDs <- tryCatch({
      cmdStdOut <- system(cmd, intern=TRUE, ignore.stderr=TRUE)
      cPIDs <- unlist(regmatches(cmdStdOut, gregexpr(patt, cmdStdOut)))
      cPIDs <- gsub("[\\(\\)]", "", cPIDs)
      cPIDs <- as.integer(cPIDs[!cPIDs %in% as.character(pid)])
      cPIDs
    },
    warning=function(w) {
      flog.debug(sprintf("getChildPIDs: %s", w))
      return(integer(0))
    }
  )
  return(childPIDs)
}

killProcessTree <- function(pid, warnFail=FALSE) {
  pidsToKill <- c(pid, getChildPIDs(pid))
  flog.debug("Killing jobs with PID(s) %s", paste(pidsToKill, collapse=", "))
  killWithSIGTERM <- tools::pskill(pidsToKill, tools::SIGTERM)
  killWithSIGKILL <- tools::pskill(pidsToKill, tools::SIGKILL)
  killSuccess <- killWithSIGTERM | killWithSIGKILL
  gc()
  if(warnFail && !all(killSuccess)) {
    msg <- "killProcessTree: Failed to kill procs with the following PIDs:"
    for(pid in pidsToKill[!killSuccess]) msg <- paste0(msg, "\n  > ", pid)
    flog.warn(msg)
  }
}

dtg2date <- function(dtg) {
  dtgAsPOSIXlt <- strptime(dtg, format="%Y%m%d%H", tz="GMT")
  rtn <- strftime(dtgAsPOSIXlt, format="%Y-%m-%d", tz="GMT")
  if(anyNA(rtn) || length(rtn)==0) rtn <- character(0)
  return(rtn)
}

date2dtg <- function(date, cycle=NULL) {
  if(anyNA(date) || length(date)==0) return(integer(0))
  year <- substr(date, 1, 4)
  month <- substr(date, 6, 7)
  day <- substr(date, 9, 10)
  if(typeof(cycle) == "integer") {
    cycle <- sprintf("%02d", cycle)
  }
  as.integer(paste0(year, month, day, cycle))
}

dtg2POSIXct <- function(dtg) {
  as.POSIXct(as.character(dtg), format="%Y%m%d%H", tz="UTC")
}

expandDateRange <- function(start, end, format="%Y%m%d") {
  start <- date2dtg(start)
  end <- date2dtg(end)
  if(length(start)==0 || length(end)==0) return(character(0))
  if(isTRUE(start>end)) {
    temp <- start
    start <- end
    end <- temp
  }
  rtn <- seq(
    as.Date(as.character(start), format=format),
    as.Date(as.character(end), format=format),
    by="days"
  )
  rtn <- strftime(rtn, format=format)
  return(rtn)
}

summariseDtgRange <- function(dateRange) {
  startDate <- dateRange[[1]]
  endDate <- dateRange[[2]]
  cycles <- tryCatch(
    as.integer(dateRange[[3]]),
    error=function(e) {flog.error("summariseDtgRange: %s", e); integer(0)}
  )
  if(length(cycles)==0) {
    cycles <- integer(0)
    minCycle <- 0
    maxCycle <- 24
  } else {
    minCycle <- min(cycles)
    maxCycle <- max(cycles)
  }
  startDtg <- date2dtg(startDate, minCycle)
  endDtg <- date2dtg(endDate, maxCycle)
  list(startDtg, endDtg, cycles)
}

expandDtgRange <- function(range, cycles=NULL) {
  startDtg <- strptime(range[[1]], format="%Y%m%d%H", tz="GMT")
  endDtg <- strptime(range[[2]], format="%Y%m%d%H", tz="GMT")
  dateTimes <- seq(from=startDtg, to=endDtg, by="hour")

  rtn <- as.integer(strftime(dateTimes, format="%Y%m%d%H", tz="GMT"))
  if(length(cycles)>0) rtn <- rtn[(rtn %% 100) %in% as.integer(cycles)]
  return(rtn)
}

formatDtg <- function(dtg) {
  n = length(dtg)
  if (n==1) {
    strftime(dtg2POSIXct(dtg), format="%Y-%m-%d %H %Z", tz="UTC")
  } else if (n==3) {
    if (dtg[[1]] == dtg[[2]]) {
      if(length(dtg[[3]])>0) {
        sprintf("[%s (%s)]", dtg[[1]], paste(dtg[[3]], collapse=", "))
      } else {
        sprintf("[%s]", dtg[[1]])
      }
    } else {
      if(length(dtg[[3]])>0) {
        sprintf(
          "[%s\u2013%s (%s)]",
          dtg[[1]], dtg[[2]], paste(dtg[[3]], collapse=", ")
        )
      } else {
        sprintf("[%s\u2013%s]", dtg[[1]], dtg[[2]])
      }
    }
  } else {
    flog.error('Invalid dtg selection: "%s"', dtg)
  }
}

# Get a statID --> stationLabel list for synop obs
readSynopStations <- function() {
  raw <- read.table(file.path("data", "allsynop.list.csv"),
                    sep=";", quote="",
                    col.names=c("statids", "designation"),
                    colClasses="character", encoding="UTF-8")
  synopStations <- raw$designation
  names(synopStations) <- raw$statids
  synopStations
}
synopStations <- readSynopStations()

putLabelsInStations <- function(stations=NULL, obname=NULL) {
  # Used in the server logic
  if(length(stations)==0) return(stations)
  if(isTRUE(obname=="synop")) {
    stationLabels <- c()
    for(statID in stations) {
      statName <- tryCatch(synopStations[statID], error=function(e) NA)
      label <- statID
      if(!anyNA(statName)) label <- sprintf("%s (%s)", statID, statName)
      stationLabels <- c(stationLabels, label)
    }
    names(stations) <- stationLabels
  } else {
    names(stations) <- stations
  }
  return(stations)
}

# Helper function to show progress message when plotting
readPlotProgressFile <- function(path) {
  fContents <- tryCatch(unlist(read.table(path), use.names=FALSE),
    error=function(e) NULL,
    warning=function(w) NULL
  )
  rtn <- list(current=fContents[1], total=fContents[2])
  return(rtn)
}
