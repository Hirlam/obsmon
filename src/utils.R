clamp <- function(value, min, max, default=max) {
  if (is.null(value)) {
    default
  } else if (value < min) {
    min
  } else if (value > max) {
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

dtg2date <- function(dtg) {
  paste(substr(dtg, 1, 4), substr(dtg, 5, 6), substr(dtg, 7, 8), sep="-")
}

date2dtg <- function(date, cycle=NULL) {
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
  if(start>end) {
    temp <- start
    start <- end
    end <- temp
  }
  startAsChar <- as.character(start)
  endAsChar <- as.character(end)
  rtn <- seq(
    as.Date(startAsChar, format=format),
    as.Date(endAsChar, format=format),
    by="days"
  )
  rtn <- strftime(rtn, format=format)
  if(is.numeric(c(start, end))) rtn <- as.integer(rtn)
  return(rtn)
}

expandDtgRange <- function(dateRange) {
  startDate <- dateRange[[1]]
  endDate <- dateRange[[2]]
  cycles <- as.integer(dateRange[[3]])
  if(length(cycles)==0) {
    cycles <- NULL
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
    flog.error("Invalid dtg selection")
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

units <- list(
    "u"    = "m/s",
    "ff"   = "m/s",
    "u10m" = "m/s",
    "ff10m"= "m/s",
    "apd"  = "m",
    "v"    = "m/s",
    "v10m" = "m/s",
    "t2m"  = "K",
    "t"    = "K",
    "q"    = "kg/m3",
    "z"    = "m",
    "rh2m" = "%",
    "snow" = "kg/m2",
    "rad"  = "K",
    "radv" = "m/s",
    "dbz"  = "db",
    "rh"   = "%",
    "bend_angle" = "rad"
)
