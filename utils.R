dtg2date <- function(dtg) {
  paste(substr(dtg, 1, 4), substr(dtg, 5, 6), substr(dtg, 7, 8), sep="-")
}

date2dtg <- function(date, cycle) {
  year <- substr(date, 1, 4)
  month <- substr(date, 6, 7)
  day <- substr(date, 9, 10)
  if(typeof(cycle) == "integer") {
    cycle <- sprintf("%02d", cycle)
  }
  as.integer(paste0(year, month, day, cycle))
}

dtg2POSIXct <- function(dtg) {
  as.POSIXct(as.character(dtg), format="%Y%m%d%H")
}

formatDtg <- function(dtg) {
  dtgStrings <- as.list(strftime(dtg2POSIXct(dtg), format="%Y-%m-%d %HZ"))
  paste0('[', do.call(partial(paste, sep=" - "), dtgStrings), ']')
}

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
