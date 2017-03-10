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
