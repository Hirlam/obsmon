dtg2date <- function(dtg) {
  paste(substr(dtg, 1, 4), substr(dtg, 5, 6), substr(dtg, 7, 8), sep="-")
}

date2dtg <- function(date) {
  as.integer(paste0(substr(date, 1, 4),
                    substr(date, 6, 7),
                    substr(date, 9, 10),
                    "00"))
}
