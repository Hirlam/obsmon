getSysInfo <- function() {
  rtn <- R.Version()$platform

  sysName <- 'unknown'
  try(
    {
      sysName <- system2("lsb_release", args=c("-i"), stdout=TRUE, stderr=FALSE)
      sysName <- strsplit(sysName, ":[[:space:]]*")[[1]][2]
    },
    silent=TRUE
  )
  rtn <- paste(rtn, sysName, sep="-")

  sysRel <- 'unknown'
  try(
    {
      sysRel <- system2("lsb_release", args=c("-r"), stdout=TRUE, stderr=FALSE)
      sysRel <- strsplit(sysRel, ":[[:space:]]*")[[1]][2]
      sysRel <- strsplit(sysRel, ".", fixed=TRUE)[[1]][1]
    },
    silent=TRUE
  )
  rtn <- paste(rtn, sysRel, sep="-")

  return(rtn)
}
