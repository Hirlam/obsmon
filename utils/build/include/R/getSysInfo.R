
getSysInfo <- function() {
  platform <- R.Version()$platform
  sysName <- 'unknown'
  sysRel <- 'unknown'
  try(
    {
      sysName <- system2("lsb_release", args=c("-i"), stdout=TRUE, stderr=FALSE)
      sysRel <- system2("lsb_release", args=c("-r"), stdout=TRUE, stderr=FALSE)
      sysName <- strsplit(sysName, ":[[:space:]]*")[[1]][2]
      sysRel <- strsplit(sysRel, ":[[:space:]]*")[[1]][2]
    },
    silent=TRUE
  )
  return(paste(platform, sysName, sysRel, sep="-"))
}
