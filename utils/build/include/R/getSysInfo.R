
getSysInfo <- function(includeSysName=FALSE, includeSysRelease=FALSE) {
  rVersion <- R.Version()
  platform <- rVersion$platform
  lang <- rVersion$language
  rtn <- paste(lang, platform, sep="-")

  if(includeSysName) {
    sysName <- 'unknown'
    try(
      {
        sysName <- system2("lsb_release", args=c("-i"), stdout=TRUE, stderr=FALSE)
        sysName <- strsplit(sysName, ":[[:space:]]*")[[1]][2]
      },
      silent=TRUE
    )
    rtn <- paste(rtn, sysName, sep="-")
  }

  if(includeSysRelease) {
    sysRel <- 'unknown'
    try(
      {
        sysRel <- system2("lsb_release", args=c("-r"), stdout=TRUE, stderr=FALSE)
        sysRel <- strsplit(sysRel, ":[[:space:]]*")[[1]][2]
      },
      silent=TRUE
    )
    rtn <- paste(rtn, sysRel, sep="-")
  }

  return(rtn)
}
