.printOnSameLine <- function(newStatus) {
  blankLine <- strrep(" ", getOption("width"))
  cat(paste0("\r", blankLine, "\r", newStatus))
}

.printProgress <- function(action, ipkg, npkgs, pkgName, pkgVersion) {
  msg <- sprintf(
    'Progress %.0f%%, %s R-lib %d/%d: %s (%s) ... ',
    100*((ipkg-1) / npkgs), action, ipkg, npkgs, pkgName, pkgVersion
  )
  .printOnSameLine(msg)
}
