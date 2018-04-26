
getExplicitlyUsedRPkgs <- function(files) {
  # These may have been imported explicitly using "library" or "require",
  # or used without explicit import (double-colon syntax).

  basePackagesR <- rownames(installed.packages(priority="base"))

  pattParentheses <- "[[:space:]]*\\(+[[:space:]]*.+[[:space:]]*\\)"
  pattLib <- paste("library+", pattParentheses, sep='')
  pattReq <- paste("require+", pattParentheses, sep='')
  patDubColon <- "[[:alpha:]]+\\w*[[:space:]]*::"

  importedPkgs <- c()
  doubleColonPkgs <- c()
  for (file in files) {
    fLines <- readLines(file)
    importedPkgs <- c(importedPkgs, grep(pattLib, fLines, value=TRUE))
    importedPkgs <- c(importedPkgs, grep(pattReq, fLines, value=TRUE))
    doubleColonPkgs <- c(doubleColonPkgs, grep(patDubColon, fLines, value=TRUE))
  }

  # Keeping only package names (i.e., no punctuation, white spaces etc.)
  importedPkgs <- unique(
    gsub("[\\(\\)[:space:]]", "",
      regmatches(importedPkgs, gregexpr("\\(.*?\\)", importedPkgs))
    )
  )
  doubleColonPkgs <- unique(
    gsub("[:[:space:]]", "",
      regmatches(doubleColonPkgs, gregexpr(patDubColon, doubleColonPkgs))
    )
  )

  # Finally building the list of main packages used.
  # Dependencies are not yet considered at this stage.
  rtn <- c(importedPkgs, doubleColonPkgs)
  rtn <- sort(unique(rtn[!(rtn %in% basePackagesR)]))
  return(rtn)
}
