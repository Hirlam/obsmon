.getThisFilePath <- function() {
  argv <- commandArgs(trailingOnly = FALSE)
  thisFilePath <- normalizePath(sub("--file=", "", argv[grep("--file=", argv)]))

  # Account for the fact that the script may be executed via a symlink
  rtn <- Sys.readlink(thisFilePath)
  if(rtn=="" || anyNA(rtn)) rtn <- thisFilePath

  return(rtn)
}

.filesAreR <- function(fPaths) {
  rtn <- c()
  for(fPath in fPaths) {
    if(!file.exists(fPath)) rtn <- c(rtn, FALSE)
    else if(tools::file_ext(fPath)=="R") rtn <- c(rtn, TRUE)
    else if(tools::file_ext(fPath)=="") {
      con = file(fPath, "r")
      line = readLines(con, n=1, warn=FALSE)
      close(con)
      if(length(line)>0) {
        rtn <- c(rtn, grepl("Rscript", line, fixed=TRUE, useBytes=TRUE))
      } else {
        rtn <- c(rtn, FALSE)
      }
    }
    else rtn <- c(rtn, FALSE)
  }
  return(rtn)
}

.filesAreR <- Vectorize(function(fPath) {
  if(!file.exists(fPath)) return(False)
  if(tools::file_ext(fPath)=="R") return(TRUE)
  if(tools::file_ext(fPath)=="") {
      con = file(fPath, "r")
      line = readLines(con, n=1, warn=FALSE)
      close(con)
      if(length(line)>0) {
        rtn <- grepl("Rscript", line, fixed=TRUE, useBytes=TRUE)
      } else {
        rtn <-  FALSE
      }
  } else {
    rtn <- FALSE
  }
  return(rtn)
})

.locateRSources <- function(path, ignore_regex=NULL) {
  # Locate R files (descend recursively through directories)
  allFiles <- list.files(path=path, recursive=TRUE, full.names=TRUE)
  # Make sure we don't include ignored patterns
  for(patt in ignore_regex) allFiles <- allFiles[!grepl(patt, allFiles)]
  # Keep only R files
  return(normalizePath(allFiles[.filesAreR(allFiles)]))
}

.getExplicitlyUsedRPkgs <- function(files) {
  # This routine returns a vector containing the names of the R packages
  # that have explicitly been used in the files listed in the input variable.
  #
  # These packages may have been imported explicitly (using the statements
  # listed in the local variable importTypeStatements), or used without
  # explicit import by means of the double-colon syntax
  #
  # Dependencies of these packages are not investigates here.

  basePackagesR <- rownames(installed.packages(priority="base"))

  # Defining the patterns to be used with regex
  pattParentheses <- "[[:space:]]*\\(+[[:space:]]*.+[[:space:]]*\\)"
  patDubColon <- "[[:alpha:]]+\\w*[[:space:]]*::"
  importTypeStatements <- c("library", "require", "requireNamespace",
                            "attachNamespace", "loadNamespace")
  importTypePatts <- c()
  for(importTypeStatement in importTypeStatements) {
    patt <- paste(importTypeStatement, "{1}", pattParentheses, sep='')
    importTypePatts <- c(importTypePatts, patt)
  }

  # Now using regex to find out which packages have been used
  explicitlyUsedPkgs <- c()
  for (file in files) {
    fLines <- trimws(readLines(file))
    fLines <- fLines[!startsWith(fLines, "#")]

    for(patt in importTypePatts) {
      linesWithImports <- grep(patt, fLines, value=TRUE)
      importTypeCalls <- regmatches(linesWithImports, regexpr(patt, linesWithImports))
      importCallsArgs <- gsub("[\\(\\)[:space:]\"]", "",
                           regmatches(importTypeCalls, regexpr("\\(.*?\\)", importTypeCalls))
                         )
      pkgsImportedInFile <- gsub(",.*", "", importCallsArgs)
      explicitlyUsedPkgs <- c(explicitlyUsedPkgs, pkgsImportedInFile)
    }

    doubleColonLines <- grep(patDubColon, fLines, value=TRUE)
    doubleColonUses <- regmatches(doubleColonLines, regexpr(patDubColon, doubleColonLines))
    doubleColonPkgsInFile <- gsub("[:[:space:]]", "", doubleColonUses)

    explicitlyUsedPkgs <- c(explicitlyUsedPkgs, doubleColonPkgsInFile)
  }

  return(sort(unique(explicitlyUsedPkgs[!(explicitlyUsedPkgs %in% basePackagesR)])))
}

.getAvailablePkgs <- function(repos) {
  tryCatch(
    {
      if(!is.null(repos)) {
        if(!any(startsWith(repos, c("file:", "www", "https://", "http://")))) {
          repos <- normalizePath(repos, mustWork=TRUE)
          repos <- paste0("file:", repos)
        }
        available.packages(repos=repos)
      } else {
        NULL
      }
    },
    warning=function(w) {warning(w); NULL},
    error=function(e) {warning(e); NULL}
  )
}

#' @export
fillPkgVersion <- Vectorize(function(pkgName, availablePkgsDb) {
  tryCatch(
    version <- availablePkgsDb[pkgName, "Version"],
    error=function(e) NULL
  )
}, vectorize.args=c("pkgName"))

#' @export
getImportedPkgs <- function(path=".", ignore_regex=NULL, availablePkgsDb=NULL) {
  if(is.null(availablePkgsDb)) availablePkgsDb <- available.packages()
  listOfRFiles <- .locateRSources(path=path, ignore_regex=ignore_regex)
  importedPkgs <- .getExplicitlyUsedRPkgs(listOfRFiles)
  df <- data.frame(Package=importedPkgs)
  df$Version <- fillPkgVersion(df$Package, availablePkgsDb=availablePkgsDb)
  return(df)
}
