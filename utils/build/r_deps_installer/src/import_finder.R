.filesAreR <- Vectorize(function(fPath) {
  if(!file.exists(fPath)) return(FALSE)
  if(tools::file_ext(fPath)=="R") return(TRUE)
  if(tools::file_ext(fPath)=="") {
    line = readLines(fPath, n=1, warn=FALSE)
    if(length(line)>0) return(grepl("Rscript", line, fixed=TRUE, useBytes=TRUE))
    return(FALSE)
  }
  return(FALSE)
})

.locateRSources <- function(path, ignore_regex=NULL) {
  # Locate R files (descend recursively through directories)
  allFiles <- list.files(path=path, recursive=TRUE, full.names=TRUE)
  # Normalise paths to, e.g., resolve symlinks
  allFiles <- normalizePath(allFiles)
  # Make sure we don't include ignored patterns
  allFiles <- allFiles[!grepl(paste(ignore_regex, collapse="|"), allFiles)]
  # Keep only R files
  rFilesMask <- .filesAreR(allFiles)
  if(any(rFilesMask)) return(allFiles[rFilesMask])
  else return(c())
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

getImportedPkgs <- function(path=".", ignore_regex=NULL, availablePkgsDb=NULL) {
  if(is.null(availablePkgsDb)) availablePkgsDb <- available.packages()
  listOfRFiles <- .locateRSources(path=path, ignore_regex=ignore_regex)
  importedPkgs <- .getExplicitlyUsedRPkgs(listOfRFiles)
  df <- data.frame(Package=importedPkgs, stringsAsFactors=FALSE)
  return(df)
}
