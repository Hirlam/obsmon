
getExplicitlyUsedRPkgs <- function(files) {
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

