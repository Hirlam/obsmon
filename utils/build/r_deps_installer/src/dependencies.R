.pkgDepsCache <- list()
.package_dependencies <- function(pkg, which, ...) {
  # Wapper to tools::package_dependencies that caches results
  if(length(pkg) > 1) stop("Vector argument not supported")
  cacheKey <- paste(pkg, paste(sort(which), collapse="_"), sep="_")
  cached <- .pkgDepsCache[cacheKey]
  if(cacheKey %in% names(.pkgDepsCache)) return(cached)
  deps <- tools::package_dependencies(pkg, which=which, ...)
  .pkgDepsCache[cacheKey] <<- deps
  return(deps)
}

getDependencies <- Vectorize(function(
  pkgName, db, which=c("Depends", "Imports", "LinkingTo"),
  recursiveDepsType=c("Depends", "Imports", "LinkingTo"),
  exclude=rownames(installed.packages(priority="base")),
  recDepth=0
){
  #  Recursively search dependencies of package "pkgName".
  #  Useful wrapper around tools::package_dependencies that gives finer
  #  control over how to determine such dependencies.
  #
  #  pkgName: Package(s) for which dependencies should be searched
  #  db: Result of calling available.packages()
  #  which: What type of dependencies should be considered for the
  #                    first search (at recursion depth == 0)
  #  recursiveDepsType: Type of dependencies included in recursive dep searches
  #  exclude: Dependencies that should not be included in the return value
  #
  #  "suggests"-type dependencies of recursive dependencies are not included.

  if(length(pkgName)==0) return(NULL)

  deps <- unlist(.package_dependencies(
    pkgName, db=db, which=which, recursive=FALSE
  ), recursive=FALSE, use.names=FALSE)
  deps <- unique(deps[!(deps %in% exclude)])

  if(length(deps)==0) return(character(0))
  depsOfDeps <- unlist(getDependencies(
    deps, db=db, which=recursiveDepsType, exclude=exclude, recDepth=recDepth+1
  ))
  return(unique(c(depsOfDeps, deps)))

}, vectorize.args=c("pkgName"), SIMPLIFY=FALSE)

fillInPkgDeps <- function(
  importedPkgsDf, availablePkgsDb=NULL, includeSuggests=FALSE
) {
  if(is.null(availablePkgsDb)) availablePkgsDb <- available.packages()

  importedPkgsDf$essentialDeps <- getDependencies(
    importedPkgsDf$Package, db=availablePkgsDb
  )
  if(includeSuggests) {
    importedPkgsDf$suggestsDeps <- getDependencies(
      importedPkgsDf$Package, db=availablePkgsDb, which=c("Suggests")
    )
  }
  return(importedPkgsDf)
}

getImportedPkgsDepsDf <- function(
  path=".", ignore_regex=NULL, availablePkgsDb=NULL, includeSuggests=FALSE
) {
  if(is.null(availablePkgsDb)) availablePkgsDb <- available.packages()
  importedPkgs <- getImportedPkgs(
    path=path, ignore_regex=ignore_regex, availablePkgsDb=availablePkgsDb
  )
  return(fillInPkgDeps(
    importedPkgsDf=importedPkgs, availablePkgsDb=availablePkgsDb,
    includeSuggests=includeSuggests
  ))
}

fillPkgVersion <- Vectorize(function(pkgName, availablePkgsDb, userDefinedVersions) {
  version <- tryCatch({
    rtn <- userDefinedVersions[pkgName, "Version"]
    if(is.null(rtn) || is.na(rtn)) rtn <- availablePkgsDb[pkgName, "Version"]
    # Prepend an "== " if dep specification starts with number
    if(grepl("^[[:digit:]]+", rtn)) rtn <- paste0("== ", rtn)
    rtn
  },
    error=function(e) NULL
  )
  return(version)
}, vectorize.args=c("pkgName"))

parsePkgVersionSpec <- function(pkgSpecification) {
  tryCatch({
    nonPkgRegex <- "[^[:alnum:].]"
    pkgSpecification <- trimws(pkgSpecification)
    pkgName <- unlist(strsplit(pkgSpecification, nonPkgRegex))[1]
    pkgVersion <- trimws(sub(pkgName, "", pkgSpecification, fixed=TRUE))
    # Add spaces between comparison operators in deps
    pkgVersion <- gsub("([^[:alnum:]._-]+[[:space:]]*)", "\\1 ", pkgVersion)
    pkgVersion <- gsub("(,[[:space:]]*)", "\\1 ", pkgVersion)
    pkgVersion <- gsub('[[:space:]]+',' ', pkgVersion)
    return(list(Package=pkgName, Version=pkgVersion))
  },
    error=function(e) {
      msg <- paste("Problems parsing package specification:", pkgSpecification)
      stop(paste(e$message, msg, sep="\n"))
    }
  )
}

readPkgVersions <- function(fpath) {
  df <- tryCatch({
    pkgNames <- c()
    pkgVersions <- c()
    for(raw in suppressWarnings(readLines(fpath))) {
      item <- parsePkgVersionSpec(raw)
      pkgNames <- c(pkgNames, item$Package)
      pkgVersions <- c(pkgVersions, item$Version)
    }
    data.frame(Package=pkgNames, Version=pkgVersions, stringsAsFactors=FALSE)
  },
    error=function(e) data.frame(Package=NULL, Version=NULL)
  )
  if(nrow(df)>0) rownames(df) <- df$Package
  return(df)
}

summarisePkgDepsDf <- function(
  pkgDepsDf, availablePkgsDb=NULL, userVersionsFile=NULL
) {
  if(nrow(pkgDepsDf)==0) return(data.frame())
  allPkgs <- unique(unlist(c(
    pkgDepsDf$essentialDeps, pkgDepsDf$suggestsDeps, pkgDepsDf$Package
  ), use.names=FALSE))
  .isType <- function(pkgName, type) {
    if(!(type %in% colnames(pkgDepsDf))) return(FALSE)
    return(pkgName %in% unlist(pkgDepsDf[type], use.names=FALSE))
  }
  depsSummary <- data.frame(
    Package=allPkgs,
    Version=NA,
    isImport=.isType(allPkgs, "Package"),
    isEssentialRecDep=.isType(allPkgs, "essentialDeps"),
    isSuggestsDep=.isType(allPkgs, "suggestsDeps"),
    stringsAsFactors=FALSE
  )
  depsSummary$Version <- fillPkgVersion(
    allPkgs,
    userDefinedVersions=readPkgVersions(userVersionsFile),
    availablePkgsDb=availablePkgsDb
  )
  return(depsSummary)
}

printDepsFromDf <- function(df, verbose=TRUE) {
  if(nrow(df)==0) {
    if(verbose) cat("No imports (and, consequently, no dependencies) found.\n")
    return(NULL)
  }

  .printDep <- function(pkg, version, verbose) {
    msg <- pkg
    if(verbose) {
      msg <- paste0("    ", msg)
      if(!is.null(version)) msg <- paste0(msg, " (", version,")")
    } else if(!is.null(version)) {
      msg <- paste(msg, version)
    }
    cat(paste0(msg, "\n"))
  }

  .printDfSummary <- function(df, dfName=NULL) {
    if(nrow(df)>0) {
      if(!is.null(dfName)) cat(paste0(dfName, ":\n"))
      for(irow in seq_len(nrow(df))) {
        .printDep(df$Package[irow], df$Version[irow], verbose)
      }
    }
  }

  if(verbose) {
    dfImports <- df[df$isImport, ]
    dfEssentialDeps <- df[df$isEssentialRecDep & !df$isImport, ]
    dfSuggests <- df[df$isSuggestsDep & !(df$isImport | df$isEssentialRecDep), ]
    # Sort by pkg name
    dfImports <- dfImports[order(dfImports$Package), ]
    dfEssentialDeps <- dfEssentialDeps[order(dfEssentialDeps$Package), ]
    dfSuggests <- dfSuggests[order(dfSuggests$Package), ]

    cat("Summary of R-pkgs that the code suggests, depends and imports:\n")
    .printDfSummary(dfSuggests, "Suggests")
    .printDfSummary(dfEssentialDeps, "Depends")
    .printDfSummary(dfImports, "Imports")
    cat("\n")
    cat("Total:", nrow(df), "R-libs.\n")
    cat("#Main (imported) R-libs:", nrow(dfImports), "\n")
    cat("#Essential dependencies for the main R-libs:", nrow(dfEssentialDeps), "\n")
    if(nrow(dfSuggests)>0) cat("#Suggests-type dependencies:", nrow(dfSuggests), "\n")
  } else {
    .printDfSummary(df[order(df$Package), ])
  }
}
