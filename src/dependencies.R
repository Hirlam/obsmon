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

summarisePkgDepsDf <- function(pkgDepsDf, availablePkgsDb=NULL) {
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
  depsSummary$Version <- fillPkgVersion(allPkgs, availablePkgsDb=availablePkgsDb)
  return(depsSummary)
}

printDepsFromDf <- function(df) {
  if(nrow(df)==0) {
    cat("No imports (and, consequently, no dependencies) found.\n")
    return(NULL)
  }

  dfImports <- df[df$isImport, ]
  dfEssentialDeps <- df[df$isEssentialRecDep & !df$isImport, ]
  dfSuggests <- df[df$isSuggestsDep & !(df$isImport | df$isEssentialRecDep), ]
  # Sort by pkg name
  dfImports <- dfImports[order(dfImports$Package), ]
  dfEssentialDeps <- dfEssentialDeps[order(dfEssentialDeps$Package), ]
  dfSuggests <- dfSuggests[order(dfSuggests$Package), ]

  .printDfSummary <- function(df, dfName) {
    if(nrow(df)>0) {
      cat(paste0(dfName, ":\n"))
      for(irow in seq_len(nrow(df))) {
        cat(paste0(
          "    ", df$Package[irow], " (=", df$Version[irow],")\n"
        ))
      }
    }
  }

  cat("Summary of R-pkgs that the code suggests, depends and imports:\n")
  .printDfSummary(dfSuggests, "Suggests")
  .printDfSummary(dfEssentialDeps, "Depends")
  .printDfSummary(dfImports, "Imports")
  cat("\n")
  cat("Total:", nrow(df), "R-libs.\n")
  cat("#Main (imported) R-libs:", nrow(dfImports), "\n")
  cat("#Essential dependencies for the main R-libs:", nrow(dfEssentialDeps), "\n")
  if(nrow(dfSuggests)>0) cat("#Suggests-type dependencies:", nrow(dfSuggests), "\n")
  cat("\n")
}
