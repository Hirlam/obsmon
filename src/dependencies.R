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

  deps <- unlist(tools::package_dependencies(
    pkgName, db=db, which=which, recursive=FALSE
  ), recursive=FALSE, use.names=FALSE)
  deps <- unique(deps[!(deps %in% exclude)])

  if(length(deps)==0) return(character(0))
  depsOfDeps <- unlist(getDependencies(
    deps, db=db, which=recursiveDepsType, exclude=exclude, recDepth=recDepth+1
  ))
  return(unique(c(depsOfDeps, deps)))

}, vectorize.args=c("pkgName"), SIMPLIFY=FALSE)

fillInPkgDeps <- function(importedPkgsDf, availablePkgsDb=NULL) {
  if(is.null(availablePkgsDb)) availablePkgsDb <- available.packages()
  importedPkgsDf$essentialDeps <- getDependencies(
    importedPkgsDf$Package,
    db=availablePkgsDb
  )
  importedPkgsDf$suggestsDeps <- getDependencies(
    importedPkgsDf$Package,
    db=availablePkgsDb,
    which=c("Suggests")
  )
  return(importedPkgsDf)
}

getImportedPkgsDepsDf <- function(
  path=".", ignore_regex=NULL, availablePkgsDb=NULL
) {
  if(is.null(availablePkgsDb)) availablePkgsDb <- available.packages()
  importedPkgs <- getImportedPkgs(
    path=path, ignore_regex=ignore_regex, availablePkgsDb=availablePkgsDb
  )
  return(fillInPkgDeps(
    importedPkgsDf=importedPkgs, availablePkgsDb=availablePkgsDb
  ))
}

summarisePkgDepsDf <- function(pkgDepsDf, availablePkgsDb=NULL) {

  essentials <- do.call(c, pkgDepsDf$essentialDeps)
  suggests <- do.call(c, pkgDepsDf$suggestsDeps)
  imports <- pkgDepsDf$Package
  depsSummary <- data.frame(Package=unique(c(essentials, suggests, imports)))
  depsSummary$Version <- fillPkgVersion(
    depsSummary$Package, availablePkgsDb=availablePkgsDb
  )

  .isType <- Vectorize(function(pkgName, type) {
    return(pkgName %in% get(type))
  }, vectorize.args=c("pkgName"))
  depsSummary$isImport <- .isType(depsSummary$Package, "imports")
  depsSummary$isEssentialRecDep <- .isType(depsSummary$Package, "essentials")
  depsSummary$isSuggestsDep <- .isType(depsSummary$Package, "suggests")

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

  cat("Summary of R-pkgs that the code suggests, depends and imports:\n")
  if(nrow(dfSuggests)>0) {
    cat("Suggests:\n")
    for(irow in seq_len(nrow(dfSuggests))) {
      cat(paste0(
        "    ",dfSuggests$Package[irow]," (=",dfSuggests$Version[irow],")\n"
      ))
    }
  }
  if(nrow(dfEssentialDeps)>0) {
    cat("Depends:\n")
    for(irow in seq_len(nrow(dfEssentialDeps))) {
      cat(paste0(
        "    ",dfEssentialDeps$Package[irow]," (=",dfEssentialDeps$Version[irow],")\n"
      ))
    }
  }
  if(nrow(dfImports)>0) {
    cat("Imports:\n")
    for(irow in seq_len(nrow(dfImports))) {
      cat(paste0(
        "    ",dfImports$Package[irow]," (=",dfImports$Version[irow],")\n"
      ))
    }
  }
  cat("\n")
  cat("Total:", nrow(df), "R-libs to be built/installed.\n")
  cat("Main R-libraries:", nrow(dfImports), "\n")
  cat("Essential dependencies for the main R-libs:", nrow(dfEssentialDeps), "\n")
  cat("Suggests-type dependencies:", nrow(dfSuggests), "\n")
  cat("\n")
}
