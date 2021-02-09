.getDependencies <- function(pkgName, db,
  recDepth=0,
  which=c("Depends", "Imports", "LinkingTo"),
  exclude=rownames(installed.packages(priority="base"))
){
  #  Recursively search dependencies of package "pkgName".
  #  Useful wrapper around tools::package_dependencies that gives finer
  #  control over how to determine such dependencies.
  #
  #  pkgName: Package(s) for which dependencies should be searched
  #  which: What type of dependencies should be considered for the
  #                    first search (at recursion depth == 0)
  #  levelTwoDepsType: What type of secondary (recursion depth>0) dependencies
  #                    should be included in the search
  #  exclude: Dependencies that should not be included in the return value
  #
  #  Called with default values for the arguments, this function returns a
  #  list of dependencies that would, in principle, be installed if one would
  #  call install.packages with the same pkgName arg. We use "in principle"
  #  because we seen cases in which install.packages showed a somewhat erratic
  #  behaviour when it comes to the dependencies it installs.

  if(length(pkgName)>1) stop("Only one package at a time is supported.")
  if(length(pkgName)==0) return(NULL)

  levelTwoDepsType=c("Depends", "Imports", "LinkingTo")
  depsType <- which
  if(recDepth>0) depsType <- levelTwoDepsType
  deps <- unlist(tools::package_dependencies(
    pkgName, db=db, which=depsType, recursive=FALSE
  ))
  deps <- deps[!(deps %in% exclude)]
  #if(is.null(deps)) return(NA)
  #if(length(deps)==0) return(deps)

  for(dep in deps) {
    deps <- c(
      .getDependencies(
        dep, db=db, recDepth=recDepth+1,
        which=levelTwoDepsType,
        exclude=exclude
      ),
      deps
    )
  }
  return(unique(deps))
}

getDependencies <- Vectorize(.getDependencies, vectorize.args=c("pkgName"))

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
  # Remove from suggests those deps that also appear as essential
  vec_setdiff <- Vectorize(setdiff)
  importedPkgsDf$suggestsDeps <- vec_setdiff(
    importedPkgsDf$suggestsDeps,
    importedPkgsDf$essentialDeps
  )
  return(importedPkgsDf)
}

getImportedPkgsDepsDf <- function(
  path=".", ignore_regex=NULL, availablePkgsDb=NULL
) {
  if(is.null(availablePkgsDb)) availablePkgsDb <- available.packages()
  importedPkgs <- importfinder::getImportedPkgs(
    path=path, ignore_regex=ignore_regex, availablePkgsDb=availablePkgsDb
  )
  return(fillInPkgDeps(
    importedPkgsDf=importedPkgs, availablePkgsDb=availablePkgsDb
  ))
}

summarisePkgDepsDf <- function(pkgDepsDf, availablePkgsDb=NULL) {

  imports <- c()
  essentials <- c()
  suggests <- c()
  for(irow in seq_len(nrow(pkgDepsDf))) {
    imports <- c(
      imports,
      unlist(pkgDepsDf$Package[irow], use.names=FALSE)
    )
    suggests <- c(
      suggests,
      unlist(pkgDepsDf$suggestsDeps[irow], use.names=FALSE)
    )
    essentials <- c(
      essentials,
      unlist(pkgDepsDf$essentialDeps[irow], use.names=FALSE)
    )
  }

  essentials <- unique(essentials)
  suggests <- unique(suggests)
  imports <- unique(imports)
  depsSummary <- data.frame(Package=unique(c(essentials, suggests, imports)))
  depsSummary$Version <- importfinder::fillPkgVersion(
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
}
