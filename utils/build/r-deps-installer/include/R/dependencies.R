
dependencies <- function(pkgName, db,
  recDepth=0,
  levelOneDepsType=c("Depends", "Imports", "LinkingTo", "Suggests"),
  levelTwoDepsType=c("Depends", "Imports", "LinkingTo"),
  exclude=rownames(installed.packages(priority="base"))
){
  #  Recursively search dependencies of package "pkgName".
  #  Useful wrapper around tools::package_dependencies that gives finer
  #  control over how to determine such dependencies.
  #
  #  pkgName: Package(s) for which dependencies should be searched
  #  levelOneDepsType: What type of dependencies should be considered for the
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

  depsType <- levelOneDepsType
  if(recDepth>0) depsType <- levelTwoDepsType
  deps <- unlist(tools::package_dependencies(pkgName, db=db,
                          which=depsType,
                          recursive=FALSE
                        )
          )
  deps <- deps[!(deps %in% exclude)]

  if(length(deps)==0) {
    return(character(0))
  } else {
    deps <- unique(c(
      dependencies(deps, db=db, recDepth=recDepth+1, exclude=exclude),
      deps
    ))
    return(deps)
  }
}
