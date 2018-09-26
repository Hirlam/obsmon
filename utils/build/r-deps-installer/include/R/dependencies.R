
dependencies <- function(pkgName, db,
  recDepth=0,
  levelOneDepsType=c("Depends", "Imports", "LinkingTo", "Suggests"),
  levelTwoDepsType=c("Depends", "Imports", "LinkingTo"),
  exclude=rownames(installed.packages(priority="base")),
  exclude_from_suggests=NULL){
  #  Recursively search dependencies if package pkgName.
  #
  #  pkgName: Package(s) for which dependencies should be searched
  #  levelOneDepsType: What type of 1st-order (recursion depth 0) dependencies
  #                    should be included in the search
  #  levelTwoDepsType: What type of secondary (recursion depth>0) dependencies
  #                    should be included in the search
  #  exclude: Dependencies that should not be included in the return value
  #
  #  In principle, we should not need to write such a function, given that
  #  install.packages is supposed to resolve dependencies (the behaviour
  #  implemented here is in principle equivalent to setting dependencies=TRUE
  #  in install.packages). Nonetheless, from our tests on the behaviour of the
  #  install.packages method itself (which defaults to recursively installing
  #  all dependencies listed under "Depends", "Imports", "LinkingTo" in the
  #  DESCRIPTION files of the main packages to be installed), it seems that
  #  that method can yield somewhat erratic results. This function therefore
  #  gives us more control over what is to be installed.

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
    if(("Suggests" %in% depsType) && length(exclude_from_suggests)>0) {
      # Some "Suggests" dependencies may be problematic, and it may be better
      # to accept the risk of removing them than having to solve issues related
      # to system dependencies. The "sf" package is one of such cases at
      # SMHI/LINDA.
      suggestsTypeDeps <- unlist(tools::package_dependencies(pkgName,
                                          db=db,
                                          which=c("Suggests"),
                                          recursive=FALSE
                                        )
                          )
      sugDeps2Rm<-suggestsTypeDeps[suggestsTypeDeps %in% exclude_from_suggests]
      deps <- deps[!(deps %in% sugDeps2Rm)]
    }

    deps <- unique(c(dependencies(deps, db=db, recDepth=recDepth+1), deps))
    return(deps)
  }
}
