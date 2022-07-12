.getDependenciesSummaryDf <- function(args, verbose=TRUE) {
  # Get imports and deps

  repos <- args$repos
  if("INSTALLER_DEPS_LOCK" %in% names(repos)) {
      # Constrain deps search to local database, if available
      repos <- repos["INSTALLER_DEPS_LOCK"]
  }

  if(verbose) {
    cat("Getting imports and dependencies...\n")
    cat("  * Getting recursive dependencies from", repos, "\n")
  }
  availablePkgsDb <- available.packages(repos=repos)
  pkgDepsDf <- getImportedPkgsDepsDf(
    path=args$sources_dir,
    ignore_regex=args$ignore,
    availablePkgsDb=availablePkgsDb,
    includeSuggests=args$include_suggests
  )
  depsSummaryDf <- summarisePkgDepsDf(
    pkgDepsDf,
    availablePkgsDb=availablePkgsDb,
    userVersionsFile=args$versions_file
  )
  rtn <- list(
    depsSummaryDf=depsSummaryDf,
    availablePkgsDb=availablePkgsDb
  )
  return(rtn)
}

install <- function(args) {
  depsSummaryAndAvPkgs <- .getDependenciesSummaryDf(args)
  depsSummaryDf <- depsSummaryAndAvPkgs$depsSummaryDf
  invisible(printDepsFromDf(depsSummaryAndAvPkgs$depsSummaryDf))
  if(nrow(depsSummaryDf)==0) quit(status=0)
  cat("\n")
  installPkgsFromDf(
    df=depsSummaryDf,
    # Remove INSTALLER_DEPS_LOCK from the repos used for install, as
    # it is used only to help lock the versions of the dependencies
    repos=args$repos[names(args$repos) != "INSTALLER_DEPS_LOCK"],
    binDirs=args$bin_repo_path,
    outputDirs=args$output_dirs,
    liveViewLog=args$live_view_install_log,
    keepFullLog=args$keep_full_install_log,
    dryRun=args$dry_run,
    configure.args=args$configure_args,
    configure.vars=args$configure_vars
  )
}

list_deps <- function(args) {
  verbose= !args$simple_listdeps & !args$lock_pkg_versions
  depsSummaryAndAvPkgs <- .getDependenciesSummaryDf(args, verbose=verbose)
  filePath <- NULL
  if(args$lock_pkg_versions) filePath <- ".installer_pkg_versions.txt"
  invisible(printDepsFromDf(
    depsSummaryAndAvPkgs$depsSummaryDf, verbose=verbose, filePath=filePath
  ))
}

create_local_repo <- function(args) {
  cat("Creating local CRAN-like repo under", args$output_dirs$sources, "...\n\n")
  depsSummaryAndAvPkgs <- .getDependenciesSummaryDf(args)
  invisible(printDepsFromDf(depsSummaryAndAvPkgs$depsSummaryDf))
  cat("\n")
  depsSummaryDf <- depsSummaryAndAvPkgs$depsSummaryDf
  createLocalRepo(pkgsDf=depsSummaryDf, destdir=args$output_dirs$sources)
  cat("Done creating local CRAN-like repo.\n")
  cat("  > Path to sources:", args$output_dirs$sources, "\n")
}

clean <- function(args) {
  dirList <- args$output_dirs
  if("all" %in% args$clean) {
    cat("Removing the following directories:\n")
    dirsToRemove <- c(args$output_rootdir, INSTALLER_OWN_INSTALL_LIB)
    for(directory in dirsToRemove) {
      cat(sprintf("    > %s\n", directory))
      unlink(directory, recursive=TRUE)
    }
  } else {
    for (dirType in names(dirList)) {
      if(!dirType %in% args$clean) next
      cat("Removing dir", dirList[[dirType]], "\n")
      unlink(dirList[[dirType]], recursive=TRUE)
    }
  }
}
