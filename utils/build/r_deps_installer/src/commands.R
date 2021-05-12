.getDependenciesSummaryDf <- function(args, verbose=TRUE) {
  # Get imports and deps
  if(verbose) {
    cat("Getting imports and dependencies...\n")
    cat("  * Getting recursive dependencies from", args$repos, "\n")
  }
  availablePkgsDb <- available.packages(repos=args$repos)
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
    repos=args$repos,
    binDirs=args$bin_repo_path,
    outputDirs=args$output_dirs,
    liveViewLog=args$live_view_install_log,
    keepFullLog=args$keep_full_install_log,
    available=depsSummaryAndAvPkgs$availablePkgsDb,
    dryRun=args$dry_run,
    configure.args=args$configure_args,
    configure.vars=args$configure_vars
  )
}

list_deps <- function(args) {
  depsSummaryAndAvPkgs <- .getDependenciesSummaryDf(
    args, verbose=!args$simple_listdeps
  )
  invisible(printDepsFromDf(
    depsSummaryAndAvPkgs$depsSummaryDf, verbose=!args$simple_listdeps
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
    cat("Removing dir", args$output_rootdir, "\n")
    unlink(args$output_rootdir, recursive=TRUE)
  } else {
    for (dirType in names(dirList)) {
      if(!dirType %in% args$clean) next
      cat("Removing dir", dirList[[dirType]], "\n")
      unlink(dirList[[dirType]], recursive=TRUE)
    }
  }
}
