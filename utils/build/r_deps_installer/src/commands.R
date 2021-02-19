.getDependenciesSummaryDf <- function(args) {
  # Get imports and deps

  args$path <- normalizePath(args$path, mustWork=TRUE)

  # Ignore the default output dir when parsing R sources
  args$ignore <- c(args$ignore, .defaultOutRootdirBasename)
  # Make sure to ignore the calling script itself, as well as its dir
  args$ignore <- unique(c(
    args$ignore,
    paste0("^", callingScriptPath(resolve_symlink=FALSE), "$"),
    paste0("^", file.path(dirname(callingScriptPath()), ".*"))
  ))

  cat("Getting imports and dependencies...\n")
  cat("  * Getting recursive dependencies from", args$repos, "\n")
  availablePkgsDb <- available.packages(repos=args$repos)
  pkgDepsDf <- getImportedPkgsDepsDf(
    path=args$path,
    ignore_regex=args$ignore,
    availablePkgsDb=availablePkgsDb,
    includeSuggests=args$include_suggests
  )
  depsSummaryDf <- summarisePkgDepsDf(
    pkgDepsDf,
    availablePkgsDb=availablePkgsDb
  )
  rtn <- list(
    depsSummaryDf=depsSummaryDf,
    availablePkgsDb=availablePkgsDb    
  )
  return(rtn)
}

install <- function(args) {
  if(is.null(args$bin_repo_path)) args$bin_repo_path <- args$output_dirs[["binaries"]]
  args$bin_repo_path <- normalizePath(args$bin_repo_path, mustWork=FALSE)
  depsSummaryAndAvPkgs <- .getDependenciesSummaryDf(args)
  depsSummaryDf <- depsSummaryAndAvPkgs$depsSummaryDf
  invisible(printDepsFromDf(depsSummaryAndAvPkgs$depsSummaryDf))
  if(nrow(depsSummaryDf)==0) quit(status=0)
  installPkgsFromDf(
    df=depsSummaryDf,
    repos=args$repos,
    binDirs=args$bin_repo_path,
    outputDirs=args$output_dirs,
    liveViewLog=args$live_view_install_log,
    keepFullLog=args$keep_full_install_log,
    available=depsSummaryAndAvPkgs$availablePkgsDb,
    configure.args=args$configure_args,
    configure.vars=args$configure_vars
  )
}

list_deps <- function(args) {
  depsSummaryAndAvPkgs <- .getDependenciesSummaryDf(args)
  invisible(printDepsFromDf(depsSummaryAndAvPkgs$depsSummaryDf))
}

create_local_repo <- function(args) {
  cat("Creating local CRAN-like repo under", args$output_dirs$sources, "...\n\n")
  depsSummaryAndAvPkgs <- .getDependenciesSummaryDf(args)
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
