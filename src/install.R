.binDirsPathSuffix <- function() {
  rVersion <- paste0(version$major, '.', gsub("\\..", "", version$minor))
  return(file.path(getSysInfo(), paste0("R_", rVersion)))
}

getPathToBinary <- function(pkgName, pkgVersion, binDirs) {
  allBinPaths <- list.files(
    file.path(binDirs, .binDirsPathSuffix()),
    pattern="\\.tar\\.gz$|\\.tgz$",
    full.names=TRUE
  )
  .pathToBinary <- Vectorize(function(pkgName, pkgVer) {
    pattPkg <- paste0("^", pkgName, "_{1}")
    pattPkg <- paste0(pattPkg, pkgVer,"_{1}")
    pattPkg <- paste0(pattPkg, "[a-zA-Z0-9]_?")
    pattPkg <- paste0(pattPkg, R.Version()$platform, "{1}")
    pattPkg <- paste0(pattPkg, "\\.tar\\.gz${1}","|", pattPkg, "\\.tgz${1}")
    pkgFile <- allBinPaths[grep(pattPkg, basename(allBinPaths))[1]]
    if(length(pkgFile)==0 || is.na(pkgFile)) return(NULL)
    return(pkgFile)
  })
  return(.pathToBinary(pkgName, pkgVersion))
}

.mvPkgsBinsToRepo <- function(buildDir, preCompiledPkgsDir) {
  gzBinFiles <- list.files(buildDir, pattern="\\.tar\\.gz$",
                  recursive=TRUE, full.names=TRUE
                )
  gzBinFiles <- normalizePath(gzBinFiles, mustWork=FALSE)
  # file.remove doesn't work cross-device, but copy && remove do. Go figure...
  file.copy(gzBinFiles, file.path(preCompiledPkgsDir, basename(gzBinFiles)))
  file.remove(gzBinFiles)
}

.installSinglePkg <- function(pkgName, lib, repos, binSaveDir, ...) {

  # Make sure to normalize lib paths before changing wd
  lib <- normalizePath(lib, mustWork=TRUE)
  binSaveDir <- normalizePath(
    file.path(binSaveDir, .binDirsPathSuffix()),
    mustWork=FALSE
  )
  dir.create(binSaveDir, recursive=TRUE, showWarnings=FALSE)

  # Make sure libs in the "lib" location can be found and loaded
  libPathsOriginal <- .libPaths()
  .libPaths(unique(c(lib, libPathsOriginal)))
  on.exit(.libPaths(libPathsOriginal))

  # Installing
  install.packages(
    pkgName, lib=lib, repos=repos, type="source",
    INSTALL_opts=c("--build"), dependencies=FALSE,
    ...
  )

  # Copying compiled binaries, so they are available next time
  .mvPkgsBinsToRepo(getwd(), binSaveDir)
}

.printOnSameLine <- function(newStatus) {
  blankLine <- strrep(" ", getOption("width"))
  cat(paste0("\r", blankLine, "\r", newStatus))
}

.printInstallStatus <- function(ipkg, npkgs, action, pkgName, pkgVersion) {
  installStatus <- sprintf(
    'Installing R-lib %d/%d (%.0f%%): %s (=%s) ...',
    ipkg, npkgs, 100*(ipkg / npkgs), pkgName, pkgVersion
  )
  .printOnSameLine(installStatus)
}

installPkgsFromDf <- function(
  df, lib, repos, binDirs, binSaveDir, logfile=NULL, ...
) {
  df$binPath <- getPathToBinary(df$Package, df$Version, binDirs=binDirs)
  dir.create(lib, showWarnings=FALSE, recursive=TRUE)

  # Build in a tmpdir to keep user's dir clean
  originalDir <- getwd()
  tmpBuildDir <- file.path(tempdir(), "tmp_build_dir")
  dir.create(tmpBuildDir, recursive=TRUE, showWarnings=FALSE)
  setwd(tmpBuildDir)
  on.exit(setwd(originalDir))

  # We'll reduce stdout and have install logs in a separate file instead.
  if(is.null(logfile)) {
    logfile <- file.path(
      originalDir,
      paste0("install_", format(Sys.time(), "%Y-%m-%d_%H%M%OS1"), ".log")
    )
  }

  # Helper function to handle install.packages errors
  .errorFunc <- function(e, df, irow) {
    if(df$isImport[irow] || df$isEssentialRecDep[irow]) {
      cat("\n")
      stop(e$message, call.=FALSE)
    } else {
      warning(paste0(
        e, "\n",
        'Error installing opt dep "', df$Package[irow], '". Skipping.'
      ))
    }
  }

  cat("Installation logfile:", logfile, "\n")
  for(irow in seq_len(nrow(df))) {
    .printInstallStatus(
      irow, nrow(df), "Installing", df$Package[irow], df$Version[irow]
    )
    tryCatch({
      # Try installing from pre-compiled binary first
      utils::untar(unlist(df$binPath[irow]), exdir=lib)
      write(
        sprintf(
          'Package "%s" installed using pre-compiled binary %s\n',
          df$Package[irow], unlist(df$binPath[irow])
        ),
        file=logfile, append=TRUE
      )
    },
      error=function(e) {
        # Fall back to building & installing from source if no binary available
        tryCatch({
          .installSinglePkg(
            df$Package[irow], lib=lib, repos=repos, binSaveDir=binSaveDir,
            quiet=TRUE, keep_outputs=TRUE, ...
          )
        },
          warning=function(e) .errorFunc(e, df, irow),
          error=function(e) .errorFunc(e, df, irow),
          finally={
            logLines <- readLines(paste0(df$Package[irow], ".out"))
            write(logLines, logfile, append=TRUE)
          }
        )
      }
    )
  }
}
