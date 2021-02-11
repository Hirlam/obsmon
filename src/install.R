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
    quiet=TRUE, keep_outputs=TRUE,
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
    'Installation (%.0f%%): %s R-lib "%s-%s" ...',
    100*(ipkg / npkgs), action, pkgName, pkgVersion
  )
  .printOnSameLine(installStatus)
}

installPkgsFromDf <- function(df, lib, repos, binDirs, binSaveDir, ...) {
  df$binPath <- getPathToBinary(df$Package, df$Version, binDirs=binDirs)
  dir.create(lib, showWarnings=FALSE, recursive=TRUE)

  # Build in a tmpdir to keep user's dir clean
  originalDir <- getwd()
  tmpBuildDir <- file.path(tempdir(), "tmp_build_dir")
  dir.create(tmpBuildDir, recursive=TRUE, showWarnings=FALSE)
  setwd(tmpBuildDir)
  on.exit(setwd(originalDir))

  # Dir where install logfiles will be saved
  logfile <- file.path(originalDir, "install_log.txt")

  .errorFunc <- function(e, df, irow) {
    logLines <- readLines(paste0(df$Package[irow], ".out"))
    write(logLines, logfile, append=TRUE)
    if(df$isImport[irow] || df$isEssentialRecDep[irow]) {
      stop(paste(e, "EITCHA", sep="\n"))
    } else {
      warning(paste0(
        e, "\n",
        'Error installing opt dep "', df$Package[irow], '". Skipping.'
      ))
    }
  }


  for(irow in seq_len(nrow(df))) {
    tryCatch({
      .printInstallStatus(
        irow, nrow(df), "Installing", df$Package[irow], df$Version[irow]
      )
      utils::untar(unlist(df$binPath[irow]), exdir=lib)
    },
      error=function(e) {
        .printInstallStatus(
          irow, nrow(df), "Building and installing",
          df$Package[irow], df$Version[irow]
        )
        tryCatch({
          .installSinglePkg(
            df$Package[irow], lib=lib, repos=repos, binSaveDir=binSaveDir, ...
          )
        },
          warning=function(e) .errorFunc(e, df, irow),
          error=function(e) .errorFunc(e, df, irow)
        )
      }
    )
  }
}
