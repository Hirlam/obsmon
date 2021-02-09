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

  # Build in a tmpdir to keep user's dir clean
  originalDir <- getwd()
  tmpBuildDir <- file.path(tempdir(), "tmp_build_dir")
  dir.create(tmpBuildDir, recursive=TRUE, showWarnings=FALSE)
  setwd(tmpBuildDir)
  on.exit(setwd(originalDir))

  # Installing
  install.packages(
    pkgName, lib=lib, repos=repos, type="source",
    INSTALL_opts=c("--build"), dependencies=FALSE,
    quiet=TRUE,
    ...
  )

  # Copying compiled binaries, so they are available next time
  .mvPkgsBinsToRepo(tmpBuildDir, binSaveDir)
}


installPkgsFromDf <- function(df, lib, repos, binDirs, binSaveDir, ...) {
  df$binPath <- getPathToBinary(df$Package, df$Version, binDirs=binDirs)
  dir.create(lib, showWarnings=FALSE, recursive=TRUE)
  for(irow in seq_len(nrow(df))) {
    cat(paste0('Installing package "', df$Package[irow], '"... \n'))

    tryCatch({
      utils::untar(unlist(df$binPath[irow]), exdir=lib)
      cat(paste0(
        '  > Package "', df$Package[irow],
        '" installed from pre-compiled binary.\n'
      ))
    },
      error=function(e) {
        tryCatch({
          .installSinglePkg(
            df$Package[irow], lib=lib, repos=repos, binSaveDir=binSaveDir, ...
          )
        },
          error=function(e) {
            if(!(df$isImport[irow] || df$isEssentialRecDep[irow])) {
              warning(paste0(
                e, "\n",
                'Error installing opt dep "', df$Package[irow], '". Skipping.'
              ))
            } else {
              stop(e)
            }
          }
        )
      }
    )
  }
}
