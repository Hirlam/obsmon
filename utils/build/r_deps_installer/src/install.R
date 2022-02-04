.binDirsPathSuffix <- function() {
  rVersion <- paste0(version$major, '.', gsub("\\..", "", version$minor))
  return(file.path(getSysInfo(), paste0("R_", rVersion)))
}

getPathToBinary <- function(pkgName, pkgVersion, binDirs) {
  allBinPaths <- unlist(sapply(
    binDirs,
    function(singleBinDir) {
      list.files(
        file.path(singleBinDir, .binDirsPathSuffix()),
        pattern="\\.tar\\.gz$|\\.tgz$",
        full.names=TRUE
      )
    }
  ))

  .pathToBinary <- Vectorize(function(pkgName, pkgVer) {
    # Match pkgName
    pattPkg <- paste0("^", pkgName, "_{1}")
    # Match any version
    pattPkg <- paste0(pattPkg, "[[:alnum:].-]{+}_{1}")
    # Match computer arch
    pattPkg <- paste0(pattPkg, "[a-zA-Z0-9]_?")
    # Match computer platform
    pattPkg <- paste0(pattPkg, R.Version()$platform, "{1}")
    # Match file extension
    pattPkg <- paste0(pattPkg, "\\.tar\\.gz${1}","|", pattPkg, "\\.tgz${1}")

    # Get non-version-specific match
    candidateFiles <- allBinPaths[grep(pattPkg, basename(allBinPaths))]
    if(length(candidateFiles)==0) return(NULL)

    # Now narrow down to required version specs
    pkgFile <- NULL
    versionSpecs <- unlist(strsplit(pkgVer, "[[:space:]]*,[[:space:]]*"))
    for(fpath in candidateFiles) {
      binVer <- unlist(strsplit(basename(fpath), "_", fixed=TRUE))[2]
      for(verReq in versionSpecs) {
        # Compare required and available versions
        operatorAndReqVer <- unlist(strsplit(trimws(verReq),"[[:space:]]{1,}"))
        op <- operatorAndReqVer[1]
        refVer <- operatorAndReqVer[2]
        verReqSatisfied <- do.call(op, list(binVer, refVer))

        # Make sure the comparison really evaluates to TRUE or FALSE
        if(!(isTRUE(verReqSatisfied) || isFALSE(verReqSatisfied))) {
          msg <- 'Invalid operator "%s" in deps version specification "%s"'
          msg <- sprintf(msg, op, paste(pkgName, op, refVer))
          stop(msg)
        }

        # If any requirement is breached, no point testing the others
        if(!verReqSatisfied) break
      }

      # Return the first file path with a version satisfying reqs
      if(verReqSatisfied) return(fpath)
    }
    # If we haven't returned so far, no file has been found. Return NULL.
    return(NULL)
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

.get_pkg_version_from_description_file <- function(descriptionFile) {
  for (line in readLines(descriptionFile)) {
    if(grepl("^Version[[:space:]]*:[[:space:]]*", line)) {
      return(trimws(sub("Version[[:space:]]*:[[:space:]]*", "", line)))
    }
  }
  return(NULL)
}

.download_pkg_source <- Vectorize(function(
    package,
    version,
    repos=c(CRAN="https://cloud.r-project.org"),
    dest_dir=getwd(),
    ...
) {
    downloaded_file_fpath <- remotes::download_version(
        package=package,
        version=version,
        repos=repos,
        type="source",
        ...
    )

    # Get actual version of the downloaded package. Needed, e.g., if "version"
    # is not specified as an exact number.
    description_file_relative_fpath = file.path(package, "DESCRIPTION")
    untar(downloaded_file_fpath, files=description_file_relative_fpath, exdir=tempdir())
    description_file_fpath <- file.path(tempdir(), description_file_relative_fpath)
    downloadedPkgVersion <- .get_pkg_version_from_description_file(description_file_fpath)
    if(is.null(downloadedPkgVersion)) {
        msg <- "ERROR: Could not determine version of downloaded pkg"
        msg <- paste(msg, sprintf("'%s'.", package), "\n")
        cat(msg)
        return (NULL)
    }

    # Rename the downloaded file to a standard name and move it to the dest dir
    standard_pkg_fname <- paste0(package, "_", downloadedPkgVersion, ".tar.gz")
    destination_fpath <- file.path(dest_dir, standard_pkg_fname)
    dir.create(dest_dir, recursive=TRUE, showWarnings=FALSE)
    file.copy(from=downloaded_file_fpath, to=destination_fpath)
    file.remove(downloaded_file_fpath)

    return(destination_fpath)
}, vectorize.args=c("package", "version"))

.install_a_pkg_version <- function(pkgName, version, lib, repos, ...) {
  # Download source with specified version
  fpath <- .download_pkg_source(pkgName, version=version, repos=repos)

  if(is.null(fpath)) {
    msg <- sprintf("ERROR: Problems downloading pkg %s (%s).", pkgName, version)
    return (NULL)
  }

  # install.packages ignores the "keep_outputs" arg if installing from local
  # files. Let's then create a one-pkg tmp CRAN-like repo and make it install
  # there instead. This way we can send output to a logfile.
  tmpRepo <- file.path(tempdir(), "tmp_repo", pkgName)
  tmpRepoSrcContribDir <- file.path(tmpRepo, "src", "contrib")
  dir.create(tmpRepoSrcContribDir, recursive=TRUE)
  on.exit(unlink(tmpRepo, recursive=TRUE))
  file.copy(from=fpath, to=tmpRepoSrcContribDir)
  file.remove(fpath)
  tools::write_PACKAGES(tmpRepoSrcContribDir)

  # Install from our temp repo
  install.packages(
    pkg=pkgName, lib=lib, repos=paste0("file:///", tmpRepo),
    dependencies=FALSE, type="source", INSTALL_opts=c("--build"),
    ...
  )
}

.installSinglePkg <- function(pkgName, version, lib, repos, binSaveDir, ...) {
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
  .install_a_pkg_version(pkgName, version=version, lib=lib, repos=repos, ...)

  # Copying compiled binaries, so they are available next time
  .mvPkgsBinsToRepo(getwd(), binSaveDir)
}

installPkgsFromDf <- function(
  df, repos, binDirs, outputDirs, liveViewLog=FALSE,
  logfile=NULL, keepFullLog=FALSE, dryRun=FALSE, ...
) {
  df$binPath <- getPathToBinary(df$Package, df$Version, binDirs=binDirs)
  unlink(outputDirs$installed, recursive=TRUE)
  dir.create(outputDirs$installed, showWarnings=FALSE, recursive=TRUE)

  # Build in a tmpdir to keep user's dir clean
  originalDir <- getwd()
  tmpBuildDir <- file.path(tempdir(), "tmp_build_dir")
  dir.create(tmpBuildDir, recursive=TRUE, showWarnings=FALSE)
  setwd(tmpBuildDir)
  on.exit({
    setwd(originalDir)
    cat("\n")
  })

  # We'll reduce stdout and have install logs in a separate file instead.
  if(is.null(logfile)) {
    logfile <- file.path(
      originalDir,
      paste0(
        ifelse(keepFullLog, "install_", "failed_install_"),
        format(Sys.time(), "%Y-%m-%d_%H%M%OS1"), ".log"
      )
    )
  }

  # Helper function to handle install.packages errors
  .errorFunc <- function(e, df, irow) {
    if(df$isImport[irow] || df$isEssentialRecDep[irow]) {
      cat("\n")
      stop(e$message, call.=FALSE)
    } else {
      warning(paste0(
        e$message, "\n",
        'Error installing opt dep "', df$Package[irow], '". Skipping.'
      ))
    }
  }

  for(irow in seq_len(nrow(df))) {
    .printProgress(
      "installing", irow, nrow(df), df$Package[irow], df$Version[irow]
    )
    tryCatch({
      # Try installing from pre-compiled binary first
      if(dryRun) {
        dryRunPath <- normalizePath(unlist(df$binPath[irow]), mustWork=TRUE)
      } else {
        utils::untar(unlist(df$binPath[irow]), exdir=outputDirs$installed)
      }
      msg <- '\nPackage "%s" installed using pre-compiled binary %s\n\n'
      msg <- sprintf(msg, df$Package[irow], unlist(df$binPath[irow]))
      if(liveViewLog) cat(msg)
      if(keepFullLog) write(msg, file=logfile, append=TRUE)
    },
      error=function(e) {
        # Fall back to building & installing from source if no binary available
        pkgInstallFailed <- TRUE
        tryCatch({
          msg <- '\nNo pre-compiled binary for package "%s (%s)". Building...\n\n'
          msg <- sprintf(msg, df$Package[irow], df$Version[irow])
          if(liveViewLog) cat(msg)
          if(keepFullLog) write(msg, file=logfile, append=TRUE)
          if(!dryRun) {
            .installSinglePkg(
              df$Package[irow],
              version=df$Version[irow],
              lib=outputDirs$installed,
              repos=repos,
              binSaveDir=outputDirs$binaries,
              quiet=!liveViewLog,
              keep_outputs=!liveViewLog,
              ...
            )
          }
          pkgInstallFailed <- FALSE
        },
          # Neither pre-compiled binary, nor successful build from source.
          warning=function(e) .errorFunc(e, df, irow),
          error=function(e) .errorFunc(e, df, irow),
          finally={
            if(keepFullLog || (pkgInstallFailed && !liveViewLog)) {
              # Append current lib's install log to common logfile
              logLines <- readLines(paste0(df$Package[irow], ".out"))
              write(logLines, logfile, append=TRUE)
            }
          }
        )
      }
    )
  }
  .printOnSameLine("Install complete.")
}
