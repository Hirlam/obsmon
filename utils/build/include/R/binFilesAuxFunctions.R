
pathToBinary <- function(pkgName, pkgVer, platform, binDir=NULL, fullBinList=NULL) {

  validBinPathSpec <- (length(binDir)*length(fullBinList) == 0) && 
                      (length(binDir) + length(fullBinList) != 0)
  if(!validBinPathSpec) {
    stop('ERROR (pathToBinary): You must specify 1 and only 1 of "binDir", "fullBinList".')
  }

  if(is.null(fullBinList)) {
    fullBinList <- list.files(binDir, pattern="\\.tar\\.gz$|\\.tgz$")
    fullBinList <- file.path(binDir, fullBinList)
  } else {
    binDir <- path.expand(dirname(fullBinList[1]))
  }

  pattPkg <- paste("^", file.path(binDir, pkgName), "_{1}", sep="")
  pattPkg <- paste(pattPkg, pkgVer,"_{1}", sep="")
  pattPkg <- paste(pattPkg, "[a-zA-Z0-9]_?", sep="")
  pattPkg <- paste(pattPkg, platform, "{1}", sep="")
  pattPkg <- paste(pattPkg,"\\.tar\\.gz${1}","|",pattPkg,"\\.tgz${1}", sep="")
  pkgFile <- grep(pattPkg, fullBinList, value=TRUE)
  if(length(pkgFile)==0) pkgFile <- NA

  return(pkgFile)
}

listBinFiles <- function(searchDirs) {
  binFilePaths <- list.files(
                    searchDirs, pattern="\\.tar\\.gz$|\\.tgz$",
                    full.names=TRUE
                  )
  return(binFilePaths)
}

mvPkgsBinsToRepo <- function(tmpDirForBuild, preCompiledPkgsDir) {
  gzBinFiles <- list.files(tmpDirForBuild, pattern="\\.tar\\.gz$", 
                  recursive=TRUE, full.names=TRUE
                )
  gzBinFiles <- normalizePath(gzBinFiles, mustWork=FALSE)
  # file.remove doesn't work cross-device, but copy && remove do. Go figure...
  file.copy(gzBinFiles, file.path(preCompiledPkgsDir, basename(gzBinFiles)))
  file.remove(gzBinFiles)
}

install_binaries <- function(fPaths, lib, overwrite=TRUE) {
  for (fPath in fPaths) {
    pkgName <- unlist(strsplit(basename(fPath), split="_",fixed=TRUE))[1]
    pkgDir <- file.path(lib, pkgName)
    if(!dir.exists(pkgDir) | overwrite) {
      # The R doc says "Not deleting a non-existent file is not a failure"
      unlink(pkgDir, recursive=TRUE)
      utils::untar(fPath, exdir=lib, compressed=TRUE)
    }
  }
}
