
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
