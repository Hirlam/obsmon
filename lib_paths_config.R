
localRLibDir <- normalizePath(
  file.path("utils", "build", "local_R_library"), mustWork=FALSE
)
localInstallRLib <- file.path(localRLibDir, "R-libs")
userRLib <- Sys.getenv("R_LIBS_USER")
if(userRLib=="") userRLib <- file.path(Sys.getenv("HOME"), "R", "library")
userRLib <- normalizePath(userRLib, mustWork=FALSE)
.libPaths(unique(c(localInstallRLib, userRLib, .libPaths())))
