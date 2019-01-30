
localRLibDir <- normalizePath(
  file.path("utils", "build", "local_R_library"), mustWork=FALSE
)
localInstallRLib <- file.path(localRLibDir, "R-libs")
userRLib <- Sys.getenv("R_LIBS_USER")
if(userRLib=="") userRLib <- file.path(Sys.getenv("HOME"), "R", "library")
userRLib <- normalizePath(userRLib, mustWork=FALSE)
libPathsObsmonWillSearch <- unique(c(localInstallRLib, userRLib, .libPaths()))
.libPaths(libPathsObsmonWillSearch)

libPathsMsg <- "Directories where obsmon looks for R libraries"
libPathsMsg <- paste(libPathsMsg, "(in order of priority):\n")
for(dir in libPathsObsmonWillSearch) {
  dirNotFoundMsg <- NULL
  if(!(dir %in% .libPaths())) dirNotFoundMsg <- '(not found)'
  libPathsMsg <- paste(libPathsMsg, " >", dir, dirNotFoundMsg, "\n")
}
libPathsMsg <- list(
  success=libPathsMsg,
  error=paste(
    libPathsMsg,
    "Please try (re)installing obsmon and running the code again.",
    "You can find installation instructions in the INSTALL.md file.\n\n",
    sep="\n"
  )
)
