
handleInstallFailure <- function(
  pkgName, callAndErrorMsg, okToProceed, ignoreBuildFail=FALSE
){
  # 'Warning' function to be used along with "tryCatch" when calling
  # install.packages. Only warnings will be issued if "Suggests"-type
  # dependencies fail to be installed, but installation will be stopped
  # if failure occurs with any other types of dependencies.

  if(ignoreBuildFail || pkgName %in% okToProceed) {
    warningCompl <- 'R-lib listed as "ok to proceed" in case of failure'
    if(ignoreBuildFail) warningCompl <- 'option "--ignoreBuildFail" passed'
    warning(sprintf(
      '%s\n> Ignoring failure with R-lib "%s": %s\n\n',
      callAndErrorMsg, pkgName, warningCompl
    ))
  } else {
    stop(callAndErrorMsg)
  }
}
