
handleInstallFailure <- function(callAndErrorMsg, okToProceed){
  # 'Warning' function to be used along with "withCallingHandlers" when calling
  # install.packages. The function will only issue warnings "Suggests"-type
  # dependencies fail to be installed, but will stop installation if any falure
  # occurs with the other types of dependencies.
  msg <- callAndErrorMsg[["message"]]
  pattBackticks <- "‘+.+’+"
  offendingFile <- gsub("‘*’*", "",
                     regmatches(msg, gregexpr(pattBackticks, msg))
                   )
  offendingPkg <- grep("\\.tar\\.gz$|\\.tgz$",
                    unlist(strsplit(offendingFile, split="/")),
                    value=TRUE
                  )
  offendingPkg <- gsub("_.+\\.tar\\.gz$|_.+\\.tgz$", "", offendingPkg)
  if(offendingPkg %in% okToProceed) {
    warning(callAndErrorMsg)
  } else {
    stop(callAndErrorMsg)
  }
}
