# obsmonVersion is shown in the GUI and also printed along with the banner
obsmonVersion <- "4.4.0"

# Having the git info gathered below is useful when providing support to users
readGitInfoFromFile <- function(fPath=".obsmon_git_info") {
  # Having an option to read git info from a file is useful, for instance,
  # if one builds an RPM from obsmon, in which case one doesn't really want
  # to keep the whole repo history, but rather a snapshot of the current
  # version. In this case, one can have the build system record the required
  # info in a file, which is then include din the RPM.
  df <- read.table(fPath, header=FALSE, sep="=", col.names=c('key','value'))
  rtn <- list()
  for(iRow in 1:nrow(df)) {
    key <- trimws(df[iRow, "key"])
    value <- trimws(df[iRow, "value"])
    rtn[[key]] <- value
  }
  return(rtn)
}
gitInfoFromFile <- tryCatch(
  readGitInfoFromFile(),
  error=function(e) NULL,
  warning=function(w) NULL
)

getGitInfo <- function(key, sourceList=gitInfoFromFile) {
  gitCommand <- switch(key,
    "commit"="git log --format='%H' -n 1",
    "branch"="git rev-parse --abbrev-ref HEAD",
    "tag"="git describe --tags --dirty",
    "author_date"="git log --format='%ai' -n 1"
  )
  rtn <- tryCatch(
    system(gitCommand, intern=TRUE, ignore.stderr=TRUE),
    warning=function(w) gitInfoFromFile[[key]],
    error=function(e) gitInfoFromFile[[key]]
  )
  if(is.null(rtn)) rtn <- "Unknown"
  return(rtn)
}

gitCommitHash <- getGitInfo("commit")
gitBranch <- getGitInfo("branch")
gitTag <- getGitInfo("tag")
gitAuthorDate <- getGitInfo("author_date")

gitInfo <- paste(
  paste("Branch:", gitBranch, sep=" "),
  paste("Tag:", gitTag, sep=" "),
  paste("Commit:", gitCommitHash, sep=" "),
  paste("Commit date:", gitAuthorDate, sep=" "),
  sep="\n"
)

# Making the banner to be shown in the terminal if run as a standalone app
obsmonNameForBanner <- paste(
"   ____  _                               ",
"  / __ \\| |                              ",
" | |  | | |__  ___ _ __ ___   ___  _ __  ",
" | |  | | '_ \\/ __| '_ ` _ \\ / _ \\| '_ \\ ",
" | |__| | |_) \\__ \\ | | | | | (_) | | | |",
"  \\____/|_.__/|___/_| |_| |_|\\___/|_| |_|",
sep="\n"
)
obsmonNameForBanner <- paste0(obsmonNameForBanner, "\n")
obsmonVersionForBanner <- paste0("                  v", obsmonVersion)
bannerDelim <- "=================================================="
obsmonBanner <- paste(
  bannerDelim,
  obsmonNameForBanner,
  obsmonVersionForBanner,
  "",
  gitInfo,
  bannerDelim,
  "\n",
  sep="\n"
)
