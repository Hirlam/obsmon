
# obsmonVersion is shown in the GUI and also printed along with the banner
obsmonVersion <- "3.0.0-beta.1"

# Having the git info gathered below is useful when providing support to users
execGitCommand <- function(gitCommand) {
  rtn <- tryCatch(
    system(gitCommand, intern=TRUE, ignore.stderr=TRUE),
    error=function(e) sprintf("Unknown (%s)", e$message),
    warning=function(w) sprintf("Unknown (%s)", w$message)
  )
  return(rtn)
}
gitCommitHash <- execGitCommand("git log --format='%H' -n 1")
gitBranch <- execGitCommand("git rev-parse --abbrev-ref HEAD")
gitTag <- execGitCommand("git describe --tags --dirty")
gitAuthorDate <- execGitCommand("git log --format='%ai' -n 1")

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
obsmonNameForBanner <- paste(obsmonNameForBanner, "\n", sep="")
obsmonVersionForBanner <- paste("                  v",obsmonVersion,sep="")
bannerDelim <- "=================================================="
obsmonBanner <- paste(
  bannerDelim, obsmonNameForBanner, obsmonVersionForBanner, "", gitInfo,
  bannerDelim, "\n",
  sep="\n"
)
