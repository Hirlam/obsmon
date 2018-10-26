
# obsmonVersion is shown in the GUI and also printed along with the banner
obsmonVersion <- "2.3.0"

# Having the git info gathered below is useful when providing support to users
gitCommitHash <- tryCatch(
  system("git log --format='%H' -n 1", intern=TRUE, ignore.stderr=TRUE),
  error=function(e) "Unknown"
)
gitBranch <- tryCatch(
  system("git rev-parse --abbrev-ref HEAD", intern=TRUE, ignore.stderr=TRUE),
  error=function(e) "Unknown"
)
gitTag <- tryCatch(
  system("git describe --tags --dirty", intern=TRUE, ignore.stderr=TRUE),
  error=function(e) "Unknown"
)
gitAuthorDate <- tryCatch(
  system("git log --format='%ai' -n 1", intern=TRUE, ignore.stderr=TRUE),
  error=function(e) "Unknown"
)
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
