# Command line args
parser <- argparse::ArgumentParser()

parser$add_argument(
  "path",
  nargs="?",
  default=getwd(),
  help='Directory to be recursively searched for R sources. Default: "."'
)

parser$add_argument(
  "--listdeps",
  action="store_true",
  help="Show the imports and dependencies and exit."
)

optsThatChoosePkgSrcRepos = parser$add_mutually_exclusive_group()
optsThatChoosePkgSrcRepos$add_argument(
  "--create-local-repo",
  action="store_true",
  help="Create a local CRAN-like repo with the needed pkg sources and exit."
)

parser$add_argument(
  "--clean",
  nargs="*",
  choices=c("installed", "sources", "binaries", "all"),
  help="Remove files created by the installer."
)

parser$add_argument(
  "--include-suggests",
  action="store_true",
  help=paste(
    'Include "suggests"-type dependencies.',
    "See <https://r-pkgs.org/description.html>"
  )
)

parser$add_argument(
  "-output-rootdir",
  default=file.path(getwd(), "installer_local_R-libs"),
  help="Where the detected R-libs & deps should be installed.",
  metavar="OUTPUT_ROOTDIR_PATH"
)

parser$add_argument(
  "-ignore",
  nargs="+",
  default=NULL,
  help="Path patterns to ignore when searching for R files."
)

optsThatChoosePkgSrcRepos$add_argument(
  "-repos",
  nargs="*",
  default=NULL,
  help=paste(
    "URL(s) to repo(s) containing R-libs sources. If using a local repo,",
    'make sure to prefix the path with "file:" (without quotes).',
    'Local repos are assumed to be a directory containing an',
    '"src/contrib" subdir structure, similar to what is shown at',
    "<https://environments.rstudio.com/repositories.html#structure-of-a-cran-like-repository>."
  )
)

parser$add_argument(
  "-bin-repo-path",
  default=NULL,
  help="Location of eventual pre-compiled R-pkg binaries.",
  metavar="BIN_REPO_DIR_PATH"
)

parser$add_argument(
  "-ca",
  action="append",
  dest="configure_args",
  default=getOption("configure.args"),
  help=paste(
    'Passed to install.packages "configure.args" argument. See',
    '<https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/install.packages>.',
    'USAGE EXAMPLE (with arbitrary values, please adjust for your usage):',
    './install -ca="--with-gdal-config=/usr/gdal23/bin/gdal-config"',
    '-ca="--with-proj-include=/usr/proj49/include"',
    '-ca="--with-proj-lib=/usr/proj49/lib"'
  )
)

parser$add_argument(
  "-cv",
  action="append",
  dest="configure_vars",
  default=getOption("configure.vars"),
  help=paste(
    'Passed to install.packages "configure.vars" argument. See',
    '<https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/install.packages>.',
    'Usage is similar to the "--configure-args" option.'
  )
)

logOptions = parser$add_mutually_exclusive_group()
logOptions$add_argument(
  "--keep-full-install-log",
  action="store_true",
  help=paste(
    "Keep the full installation log. If not passed, then only",
    "failed package install logs are kept (if any)."
  )
)

logOptions$add_argument(
  "--live-view-install-log",
  action="store_true",
  help=paste(
    'Show "live" compilation messages in the console',
    'instead of sending them to a log file.'
  )
)

args <- parser$parse_args()

# Validation and special defaults
args$path <- normalizePath(args$path, mustWork=TRUE)

args$output_rootdir <- normalizePath(args$output_rootdir, mustWork=FALSE)
args$output_dirs <- list(
  installed=file.path(args$output_rootdir, "R-libs"),
  sources=file.path(args$output_rootdir, "src"),
  binaries=file.path(args$output_rootdir, "compiled_binaries")
)
if(is.null(args$bin_repo_path)) args$bin_repo_path <- args$output_dirs[["binaries"]]
args$bin_repo_path <- normalizePath(args$bin_repo_path, mustWork=FALSE)

# Ignore outputs when parsing R sources
if(is.null(args$ignore)) args$ignore <- args$output_rootdir
# Make sure to ignore the calling script itself, if called via a
# symlink located somewhere else
args$ignore <- unique(c(paste0("^", callingScriptPath(), "$"), args$ignore))

# Handle "--create-local-repo" and "-repos" (which are mutually exclusive)
if(is.null(args$repos)) {
  # Set the default here, as argparse drops the list names when it parses opts.
  # Make the default be whatever the user chooses (e.g., in their .Rprofile),
  # except for CRAN, which will (i) be added, if missing, or (ii) reset
  # if present. The CRAN url used here performs automatic redirection to
  # servers worldwide.
  repos <- getOption("repos")
  repos["CRAN"] <- "https://cloud.r-project.org"
  args$repos <- repos
}
.validateRepos <- Vectorize(function(repo) {
  if(startsWith(repo, "file:")) {
    repo <- paste0(
      "file:",
      normalizePath(sub("file:", "", repo), mustWork=FALSE)
    )
  }
  return(repo)
})
args$repos <- .validateRepos(args$repos)

if(args$create_local_repo) {
  # If creating local repo, then make sure CRAN is the 1st repo
  args$repos <- c(args$repos["CRAN"], args$repos)
  args$repos <- args$repos[!duplicated(args$repos)]
}

# If the user doesn't specify what to clean, clean just installed packages
if(!is.null(args$clean) && length(args$clean)==0) args$clean <- "installed"
