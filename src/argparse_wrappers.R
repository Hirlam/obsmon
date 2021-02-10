# Command line args
parser <- argparse::ArgumentParser()
parser$add_argument(
  "path",
  nargs="?",
  default=getwd(),
  help="Path to the dir containing the R sources."
)
parser$add_argument(
  "-ignore",
  nargs="+",
  default=NULL,
  help="Path to directories or files to be ignored."
)
parser$add_argument(
  "-install-path",
  default=file.path(getwd(), "local_R-libs", "R-libs"),
  help="Path to the dir where the R-libs will be installed."
)
parser$add_argument(
  "-bin-repo-path",
  default=NULL,
  help="Path to the dir containing pre-compiled R-pkg binaries."
)
parser$add_argument(
  "-bin-save-path",
  default=NULL,
  help="Path where eventual compiled libs will be saved."
)
parser$add_argument(
  "--listdeps",
  action="store_true",
  help="Show the dependencies and quit."
)

optsThatChoosePkgSrcRepos = parser$add_mutually_exclusive_group()
optsThatChoosePkgSrcRepos$add_argument(
  "-repos",
  nargs="*",
  default=NULL,
  help=paste(
    "URL(s) to repo(s) containing R-libs sources. If it is a local repo,",
    'then (i) make sure to prefix the path with "file:" (without quotes),',
    'and (ii) it will be assumed to be a directory containing an',
    '"src/contrib" subdir structure.'
  )
)
optsThatChoosePkgSrcRepos$add_argument(
  # Instead of using action="store_true", set this opt up such that,
  # if passed without any value, then it becomes TRUE, but if a value
  # is passed, then it takes on the passed value.
  "--create-local-repo",
  nargs="?",
  const=TRUE,
  default=FALSE,
  help="Create a local CRAN-like repo with the needed packages and quit."
)

args <- parser$parse_args()

# Validation and special defaults
args$path <- normalizePath(args$path, mustWork=TRUE)
args$install_path <- normalizePath(args$install_path, mustWork=FALSE)

if(is.null(args$ignore)) args$ignore <- basename(args$install_path)

if(is.null(args$bin_save_path)) {
  args$bin_save_path <- file.path(
    dirname(args$install_path), "compiled_binaries"
  )
}

args$bin_save_path <- normalizePath(args$bin_save_path, mustWork=FALSE)

if(is.null(args$bin_repo_path)) args$bin_repo_path <- args$bin_save_path

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

if(isTRUE(args$create_local_repo)) {
  # In this case, the user passed the arg without a value.
  args$create_local_repo <- file.path(
    dirname(args$install_path), "src", "contrib"
  )
}
if(!isFALSE(args$create_local_repo)) {
  args$create_local_repo <- normalizePath(
    args$create_local_repo, mustWork=FALSE
  )
  # If creating local repo, then make sure CRAN is the 1st repo
  args$repos <- c(args$repos["CRAN"], args$repos)
  args$repos <- args$repos[!duplicated(args$repos)]
}
