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
  default="local_R-libs",
  help="Path to directories or files to be ignored."
)
parser$add_argument(
  "-repos",
  nargs="*",
  default=getOption("repos"),
  help=paste(
    "URL(s) to repo(s) containing R-libs sources. If it is a local repo,",
    'then (i) make sure to prefix the path with "file:" (without quotes),',
    'and (ii) it will be assumed to be a directory containing an',
    '"src/contrib" subdir structure.'
  )
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
  # Instead of using action="store_true", set this opt up such that,
  # if passed without any value, then it becomes TRUE, but if a value
  # is passed, then it takes on the passed value.
  "--create-local-repo",
  nargs="?",
  const=TRUE,
  default=FALSE,
  help="Create a local CRAN-like repo with the needed packages and quit."
)
parser$add_argument(
  "--listdeps",
  action="store_true",
  help="Show the dependencies and quit."
)
args <- parser$parse_args()

# Validate input paths
args$path <- normalizePath(args$path, mustWork=TRUE)
args$install_path <- normalizePath(args$install_path, mustWork=FALSE)

.validateRepos <- Vectorize(function(repo) {
  if(startsWith(repo, "file:")) {
    repo <- paste0(
      "file:",
      normalizePath(sub("file:", "", repo), mustWork=FALSE)
    )
  }
  return(repo)
}, USE.NAMES=FALSE)
args$repos <- .validateRepos(args$repos)

if(is.null(args$bin_save_path)) {
  args$bin_save_path <- file.path(
    dirname(args$install_path), "compiled_binaries"
  )
}
args$bin_save_path <- normalizePath(args$bin_save_path, mustWork=FALSE)

if(is.null(args$bin_repo_path)) args$bin_repo_path <- args$bin_save_path

if(isTRUE(args$create_local_repo)) {
  args$create_local_repo <- file.path(
    dirname(args$install_path), "src", "contrib"
  )
}
if(!isFALSE(args$create_local_repo)) {
  args$create_local_repo <- normalizePath(
    args$create_local_repo, mustWork=FALSE
  )
}
