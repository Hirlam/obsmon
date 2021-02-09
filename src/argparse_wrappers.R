# Command line args
parser <- argparse::ArgumentParser()
parser$add_argument(
  "path",
  nargs="?",
  default=".",
  help="Path to the dir containing the R sources."
)
parser$add_argument(
  "-ignore",
  nargs="+",
  default="local_R-libs_install",
  help="Path to directories or files to be ignored."
)
parser$add_argument(
  "-repos",
  nargs="*",
  default=getOption("repos"),
  help="Repo(s) containing R-libs sources."
)
parser$add_argument(
  "-install-path",
  default=file.path(".", "local_R-libs_install", "R-libs"),
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
args <- parser$parse_args()

# Validate input paths
args$path <- normalizePath(args$path, mustWork=TRUE)
args$install_path <- normalizePath(args$install_path, mustWork=FALSE)

if(is.null(args$bin_repo_path)) {
  args$bin_repo_path <- file.path(
    dirname(args$install_path), "compiled_binaries"
  )
}
args$bin_repo_path <- normalizePath(args$bin_repo_path, mustWork=FALSE)

if(is.null(args$bin_save_path)) {
  args$bin_save_path <- file.path(
    dirname(args$install_path), "compiled_binaries"
  )
}
args$bin_save_path <- normalizePath(args$bin_save_path, mustWork=FALSE)
