# Command line args
parser <- argparse::ArgumentParser()

.defaultCommand <- "install"
subparsers <- parser$add_subparsers(
  title="Script subcommands",
  dest="command",
  description=paste(
    "The valid script subcommands are listed below. If no subcommand is",
    paste0('passed, then the "', .defaultCommand, '"'), "mode wil be adopted",
    "(with eventual positional args taking on their default values).",
    "Note that the subcommands also accept their own arguments",
    '-- in particular the "-h" and "--help" arguments.'
  )
)

################################################################
# Define commands (without their command line options for now) #
################################################################
parser_install <- subparsers$add_parser(
  "install",
  help="Install imported packages and recursive dependencies."
)

parser_listdeps <- subparsers$add_parser(
  "listdeps",
  help="Show detected imports and recursive dependencies."
)

parser_create_local_repo <- subparsers$add_parser(
  "create-local-repo",
  help="Create a local CRAN-like src repo for the needed pkgs."
)

parser_clean <- subparsers$add_parser(
  "clean",
  help="Remove files created by the installer."
)


################################################
# Options that are common to multiple commands #
################################################
for(p in c(parser_install, parser_listdeps, parser_create_local_repo)) {
  p$add_argument(
    "sources_dir",
    nargs="?",
    default=getwd(),
    help='Directory to be recursively searched for R sources. Default: "."'
  )

  p$add_argument(
    "-ignore",
    nargs="+",
    default=NULL,
    help="Path patterns to ignore when searching for R files."
  )

  p$add_argument(
    "--include-suggests",
    action="store_true",
    help=paste(
      'Include "suggests"-type dependencies.',
      "See <https://r-pkgs.org/description.html>"
    )
  )

  .localVersionsFile <- file.path(getwd(), ".installer_pkg_versions.txt")
  p$add_argument(
    "-versions-file",
    default=ifelse(file.exists(.localVersionsFile), .localVersionsFile, FALSE),
    help=paste(
      'Optional file listing pkg versions. Packages not listed will have',
      'their versions determined by what is found in the used source repos.',
      'Default: "./.installer_pkg_versions.txt", if it exists, or',
      '"SOURCES_DIR/.installer_pkg_versions.txt" otherwise.'
    )
  )
}

for(p in c(parser_install, parser_listdeps)) {
  p$add_argument(
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
}

.defaultOutRootdirBasename <- ".installer_local_R-libs"
for(p in c(parser_install, parser_create_local_repo, parser_clean)) {
  p$add_argument(
    "-output-rootdir",
    default=file.path(getwd(), .defaultOutRootdirBasename),
    help=paste(
      "Where the installer puts R-libs, sources and binaries.",
      "Default:", paste0('"', .defaultOutRootdirBasename, '"')
    ),
    metavar="OUTPUT_ROOTDIR_PATH"
  )
}


#########################################################
# Define options that apply only to the install command #
#########################################################
parser_install$add_argument(
  "-bin-repo-path",
  default=NULL,
  nargs="+",
  help="Location of eventual pre-compiled R-pkg binaries.",
  metavar="BIN_REPO_DIR_PATH"
)

parser_install$add_argument(
  "-ca", "--configure-args",
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

parser_install$add_argument(
  "-cv", "--configure-vars",
  action="append",
  dest="configure_vars",
  default=getOption("configure.vars"),
  help=paste(
    'Passed to install.packages "configure.vars" argument. See',
    '<https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/install.packages>.',
    'Usage is similar to the "--configure-args" option.'
  )
)

logOptions = parser_install$add_mutually_exclusive_group()
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

parser_install$add_argument(
  "--dry-run",
  action="store_true",
  help="Do everything except actually building and installing packages."
)

################################################
# Options that apply only to the clean command #
################################################
parser_clean$add_argument(
  "clean",
  nargs="*",
  default="installed",
  choices=c("installed", "sources", "binaries", "all")
)


###################################################
# Options that apply only to the listdeps command #
###################################################
parser_listdeps$add_argument(
  "--simple",
  dest="simple_listdeps",
  action="store_true",
  help="Keep stdout simple."
)

parser_listdeps$add_argument(
  "--lock-versions",
  dest="lock_pkg_versions",
  action="store_true",
  help="Create a file locking the versions of the R-packages to be installed."
)


############################################################
# Options that apply only to the create-local-repo command #
############################################################
parser_create_local_repo$add_argument(
  "--only-metadata",
  action="store_true",
  help="Keep only the repo's metadata, not the source files."
)

###############################
# Parsing and validating args #
###############################
.msgIfMissingCmd <- "error: the following arguments are required: command"
args <- tryCatch({
  tmpArgs <- parser$parse_args()
  if(is.null(tmpArgs$command)) {
    stop(.msgIfMissingCmd)
  }
  tmpArgs
  },
  error=function(e) {
    # Set command to .defaultCommand if no command is passed
    argv <- commandArgs(trailingOnly=TRUE)
    firstArgvIsOptArg <- startsWith(argv[1], "-")
    if(anyNA(firstArgvIsOptArg)) firstArgvIsOptArg <- FALSE
    missCmd <- firstArgvIsOptArg || isTRUE(grepl(.msgIfMissingCmd, e$message))
    if(missCmd) parser$parse_args(args=c(.defaultCommand, argv))
    else stop(e$message)
  }
)


###########################
# Arg checks and defaults #
###########################
if("sources_dir" %in% names(args)) {
  args$sources_dir <- normalizePath(args$sources_dir, mustWork=TRUE)
  if(("versions_file" %in% names(args)) && isFALSE(args$versions_file)) {
    args$versions_file <- file.path(
      args$sources_dir,
      basename(.localVersionsFile)
    )
  }
}

if("ignore" %in% names(args)) {
  # Ignore the default output dir when parsing R sources
  args$ignore <- c(args$ignore, .defaultOutRootdirBasename)
  # Make sure to ignore the calling script itself, as well as its dir
  args$ignore <- unique(c(
    args$ignore,
    paste0("^", callingScriptPath(resolve_symlink=FALSE), "$"),
    paste0("^", file.path(dirname(callingScriptPath()), ".*"))
  ))
}

if("output_rootdir" %in% names(args)) {
  args$output_rootdir <- normalizePath(args$output_rootdir, mustWork=FALSE)
  args$output_dirs <- list(
    installed=file.path(args$output_rootdir, "R-libs"),
    sources=file.path(args$output_rootdir, "src"),
    binaries=file.path(args$output_rootdir, "compiled_binaries")
  )
}

# Append the installer's default bin saveDir to bin_repo_path, so that
# eventual binaries compiled during install, but not available in the
# passed bin_repo_path, can also be used if installation is restarted.
if ("bin_repo_path" %in% names(args)) {
  parsedBinRepoPaths <- c()
  for (binRepoPath in args$bin_repo_path) {
    parsedBinRepoPaths <- c(
      parsedBinRepoPaths,
      unlist(strsplit(binRepoPath, ":"))
    )
  }
  args$bin_repo_path <- parsedBinRepoPaths

  args$bin_repo_path <- unique(c(
    args$bin_repo_path,
    args$output_dirs[["binaries"]]
  ))
  args$bin_repo_path <- normalizePath(args$bin_repo_path, mustWork=FALSE)
}

if(("repos" %in% names(args)) || args$command == "create-local-repo") {
  if(is.null(args$repos)) {
    # Set the default here, as argparse drops the list names when it parses opts.
    # Make the default be whatever the user chooses (e.g., in their .Rprofile),
    # except for CRAN, which will (i) be added, if missing, or (ii) reset
    # if present. The CRAN url used here performs automatic redirection to
    # servers worldwide.
    repos <- getOption("repos")
    repos["CRAN"] <- "https://cloud.r-project.org"
    local_repo_path <- file.path(getwd(), ".installer_local_pkg_repo")
    if(args$command != "create-local-repo") {
      if (file.exists(file.path(local_repo_path, "src", "contrib", "PACKAGES"))) {
        repos["INSTALLER_DEPS_LOCK"] <- paste0("file:", local_repo_path)
        repos <- c(
          repos["INSTALLER_DEPS_LOCK"],
          repos[names(repos) != "INSTALLER_DEPS_LOCK"]
        )
      }
    }
    args$repos <- repos
  } else {
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
  }
}

if(
  ("lock_pkg_versions" %in% names(args)) ||
  (args$command %in% c("listdeps", "create-local-repo"))
) {
  args$include_suggests <- TRUE
}

# Some versions of argparse since Jan 2021 seem to be adding
# an NA to these when the getOption used as default returns NULL.
.remove_na <- function(vec) vec[!is.na(vec)]
if ("configure_args" %in% names(args)) args$configure_args <- .remove_na(args$configure_args)
if ("configure_vars" %in% names(args)) args$configure_vars <- .remove_na(args$configure_vars)

