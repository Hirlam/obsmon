# Command line args
parser <- argparse::ArgumentParser()

.defaultCommand <- "install"
subparsers <- parser$add_subparsers(
  title="Script subcommands",
  dest="command",
  required=TRUE,
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
    "path",
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

for(p in c(parser_install, parser_create_local_repo, parser_clean)) {
  p$add_argument(
    "-output-rootdir",
    default=file.path(getwd(), "installer_local_R-libs"),
    help="Where the installer puts R-libs, sources and binaries.",
    metavar="OUTPUT_ROOTDIR_PATH"
  )
}


#########################################################
# Define options that apply only to the install command #
#########################################################
parser_install$add_argument(
  "-bin-repo-path",
  default=NULL,
  help="Location of eventual pre-compiled R-pkg binaries.",
  metavar="BIN_REPO_DIR_PATH"
)

parser_install$add_argument(
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

parser_install$add_argument(
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


################################################
# Options that apply only to the clean command #
################################################
parser_clean$add_argument(
  "clean",
  nargs="*",
  default="installed",
  choices=c("installed", "sources", "binaries", "all")
)


###############################
# Parsing and validating args #
###############################
args <- tryCatch(
  parser$parse_args(),
  error=function(e) {
    # Set command to .defaultCommand if no command is passed
    msgIfMissingCmd <- "error: the following arguments are required: command"
    if(isTRUE(grepl(msgIfMissingCmd, e$message))) {
      argv <- c(.defaultCommand, commandArgs(trailingOnly=TRUE))
      parser$parse_args(args=argv)
    } else {
      stop(e)
    }
  }
)

if("output_rootdir" %in% names(args)) {
  args$output_rootdir <- normalizePath(args$output_rootdir, mustWork=FALSE)
  args$output_dirs <- list(
    installed=file.path(args$output_rootdir, "R-libs"),
    sources=file.path(args$output_rootdir, "src"),
    binaries=file.path(args$output_rootdir, "compiled_binaries")
  )
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
