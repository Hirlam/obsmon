# Install argparse locally if not available globally
.localInstallLib <- file.path(dirname(callingScriptPath()), "pkg_local_R-libs")
.installThisPkgDepLocally <- function(pkg) {
  if(!suppressWarnings(require(pkg, character.only=TRUE, quietly=TRUE))) {
    cat(paste0('Setting up R-pkg "', pkg, '" needed by the installer...\n'))
    dir.create(.localInstallLib, recursive=TRUE, showWarnings=FALSE)
    # If .localInstallLib didn't exist before, we need this .libPaths again
    .libPaths(c(.localInstallLib, .libPaths()))
    install.packages(
      pkg, lib=.localInstallLib, quiet=TRUE,
      repos="https://cloud.r-project.org"
    )
  }
}
.libPaths(c(.localInstallLib, .libPaths()))
.installThisPkgDepLocally("argparse")

# Source needed files
.sourceNeededFiles <- function(
  sourcesDir=file.path(dirname(callingScriptPath()), "src"))
{
  sourceFiles <- file.path(sourcesDir, c(
    "argparse_wrappers.R",
    "import_finder.R",
    "dependencies.R",
    "install.R",
    "get_sys_info.R",
    "create_local_repo.R"
  ))
  for(sourceFile in sourceFiles) source(sourceFile, local=FALSE)
}
.sourceNeededFiles()
