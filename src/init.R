# Install argparse locally if not available globally
.localInstallLib <- file.path(dirname(callingScriptPath()), "pkg_local_R-libs")
.installPkgLocallyIfMissingGlobally <- function(pkg, minPkgVersion="0") {
  .libPaths(c(.localInstallLib, .libPaths()))
  pkgIsInstalled <- tryCatch(
    packageVersion(pkg) >= minPkgVersion,
    error=function(e) FALSE
  )
  if(!pkgIsInstalled) {
    cat(paste0('Setting up R-pkg "', pkg, '" needed by the installer...\n'))
    dir.create(.localInstallLib, recursive=TRUE, showWarnings=FALSE)
    .libPaths(c(.localInstallLib, .libPaths()))
    install.packages(
      pkg, lib=.localInstallLib, quiet=TRUE,
      repos="https://cloud.r-project.org"
    )
  }
}
# We need argparse>=2.0.0 because of the
# "add_mutually_exclusive_group" functionality
.installPkgLocallyIfMissingGlobally("argparse", minPkgVersion="2.0.0")

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
    "create_local_repo.R",
    "commands.R"
  ))
  for(sourceFile in sourceFiles) source(sourceFile, local=FALSE)
}
.sourceNeededFiles()
