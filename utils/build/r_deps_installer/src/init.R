# Install argparse locally if not available globally
INSTALLER_OWN_INSTALL_LIB <- file.path(
  dirname(callingScriptPath()),
  "pkg_local_R-libs"
)
.libPaths(c(INSTALLER_OWN_INSTALL_LIB, .libPaths()))
.depsDbDf <- tryCatch({
  tools::write_PACKAGES(INSTALLER_OWN_INSTALL_LIB, unpacked=TRUE)
  readRDS(file.path(INSTALLER_OWN_INSTALL_LIB, "PACKAGES.rds"))
},
  error=function(e) NULL
)

.pkgIsInstalled <- Vectorize(function(pkg, minPkgVersion="0") {
  rtn <- tryCatch(
    packageVersion(pkg) >= minPkgVersion,
    error=function(e) FALSE
  )
  return(rtn)
})

.pkgCanBeLoaded <- function(pkg, minPkgVersion="0") {
  tryCatch({
    deps <- unlist(
      tools::package_dependencies(pkg, recursive=FALSE, db=.depsDbDf),
      use.names=FALSE
    )
    .pkgIsInstalled(pkg, minPkgVersion) && all(.pkgIsInstalled(deps))
  },
    error=function(e) FALSE
  )
}

.installPkgLocallyIfMissingGlobally <- function(pkg, minPkgVersion="0") {
  if(!.pkgCanBeLoaded(pkg, minPkgVersion)) {
    cat(paste0('Setting up R-pkg "', pkg, '" needed by the installer...\n'))
    dir.create(INSTALLER_OWN_INSTALL_LIB, recursive=TRUE, showWarnings=FALSE)
    .libPaths(c(INSTALLER_OWN_INSTALL_LIB, .libPaths()))
    install.packages(
      pkg, lib=INSTALLER_OWN_INSTALL_LIB, quiet=TRUE,
      repos="https://cloud.r-project.org"
    )
  }
}
# We need argparse>=2.0.0 because of the
# "add_mutually_exclusive_group" functionality
.installPkgLocallyIfMissingGlobally("argparse", minPkgVersion="2.0.0")
.installPkgLocallyIfMissingGlobally("remotes", minPkgVersion="2.0.0")

# Source needed files
.sourceNeededFiles <- function(
  sourcesDir=file.path(dirname(callingScriptPath()), "src"))
{
  sourceFiles <- file.path(sourcesDir, c(
    "aux_print.R",
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
