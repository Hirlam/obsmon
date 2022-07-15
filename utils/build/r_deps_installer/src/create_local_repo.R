createLocalRepo <- function(pkgsDf, destdir, onlyMetadata=FALSE) {
  repos <- c(CRAN="https://cloud.r-project.org")
  srcDir <- file.path(destdir, "src", "contrib")
  unlink(srcDir, recursive=TRUE)
  .download_pkg_source(pkgsDf$Package, pkgsDf$Version, repos=repos, dest_dir=srcDir)
  .printOnSameLine("Done downloading sources. Updating PACKAGES database...\n")
  tools::write_PACKAGES(dir=srcDir)
  if (onlyMetadata) {
    unlink(file.path(srcDir, paste0("*", "_", "*", ".tar.gz")))
  }
}
