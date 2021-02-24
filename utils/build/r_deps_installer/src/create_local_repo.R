createLocalRepo <- function(pkgsDf, destdir) {
  repos <- c(CRAN="https://cloud.r-project.org")
  srcDir <- file.path(destdir, "contrib")
  unlink(srcDir, recursive=TRUE)
  dir.create(srcDir, recursive=TRUE, showWarnings=FALSE)
  for(irow in seq_len(nrow(pkgsDf))) {
    pkgName <- pkgsDf$Package[irow]
    version <- pkgsDf$Version[irow]
    .printProgress("downloading", irow, nrow(pkgsDf), pkgName, version)
    fpath <- remotes::download_version(pkgName, version=version, repos=repos)
    file.copy(
      fpath,
      file.path(srcDir, paste0(pkgName, "_", version, ".tar.gz")
    ))
    unlink(fpath)
  }
  .printOnSameLine("Done downloading sources. Updating PACKAGES database...\n")
  tools::write_PACKAGES(dir=srcDir)
}
