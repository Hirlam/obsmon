createLocalRepo <- function(pkgsDf, destdir) {
  repos <- c(CRAN="https://cloud.r-project.org")
  pkgs <- unlist(pkgsDf$Package)
  srcDir <- file.path(destdir, "contrib")
  dir.create(srcDir, recursive=TRUE, showWarnings=FALSE)
  download.packages(pkgs, destdir=srcDir, repos=repos, type="source")
  tools::write_PACKAGES(dir=srcDir)
}
