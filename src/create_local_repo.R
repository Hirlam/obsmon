createLocalRepo <- function(pkgsDf, destdir) {
  repos <- c(CRAN="https://cloud.r-project.org")
  pkgs <- unlist(pkgsDf$Package)
  dir.create(destdir, recursive=TRUE, showWarnings=FALSE)
  download.packages(pkgs, destdir=destdir, repos=repos, type="source")
  tools::write_PACKAGES(dir=destdir)
}
