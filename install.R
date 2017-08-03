repo=c("http://cran.ma.imperial.ac.uk/")
lib=Sys.getenv("R_LIBS_USER")

pkgs <- c(
    "Cairo",
    "chron",
    "dplyr",
    "futile.logger",
    "ggplot2",
    "gridExtra",
    "httpuv",
    "jpeg",
    "leaflet",
    "mapproj",
    "pbapply",
    "pryr",
    "RcppTOML",
    "RSQLite",
    "reshape2",
    "scales",
    "shiny",
    "shinyjs",
    "V8"
)

installedPkgs <- installed.packages(lib.loc=lib)

newPkgs <- pkgs[!(pkgs %in% installedPkgs)]

update.packages(lib.loc=lib, repos=repo, ask=FALSE)
if(length(newPkgs)>0) {
  install.packages(newPkgs, lib=lib, repos=repo)
}
