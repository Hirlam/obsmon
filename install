#!/usr/bin/env Rscript

repo=c("http://cran.ma.imperial.ac.uk/")
lib=Sys.getenv("R_LIBS_USER")
if(lib=="") lib=file.path(Sys.getenv("HOME"), "R", "library")
if(!dir.exists(lib)) dir.create(lib, recursive=TRUE)

pkgs <- c(
    "Cairo",
    "chron",
    "dbplyr",
    "dplyr",
    "flock",
    "futile.logger",
    "future",
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

withCallingHandlers(update.packages(lib.loc=lib, repos=repo, ask=FALSE),
                    warning = function(w) stop(w))
if(length(newPkgs)>0) {
  withCallingHandlers(install.packages(newPkgs, lib=lib, repos=repo),
                      warning = function(w) stop(w))
}
