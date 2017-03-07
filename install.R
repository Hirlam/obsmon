repo=c("http://cran.ma.imperial.ac.uk/")
lib=Sys.getenv("R_LIBS_USER")

pkgs <- c("httpuv",
          "shiny",
          "RSQLite",
          "ggplot2",
          "jpeg",
          "reshape2",
          "chron",
          "scales",
          "mapproj",
          "gridExtra",
          "leaflet",
          "futile.logger",
          "shinyjs",
          "yaml")

installedPkgs <- installed.packages(lib.loc=lib)

newPkgs <- pkgs[!(pkgs %in% installedPkgs)]

update.packages(lib.loc=lib, repos=repo, ask=FALSE)
if(length(newPkgs)>0) {
  install.packages(newPkgs, lib=lib, repos=repo)
}
