## Installation
As stated in the [README file](./README.md), obsmon is a web application written in R using the Shiny framework. As such, it can either be run as a standalone server, or within a Shiny Server. Obsmon also makes use of a number of R packages that need to be installed before you can use it. The included `install` script will help you to install these R packages regardless of how you intend to run obsmon. For instance, the R packages that need to be installed can be listed by running `./install --listdeps`.

It is helpful to know that Obsmon looks for packages installed in the following paths (listed in order of priority):
1. `./utils/build/local_R_library/R-libs`
2. The path listed in the environment variable `R_LIBS_USER`, if set
3. `$HOME/R/library`, if `R_LIBS_USER` is not set
4. The default R library search paths. These vary depending on your system. You can check these out by using, for instance, the R function `.libPaths()`.

There is unfortunately no robust way currently to programatically determine the *system* dependencies of the needed R packages. The script `install_sys_deps.sh`, located under `./utils/build`, should install all system dependencies needed by Obsmon under RHEL 7 (or CenOS 7), but that script may occasionally need to be updated. You can find more information about system dependencies in the  section [System Dependencies](#system-dependencies). See also the [note about the "sf" R package](Note-about-the-"sf"-R-package:) further down in this document

### Installing Obsmon to be used in standalone mode
To get obsmon up and running quickly, follow the instructions given below:

1. Install R. Chances are, it is available in your distributions repositories.
2. Go to the obsmon directory and execute `./install` -- or, alternatively, `./install --local-install` (if you want the installation of Obsmon to be completely separated from your current R environment). This will install the needed R packages, provided that all system dependencies are fulfilled.
3. Copy `config.toml.example` to `config.toml` and adapt it to your data paths.
   It is highly recommended to start out with only one experiment that doesn't
   contain to much data. You can easily add more experiments once you are sure
   everything is working fine.
4. Execute `./obsmon` in the obsmon directory. In the output you will find a line similar to the following:
   ```
   Listening on http://127.0.0.1:5391
   ```
   Point your browser to the address you see in your output.
   * NB: This only works if the browser is running on the same machine as obsmon. Otherwise you will need to use X forwarding or SSH port forwarding to connect to the server.
5. Alternatively, if you want the browser to open automatically, you can run obsmon as: `./obsmon --launch`.

If all went well, you should now see some output on the console about the
initialization of the experiments you configured. Particularly for the first
start after the configuration of a new experiment, this can take some time
because obsmon needs to build up its cache. Please be patient.

### Installing Obsmon to run in a Shiny Server
If you want to offer obsmon to a larger number of users, you probably want to
run it inside the Shiny Server. Its installation is beyond the scope of this
document. You can find more information about this at, e.g., the [RStudio Shiny Server download website](https://www.rstudio.com/products/shiny/download-server). In the following instructions, we assume that the Shiny Server is already installed and running.

1. Put the obsmon directory into the `site_dir` directory as configured in your
   `shiny-server.conf`, or, alternatively, add a `location` stance to your
   `shiny-server.conf` listing the obsmon directory.
   * You may want to configure in your
   `shiny-server.conf` the location of your log files. If you do not do so, the log will most likely end up somewhere such as `/var/log/shiny-server/obsmon-*.log`, although this cannot be guaranteed.
2. Install the R package dependencies into your system-wide R library or, if you prefer, into any of the directories listed at the beginning of this file.
3. Copy `config.toml.example` to `/etc/obsmon/config.toml` and adapt it to your needs.
4. Run your Shiny Server and connect to it with your browser. At that moment
   obsmon will start to build its cache, which can take considerable amounts of
   time for a freshly configured experiment. The progress is shown in the GUI on the browser.

### System dependencies
Below is a list of system dependencies for R packages used in obsmon. The method used to obtain the list was rather brute-force: Attempting to install obsmon on a clean system installation (CentOS 7, minimal) and keeping note of the system packages that needed to be installed in order to fix the errors that appeared along the way. That means, among other things, that the list is not exhaustive, in the sense that many other packages are installed automatically as "dependencies of dependencies", being thus omitted here.

The system dependencies listed here can be installed on RHEL 7 (or CenOS 7) by using the script `install_sys_deps.sh`, located under `./utils/build`. If you find that any dependencies are missing, please update that file (and this one as well).

In the following, the headers give the names of the R packages, and the items in the bullets refer to the Linux packages (RHEL 7) that need to be installed. The list was last updated on 2018-03-22.

N.B.: The names of these system dependencies may (most likely will) differ if you use another system (e.g. ubuntu).

Cairo:
  * cairo-devel
  * libXt-devel

DBI:
  * libcurl-devel (for "curl" package)
  * openssl-devel (for "openssl" package)
  * libxml2-devel (for "xml2" package)

dbplyr:
  * mariadb-devel (for "RMariaDB" package)
  * postgresql-devel (for "RPostgreSQL" package)

leaflet:
  * geos-devel (for "rgdal" package)
  * proj-devel (for "rgdal" package, see note below)
  * proj-epsg (for "rgdal" package, see note below)
  * gdal-devel (for "rgdal" and "sf" packages, see note below)
  * udunits2-devel (for "udunits2" dependency of "sf")

shinyjs:
  * v8-devel (for "V8" package)

#### Note about the "sf" R package:
The sf package requires the gdal-devel v > 2.0.0 to be installed in the system. This version, however, is not currently available for RHEL Client
release 7.4 (Maipo) via simple "yum install". Two solutions are:

1. Get the needed RPMs/RPM sources for rgdal v2.x.x, as well as the needed
   dependencies, and rebuild/install them manually. This can be done on RHEL/CentOS 7 by running the provided `install_sys_deps.sh` script with the option `--enable_R_sf`.
2. Do not install the sf R package at all. In this case, the current system
   dependencies can be installed on RHEL/CentOS 7 by just running `install_sys_deps.sh`.
   This option carries only a minimal risk of leading to failures in the
   future, as the "sf" package appears only as a ["Suggests"-type dependency](http://r-pkgs.had.co.nz/description.html) of the "leaflet" R package.

Option (2) is configured by default in the `install` script. This choice was made because option (1) updates the system's "proj*", "libgeotiff*", "libspatialite*", "ogdi*" and "gdal*"packages, which may lead to conflicts with other R-based software already in use (especially in the case of "proj*"). If you really want to install obsmon with support to sf anyway, then follow the instructions in option (1) and then include the command line option `--sf-suggests-support` when running the obsmon `install` script. But remember: You have been warned!
