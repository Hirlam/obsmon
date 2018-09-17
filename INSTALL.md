## Installation
As stated in the [README file](./README.md), obsmon is a web application written in R using the Shiny framework. Because of this, it can either be run as a standalone server, or within the Shiny Server. Additionally, it makes use of a number of R packages that have to be installed in any case. To facilitate development, by default obsmon in standalone mode will use a personal library at `$HOME/R/library` as the location for all R packages, whereas in Shiny Server it will use the standard library of your system, that depends on your R installation and usually will be found somewhere like `/usr/lib64/R/library`.

### Standalone
To get obsmon up and running quickly, follow these steps:

1. Install R. Chances are, it is available in your distributions repositories.
2. Execute `./install` in the obsmon directory. This will install the needed R
   packages, provided that all system dependencies are fulfilled. If you want the installation of Obsmon to be completely separated from your current R environment, you can, alternatively, run `./install --local-install`.
   * There is unfortunately no robust way currently to programatically determine the *system* dependencies of the R packages. The script `install_sys_deps.sh`, located under `./utils/build`, should install all dependencies needed at SMHI, but it may ocasionally need to be updated (also, please see the [note about the "sf" R package](Note-about-the-"sf"-R-package:) further down in this document).
3. Copy `config.toml.example` to `config.toml` and adapt it to your data paths.
   It is highly recommended to start out with only one experiment that doesn't
   contain to much data. You can easily add more experiments once you are sure
   everything is working fine.
4. Execute `./obsmon` in the obsmon directory. At the end of the output you    
   will find a line like
   ```
   Listening on http://127.0.0.1:5391
   ```
   If you want the browser to be open automatically, then run `./obsmon --launch` instead.
5. Point your browser to this address. Note, that this only works if the
   browser is running on the same machine as obsmon. Otherwise you will need to use ssh port forwarding or x forwarding to connect to the server.

If all went well, you should now see some output on the console about the
initialization of the experiments you configured. Particularly for the first
start after the configuration of a new experiment, this can take some time
because obsmon needs to build up its cache. Please be patient.

### Shiny Server
If you want to offer obsmon to a larger number of users, you probably want to
run it inside the Shiny Server. Its installation is beyond the scope of this
document, so, moving forward, we will assume that it is already installed and
running. From there, follow these steps:

1. Put the obsmon directory into the `site_dir` directory as configured in your
   `shiny-server.conf`, or, alternatively, add a `location` stance to your
   `shiny-server.conf` listing the obsmon directory.
2. Install the R package dependencies (and their system dependencies, if
   needed) into your system wide R library. The R packages that need to be installed can be listed by running `install --listdeps`. For system dependencies, please see the section [System Dependencies](#system-dependencies) below.
3. Copy `config.toml.example` to `/etc/obsmon/config.toml` and adapt to your
   needs.
4. Run your Shiny Server and connect to it with your browser. At that moment
   obsmon will start to build its cache, which can take considerable amounts of
   time for a freshly configured experiment. To check the progress, follow the
   pertinent Shiny Server log file. Its location can be configured in your
   `shiny-server.conf`, but is most likely
   `/var/log/shiny-server/obsmon-*.log`.

### System dependencies
Below is a list of system dependencies for R packages used in obsmon. The method used to obtain the list was rather brute-force: Attempting to install obsmon on a clean system installation (CentOS 7, minimal) and keeping note of the system packages that needed to be installed in order to fix the errors that appeared along the way. That means, among other things, that the list is not exhaustive, in the sense that many other packages are installed automatically as "dependencies of dependencies", being thus omitted here.

The system dependencies listed here can be installed at SMHI using the script `install_sys_deps.sh`, located under `./utils/build`. If you find that any dependencies are missing, please update that file (and this one as well).

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
release 7.4 (Maipo) at SMHI/LINDA via simple "yum install". Two solutions are:

1. Get the needed RPMs/RPM sources for rgdal v2.x.x, as well as the needed
   dependencies, and rebuild/install them manually. This can be done at SMHI
   by running the provided "install_sys_deps.sh" script with the option "--enable_R_sf".
2. Do not install the sf R package at all. In this case, the current system
   dependencies can be installed at SMHI by just running "install_sys_deps.sh".
   This options carries only a minimal risk of leading to failures in the
   future, as the "sf" package appears only as a ["Suggests"-type dependency](http://r-pkgs.had.co.nz/description.html) of the "leaflet" R pkg.

Option (2) is the preferred one at SMHI, because option (1) updates the
system "proj*", "libgeotiff*", "libspatialite*", "ogdi*" and "gdal*"packages,
which leads to conflicts with other R-based software used locally (especially
in the case of "proj*").
