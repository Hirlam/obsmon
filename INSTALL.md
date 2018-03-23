## Installation
As stated above, obsmon is a web application written in R using the Shiny
framework. Because of this, it can either be run as a standalone server, or
within the Shiny Server. Additionally, it makes use of a number of R packages
that have to be installed in any case. To facilitate easy development, by
default obsmon in standalone mode will use a personal library at
`$HOME/R/library` as the location for all R packages, whereas in Shiny Server it
will use the standard library of your system, that depends on your R
installation and usually will be found in something like
`/usr/lib64/R/library`.

### Standalone
To get obsmon up and running quickly, follow these steps:

1. Install R. Chances are, it is available in your distributions repositories.
2. Execute `./install` in the obsmon directory. This will install the needed R
   packages.
3. Copy `config.toml.example` to `config.toml` and adapt it to your data paths.
   It is highly recommended to start out with only one experiment that doesn't
   contain to much data. You can easily add more experiments once you are sure
   everything is working fine.
4. Execute `./obsmon` in the obsmon directory. At the end of the output you will
   find a line like

   ```
   Listening on http://127.0.0.1:5391
   ```

5. Point your browser to this address. Note, that this only works if the browser
   is running on the same machine as obsmon. Otherwise you will need to use ssh
   port forwarding or x forwarding to connect to the server.

If all went well, you should now see some output on the console about the
initialization of the experiments you configured. Particularly for the first
start after the configuration of a new experiment, this can take some time
because obsmon needs to build up its cache. Please be patient.

### Shiny Server
If you want to offer obsmon to a larger number of users, you probably want to
run it inside the Shiny Server. Its installation is beyond the scope of this
document, so moving forward we will assume, that it is already installed and
running.
From there, follow these steps:

1. Put the obsmon directory into the `site_dir` directory as configured in your
   `shiny-server.conf`, or, alternatively, add a `location` stance to your
   `shiny-server.conf` listing the obsmon directory.
2. Install the R package dependencies (together with their dependencies) listed
   in `install.R` into your system wide R library.
3. Copy `config.toml.example` to `/etc/obsmon/config.toml` and adapt to your
   needs.
4. Run your Shiny Server and connect to it with your browser. At that moment
   obsmon will start to build its cache, which can take considerable amounts of
   time for a freshly configured experiment. To check the progress, follow the
   pertinent Shiny Server log file. Its location can be configured in your
   `shiny-server.conf`, but is most likely
   `/var/log/shiny-server/obsmon-*.log`.

### Dependencies
List of system dependencies for R packages used in Obsmon.
OS: CentOS 7, clean installation.
Date: 2018-03-22

Cairo:
  * cairo-devel
  * libXt-devel

DBI:
  * libcurl-devel (for "curl" dependency)
  * openssl-devel (for "openssl" dependency)
  * libxml2-devel (for "xml2" dependency)

dbplyr:
  * mariadb-devel (for "RMariaDB" dependency)
  * postgresql-devel (for "RPostgreSQL" dependency)

leaflet:
  * geos-devel (for "rgdal" dependency)
  * proj-devel (for "rgdal" dependency, see note below)
  * proj-epsg (for "rgdal" dependency, see note below)
  * gdal-devel (for "rgdal" and "sf" dependencies, see note below)
  * udunits2-devel (for "udunits2" dependency of "sf")

shinyjs:
  * v8-devel (for "V8" dependency)

#### Note about the "sf" R package:
The sf dependency requires gdal-devel v > 2.0.0. This version, however, is
not currently available for RHEL Client release 7.4 (Maipo) at SMHI/LINDA
via simple "yum install". Two solutions are:

(i) Get the needed RPMs/RPM sources for rgdal v2.x.x, as well as the needed
    dependencies, and rebuild/install them manually. This can be done at SMHI
    by using the "install_sys_deps.sh" script provided and passing the option
    "--enable_R_sf".
(ii) Do not install the sf R package at all. In this case, the current system
     dependencies can be installed at SMHI by running "install_sys_deps.sh".
     Although this is obviously not the ideal option, this carries only a
     minimal risk of leading to failures in the future, as the "sf" package
     appears only as a "Suggests"-type pkg dependency of the "leaflet" R pkg.

Option (ii) is the preferred one at SMHI, because option (i) updates the
system "proj*", "libgeotiff*", "libspatialite*", "ogdi*" and "gdal*"packages,
which leads to conflicts with other R-based software used locally (especially
in the case of "proj*").
