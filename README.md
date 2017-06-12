# Readme
Obsmon is a tool for observation monitoring in the [Harmonie-Arome NWP
System](http://hirlam.org/). The backend is part of the scripting system
and produces databases with the relevant information as part of the
postprocessing. This readme is part of the frontend, that allows the
analysis and visualization of this data.

The frontend is realized as a web interface written in
[R](https://www.r-project.org/), build on the
[Shiny](https://shiny.rstudio.com/) web application framework.
It can be run as a standalone server, or inside the [Shiny
Server](https://www.rstudio.com/products/shiny/shiny-server/).
In this document obsmon always refers to the frontend, unless specified
otherwise.

The rest of this document is structured as follows: First the configuration
is described. Then the installation procedures for both modes of operation are
detailed.


## Configuration
In order for obsmon to work, it needs to know where to find the databases.
This information must be provided by means of the configuration file.
The name of the configuration file is `config.toml`. By default, obsmon will
first look in the installation directory for this file. If it does not find
the config file there it will look for `/etc/obsmon/config.toml`.

The configuration is written in [TOML](https://github.com/toml-lang/toml)
format, which is similar to the widespread ini format. It is made up of sections
that are called tables in TOML parlance.
The obsmon configuration file can contain a `[general]` table or section, and must
contain one or more `[[experiments]]` tables. All options are demonstrated in
`config.toml.example` and described in detail in the following.

### General options
The general options are configured in the `[general]` section and at the moment
consist of

- `cacheDir` specifies the directory where obsmon will store its cache. It
  contains metadata like the obtypes and stations used in particular databases.
- `logLevel` determines the amount of detail that is logged. In standalone mode
  everything is logged to `stderr`. If run in shiny server, the log ends up in
  the respective shiny server log. For details refer to [its
  documentation](http://docs.rstudio.com/shiny-server/#logging-and-analytics).
  Valid values are `TRACE`, `DEBUG`, `INFO`, `WARN`, `ERROR`, `FATAL`, with
  `TRACE` giving the greatest amount of details.

### Experiments
Experiments are configured by adding one `[[experiments]]` section per
experiment. Note the mandatory double brackets as opposed to the single brackets
used for the `[general]` section. This is because there can be more than one
`[[experiments]]` section.

Every experiment contains three keys:

- `displayName` is used to identify the experiment in the web interface.
- `baseDir` is the path to the directory containing the data.
- `experiment` is the name of the experiment in the filesystem.

The data is located based on `baseDir` and `experiment`: The current version of
the backend produces three sets of databases for every experiment which are
located within one main directory that contains three subdirectories, `ccma`,
`ecma`, and `ecma_sfc` for minimization, screening, and canari data,
respectively. Each of these contains a number of subdirectories, one
for every cycle of the experiment, named according to the standard dtg format
`YYYYmmddHH`. These, in turn, contain the actual database files, named
`ccma.db`, `ecma.db`, `ecma.db`(sic), again respectively.

Thus obsmon finds the data by combining into one path the `baseDir`, followed by
`experiment`, then the appropriate directory for minimization, screening, or
canari data, next the necessary cycle and finally the pertinent `.db` file.
The reason for separating `baseDir` and `experiment` is that it is a frequent
use case to have a family of experiments that is contained in the same parent
directory. In this case in the future it will be permitted to use file globbing
in the experiment key. Then obsmon will add one experiment for every matching
directory and group the corresponding entries in the web interface according to
the provided `displayName` key.


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
