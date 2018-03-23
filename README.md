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

The rest of this document describes the configuration of obsmon.
The installation procedures for both modes of operation are detailed in the
[INSTALL.md](./INSTALL.md) file.


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


