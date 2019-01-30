# Readme
Obsmon is a tool for observation monitoring in the [Harmonie-Arome NWP
System](http://hirlam.org/). It is composed of two parts: The backend and
the frontend. The backend is part of the scripting system and produces
databases with the relevant information as part of the postprocessing. The
frontend allows the analysis and visualization of this data. ***This
software, and all files contained here, correspond only to the frontend
part of the full Obsmon package***. In this document, however, "obsmon"
will always refer to the frontend part, unless otherwise specified.

Obsmon is implemented as a web interface written in
[R](https://www.r-project.org/), build on the
[Shiny](https://shiny.rstudio.com/) web application framework.
It can be run as a standalone server, or inside the [Shiny
Server](https://www.rstudio.com/products/shiny/shiny-server/).

This document describes how to configure and run obsmon.


## Installation
For instructions regarding installation, please take a look at the
[INSTALL.md file](./docs/INSTALL.md) located under the `docs` directory.


## Configuration
The first step is to configure the paths to your experiments' data. This
is done by creating a `config.toml` configuration file. Please take a look
at the [config.toml.example file](./docs/config.toml.example), located
under the `docs` directory, for an example of the file format and options.
  - You can optionally  provide another name/path for your config file
    if you so wish. You can do so by setting the value of the environment
    variable `OBSMON_CONFIG_FILE` to the desired full path.
  - Obsmon will look for the config file in the following paths
    (listed in order of priority):
    1. The value of the environment variable `OBSMON_CONFIG_FILE`, if set
    2. Under the obsmon installation directory (same directory where this
       file is located, if you have not changed its location)
    3. Under `/etc/obsmon/$USER`.

The configuration file is written in the
[TOML](https://github.com/toml-lang/toml) format, which is similar to the
widespread ini format. It is made up of sections (called "tables"
in TOML parlance). For obsmon, it may contain a `[general]` section,
must contain at least one `[[experiments]]` table, and may also contain
multiple `[[multiPlots]]` sections.

  - **Note the mandatory double brackets** in `experiments` and
    `multiPlots`, as opposed to the single brackets used for `general`. **Sections defined with single brackets cannot have repeated names,
    whereas multiple double-bracketed sections with the same name are
    allowed.**

The main config options are described below. For a more concrete
example of how this works, we encourage you to take a look at the
[config.toml.example file](./docs/config.toml.example).

#### General options
The general options are configured in the `[general]` section and at the
moment consist of

- `cacheDir`: Directory where obsmon will store its cache.
    - The cache contains metadata like the obtypes and stations used in
    the experiment's databases for given dates/cycles.
- `logLevel`: Amount of detail that is logged.
    - Valid values are: `TRACE`, `DEBUG`, `INFO`, `WARN`, `ERROR`,
    `FATAL`, with `TRACE` giving the greatest level of detail
    - In standalone mode everything is logged to `stderr`
    - If running on a shiny server, then
      the log ends up in the respective shiny server log. For more details
      about logging while running on a shiny server, please take a look at
      the [shiny documentation](http://docs.rstudio.com/shiny-server/#logging-and-analytics)
- `initCheckDataExists`: Whether or not to assert the existence of the
   experiments' data files at startup.
   - If `false` (or not set), then Obsmon will only determine the
     available DTGs at startup by looking at the directories under the
     experiments' `path`s (which is quick).
   - If set to `true`, the DTGs will only be shown as being available if
     the corresponding data files actually exist inside the directories
     under the corresponding `path`s. This is slow and not generally
     necessary, but is safe if you absolutely need it.
- `maxExtraParallelProcs` specifies the max number of extra parallel
    tasks that obsmon is allowed to execute in an asynchronous manner
    at any given time. May be set to any number greater or equal to zero.
    If it is set to an invalid value (or is not set), then the max number
    of allowed tasks will be unlimited.
    - Asynchronous/parallel tasks are used, e.g., to be able to
      load/cache experiments without blocking the GUI, as well as to
      allow having a "cancel plot" functionality
    - These extra tasks are short-lived and are not usually
      computationally intensive, so we advise you not to set this option
      unless you have good reasons to do so.
    - You can also control this by setting the environment variable
      `OBSMON_MAX_N_EXTRA_PROCESSES`, but the valued set via config
      file takes precedence.
- `showCacheOptions`: Whether or not to show advanced cache options.
    - If set to `true`, then the GUI will feature two buttons that allow
      rewriting or resetting the cache files
      - Since obsmon may be run via shiny server and/or shared by
        multiple users, these buttons are hidden by default to prevent
        one user from changing the cache configuration used by the
        others. You would typically use this to solve problems with a
        corrupted cache file. This is safe to use if you know you won't
        inadvertently affect other users.
    - If `false` (or not set), then these options will be hidden.


#### Experiments
Experiments are configured by adding one `[[experiments]]` section per
experiment.

Every `[[experiments]]` entry contains two keys:

- `displayName` is used to identify the experiment in the web interface.
- `path` is the path to the directory containing the data.

Obsmon expects to find directories named `ccma`, `ecma`, `ecma_sfc`
under the directory specified in `path` (or at least one of them).
They are assumed to contain the databases corresponding to
minimization, screening and CANARI data, respectively. Each of these
directories is expected to contain subdirectories named according to
the standard dtg format `YYYYmmddHH` (one such directory for Each
available DTG in the experiment), which, in turn, should contain the
actual database files, named, again respectively, `ccma.db`, `ecma.db`,
`ecma.db`(sic).

Obsmon finds thus the data by combining, into a single path, (i) the
value passed in `path`, (ii) the appropriate directory for minimization,
screening, or CANARI data, (iii) the date(s) and cycle(s) selected in the
GUI, and finally (iv) the appropriate `.db` file.

#### multiPlots
 A `[[multiPlot]]` section allows users to configure a larger multiplot
 that may include, for instance, different observation types, variables,
 satellites, sensor names etc. Plots within a multiplot must correspond
 to the same experiment, plot type, database (e.g. "ecma_sfc") and DTG(s).
 Once again, for a concrete example, please take a look at the
 [config.toml.example file](./docs/config.toml.example).


## Running Obsmon

#### Running in standalone mode
Just run `./obsmon` Once you have finished installation and set up the
config file.
  - You can also run `./obsmon --launch` if you want your browser to be
    launched automatically.

**For a list of currently supported command line options**, please run
`./obsmon -h`.

#### Running in a Shiny Server
This means running the server and connecting it to your browser. Please
take a look at the [INSTALL.md file](./docs/INSTALL.md) located under the
`docs` directory.
