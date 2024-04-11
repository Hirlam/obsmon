[The config file](@id config-file) 
===============

The next step after installation is to create a configuration file. Such
a file has the main purpose of telling obsmon where to find your
experiments, but it can also be used to control how the code works. It
is written in [TOML](https://github.com/toml-lang/toml), which is
similar to the widespread `ini` format.

Obsmon looks for a configuration file named `config.toml` under the
following directories (listed in order of priority):

1.  `$HOME/.obsmon`

2.  `/etc/obsmon/$USER`

3.  The obsmon installation directory

Alternatively, you can instead provide the full path to a valid
configuration file using the `OBSMON_CONFIG_FILE` environment variable,
in which case you are free to choose the file name. This takes higher
priority over the other options.

A TOML config file is made up of sections (called \"tables\" in TOML
parlance). A valid config file for obsmon:

-   May contain one `[general]` section

-   Must contain at least one `[[experiments]]` table

-   May contain multiple `[[multiPlots]]` sections

-   May contain one `[domain]` section

The next sections describe the config file options for the `[general]`
and `[[experiments]]` tables. The configuration of `[[multiPlots]]` and
`[domain]` will be discussed in
[multiplots](@ref), [domains](@ref) , respectively. An example of a
simple config file is presented in
[4.3](@ref config-file-example). For a more complex config file
example, see the config file template `docs/config.toml.example`.
*Options that have default values are optional.*

[Options for the `[general]` section](@id config-file-general) 
-----------------------------------

As the name suggests, the `[general]` section controls how the code
works in general. The currently available options are:

-   `appTimeout`: Time interval, in seconds, after which obsmon will
    stop and exit after all browser sessions have been closed.\
    Default: `"Inf"`\
    Acceptable values: Any number greater than or equal to zero. Passing
    invalid values will cause it to fall back to the default.\
    \
    This option is similar but not equal to the option `sessionTimeout`
    described further below.

-   `cacheDir`: User-configurable part of the path to where obsmon will
    store its cache.\
    Default: `$HOME/.obsmon/experiments_cache`\
    \
    The path initially passed to this parameter will be further appended
    by a directory named as `obsmon_vMAJOR.MINOR`. This is to avoid
    conflict when updating obsmon. Furthermore, cache files for each
    experiment will be stored separately inside directories named
    according to the experiments' names. For more information on cache,
    please read [5.7](@ref cache).

-   `configName`: Name of your config file.\
    Default: `""`\
    Acceptable values: Any string.\
    \
    The text assigned to `configName` (if any) will be printed out to
    the logs as `INFO` (see option `logLevel` below), and will also be
    displayed in the GUI (on the right-hand side of the place where the
    code version info is displayed).

-   `logLevel`: Amount of detail that is logged.\
    Default: `WARN`\
    Valid values (in decreasing order of detail): `TRACE`, `DEBUG`,
    `INFO`, `WARN`, `ERROR` and `FATAL`

    -   In standalone mode, everything is logged to `stderr`

    -   If running on a shiny server, then the log ends up in the
        respective shiny server log. For more details about logging
        while running on a shiny server, please take a look at the
        [shiny documentation for logging and
        analytics](http://docs.rstudio.com/shiny-server/#logging-and-analytics)

-   `maxAvgQueriesPerProc`: Maximum number of database queries (in
    average) that a single process (computer thread) is allowed to
    perform when preparing plots.\
    Default: `"Inf"`\
    Acceptable values: Any integer number greater than or equal to one,
    or \"Inf\". Passing invalid values will cause it to fall back to the
    default.\
    \
    Data for obsmon plots is stored in separate files for each `DTG`.
    Therefore, if your plot requires `nDTGs`, then `nDTGs` independent
    database files will be queried. With the `maxAvgQueriesPerProc`
    option, these queries will be divided into
    `max{1, ceiling(nDTGs/maxAvgQueriesPerProc)}` groups that will then
    be processed in parallel.\
    \
    The optimal value for this parameter is, of course system-dependent,
    and the default `"Inf"` leads to no query parallelisation.

-   `maxExtraParallelProcs`: Maximum number of extra tasks that obsmon
    is allowed to execute, at any given time, in parallel to the main
    process.\
    Default: 4 $\times$ `#availableCores`\
    Acceptable values: Any integer number greater than or equal to zero.
    Passing invalid values will cause it to fall back to the default.\
    \
    Asynchronous/parallel tasks are used, e.g., to be able to load and
    cache experiments without blocking the GUI, as well as to allow
    having a \"cancel plot\" functionality. It is worth mentioning that:

    -   These extra tasks are short-lived and are not usually
        computationally intensive (not all at the same time, at least),
        so we advise you not to set this parameter to a smaller value
        unless you have good reasons to do so.

    -   The `OBSMON_MAX_N_EXTRA_PROCESSES` environment variable can also
        be used in order to control this setting, but the value set via
        the config file takes precedence.

-   `multiPlotsEnableInteractivity`: Whether or not to allow
    `multiPlots` (see [5.4](@ref multiplots) to be interactive.\
    Default: `false`\
    Accepted values: `true` or `false`\
    \
    We have chosen to make this option `false` by default to avoid
    memory issues, as large numbers of individual plots may in principle
    be generated within a single `multiPlot`. This is especially
    important when deploying obsmon in a Shiny Server.[^6]

-   `plotsEnableInteractivity`: Whether or not to allow *regular plots*
    to be interactive.\
    Default: `true`\
    Accepted values: `true` or `false`

-   `sessionTimeout`: Time interval, in seconds, after which any idle
    obsmon web session should be terminated.\
    Default: `"Inf"`\
    Acceptable values: Any number greater than or equal to zero. Passing
    invalid values will cause it to fall back to the default.\
    \
    Users will receive at least 60s warning that their sessions will be
    terminated if they remain idle.

-   `showCacheOptions`: Whether or not to show advanced cache options
    (see [5.7.1](@ref advanced-cache-opts)).\
    Default: `false`\
    Accepted values: `true` or `false`\

[Options for the `[[experiments]]` section](@id config-file-experiments) 
-----------------------------------------

This the section where you tell obsmon how to locate your experiment
files and how you want your experiments to be named. You should include
one `[[experiments]]` section for each experiment, each section defining
the following two keys:

-   `displayName`: Used to identify the experiment in the web
    interface.\
    Accepted values: Any (you are free to choose the name of your
    experiment)

-   `path`: Path to the directory containing the experiment data.\
    Allowed values: Any valid experiment path. More details below.

Obsmon expects to find directories named `ccma`, `ecma`, `ecma_sfc`
under the directory specified in `path` (or at least one of them). These
directories are assumed to contain the databases corresponding to
minimisation, screening and CANARI data, respectively. Each of these
directories is expected to contain sub-directories named according to
the standard dtg format `YYYYmmddHH` (one such directory for each
available DTG in the experiment), which, in turn, should contain the
actual database files, named, again respectively, `ccma.db`, `ecma.db`
and `ecma.db` (sic).

Obsmon finds the data by combining into a single path (i) the value
passed in `path`, (ii) the appropriate directory for minimisation,
screening, or CANARI data, (iii) the date(s) and cycle(s) selected in
the GUI, and finally (iv) the appropriate `.db` file.

[Config file example](@id config-file-example) 
-------------------

A simple config file may look like the following:

    [general]
        logLevel = "INFO"
        cacheDir = ".Rcache"

    [[experiments]]
        displayName = "First Experiment"
        path = "/full/path/to/experiment1"

    [[experiments]]
        displayName = "Second Experiment"
        path = "/full/path/to/experiment2"

For a more complete example, please take a look at the template config
file `docs/example/config.toml`. A few notes:

-   Comments can be added using a `#` character and are optional

-   Indentation is also optional, but highly encouraged

[^6]: [Problems are known to occasionally occur in this
    case](https://stackoverflow.com/questions/46420121/shiny-app-with-plotly-disconnects-from-server-after-rendering-2-plotly-graphs-u).
    We have indeed experienced a few.
