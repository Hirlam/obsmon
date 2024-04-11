[Using obsmon](@id usage)
============

Once installed ([3](@ref install)) and minimally configured
([4](@ref config-file), obsmon is typically quite simple to use.
This section discusses those features of the code that may be less
straightforward to use, or about which we most commonly receive
questions from users. If you have any doubts that are not covered here,
you recommend you to take a look at the FAQ in
[6](@ref faq).

[Interactive plots with editable elements](@id usage-plots-interactive) 
----------------------------------------

Most plots in obsmon allow you to zoom, pan, hover etc. A less obvious
but useful feature of these plots is the ability to hide and show parts
of the plotted data. You can do so by single- or double-clicking on the
items in the legend. You can also edit plot titles, legends, axes
labels, as well as change the position of some of these elements by
dragging them.

This feature is enabled by default for regular plots, and it can
switched off/on by using the config file option
`plotsEnableInteractivity` (see
[4.1](@ref config-file-general)). The default behaviour for
`multiPlots` ([5.4](@ref multiplots), on the other hand, is different, and
interactivity is disabled for such plots unless otherwise specified via
config file option `multiPlotsEnableInteractivity`.

[Physical Units in the Plots](@id plotUnits) 
---------------------------

For the plots in the main area, it is possible to customise the physical
units used for some physical quantities. You can modify these by
entering the name of the new units in the appropriate GUI text input.
The new units must, of course, be compatible with the default ones.

[Explore The Data Used in Your Plots](@id queryAndData) 
-----------------------------------

For every plot, there is also available a **Query & Data** tab where you
can find the SQLite query used to fetch the data along with the fetched
raw data, as well as the aactuall data used in the plot.

Note that:

-   The tables are searchable

-   You can use the arrows next to the tables' column labels to sort
    data

    -   You can *sort by multiple columns* by pressing the `shift` key
        when clicking on these arrows

-   You can export the tables' data as text or CSV files

[Producing multiple plots in one go with `multiPlots`](@id multiplots) 
----------------------------------------------------

A `multiPlot` is a collection of multiple plots performed as a result of
a single plot request. It provides a way to reduce the number of
operations (or "clicks\") a user needs to perform in order to generate
multiple plots.

The parameters for each `multiPlot` are defined in the configuration
file through the use of `[[multiPlot]]` sections. In the following, we
will discuss the main aspects of how to configure a `multiPlot`. There
is no limit to the number of `multiPlots` that can be configured. To add
a new `multiPlot`, just add a new `[[multiPlot]]` section to the
configuration file. We encourage you to take a look at the template
config file `docs/example/config.toml` for more concrete examples.

### [Parameters shared by all plots within the same `multiPlot`](@id multiplots-common-params) 

The following parameters apply for all plots contained within a
`multiPlot` and must be specified when defining a new `multiPlot`:

-   `displayName`: Used to identify the `multiPlot` in the web interface.  Accepted values: Any (you are free to choose the name of your `multiPlot`).

-   `experiment`: Which experiment to use.  Accepted values: The `displayName` of a valid `[[experiments]]` entry. See [4.2](@ref config-file-experiments).

-   `database`: Which experiment database to get data from.Accepted values: `ccma`, `ecma` or `ecma_sfc`.

-   `plotType`: The type of plot that will be performed.  Accepted values: Any `plotType` ordinarily supported by obsmon

-   DTG-related parameters.

    -   `startDate`: A date in the `"YYYY-MM-DD"` format or an integer
        number `N` less than or equal to zero. If the integer number
        format is used, then it will represent a date
        $\mid \texttt{N} \mid$ days before the day the plot is
        requested.

    -   `endDate`: A date in the `"YYYY-MM-DD"` format. *Do not use this
        if `startDate` is a negative integer or if `nDays` (see below)
        is used.*. The default, if `startDate` is not a negative integer
        and `nDays` is not passed, is 'today' -- whatever day today is.

    -   `nDays`: A non-negative integer number. Sets the end date for
        the plot to `startDate + nDays - 1`. *Do not use this if
        `startDate` is a negative integer or if `endDate` is explicitly
        set.*

    -   `cycles`: Cycle number(s) in the `"HH"` format. E.g.: `"03"` (to
        select a single cycle) or `["00", "03", "12"]` (to select
        multiple cycles).

### [Minimal `[[multiPlot]]` entry configuration](@id multiplots-minimal-config) 

The parameters described in [5.4.1](@ref multiplots-common-params) are already enough for a minimal `[[multiPlot]]` configuration. The following definition, for instance, is a perfectly valid `multiPlot` entry:

```
    [[multiPlots]]
        displayName = "A minimal multiPlot config entry"
        experiment = "The name of my experiment"
        plotType = "Number of Observations"
        database = "ecma"
        startDate = -30
```
Such an entry will create a `multiPlot` that will contain one plot of the "Number of Observations" type for every valid combination of observation type, name, variable, levels, station, satellite name, sensor, channels, etc. The default behaviour can thus be summarised as: *Everything is included unless otherwise specified.*

### Selecting what to include/exclude from a `multiPlot`

The total number of individual plots composing a `multiPlot` depends on what you choose to include and/or exclude. As explained in [5.4.2](@ref multiplots-minimal-config), obsmon adopts the convention that a `multiPlot` will include everything that can possibly make sense within its context, unless specified otherwise.

Including and/or excluding parameters such as observation types, variables, levels, stations, satellites, channels, etc. is relatively simple. There are, however, many such options, and explaining them all here would take many more lines than actually writing down the corresponding entries in the configuration file. For this reason, we have instead produced a template `config.toml.example` file containing a comprehensive selection of examples on how to setup `multiPlots`. This file can be found under the `docs` directory.

We highly recommend that you read and understand the template config file if you want to setup `multiPlots`, and we encourage you to copy the entries from that file and adapt them to what you need. Finally, feel free to get in touch should you think something is missing.

[Batch mode](@id batch-mode) 
----------

Batch mode allows producing plots without the use of the GUI. When run in batch mode, obsmon will produce the appropriate pre-configured plot(s), save the results in individual files (under appropriately named directories, see more below), and then exit.

There are many usage scenarios where such a functionality can come in handy. For instance, remotely opening a web browser from ECMWF can be very slow, as discussed. In such cases, one can run obsmon in batch mode and then retrieve the generated files via, e.g., `ftp`.  Another usage case example would be calling obsmon from a script that runs as part of a `cron` job, thus allowing one to regularly produce plots without user intervention.

### [Configuration of batch-mode plots](@id batch-mode-configuration) 

Batch-mode plots in obsmon are simply `multiPlots` activated for this type of use. Therefore, the first step to configure a batch-mode plot is to setup a valid `multiPlot` as described in [5.4](@ref multiplots).  Properly configured `multiPlots` can then be activated for use in batch mode by adding, *under the main level in the corresponding `[[multiPlots]]` entry*, either:

-   A `[multiPlots.batchMode]` table, which may be empty or contain any of the the following entries:

    -   `enable`: Whether or not to enable the `multiPlot` for use in batch mode.
        Default: `true`
        Accepted values: `true` or `false`

    -   `parentDir`: Where to put the directory containing the plots produced by the `multiPlot` when run in batch mode.  Default: The directory obsmon is being executed from.  Accepted values: Any path where the user running obsmon has write access to. Both relative and absolute paths are accepted.  Relative paths are assumed to be relative to where obsmon is being executed from. A `parentDir` may be shared by multiple `multiPlots`.

    -   `dirName`: The name of the directory where to put the produced plots. *This is just the directory name*. It will be prepended
        by `parentDir` to generate a full path. 

        Default: `obsmon_batch_MPNAME_TIMESTAMP`, where `MPNAME` is a version of the `multiPlot`'s `displayName` in lowercase and with any non-[word
        characters](https://www.regular-expressions.info/shorthand.html) (or sequence of such characters) replaced by a single
        underscore, and `TIMESTAMP` is the time (in [%H%M%S format](http://pubs.opengroup.org/onlinepubs/009695399/functions/strftime.html)) when the directory was created.

        Accepted values: Any valid directory name such that the full path `parentDir`/`dirName` does not already exist.

    -   `fileType`: The type of the graphics files produced.
        
        Default: `png` 

        Accepted values: Most of the commonly used file types that support saving graphics (e.g., `pdf`, `jpeg`, `tiff`, `png`, `bmp`). Note, however, that this is system-dependent.[^7]

    -   `dpi`: Resolution of the generated figures (in dots per inch).

        Default: `300`

        Accepted values: Any integer greater than zero.

    -   `figHeight`: Height of the generated figures (in inches).
    
        Default: `6`

        Accepted values: Any number greater than zero.

    -   `figWidth`: Width of the generated figures (in inches).

        Default: `10`

        Accepted values: Any number greater than zero.

    Example (compare with
    [5.4.2](@ref multiplots-minimal-config)):
    
    ``` 
        [[multiPlots]]
            displayName = "An arbitrary multiPlot"
            experiment = "My experiment"
            plotType = "Number of Observations"
            database = "ecma"
            startDate = -30
            [multiPlots.batchMode]
                parentDir = "/home/user/obsmon_batch_mode_plots"
    ```

or, if you want to keep it simple,

-   `batchMode = true`
    
    And that is it. This is of course much simpler than going via option
    (a), but it does not allow any customisation. Example (compare with
    [5.4.2](@ref multiplots-minimal-config)):

    ```toml
        [[multiPlots]]
            displayName = "An arbitrary multiPlot"
            experiment = "My experiment"
            plotType = "Number of Observations"
            database = "ecma"
            startDate = -30
            batchMode = true
    ```
Using option (a) with an empty `[multiPlots.batchMode]` table has the
same effect as using option (b). Finally, setting `batchMode = false` in
option (b) is also allowed, in which case the corresponding `multiPlot`
will, rather unsurprisingly, not become activated for use in batch mode.

### Running obsmon in batch mode

Just use the `–batch` command line option:
```
./obsmon --batch
```

[Domain Geometry & Grid](@id domains) 
----------------------

Interactive maps support the use of a domain's geometry and grid. If
set, these are employed so that:

-   Display and domain projections match

-   Initial zoom in config is based on the domain's boundaries

-   Plots of the \"Average Maps\" type can perform grid-averages

To enable domain use, just enter valid configurations in the fields
under the `Domain Geometry & Grid` tab in the GUI. The geometry and grid
resolution parameters used in obsmon are a subset of those specified in
Harmonie.

To set default parameters, which will be loaded in the GUI at startup,
you need to add a `[domain]` section to your config file. For example,
to configure a coarser version of the the `METCOOP25C` domain, you can
add the following section to your config file:

```toml
    [domain]
        nlon = 90      
        nlat = 96
        lonc = 16.763011639                             
        latc = 63.489212956                                                                                 
        lon0 = 15.0                         
        lat0 = 63.0
        gsize = 25000
        lmrt = false
```
**N.B.:** Please pay special attention to the `nlon`, `nlat` and `gsize`
parameters when copying configs from Harmonie domains. [The Harmonie
grids are normally much thinner than you would typically want to use in
obsmon, and using them without tweaking these parameters will most
likely lead to long processing times for plots involving grid
averages]{.underline}.

[Caching](@id cache) 
-------

Every time you select a new `{experiment, database, DTG(s)}`
combination, obsmon collects metadata such as available observation
types and names, variables and station IDs from the relevant data files.
This information is then saved to `sqlite` cache files. Cached
information is used to populate the menus in the GUI with choices that
better reflect your experiments' data.

Caching occurs automatically and asynchronously. How long it takes for
it to be completed depends on factors such as how many *new* DTGs you
have selected or, for instance, whether your experiment's data files are
located in the same computer as obsmon (faster) or in a network mount
point (slower). While caching is not finished (or if cache information
cannot be retrieved for whatever reason), the menus in the GUI become
populated with a set of default values. [You can therefore continue to
explore your data even if cached info is unavailable or
incomplete]{.underline}.

### [Advanced cache options](@id advanced-cache-opts) 

Advanced cache-related options can be enabled either by setting the
parameter `showCacheOptions` to `true` in the config file (see
[4.1](@ref config-file-general)) or by creating a file named
`.obsmon_show_cache_options` inside the main obsmon directory. The GUI
will then feature two buttons that allow rewriting or resetting the
cache files. You would typically use this to solve problems with a
corrupted cache file.

These buttons are hidden by default to prevent individual users from
changing the contents of cache files when using a single shared obsmon
installation (be it available locally or via web). Nevertheless, is safe
to use these advanced cache-resetting options if you know you won't
inadvertently affect other users. In fact, it is actually safe to
manually remove the cache files if the same circumstances apply. In the
worst case scenario, completely resetting/removing the cache files will
only cause obsmon to re-cache information when/if necessary. Your
experiments' data files will not be affected if you click on any of
these buttons.

[^7]: For more details, see the documentation of the `ggsave` function
    from the [ggplot2](https://www.rdocumentation.org/packages/ggplot2)
    `R` package.

