# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com)
and this project adheres to [Semantic Versioning](http://semver.org).

## [4.4.0] 2022-08-03
### Added
- Allow passing `-repos=cran` the `install` script wherever applicable
- Option `--lock` to `install` script, which can be used to create an
  `.installer_pkg_versions.txt` R-package version locker file
- Option `--only-metadata` to the `create-local-repo` command of the `install` script
- Info about advanced installation options, as well as support information, in the PDF doc
### Changed
- Update versions of R-packages
- Default to not having the `.installer_pkg_versions.txt` file
### Fixed
- Bugs in the `createrepo` command of the `ìnstall` script
- Output format in the `listdeps` command of the `ìnstall` script


## [4.3.2] 2022-05-24
### Fixed
- Fix excessive jitter in interactive map plots when the number of
  plotted observations is small.


## [4.3.1] 2022-02-08
### Changed
- Center maps using domain center if a domain is configured.


## [4.3.0] 2022-02-07
### Fixed
- Install issue related to config args
- Install issue that caused dependencies to be queried for the most
  recent versions of the R-packages, even if the versions of the
  dependencies were locked using the .installer\_pkg\_versions.txt file


## [4.2.0] 2021-01-04
### Added
- Support to specifying domain geometry and grid for use in interactive map plots
    - If set, domains are employed so that:
        - Display and domain projections match
        - Domain boundaries are used for setting up initial zoom in
        - Plots of the "Average Maps" type can perform grid-averages
- New plots:
    - "First Guess and Analysis Departure"
        - See note under the "Fixed" section below
    - "Average First Guess and Analysis Departure"
    - "Average First Guess and Analysis Bias/RMS"
    - "Station Average First Guess and Analysis Departure"
- "Standardise Levels in the Final Plot" UI switch
- Option to reverse colorscale in maps
- Support to TITAN QC flags
### Fixed
- Fix name of "First Guess and Analysis Bias/RMS" plot
    - Which was wrongly named "First Guess and Analysis Departure" before
- Issue, due to a bug in the R-lib tibble v3.1.1, that could cause some
  plots to crash
- Various minor fixes
### Changed
- Improvements to GUI responsiveness
- Show level values based on database table (obsmon/usage) queried by plot
- Menu to change colormap and scale in map plots is now collapsible
- "Average Maps" plots now perform grid-averages if a domain is specified


## [4.1.0] 2021-05-17
### Added
- New "Number of Active Observations" plot type
- Editable colour scale and data range on applicable plots
- Possibility to choose units for physical quantities where applicable
- Button to toggle "show only standard levels" (when applicable)
- Icons to notify about UI field values being cached or refreshed
### Changed
- Searchable fields in main tab
- Give each plot inside a multiPlot its own independent
  "Plot", "Map" and "Query & Data" tabs
- Style of UI fields "Cycles", "Levels", "Channels"
- Interrupt cache if user changes experiment/database/DTG, and start
  caching the relevant files for the new selection.
- Land-sea departure plot can now be made interactive
- Some plots now have slightly different data columns in "Quey and Data"
### Fixed
- Missing bias panel in "Station Diagnostics Plot" if variable != "apd"
- Missing error message when server cannot be created
- Bug that would cause the installer not to respect package versions
  and install the latest ones instead


## [4.0.0] 2021-02-25
### Added
- New plot types:
    - First Guess Map
    - Average First Guess Map
    - First-Guess Departure Timeseries
    - Analysis Departure Timeseries
- multiPlots: Ability to generate multiple "single-date" plots.
      - If multiple dates/cycles are specified using "startDate", "endDate",
        "nDays" and "cycles", then a multiPlot with multiple single-DTG plots will
        be created using all combinations of dates and cycles.

### Changed
- Install script (removed old, put a new one in place)
- Redefine analysis increment in maps ("fg_dep-an_dep" instead of "an_dep-fg_dep")
- Remote CRAN mirror address in install script
- Path to libv8.so (see .Rprofile file) for installs on ECGB
- multiPlots:
    - Config options "date" and "cycle" no longer exist. They used to
      apply to plot types that required a single DTG. Now, for all the plot types,
      dates and cycles are specified using "startDate", "endDate", "nDays" and
      "cycles" (the same way it has been done for plots that use a date range).
      Please check the documentation for more usage details.
    - "nDays" config parameter now sets the endDate to "startDate + nDays - 1".
      The previous value was "startDate + nDays".
    - "startDate" param can now also be zero, in which case it is taken to be
      "today" -- whatever day today is.

### Removed
- Sources for R-lib dependencies
    - The new install script has options to create a CRAN-like repo and install
      dependencies from it, if users wish to use such functionality (e.g., to
      freeze the versions of the used R-libs in the same way it used to be before
      this release). The new install script also supports specifying pkg versions
      via a "versions file".

### Fixed
- Install in R version 4
- "invalid units" error in plots when running on R version 4


## [3.11.0] 2020-09-01
### Added
- Support to new scatt data format
- New "First Guess Map" and "Average First Guess Map" plot types


## [3.10.0] 2020-06-11
### Added
- Support to netatmo stations


## [3.9.0] 2020-03-16
### Added
- metop3 defaults for the GUI's "Satellite" menu
- Explicit libv8.so path info in the .Rprofile file, so that the
  V8 R-lib can be loaded on ECGB after their most recent system update

### Changed
- Add jitter to scatter plots performed over maps, so user can get
  popup info on more layers of data than just the last one plotted
  (for cases when there are overlapping points)
- Install script now requires that eventual pre-compiled binaries are
  compiled with the same major.minor version of R used during install

### Fixed
- Bug that would cause satem map plots to fail with message
  "Error: object 'level' not found"
- Install of V8 library on RHEL8 (updated v8 to v3.0.1)


## [3.8.2] 2019-12-06
### Fixed
- Install of V8 library on debian (updated v8 to v2.3)


## [3.8.1] 2019-11-29
### Changed
- Create cacheDir with default system permissions (instead of specifying 0755)
- Do now stop if cacheDit cannot be written to. Show a warning instead, as obsmon
  can be used without caching.


## [3.8.0] 2019-11-27
### Added
- Some new plot types:
    - "Average First Guess Departure + Bias Correction Map"
    - "Average Bias Correction Map"
    - "Average Observations Map"
- Config file option "configName"
- Progress bar on GUI when querying data files to produce plots
- .Rrofile to help shiny-server to find R-libs

### Changed
- Reduce marker opacities and line widths in interactive Maps Plots for
  better visibility in dense plots
- Some improvements to caching for better UI responsiveness
    - In particular, pause caching while producing plot/multiPlot

### Fixed
- Installation error om Debian caused by R-pkgs vdiffr and freetypeharfbuzz


## [3.7.0] 2019-11-21
### Added
- Interactivity to "Maps" plots
- Interactivity to "Station Diagnostics" plots
- Editable plot elements (titles, axes and legend text and position)
  in interactive plots
- Config file options "appTimeout" and "plotsEnableInteractivity"
- Command-line option "-maxRetriesIfPortBusy"

### Changed
- "Timeseries"-type plots: Hide levels for which all nobs are zero
- Better axes labels in "Station Diagnostics" plots
- Dimensions of figures exported from interactive plots (755x1280 -> 755x1200)

### Removed
- Title of legend from "timeseries" plots

### Fixed
- Installation error caused by R-package lambda.R


## [3.6.0] 2019-10-22
### Added
- Issue a notification and mark experiments as unavailable when they
  are selected in the GUI but the code is unable to read data from them

### Changed
- Observation Usage plot: Change colours, shapes and fills for better
  print results (especially for stations with status="passive", which
  used to be markes in yellow and could be difficult to see sometimes)

### Fixed
- Issue that could cause the code to leave orphan processes behind
  after cancelling plots/multiPlots, which would lead to memory leaks
- Issue that could occasionally cause the code to crash if attempting
  to select an experiment that contained no valid data
- Mixup in latitude/longitude axis labels in map plots


## [3.5.0] 2019-10-16
### Added
- "Station Vertical Profile: FG & Analysis Departure" plot
- Notify caching progress in the GUI
- Use defaults + partial cache data to populate menus while cache is not complete

### Changed
- Show "Pressure (Pa)" or "Height (m)" in plot labels instead of just "Level"
- Vertical profile plots:
    - Now show zero-pressure level at the top
    - Solid lines now pass through mean values instead of through all data points

### Fixed
- Bug in query stub which would cause "Average Analysis Departure Map" plots
  to fail
- Inverted sign in Analysis Increment maps
- Mixup in height/pressure labels in First-guess & departure plots
- Minor fixes to install script

### Removed
- Command line option "--sf-suggests-support" from install script

## [3.4.0] 2019-09-27
### Added
- Support to aeolus observations
- Session timeout capability (new sessionTimeout config file option)
- New "--ignore-build-fail" option in install script

### Changed
- More limit on the max number of processes a session can spawn
- Do not put observations on cache if nobs_total==0

### Fixed
- Reset cache entries if they were created prior to the last-modified
  date of the corresponding source databases
- Update available levels shown in the GUI also as a function of the
  currently selected station(s), when applicable.


## [3.3.2] 2019-06-19
### Added
- "Observation Value" plot

### Changed
- File name for exported interactive plots/multiPlots. Used to be "newplot.png", now it is:
    - obsmon_plot.png, for regular plots
    - obsmon_multiplot_N.png, for the N-th plot inside a multiPlot

### Fixed
- Size of saved figure for interactive plots/multiPlots exported as png
- Overlap between title and graph in interactive multiPlots


## [3.3.1] 2019-05-28
### Added
- Help script to install system dependences on Ubuntu
    - utils/build/install_sys_deps_ubuntu.sh
### Changed
- Name of help script to install system dependencies on RHEL/CentOS
    - utils/build/install_sys_deps_centOS_or_rhel.sh


## [3.3.0] 2019-05-21
### Added
- Option to download doc file for browsers without a working PDF plugin

### Changed
- More informative plot titles
- White background for plots (instead of grey)

### Fixed
- Annoying prompt to download doc file in browsers without a working pdf plugin
- Issue that would prevent station choice from being cleared when changing from plots
  that require station(s) to others that do not, causing query to return empty.
- Issue that would cause station choices to become intermittently unavailable while caching


## [3.2.0] 2019-05-14
### Added
- New config file option: maxAvgQueriesPerProc
    - Possibility to make plots faster by parallelising underlying database queries
- Profile with Rprof when using "--debug" argument

### Fixed
- Issue that prevented error messages produced during plots (if any) from being shown
- Issue that could prevent experiment data from being refreshed if a session (browser
  window/tab) was left open by an user for prolonged periods of time. This was fixed by
  the experiment initialisation changes described below.
- Issue that could cause the code to crash when trying to check whether DTGs selected
  in the UI were cached (issue would appear when cache files were locked by sqlite)

### Changed
- Faster experiment initialisation
    - No longer checking existence of data files. Data file names are generated on
      the fly as needed, and errors are handled if files do not exist.
    - No longer performed asyncronously, given that it is very fast now
- Each new session performs its own experiment initialisation
    - This makes it possible to refresh available data for the configured experiments
      by just refreshing the obsmon window, instead of having to kill and restart the code.
      It is still necessary to kill and restart the code if new experiments are included
      or if experiment paths are changed in the config file.
- Lock UI while updating DTGs or if DTGs unavailable

### Removed
- Options deprecated by changes in experiment initialisation strategy:
    - nRetriesMax
    - initCheckDataExists


## [3.1.1] 2019-04-08
### Changed
- Default paths for cacheDir and config file search


## [3.1.0] 2019-04-05
### Added
- Batch mode

### Fixed
- Issue that would cause cache to end up in incorrect files in some occasions
- Issue that would prevent caching from being triggered in some occasions


## [3.0.0] 2019-03-14
### Added
- User-configured "multiPlots"
    - Produce one or more pre-configured plots at once
    - Similar idea as the earlier "pre-defined plots"
        - But configurable by user via config.toml file
- Interactive plots (ability to zoom/pan/hover, etc)
- Vertical profile plots
- Option to export data tables as txt/csv files
- "Cancel plot" button (appears only after clicking in "Plot")
- Better documentation in PDF format (and accessible from the GUI)
- New config options supported (to be set under the "general" section)
    - initCheckDataExists (true/false, default: false)
      - Assert, at initialisation time, whether or not all data files for all
        experiments exist
    - showCacheOptions (true/false, default: false): to show/hide advanced
      cache-related options
        - Options can also be shown by creating a file named ".obsmon_show_cache_options"
          in the obsmon directory
    - maxExtraParallelProcs: control max number of parallel tasks
        - Can also be set via env var OBSMON_MAX_N_EXTRA_PROCESSES

### Fixed
- Show timezone in dates at the end of plot titles instead of "Z"
- Station labels in maps are now consistent with the ones shown in the UI

### Changed
- Experiment path specification via "path" keyword in config file
    - Old "baseDir" and "experiment" now deprecated and will be ignored
        - Warning issued in this case
- Obsmon can now work even if observations are not cached
    - Auto-discovery cache is now performed only for selected date(s) and cycle(s)
        - Caching starts automatically when a new DTG, experiment or database is selected
        - While cache is not finished, the fields in the GUI are populated with all
          possible values they may have
        - The GUI is refreshed once cache finishes as to only show choices available
          according to the relevant data files
- Some UI redesign
- Show a spinner instead of a progress bar when an output is being prepared
- Allow selection of multiple stations in some plots

### Removed
- Progress bars from standard output


## [2.3.0] 2018-10-26
### Added
- Option to select only standard levels/channels
    - The standard levels are considered to be those read from the
      "obsmon" table in the sql data files.
    - Selecting "standard" is equivalent to selecting "all" in v<=2.2.0.
      See "Fixed missing level values" under [Fixed](#Fixed)
- Git repo info printed in the standard output. This is to make it
  easier to provide users with support.

### Fixed
- Missing levels
    - In some occasions, available data would become impossible to select
      because information about non-standard level values would not be
      added to the cache. This is now fixed.
- Experiment selection is kept when updating caching information in the GUI


## [2.2.0] 2018-09-17
### Added
- "AverageMaps" plot category
    - Average First Guess Departure Map
    - Average Analysis Departure Map
    - Average Analysis Increment Map
- Possibility to start obsmon from any directory (obsmon must be in the PATH)
- Support to command line options in (both in the install script as well as in
  obsmon itself)
- Possibility to install R-packages used by obsmon locally (as part of obsmon
  itself), making it independent of R libraries installed in the system. This
  is useful for packaging.
- Support to offline installation
- Support to using pre-compiled R libraries
- Progress of caching process shown in the GUI

### Changed
- Auto-discovery cache is now sqlite-based (Fixes: #148)
- Auto-discovery cache is also done asynchronously now. This means that, if
  multiple experiments are being dealt with, the auto-discovery caching for
  all of them can be performed simultaneously. Any experiment can now be
  selected as soon as it is ready, without the need to wait for the others
  to finish caching.
- Info about installtion moved to INSTALL.md file
- Automatic identification of imported R libraries during install
    - R dependencies recursively determined prior to installation
- obsmon and install executables are now R scripts (used to be bash)

### Fixed
- Bias correction and First Guess Departure+Bias Correction maps are back
- Look for an alternative TCP port if cannot use the one initially chosen
- Some issues dealing with unavailable data


## [2.1.0] - 2017-10-20
### Added
- Date selection for single times
- Cycle selection for date ranges
- Plottype filtering offers only plots valid for selected criteria
- Auto-discovery of obtypes and stations (cached)
- Progress bar for long operations
- Plot registry facilitates adding new plots/plot maintenance

### Changed
- UI retains user choices
- Streamlined UI
- Window resizing supported
- Surface diagnostics moved to more general station diagnostics plottype
- Plottypes split in categories
- Data table interface improved
- Color handling improved
- Cairo based plotting improves plot quality
- TOML based config.toml replaces hardcoded and environment configuration
- Only install missing packages in install.R
- Logging via futile.logger replaces print statements
- Database handling now via object oriented interface
- Plotting via object oriented interface

### Removed
- Pre-defined plots
- Dump database
- Settings tab
- Environment variables for configuration
