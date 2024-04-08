[Frequently asked questions](@id faq) 
==========================

[Questions about installation](@id questions-about-installation) 
----------------------------

-   Do I need root (admin) permissions to install obsmon?\
    \
    A: In general yes (but not at ECMWF). In most cases, you should at
    least be able to run your system's package installer using `sudo`
    (e.g., `sudo yum` or `sudo apt-get`), so you can install the system
    dependencies.

-   Installation is taking too long. Is there any way to speed it up?\
    \
    A: Generally not, unfortunately. Many of the required `R` libraries
    need to be compiled and they, in turn, generally require extra
    system packages to be installed. *If you plan to install obsmon
    multiple times, however, please read
    [3.5.2](@ref install-precompiled)*. Note also that if
    installation is restarted after failure or interruption it will
    resume from where it stopped, not start from scratch.

-   I want to install a different version of R-lib `X` instead of the
    one that the `install` script is using. How do I do this?\
    \
    A: See [3.5](@ref install-advanced).

-   My installation fails and I cannot see why. What do I do?\
    \
    A: Please run "`./install –live`". This will print all installation
    messages in real time.

[Questions about caching](@id questions-about-caching) 
-----------------------

-   The popup info in the some UI menus says that cache is ongoing. What
    does that mean and what should I do?\
    \
    A: That just means that obsmon has not yet cached all information it
    needs about the selected experiment/database/DTG(s) in order to
    accurately populate the menus' choices. The presented values may
    change in such cases. Whenever applicable, you will be offered
    choices that combine some defaults and whatever has already been
    cached. You normally do not need to do anything: the message will
    disappear as soon as caching is finished. *You can continue to use
    obsmon even if this message is shown*.

[Questions about plotting](@id questions-about-plotting)
------------------------

-   My plot says "Query returned no data\". What does that mean?\
    \
    A: That means that your experiment does not contain the data needed
    for the plot according to the parameters you chose. This should be a
    rare occurrence when cache finishes normally, but can happen more
    often when caching is not available or incomplete.

-   My plot says "Could not produce plot: The required data file(s)
    might be inaccessible.\". What does that mean?\
    \
    A: That may indicate that the required experiment data files are not
    available. Please double-check that they exist. Contact us if they
    do.

-   My plot is empty. What happened?\
    \
    A: The most probably cause is that data associated with your plot
    request could indeed be found, but the number of observations is
    zero. Please take a look at the data under the "Query and Data\"
    tab.

-   How do I save my plot as a figure?\
    \
    A: If the plot is interactive (e.g., if you can zoom in), then you
    will find such an option in the menu that appears on the top right
    of the figure when you place the mouse over it. If the plot is not
    interactive, you can just right-click with your mouse and choose to
    save it.

-   How can I save the data used to produce my plot?\
    \
    A: Go to the "Query and Data\" tab and click in the appropriate
    button to export the data as `txt` or `csv`.

[Questions about `multiPlots`](@id questions-about-multiplots) 
----------------------------

-   I do not see a `multiPlots` tab in the GUI, even though I have
    configured a `multiPlot`. What happened?\
    \
    A: This indicates that there may be an error in the configuration of
    your `multiPlots`. Obsmon will only feature a `multiPlots` tab if at
    least one valid `multiPlot` configuration is found.

-   I do not see one (or more) of my `multiPlots` in the list. What
    happened?\
    \
    A: This most likely means that there is an error in the
    configuration of the missing `multiPlots`. Obsmon will only show the
    `multiPlots` for which the configuration passed without errors.

-   My `multiPlot` is taking too long to finish. What is going on?\
    \
    A: How long it takes for a `multiPlot` to be completed depends on
    factors such as how much you have chosen to include or exclude from
    the plots (see [5.4](@ref multiplots)), as well as where the required data
    files are located (faster if stored locally, slower if stored
    remotely). If you specify a date range, then longer time spans will
    naturally imply longer processing times for the `multiPlots`, as
    each new DTG corresponds to a new file to query data from.
    Additionally, some plots, such as "Station Diagnostics\", require
    some statistics to be performed on the data before it can be
    plotted. This can also increase processing times.

-   My `multiPlots` are not interactive. What happened?\
    \
    A: Interactivity is switched off by default in the case of
    `multiPlots`. It can be switched on using the
    `multiPlotsEnableInteractivity` config file option. See
    [4.1](@ref config-file-general)

