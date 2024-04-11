# [Installing, executing and updating](@id install)

Use the `install` script to install the `R` libraries needed. The main
system requirements are:

- A Linux operating system
- A working `R (>= 4.0.5)` interpreter

- `python (>= 3.6.7)`

- Internet connection

Instructions for the various installation modes are given in
[install-standalone](@ref install-standalone) [install-server](@ref install-server) 
goes through the recommended steps to update the code. The required
system packages are listed in [3.4](@ref sysdeps), and the paths where obsmon looks for installed
`R` libraries are listed in [3.3](@ref libPaths). Finally, the information presented in
[3.5.2](@ref install-precompiled) about the use of pre-compiled
binaries for the `R` libraries may save you a significant amount of time
if you wish to install obsmon in multiple identical computers.

## Installing and executing

### [Standalone mode](@id install-standalone)

1. Go to the `obsmon` directory and execute:[^2]

   ```bash
   ./install
   ```

   - If required system packages are missing, then the installation
     will stop. Install the relevant system package(s) and execute
     `./install` again. [Obsmon has helper scripts to install system
     dependencies in some Linux distributions]{.underline} -- see
     [3.4](@ref sysdeps) for more details.

   - You may need to customise installation for some R-packages
     (non-standard paths for system libraries, for instance). In such
     cases, please use the `install` script's `-ca` and/or `-cv`
     arguments. These options are, respectively, passed to the
     `configure.args` and `configure.vars` arguments of R's
     [`install.packages` function](https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/install.packages).

2. Create a `config.toml`. You can find a detailed discussion about the configuration file in [4](@ref config-file). Also, take a look at the template config file `docs/example/config.toml`.

3. To run obsmon, just execute[^3]

   ```bash
   ./obsmon 
   ```

    The output will show something similar to

    ```bash
    Listening on http://127.0.0.1:5391
    ```

    Point your browser to the address you see in your output.[^4]

4. Alternatively, if you want the browser to open automatically, you can run obsmon as

   ```bash
   ./obsmon --launch
   ```

Finally, for a list of command line options currently supported by
obsmon in standalone mode, please run:
   
```bash
./obsmon -h
```

### [Through a Shiny Server](@id install-server) 

If you want to offer obsmon to a larger number of users, you may want to deploy it using a web server. A canonical choice in this case would be to use a [Shiny Server](https://www.rstudio.com/products/shiny). Shiny Server installation is beyond the scope of this document.In the following, we assume that the Shiny Server is already installed and running.

1. Put the obsmon directory into the `site_dir` directory as configured in your `shiny-server.conf`, or, alternatively, add a `location`   stance to your `shiny-server.conf` listing the obsmon directory.

   - You may want to configure in your `shiny-server.conf` the location of your log files. If you do not do so, they will most likely end up somewhere such as `/var/log/shiny-server/obsmon-*.log`, although this cannot be  guaranteed

2. Install the required `R` libraries. We recommend that you use the included `install` script with the default settings.

   - If you don't follow the recommended install approach, then we *highly recommend* that you manually specify the paths to the directories where you installed the `R` libraries. You can do this by using the `.libPaths` (`R`-language) command inside an `.Rprofile` file placed in the obsmon directory. otherwise, you may have issues with conflicting `R` libraries between obsmon and the shiny server.

3. Put a valid `config.toml` file inside either the obsmon directory or at `/etc/obsmon`. See the file `docs/example/config.toml` for a template. See also [4](@ref config-file).

4. Run your Shiny Server and connect to it with your browser.

### [At ECMWF (Atos Bologna)](@id install-ecmwf-atos)

To make your own installation of Obsmon:
Follow the instructions given ini [3.1.1](@ref install-standalone).

The compilation requires the following modules to provide system dependencies:

```bash
module load R
module load gdal
module load gproj
module load geos
```

Tested with current (April 2024) defaults: `R/4.2.2 geos/3.11.1 proj/9.1.1 gdal/3.6.2`.

Atos HPC currently does not provide a browser to connect to Shinny applications. However, for those using Virtual Desktop Infrastructure (VDI) provided by ECMWF, one option is to use SOCKS port forwarding from the Linux Virtual Desktop:

1. Configure proxy to use SOCKS port-forwarding.

   Open the Firefox browser on DVI virtual machine, and go to Proxy settings (`Settings -> Network Settings -> Settings`). Choose Manual Proxy Configuration and SOCKSv4. Use SOCKS Host: `127.0.0.1` and Port: `5000`.

2. Connect to HPC and run Obsmon.

   ```bash
   ssh -D 5000 your_username@hpc-login
   ```

3. Go to your obsmon installation and execute Obsmon:

   ```bash
   module load R
   ./obsmon
   ```

4. Connect to Shinny application through browser

In Firefox, navigate to `http://0.0.0.0:5391/`
where 5391 is the default port used by Obsmon (please change if your Obsmon application is using another port).

### [At SMHI](@id install-smhi)

You can follow the instructions given in
[3.1.1](@ref install-standalone), but you can substantially reduce
installation time by passing an appropriate value to the
`-bin-repo-path` option of the `install` script (to make use of
pre-compiled binaries; more on this on
[3.5.2](@ref install-precompiled). Please take a look at our
internal Obsmon wiki page for more details. If you prefer, we can also
produce a package that you can install using our package manager.

## [Updating](@id install-update)

Please run:

```bash
git pull -rebase && ./install
```

This assumes that:

- You have not modified obsmon

- You have followed the recommended installation approach when first installing the code.

Updating is normally faster than performing a completely new install, as obsmon tries to use pre-compiled binaries generated during the previous install/update (see [3.5.2](@ref install-precompiled)). However, you may occasionally need to install new system dependencies when you update obsmon (see
[3.4](@ref sysdeps)). **N.B.**: You should always update the code after having switched
branches.

## [R library search paths](@id libPaths)

Obsmon looks for R libraries installed in the following paths (listed in
order of priority):

1. `.installer_local_R-libs/R-libs`

2. The default R library search paths. These vary depending on your system.[^5]

If you wish for obsmon to look somewhere else for R-libs, then you can, *e.g.*, edit the [`.libPaths`](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/libPaths) call in the `.Rprofile` file located inside the obsmon root directory.

## [System dependencies](@id sysdeps)

### CentOS, RHEL or Ubuntu

list the system dependencies for the R libraries used in obsmon under
CentOS/RHEL and Ubuntu, respectively. You can find [helper
scripts]{.underline} under `utils/build` to install system dependencies
in these Linux distributions. Please feel free to notify us if any
dependencies are missing. But please mind that these scripts are only
[meant to guide you in your install process]{.underline}. We will gladly
try to help you with this should you face problems, but [we can't
guarantee that we will be able to do this for every system]{.underline}.
If possible, [please refer to your IT department first for problems
regarding system dependencies]{.underline}.

```bash
   Required system package   First R lib that asks for it during install  
  ------------------------- --------------------------------------------- --
         libXt-devel                            Cairo                     
         cairo-devel                            Cairo                     
        libcurl-devel                 DBI (via curl dependency)           
        openssl-devel               DBI (via openssl dependency)          
        libxml2-devel                 DBI (via xml2 dependency)           
        mariadb-devel             dbplyr (via RMariaDB dependency)        
         geos-devel                leaflet (via rgdal dependency)         
         proj-devel                leaflet (via rgdal dependency)         
          proj-epsg                leaflet (via rgdal dependency)         
         gdal-devel                leaflet (via rgdal dependency)         
          v8-devel                   shinyjs (via V8 dependency)          
```

**CentOS 7/RHEL 7** system dependencies for obsmon. The list was
  produced by installing the code on a newly installed, minimal system
  and keeping note of the packages required during the process. Many
  other system packages are installed as dependencies of those listed
  here and have been omitted. Last updated on 2018-03-22.

### [sysdeps-ubuntu](@id sysdeps-ubuntu)

```bash
    Required system package     First R lib that asks for it during install  
  ---------------------------- --------------------------------------------- --
      libcurl4-openssl-dev                         curl                      
           libssl-dev                             openssl                    
          libxml2-dev                              xml2                      
   libmariadb-client-lgpl-dev                    RMariaDB                    
         libcairo2-dev                            gdtools                    
         libgeos++-dev                             rgeos                     
          libgdal-dev                              rgdal                     
           libxt-dev                               Cairo                     
         libv8-3.14-dev                             V8                       
```

**Ubuntu 18.04.2 LTS** system dependencies for obsmon. The list was produced by installing the code on a newly installed system and  keeping note of the packages required during the process. Many other  system packages are installed as dependencies of those listed here and  have been omitted. Last updated on 2019-05-28.

### Other Linux distributions

There is currently no robust way to programmatically determine the system dependencies for all Linux distributions. The recommended approach at the moment is to proceed as indicated in [install-standalone](@ref install-standalone), [install-server](@ref install-server) and install the system dependencies as they are requested during installation. You can nevertheless use the names of the Linux packages shown in [sysdeps](@ref sysdeps), [sysdeps-ubuntu](@ref sysdeps-ubuntu) as reference, as, although the names change from one distribution to another, they are generally similar.

## [Advanced install options](@id install-advanced) 

### Changing the versions of the installed `R` libraries

The provided `install` script uses metadata stored in the file
`.installer_local_pkg_repo/src/contrib/PACKAGES` to solve R-package
dependencies as well as determine which versions of them will be
installed. We use this to ensure that the installs are reproducible,
which greatly simplifies support and maintenance. There may be cases,
however, where one may want/need to install package versions that are
different from those in the aforementioned `PACKAGES` file. This can be
accomplished by using either of the following options:

a. `pkg_versions_control:cran_latest`
   Running  `./install -repos=cran` (instead of just `./install`)

   This instructs the install script to ignore the metadata in the
   `.installer_local_pkg_repo/src/contrib/PACKAGES` file and, instead,
   use CRAN to determine the package versions and dependencies. This is
   the best option if you want to install the latest versions of all
   packages.

b. `pkg_versions_control:versions_file` Creating an `.installer_pkg_versions.txt` file

   This allows you to handpick the versions of any number of used
   R-packages. A typical usage case would be to select specific
   versions of a few packages. Packages not listed in the file will
   have their versions determined by the default method. You can create
   a template file by running "`./install listdeps -lock`" and then
   edit it (keeping the same format).

   **N.B.:** Changing the versions of only selected R-packages can
   cause some dependencies to be broken. Use this option with caution.

It is also possible to combine these options, which may be handy in some
cases. Mind, however, that using R-libraries with versions different
from the ones originally configured in obsmon can make it more difficult
for us to provide you with support, should you need it.

### [Using pre-compiled binaries for the `R` libraries](@id install-precompiled) 

When you use the `install` script to install the `R` libraries needed by
obsmon, it saves the binaries for all successfully compiled `R`
libraries. Conversely, before attempting to compile any `R` library, the
`install` script looks for appropriate binaries it may have previously
compiled.

This mechanism allows installation to resume from where it stopped in
case it fails or is interrupted. Another (perhaps not so obvious)
advantage is that it can lead to a very significant reduction in the
installation times whenever:

1. You need to update obsmon

2. You need to install the code multiple times in the same computer
    (e.g., for multiple users in an independent way)

3. You need to install obsmon in multiple identical computers

For updates, you just need to follow the recommended update approach
([3.2](@ref install-update)). For the other cases, you only need to
run the `install` script from scratch once. Then, for subsequent
installs, you may (i) copy the appropriate pre-compiled libraries
directory created during the first `install` run to some accessible
location in your computer(s), and (ii) pass the path to this directory
in the next `install` script run via the `-bin-repo-path` command line
argument. By doing so, no libraries will be unnecessarily compiled.

[^2]: Please run `./install -h` for more help about the install script.

[^3]: You can also put a symlink to the obsmon executable somewhere in
    your `PATH`. If you do so, you will be able to run obsmon as a
    command from any directory.

[^4]: This only works if the browser is running on the same machine as
    obsmon. Otherwise you will need to use something like [SSH local
    port
    forwarding](https://www.ssh.com/ssh/tunneling/example#sec-Local-Forwarding)
    (e.g., `ssh -L 5391:localhost:5391 user@computer`) or, less
    preferably, X forwarding to connect to the server.
[^5]: You can check these out by using, for instance, the R function
    [`.libPaths()`](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/libPaths).
