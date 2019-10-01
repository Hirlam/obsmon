# Readme

Utility program to identify, download and install R-libraries
needed by program written in R.

To use this code, simply create a symlink to the r_deps_installer
file inside the directory you want to be scanned and then run the
script. The program will then:
    (1) Perform a recursive scan of the directory tree and locate
        files containing R code
    (2) Scan these files to identify the R-libraries they use
    (3) Determine the recursive dependencies of such libraries
    (4) Create, if not previously done, a local CRAN-like repo
        containing the source codes of all detected libraries
        and dependencies (see option --allow-downloads)
    (5) Compile and install such packages from the sources saved
        in the local repo

The only requirement of this program is that an R interpreter is
installed in the system. It purposely does not depend on any other
R-package: The whole idea is that one should not need to install
packages to be able to run a package installer.
