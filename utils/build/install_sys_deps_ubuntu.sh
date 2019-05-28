#!/usr/bin/env bash

pkgs_append () { PKGS="${PKGS} ${1}" ; }

pkgs_append git
pkgs_append r-base

# For R-lib "curl"
pkgs_append libcurl4-openssl-dev
# For R-lib "openssl"
pkgs_append libssl-dev
# For R-lib "xml2"
pkgs_append libxml2-dev

# For R-lib "RMariaDB"
if [[ `lsb_release -rs` < "16.04" ]]
then
   pkgs_append libmariadbclient-dev
else
   pkgs_append libmariadb-client-lgpl-dev
fi

# For R-lib "RPostgreSQL"
pkgs_append libpq-dev
# For R-lib "gdtools"
pkgs_append libcairo2-dev
# For R-lib "rgeos"
pkgs_append libgeos++-dev
# For R-lib "rgdal"
pkgs_append libgdal-dev
# For R-lib "Cairo"
pkgs_append libxt-dev
# For R-lib "V8"
pkgs_append libv8-3.14-dev

sudo apt-get install -y $PKGS
