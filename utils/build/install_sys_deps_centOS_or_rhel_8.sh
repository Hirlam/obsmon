#!/usr/bin/env bash
# Intended for use in CentOS/RHEL 8.

# Parsing command line arguments
LIST_MISSING_PKGS=false # List missing pkgs and exit?
while [[ $# -gt 0 ]]; do
  key="$1"
  case $key in
    -lm|--list-missing)
      LIST_MISSING_PKGS=true
      shift # past argument
    ;;
    *)    # unknown option
      shift # past argument
    ;;
  esac
done

WORKING_DIR=`pwd`

# ${PKGS_YUM}: Pkgs to be installed via yum
yum_pkgs_append () { PKGS_YUM="${PKGS_YUM} ${1}" ; }

# Needed to build
yum_pkgs_append "R"
yum_pkgs_append "gcc make cmake gcc-c++ wget"
# Needed by R packages
yum_pkgs_append "cairo-devel libXt-devel"
yum_pkgs_append "libcurl-devel openssl-devel libxml2-devel"
yum_pkgs_append "mariadb-devel postgresql-devel"
yum_pkgs_append "v8-devel"
yum_pkgs_append "geos-devel"
yum_pkgs_append "libkml libkml-devel"
yum_pkgs_append "proj52 proj52-devel proj52-epsg"
yum_pkgs_append "libgeotiff"
yum_pkgs_append "ogdi"
yum_pkgs_append "udunits2"
yum_pkgs_append "python3-devel python3-numpy"
yum_pkgs_append "gdal gdal-devel"

if [ "${LIST_MISSING_PKGS}" = true ]; then
  YUM_INSTALLED=`yum list installed`
  for PKG in ${PKGS_YUM} ${PKGS}; do
    if [[ $YUM_INSTALLED != *"${PKG}"* ]]; then 
      echo "${PKG}"
    fi
  done
  exit 0
fi

# Installing packages that do not require further medling
sudo yum -y install ${PKGS_YUM} || { echo "Problems installing ${PKGS_YUM}"; exit 1; }

if [ -z "${PKGS}" ]; then
  echo "Finished."
  exit 0
fi

cd ${WORKING_DIR}
