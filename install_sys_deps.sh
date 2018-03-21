#!/usr/bin/env bash

WORKING_DIR=`pwd`
TMPDIR='/tmp/obsmon_sys_deps'
RPM_DEST_DIR="${WORKING_DIR}/obsmon_deps_RHEL7"
FC25_REPO='http://dl.fedoraproject.org/pub/fedora/linux/releases/25/Everything/source/tree/Packages'
REPO_LIBKML='http://cbs.centos.org/kojifiles/packages/libkml/1.3.0/3.el7/x86_64'

pkgs_append () {
  PKGS="${PKGS} ${1}"
}
yum_pkgs_append () {
  PKGS_YUM="${PKGS_YUM} ${1}"
}

# epel-release is needed before checking the other dependencies
sudo yum -y install "epel-release" || { echo "Problems installing epel-release"; exit 1; }
# ${PKGS_YUM}: Pkgs that can be directly installed via yum
#              Python3* needed to be installed manually on CentOS7 (like we do 
#              here for libkml), but can be installed via yum on SMHI/LINDA
yum_pkgs_append "R"
yum_pkgs_append "gcc make cmake gcc-c++ wget"
yum_pkgs_append "rpm-build redhat-rpm-config"
yum_pkgs_append "cairo-devel libXt-devel"
yum_pkgs_append "libcurl-devel openssl-devel libxml2-devel"
yum_pkgs_append "mariadb-devel postgresql-devel"
yum_pkgs_append "geos-devel udunits2-devel v8-devel v8-314-devel"
yum_pkgs_append "python3-devel python3-numpy"
sudo yum -y install ${PKGS_YUM} || { echo "Problems installing ${PKGS_YUM}"; exit 1; }
# ${PKGS}: Packages that need a bit more work
#          NB.: The order of the pkgs listed below DOES matter.
pkgs_append "libkml-1.3.0-3.el7.x86_64.rpm"
pkgs_append "libkml-devel-1.3.0-3.el7.x86_64.rpm"
pkgs_append "proj-4.9.2-2.fc24.src.rpm"
pkgs_append "libgeotiff-1.4.0-7.fc24.src.rpm"
pkgs_append "libspatialite-4.3.0a-2.fc24.src.rpm"
pkgs_append "ogdi-3.2.0-0.26.beta2.fc24.src.rpm"
pkgs_append "gdal-2.1.0-8.fc25.src.rpm"

# Preparing for rpmbuild
if [ ! -f "${HOME}/.rpmmacros" ]; then
  mkdir -p ${HOME}/rpmbuild/{BUILD,RPMS,SOURCES,SPECS,SRPMS}
  echo '%_topdir %(echo $HOME)/rpmbuild' > ${HOME}/.rpmmacros
fi
RPMBUILD_DIR=`rpmbuild --eval='%_topdir'`
SPECS_DIR=`rpmbuild --eval='%_specdir'`
COMPILED_RPMS_SYS_DIR=`rpmbuild --eval='%_rpmdir/%{_arch}'`

# Producing/installing RPMs
mkdir -p ${TMPDIR}
for PKG in ${PKGS}; do
  echo "Handling package ${PKG}" 

  if [[ ! $PKG =~ \.rpm$ ]]; then
    # If not an *.rpm, assume to be a package name installable via yum
    sudo yum -y install ${PKG} || { echo "Problems installing ${PKG}"; exit 1; }
    continue
  fi

  PKG_NAME=$(echo "$PKG" | sed 's/-[0-9].*//')
  PKG_VER=$(echo "$PKG" | sed "s/${PKG_NAME}-//" | sed 's/\.\w*\.src\.rpm$//')
  PKG_FIRST_LETTER=${PKG:0:1}
  PKG_RPM_DESTDIR="${RPM_DEST_DIR}/${PKG_NAME}"
  if [[ $PKG_NAME = *"libkml"* ]]; then
    REPO=${REPO_LIBKML}
  else
    REPO=${FC25_REPO}/${PKG_FIRST_LETTER}
  fi

  if [ ! -d ${PKG_RPM_DESTDIR} ]; then
    # Getting and/or rebuilding needed RPMs
    cd ${TMPDIR}
    [ -f "${PKG}" ] || wget ${REPO}/${PKG}

    if [[ $PKG =~ \.src\.rpm$ ]]; then
      echo "Rebuilding RPM ${PKG}"

      rpm --nomd5 -i ${PKG} || { echo "Problems unpacking ${PKG}"; exit 1; }

      SPEC_FILE=${SPECS_DIR}/${PKG_NAME}.spec
      if [[ $PKG_NAME = *"gdal"* ]] && ( rpm -qa | grep -qw 'bash-completion' ); then
        GDAL_BC_FILE_PATH='%{_exec_prefix}/%{_sysconfdir}/bash_completion.d/gdal-bash-completion.sh'
        sed -i "/^%files$/a ${GDAL_BC_FILE_PATH}" ${SPEC_FILE}
        if [ $? -ne 0 ]; then
          echo "Problems editing ${PKG_NAME} spec file."
          exit 1
        fi
      fi
      REQUIREMENTS=`rpmspec -q --buildrequires --requires ${SPEC_FILE}`
      [ -n "$REQUIREMENTS" ] && sudo yum -y install ${REQUIREMENTS}

      rm -rf ${COMPILED_RPMS_SYS_DIR}/${PKG_NAME}*-${PKG_VER}*
      rpmbuild -ba ${SPEC_FILE} || { echo "Problems building ${PKG}"; exit 1; }

      cd ${COMPILED_RPMS_SYS_DIR}
    fi

    mkdir -p ${PKG_RPM_DESTDIR}
    cp ${PKG_NAME}*-${PKG_VER}* ${PKG_RPM_DESTDIR}
  fi

  cd ${PKG_RPM_DESTDIR}
  sudo yum localinstall -y ${PKG_NAME}*-${PKG_VER}*
  if [ $? -eq 0 ]; then
    echo "Done with ${PKG}"
    echo ''
  else
    echo "Problems installing ${PKG_NAME}."
    echo "You may want to remove the dir ${PKG_RPM_DESTDIR} and try again."
    exit 1
  fi

done

cd ${WORKING_DIR}
