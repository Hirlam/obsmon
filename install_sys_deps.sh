#!/usr/bin/env bash

WORKING_DIR=`pwd`
RPMBUILD_DIR="${HOME}/rpmbuild"
TMPDIR='/tmp/obsmon_sys_deps'
RPM_DEST_DIR="${WORKING_DIR}/obsmon_deps_RHEL7"
REPO='http://dl.fedoraproject.org/pub/fedora/linux/releases/25/Everything/source/tree/Packages'
REPO_LIBKML='http://cbs.centos.org/kojifiles/packages/libkml/1.3.0/3.el7/x86_64'

PKGS="${PKGS} proj-4.9.2-2.fc24.src.rpm"
PKGS="${PKGS} libgeotiff-1.4.0-7.fc24.src.rpm"
PKGS="${PKGS} libspatialite-4.3.0a-2.fc24.src.rpm"
PKGS="${PKGS} ogdi-3.2.0-0.26.beta2.fc24.src.rpm"
PKGS="${PKGS} gdal-2.1.0-8.fc25.src.rpm"
LIBKML_PKGS='libkml-1.3.0-3.el7.x86_64.rpm libkml-devel-1.3.0-3.el7.x86_64.rpm'
PKG_COUNTER=0


PKG_COUNTER=$[$PKG_COUNTER +1]
PKG_RPM_DESTDIR="${RPM_DEST_DIR}/${PKG_COUNTER}_libkml"
mkdir -p ${PKG_RPM_DESTDIR}
cd ${PKG_RPM_DESTDIR}
for PKG in ${LIBKML_PKGS}; do
  if [ ! -f "${PKG}" ]; then
    wget ${REPO_LIBKML}/${PKG}
  fi
done

mkdir -p ${TMPDIR}
for PKG in ${PKGS}; do  
 
  PKG_COUNTER=$[$PKG_COUNTER +1]
  PKG_FIRST_LETTER=${PKG:0:1}
  PKG_NAME=$(echo "$PKG" | sed 's/-[0-9].*//')
  PKG_VER=$(echo "$PKG" | sed "s/${PKG_NAME}-//" | sed 's/\.\w*\.src\.rpm$//')
  PKG_RPM_DESTDIR="${RPM_DEST_DIR}/${PKG_COUNTER}_${PKG_NAME}"

  if [ -d ${PKG_RPM_DESTDIR} ]; then
    echo "Dir ${PKG_RPM_DESTDIR} already exists."
    echo "  > Skipping package ${PKG}."
    continue
  fi

  cd ${TMPDIR}
  if [ ! -f "${PKG}" ]; then
    wget ${REPO}/${PKG_FIRST_LETTER}/${PKG}
  fi

  rpm --nomd5 -i ${PKG}
  if [ $? -eq 0 ]; then
    echo 'Done unpacking RPM'
    echo ''
  else
    exit 1
  fi

  cd ${RPMBUILD_DIR}/SPECS
  if [[ $PKG_NAME = *"gdal"* ]]; then
    sed -i '/%files/a %{_exec_prefix}/etc/bash_completion.d/gdal-bash-completion.sh' ${PKG_NAME}.spec
    if [ $? -ne 0 ]; then
      exit 1
    fi
  fi
  REQUIREMENTS=`rpmspec -q --buildrequires --requires ${PKG_NAME}.spec`
  if [ -n "$REQUIREMENTS" ]; then
    sudo yum -y install ${REQUIREMENTS}
  fi

  rm -rf ${RPMBUILD_DIR}/RPMS/x86_64/${PKG_NAME}*-${PKG_VER}*
  rpmbuild -ba ${PKG_NAME}.spec
  if [ $? -eq 0 ]; then
    echo 'Done building RPM'
    echo ''
  else
    exit 1
  fi

  cd ${RPMBUILD_DIR}/RPMS/x86_64
  mkdir -p ${PKG_RPM_DESTDIR}

  cp ${PKG_NAME}*-${PKG_VER}* ${PKG_RPM_DESTDIR}
  cd ${PKG_RPM_DESTDIR}
  sudo yum install -y ${PKG_NAME}*-${PKG_VER}*

done

cd ${WORKING_DIR}
