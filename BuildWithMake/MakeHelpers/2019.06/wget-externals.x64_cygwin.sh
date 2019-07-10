#!/bin/bash

# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

export SV_EXTERNALS_VERSION_NUMBER=2019.06

if [ -z "$SV_EXTERNALS_RELEASE_DATE" ]; then
  export SV_EXTERNALS_RELEASE_DATE=2019.07.05
fi

export SV_COMPILER_VERSION=19.16
export CXX_COMPILER_VERSION=msvc-19.16
# compiler must now be present on the cmd line
export EXTERNALS_PARENT_URL=http://simvascular.stanford.edu/downloads/public/simvascular/externals/2019.06/windows/windows/10.0/msvc/19.16/x64/release/$SV_EXTERNALS_RELEASE_DATE
export EXTERNALS_TAR_FILE_PREFIX=windows.10.0.msvc.19.16.x64.release.$SV_EXTERNALS_RELEASE_DATE

export EXTERNALS_TOP=`pwd`/ext

# default is windows

echo "CLUSTER=x64_cygwin" > cluster_overrides.mk
echo "SV_COMPILER=msvc" >> cluster_overrides.mk
echo "SV_COMPILER_VERSION=$SV_COMPILER_VERSION" >> cluster_overrides.mk
echo "CXX_COMPILER_VERSION=$CXX_COMPILER_VERSION" >> cluster_overrides.mk
echo "FORTRAN_COMPILER_VERSION=ifort" >> cluster_overrides.mk

echo "SV_EXTERNALS_VERSION_NUMBER=$SV_EXTERNALS_VERSION_NUMBER" >> global_overrides.mk
echo "SV_EXTERNALS_RELEASE_DATE=$SV_EXTERNALS_RELEASE_DATE" >> global_overrides.mk
echo "OPEN_SOFTWARE_BINARIES_TOPLEVEL=`cygpath -m $EXTERNALS_TOP/bin`" >> global_overrides.mk

mkdir -p $EXTERNALS_TOP
chmod -R a+rwx $EXTERNALS_TOP
mkdir -p $EXTERNALS_TOP/tarfiles
mkdir -p $EXTERNALS_TOP/bin

pushd $EXTERNALS_TOP/tarfiles

  wget $EXTERNALS_PARENT_URL/$EXTERNALS_TAR_FILE_PREFIX.freetype.2.6.3.tar.gz
  wget $EXTERNALS_PARENT_URL/$EXTERNALS_TAR_FILE_PREFIX.gdcm.2.6.3.tar.gz
  wget $EXTERNALS_PARENT_URL/$EXTERNALS_TAR_FILE_PREFIX.hdf5.1.10.1.tar.gz
  wget $EXTERNALS_PARENT_URL/$EXTERNALS_TAR_FILE_PREFIX.itk.4.13.2.tar.gz
  wget $EXTERNALS_PARENT_URL/$EXTERNALS_TAR_FILE_PREFIX.mitk.2018.04.2.tar.gz
  wget $EXTERNALS_PARENT_URL/$EXTERNALS_TAR_FILE_PREFIX.mmg.5.3.9.tar.gz
  wget $EXTERNALS_PARENT_URL/$EXTERNALS_TAR_FILE_PREFIX.opencascade.7.3.0.tar.gz
  wget $EXTERNALS_PARENT_URL/$EXTERNALS_TAR_FILE_PREFIX.python.3.5.5.tar.gz
  wget $EXTERNALS_PARENT_URL/$EXTERNALS_TAR_FILE_PREFIX.qt.5.11.3.tar.gz
  wget $EXTERNALS_PARENT_URL/$EXTERNALS_TAR_FILE_PREFIX.tcltk.8.6.4.tar.gz
  wget $EXTERNALS_PARENT_URL/$EXTERNALS_TAR_FILE_PREFIX.tinyxml2.6.2.0.tar.gz
  wget $EXTERNALS_PARENT_URL/$EXTERNALS_TAR_FILE_PREFIX.vtk.8.1.1.tar.gz

popd

pushd $EXTERNALS_TOP/bin
for i in $EXTERNALS_TOP/tarfiles/$EXTERNALS_TAR_FILE_PREFIX.*.tar.gz; do
    echo "untar $i"
    tar xzf $i
done
popd

