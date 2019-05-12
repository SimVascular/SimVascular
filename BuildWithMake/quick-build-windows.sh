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

#export SV_EXTERNALS_VERSION_NUMBER=2018.01
#export SV_EXTERNALS_VERSION_NUMBER=2018.05
export SV_EXTERNALS_VERSION_NUMBER=2019.02

if [ $SV_EXTERNALS_VERSION_NUMBER == '2018.01' ]; then
  source CygwinHelpers/msvc_2013_x64
  export PARENT_URL=http://simvascular.stanford.edu/downloads/public/simvascular/externals/2018.01/windows/windows/10.0/msvc/18.0/x64/relwithdebinfo/2018.01.24
  export TAR_FILE_PREFIX=windows.10.0.msvc.18.0.x64.relwithdebinfo.2018.01.24
elif [ $SV_EXTERNALS_VERSION_NUMBER == '2018.05' ]; then
  source CygwinHelpers/msvc_2015_x64.sh
  export PARENT_URL=http://simvascular.stanford.edu/downloads/public/simvascular/externals/2018.05/windows/windows/10.0/msvc/19.0/x64/release/2019.02.15
  export TAR_FILE_PREFIX=windows.10.0.msvc.19.0.x64.release.2019.02.15
elif [ $SV_EXTERNALS_VERSION_NUMBER == '2019.02' ]; then
  source CygwinHelpers/msvc_2015_x64.sh
  export PARENT_URL=http://simvascular.stanford.edu/downloads/public/simvascular/externals/2019.02/windows/windows/10.0/msvc/19.0/x64/release/2019.03.06
  export TAR_FILE_PREFIX=windows.10.0.msvc.19.0.x64.release.2019.03.06
fi

export EXTERNALS_TOP=`pwd`/ext


# default is windows

echo "OPEN_SOFTWARE_BINARIES_TOPLEVEL=`cygpath -m $EXTERNALS_TOP/bin`" >> global_overrides.mk
echo "SV_EXTERNALS_PREBUILT_QT=0" >> global_overrides.mk

mkdir -p $EXTERNALS_TOP
chmod -R a+rwx $EXTERNALS_TOP
mkdir -p $EXTERNALS_TOP/tarfiles
mkdir -p $EXTERNALS_TOP/bin

pushd $EXTERNALS_TOP/tarfiles

if [ $SV_EXTERNALS_VERSION_NUMBER == '2018.01' ]; then
  wget $PARENT_URL/$TAR_FILE_PREFIX.freetype.2.6.3.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.gdcm.2.6.1.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.itk.4.7.1.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.mitk.2016.03.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.mmg.5.1.0.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.opencascade.7.0.0.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.python.2.7.13.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.qt.5.4.2.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.tcltk.8.6.4.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.vtk.6.2.0.tar.gz
elif [ $SV_EXTERNALS_VERSION_NUMBER == '2018.05' ]; then
  wget $PARENT_URL/$TAR_FILE_PREFIX.freetype.2.6.3.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.gdcm.2.6.3.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.hdf5.1.10.1.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.itk.4.13.0.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.mitk.2018.04.0.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.mmg.5.3.9.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.opencascade.7.3.0.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.python.3.5.5.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.qt.5.6.3.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.tcltk.8.6.4.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.tinyxml2.6.2.0.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.vtk.8.1.1.tar.gz
elif [ $SV_EXTERNALS_VERSION_NUMBER == '2019.02' ]; then
  wget $PARENT_URL/$TAR_FILE_PREFIX.freetype.2.6.3.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.gdcm.2.6.3.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.hdf5.1.10.1.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.itk.4.13.0.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.mitk.2018.04.0.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.mmg.5.3.9.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.opencascade.7.3.0.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.python.3.5.5.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.qt.5.6.3.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.tcltk.8.6.4.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.tinyxml2.6.2.0.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.vtk.8.1.1.tar.gz
fi

popd

pushd $EXTERNALS_TOP/bin
for i in $EXTERNALS_TOP/tarfiles/$TAR_FILE_PREFIX.*.tar.gz; do
    echo "untar $i"
    tar xzf $i
done
popd

make fast

