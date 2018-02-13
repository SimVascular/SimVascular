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

export EXTERNALS_TOP=`pwd`/ext

export PARENT_URL=http://simvascular.stanford.edu/downloads/public/simvascular/externals/2018.01/darwin/mac_osx/10.11/clang/8.0/x64/relwithdebinfo/2018.01.23/

export TAR_FILE_PREFIX=mac_osx.10.11.clang.8.0.x64.relwithdebinfo.2018.01.23

# default is windows, so override with gnu compilers on mac_osx

echo "CLUSTER=x64_macosx" > cluster_overrides.mk
echo "CXX_COMPILER_VERSION=clang" >> cluster_overrides.mk
echo "FORTRAN_COMPILER_VERSION=ifort" >> cluster_overrides.mk
echo "SV_COMPILER=clang" >> cluster_overrides.mk
echo "SV_COMPILER_VERSION=8.0" >> cluster_overrides.mk

echo "OPEN_SOFTWARE_BINARIES_TOPLEVEL=$EXTERNALS_TOP/bin/clang/8.0/x64/relwithdebinfo" >> global_overrides.mk
echo "OPEN_SOFTWARE_BUILDS_TOPLEVEL=$EXTERNALS_TOP/build/clang/8.0/x64/relwithdebinfo" >> global_overrides.mk
echo "OPEN_SOFTWARE_SOURCES_TOPLEVEL=$EXTERNALS_TOP/src/clang-8.0/x64/relwithdebinfo" >> global_overrides.mk
echo "LICENSED_SOFTWARE_TOPLEVEL=" >> global_overrides.mk

sudo mkdir -p $EXTERNALS_TOP
sudo chmod -R a+rwx $EXTERNALS_TOP
mkdir -p $EXTERNALS_TOP/bin/clang/8.0/x64/relwithdebinfo/tarfiles
mkdir -p $EXTERNALS_TOP/bin/clang/8.0/x64/relwithdebinfo

pushd $EXTERNALS_TOP/bin/clang/8.0/x64/relwithdebinfo/tarfiles
wget $PARENT_URL/$TAR_FILE_PREFIX.freetype.2.6.3.tar.gz
wget $PARENT_URL/$TAR_FILE_PREFIX.gdcm.2.6.1.tar.gz
wget $PARENT_URL/$TAR_FILE_PREFIX.itk.4.7.1.tar.gz
wget $PARENT_URL/$TAR_FILE_PREFIX.mitk.2016.03.tar.gz
wget $PARENT_URL/$TAR_FILE_PREFIX.mmg.5.1.0.tar.gz
wget $PARENT_URL/$TAR_FILE_PREFIX.opencascade.7.0.0.tar.gz
wget $PARENT_URL/$TAR_FILE_PREFIX.python.2.7.13.tar.gz
wget $PARENT_URL/$TAR_FILE_PREFIX.tcltk.8.6.4.tar.gz
wget $PARENT_URL/$TAR_FILE_PREFIX.vtk.6.2.0.tar.gz
popd

pushd $EXTERNALS_TOP/bin/clang/8.0/x64/relwithdebinfo
for i in $EXTERNALS_TOP/bin/clang/8.0/x64/relwithdebinfo/tarfiles/$TAR_FILE_PREFIX.*.tar.gz; do
    tar xvzf $i
done
popd

make

