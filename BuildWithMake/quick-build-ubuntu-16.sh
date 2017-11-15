#/bin/bash

export EXTERNALS_TOP=`pwd`/ext

export PARENT_URL=http://simvascular.stanford.edu/downloads/public/simvascular/externals/linux/ubuntu/16.04/gnu/5.4/x64/relwithdebinfo/2017.04.06

export TAR_FILE_PREFIX=ubuntu.16.04.gnu.5.4.x64.relwithdebinfo.2017.04.06

# default is windows, so override with gnu compilers and linux

echo "CLUSTER=x64_linux" > cluster_overrides.mk
echo "CXX_COMPILER_VERSION=gcc" >> cluster_overrides.mk

echo "FORTRAN_COMPILER_VERSION=gfortran" >> cluster_overrides.mk

echo "OPEN_SOFTWARE_BINARIES_TOPLEVEL=$EXTERNALS_TOP/bin/gnu/5.4/x64" >> global_overrides.mk
echo "OPEN_SOFTWARE_BUILDS_TOPLEVEL=$EXTERNALS_TOP/build/gnu/5.4/x64" >> global_overrides.mk
echo "OPEN_SOFTWARE_SOURCES_TOPLEVEL=$EXTERNALS_TOP/src" >> global_overrides.mk
echo "LICENSED_SOFTWARE_TOPLEVEL=" >> global_overrides.mk

sudo mkdir -p $EXTERNALS_TOP
sudo chmod -R a+rwx $EXTERNALS_TOP
mkdir -p $EXTERNALS_TOP/tarfiles
mkdir -p $EXTERNALS_TOP/bin/gnu/5.4/x64

pushd $EXTERNALS_TOP/tarfiles
wget $PARENT_URL/$TAR_FILE_PREFIX.freetype.2.6.3.tar.gz
wget $PARENT_URL/$TAR_FILE_PREFIX.gdcm.2.6.1.tar.gz
wget $PARENT_URL/$TAR_FILE_PREFIX.itk.4.7.1.tar.gz
wget $PARENT_URL/$TAR_FILE_PREFIX.mitk.2016.03.tar.gz
wget $PARENT_URL/$TAR_FILE_PREFIX.mmg.5.1.0.tar.gz
wget $PARENT_URL/$TAR_FILE_PREFIX.opencascade.7.0.0.tar.gz
wget $PARENT_URL/$TAR_FILE_PREFIX.python.2.7.11.tar.gz
wget $PARENT_URL/$TAR_FILE_PREFIX.tcltk.8.6.4.tar.gz
wget $PARENT_URL/$TAR_FILE_PREFIX.vtk.6.2.0.tar.gz
popd

pushd $EXTERNALS_TOP/bin/gnu/5.4/x64
for i in $EXTERNALS_TOP/tarfiles/$TAR_FILE_PREFIX.*.tar.gz; do
    tar xvzf $i
done
popd

make
