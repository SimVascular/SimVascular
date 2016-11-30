#/bin/bash

export PARENT_URL=http://http://simvascular.stanford.edu/downloads/public/simvascular/externals/linux/ubuntu_14/gnu-4.8/2016.09.05

# default is windows, so override with gnu compilers and linux

echo "CLUSTER=x64_linux" > cluster_overrides.mk
echo "CXX_COMPILER_VERSION=gcc" >> cluster_overrides.mk
echo "FORTRAN_COMPILER_VERSION=gfortran" >> cluster_overrides.mk

export EXTERNALS_TOP=/usr/local/sv/ext

sudo mkdir -p $EXTERNALS_TOP
sudo chmod -R a+rwx $EXTERNALS_TOP
mkdir $EXTERNALS_TOP/tarfiles
mkdir $EXTERNALS_TOP/bin

pushd $EXTERNALS_TOP/tarfiles
wget $PARENT_URL/linux.gnu-4.8.x64.freetype-2.6.3-BUILD2016-09-05.tar.gz
wget $PARENT_URL/linux.gnu-4.8.x64.gdcm-2.6.1-BUILD2016-09-05.tar.gz
wget $PARENT_URL/linux.gnu-4.8.x64.itk-4.7.1-BUILD2016-09-05.tar.gz
wget $PARENT_URL/linux.gnu-4.8.x64.mitk-2016.03-BUILD2016-11-29.tar.gz
wget $PARENT_URL/linux.gnu-4.8.x64.mmg-5.1.0-BUILD2016-11-29.tar.gz
wget $PARENT_URL/linux.gnu-4.8.x64.opencascade-7.0.0-BUILD2016-09-05.tar.gz
wget $PARENT_URL/linux.gnu-4.8.x64.python-2.7.11-BUILD2016-09-05.tar.gz
wget $PARENT_URL/linux.gnu-4.8.x64.tcltk-8.6.4-BUILD2016-09-05.tar.gz
wget $PARENT_URL/linux.gnu-4.8.x64.vtk-6.2.0-BUILD2016-09-05.tar.gz
popd

pushd $EXTERNALS_TOP/bin
for i in ../tarfiles/linux.gnu-4.8.x64.*.tar.gz; do
    tar xvzf $i
done
popd

make
