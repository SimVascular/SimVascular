#!/bin/bash

export PARENT_URL=http://simvascular.stanford.edu/downloads/public/simvascular/externals/linux/ubuntu/14.04/gnu/4.8/x64/RelWithDebInfo/2016.12.11

sudo mkdir -p $EXTERNALS_TOP
sudo chmod -R a+rwx $EXTERNALS_TOP
mkdir -p $EXTERNALS_TOP/tarfiles
mkdir -p $EXTERNALS_TOP/bin/gnu/4.8/x64

pushd $EXTERNALS_TOP/tarfiles
wget $PARENT_URL/ubuntu.14.04.gnu.4.8.x64.RelWithDebInfo.2016.12.11.freetype.2.6.3.tar.gz
wget $PARENT_URL/ubuntu.14.04.gnu.4.8.x64.RelWithDebInfo.2016.12.11.gdcm.2.6.1.tar.gz
wget $PARENT_URL/ubuntu.14.04.gnu.4.8.x64.RelWithDebInfo.2016.12.11.itk.4.7.1.tar.gz
wget $PARENT_URL/ubuntu.14.04.gnu.4.8.x64.RelWithDebInfo.2016.12.11.mitk.2016.03.tar.gz
wget $PARENT_URL/ubuntu.14.04.gnu.4.8.x64.RelWithDebInfo.2016.12.11.mmg.5.1.0.tar.gz
wget $PARENT_URL/ubuntu.14.04.gnu.4.8.x64.RelWithDebInfo.2016.12.11.opencascade.7.0.0.tar.gz
wget $PARENT_URL/ubuntu.14.04.gnu.4.8.x64.RelWithDebInfo.2016.12.11.python.2.7.11.tar.gz
wget $PARENT_URL/ubuntu.14.04.gnu.4.8.x64.RelWithDebInfo.2016.12.11.tcltk.8.6.4.tar.gz
wget $PARENT_URL/ubuntu.14.04.gnu.4.8.x64.RelWithDebInfo.2016.12.11.vtk.6.2.0.tar.gz
popd

pushd $EXTERNALS_TOP/bin/gnu/4.8/x64
for i in $EXTERNALS_TOP/tarfiles/ubuntu.14.04.gnu.4.8.x64.RelWithDebInfo.2016.12.11.*.tar.gz; do
    echo "untarring ($i)..."
    tar xzf $i
done
popd
