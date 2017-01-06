#!/bin/bash

export PARENT_URL=http://simvascular.stanford.edu/downloads/public/open_source/linux/qt/5.4

sudo mkdir -p /opt/Qt5.4.2
sudo chmod -R a+rwx /opt/Qt5.4.2

mkdir -p ~/tmp/tarfiles
pushd ~/tmp/tarfiles
wget $PARENT_URL/Qt5.4.2-ubuntu-14.04.tar.gz
echo "untarring (Qt5.4.2-ubuntu-14.04.tar.gz)..."
sudo tar --directory=/ -xzf ./Qt5.4.2-ubuntu-14.04.tar.gz
rm Qt5.4.2-ubuntu-14.04.tar.gz
popd
