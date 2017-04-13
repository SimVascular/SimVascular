#!/bin/bash

if [[ "$TRAVIS_OS_NAME" == "linux" ]]
then
  export PARENT_URL=http://simvascular.stanford.edu/downloads/public/open_source/linux/qt/5.4

  sudo mkdir -p /opt/Qt5.4.2
  sudo chmod -R a+rwx /opt/Qt5.4.2

  mkdir -p ~/tmp/tarfiles
  pushd ~/tmp/tarfiles
  wget $PARENT_URL/Qt5.4.2-ubuntu-14.04.tar.gz
  echo "untarring (Qt5.4.2-ubuntu-14.04.tar.gz)..."
  sudo tar --directory=/ -xzf ./Qt5.4.2-ubuntu-14.04.tar.gz
  rm Qt5.4.2-ubuntu-14.04.tar.gz
  export Qt5_DIR="/opt/Qt5.4.2/5.4/gcc_64/lib/cmake/Qt5"
  popd
elif [[ "$TRAVIS_OS_NAME" == "osx" ]]
then
  export PARENT_URL=http://simvascular.stanford.edu/downloads/public/open_source/mac_osx/qt/5.4

  sudo mkdir -p /opt/Qt5.4.2
  sudo chmod -R a+rwx /opt/Qt5.4.2

  mkdir -p ~/tmp/tarfiles
  pushd ~/tmp/tarfiles
  wget $PARENT_URL/qt-opensource-mac-x64-clang-5.4.2.tar.gz
  echo "untarring (qt-opensource-mac-x64-clang-5.4.2.dmg)..."
  sudo tar --directory=/opt -xzf ./qt-opensource-mac-x64-clang-5.4.2.tar.gz
  rm qt-opensource-mac-x64-clang-5.4.2.tar.gz
  export Qt5_DIR="/opt/Qt5.4.2/5.4/clang_64/lib/cmake/Qt5"
  popd
fi
