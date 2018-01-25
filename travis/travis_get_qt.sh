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
  popd
elif [[ "$TRAVIS_OS_NAME" == "osx" ]]
then
  export PARENT_URL=http://simvascular.stanford.edu/downloads/public/open_source/mac_osx/qt/5.4

  sudo mkdir -p /usr/local/package/Qt5.4.2
  sudo chmod -R a+rwx /usr/local/package/Qt5.4.2

  mkdir -p ~/tmp/tarfiles
  pushd ~/tmp/tarfiles
  wget $PARENT_URL/qt-opensource-mac-x64-clang-5.4.2-usrlocalpackage.tar.gz
  echo "untarring (qt-opensource-mac-x64-clang-5.4.2-usrlocalpackage.tar.gz)..."
  sudo tar --directory=/ -xzf ./qt-opensource-mac-x64-clang-5.4.2-usrlocalpackage.tar.gz
  rm qt-opensource-mac-x64-clang-5.4.2-usrlocalpackage.tar.gz
  popd
fi
