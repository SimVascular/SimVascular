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

### install latest version of CMake for Ubuntu
if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
  wget http://simvascular.stanford.edu/downloads/public/open_source/linux/cmake/cmake-3.10.3-Linux-x86_64.sh
  chmod a+rx ./cmake-3.10.3-Linux-x86_64.sh
  sudo mkdir -p /usr/local/package/cmake-3.10.3
  sudo ./cmake-3.10.3-Linux-x86_64.sh --prefix=/usr/local/package/cmake-3.10.3 --skip-license
  sudo ln -s /usr/local/package/cmake-3.10.3/bin/ccmake    /usr/local/bin/ccmake
  sudo ln -s /usr/local/package/cmake-3.10.3/bin/cmake     /usr/local/bin/cmake
  sudo ln -s /usr/local/package/cmake-3.10.3/bin/cmake-gui /usr/local/bin/cmake-gui
  sudo ln -s /usr/local/package/cmake-3.10.3/bin/cpack     /usr/local/bin/cpack
  sudo ln -s /usr/local/package/cmake-3.10.3/bin/ctest     /usr/local/bin/ctest
fi
if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
  wget http://simvascular.stanford.edu/downloads/public/open_source/mac_osx/cmake/cmake-3.10.3-Darwin-x86_64.tar.gz
  tar -xzvf cmake-3.10.3-Darwin-x86_64.tar.gz
  sudo rm /usr/local/bin/cmake
  sudo rm /usr/local/bin/ccmake
  sudo rm /usr/local/bin/ctest
  sudo rm /usr/local/bin/cpack
  sudo cmake-3.10.3-Darwin-x86_64/CMake.app/Contents/bin/cmake-gui --install
fi

#compilers
if [[ "$TRAVIS_OS_NAME" == "linux" ]]
then
  export CC="gcc"
  export CXX="g++"
elif [[ "$TRAVIS_OS_NAME" == "osx" ]]
then
  export CC="clang"
  export CXX="clang++"
fi
