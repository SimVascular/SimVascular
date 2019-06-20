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

set -e

MAKE="make --jobs=$NUM_THREADS --keep-going"

if $WITH_CMAKE; then
  if [[ "$TRAVIS_OS_NAME" == "linux" ]]
  then
     source $SCRIPTS/travis_cmake_linux.sh
  elif [[ "$TRAVIS_OS_NAME" == "osx" ]]
  then
     source $SCRIPTS/travis_cmake_macosx.sh
  elif [[ "$TRAVIS_OS_NAME" == "windows" ]]
  then
     echo "$SCRIPTS/travis_cmake_windows.bat $cwd $SV_EXTERNALS_VERSION_NUMBER"
     $SCRIPTS/travis_cmake_windows.bat $cwd $SV_EXTERNALS_VERSION_NUMBER
  fi
else
  echo "Building with just make (i.e. NOT cmake!)"
  pushd BuildWithMake
  if [[ "$TRAVIS_OS_NAME" == "linux" ]]
  then
     source ./quick-build-linux.sh
  elif [[ "$TRAVIS_OS_NAME" == "osx" ]]
  then
     if [ "$SV_EXTERNALS_VERSION_NUMBER" == "2018.01" ]; then
	 echo "QT_TOP_DIR=/opt/Qt5.4.2/5.4/clang_64" > ./pkg_overrides.mk
     fi
     source ./quick-build-macosx.sh
  elif [[ "$TRAVIS_OS_NAME" == "windows" ]]
  then
     echo "ERROR: Make not working yet for travis/Windows platform!"
  fi
  popd
fi

