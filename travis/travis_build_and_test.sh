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
MAKE_TEST="xvfb-run -a make test ARGS=-V"

if $WITH_CMAKE; then
  mkdir -p $BUILD_DIR
  cd $BUILD_DIR
  CMAKE_BUILD_ARGS=""
  if $BUILD_TEST; then
     CMAKE_BUILD_ARGS="$CMAKE_BUILD_ARGS -DBUILD_TESTING:BOOL=1  -DSV_TEST_DIR:PATH=$SV_TEST_DIR/automated_tests -DSV_NO_RENDERER:BOOL=1"
  fi
  echo CMAKE_BUILD_ARGS: $CMAKE_BUILD_ARGS
  source $SCRIPTS/travis_cmake_config.sh
  pushd $BUILD_DIR
  $MAKE
  popd
  if $BUILD_TEST; then
    $MAKE_TEST
  fi
#  $MAKE clean
  cd -

else
  echo "Building with just make (i.e. NOT cmake!)"
  pushd BuildWithMake
  if [[ "$TRAVIS_OS_NAME" == "linux" ]]
  then
     source ./quick-build-linux.sh
  elif [[ "$TRAVIS_OS_NAME" == "osx" ]]
  then
     echo "QT_TOP_DIR=/opt/Qt5.4.2/5.4/clang_64" > ./pkg_overrides.mk
     source ./quick-build-macosx.sh
  fi
  popd
fi

