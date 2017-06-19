#!/bin/bash

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

