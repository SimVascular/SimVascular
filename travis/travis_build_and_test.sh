#!/bin/bash

set -e

MAKE="make --jobs=$NUM_THREADS --keep-going"
MAKE_TEST="xvfb-run -a make test ARGS=-V"

echo "Qt5 Dir: $Qt5_DIR"
# Get externals
mkdir -p $SV_EXTERNALS_BUILD_DIR
pushd $SV_EXTERNALS_BUILD_DIR
cmake -DQt5_DIR=$Qt5_DIR ../
$MAKE
popd

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
    make --keep-going
    popd
fi

