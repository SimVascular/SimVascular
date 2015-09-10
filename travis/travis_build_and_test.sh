#!/bin/bash

set -e
MAKE="make --jobs=$NUM_THREADS --keep-going"
MAKE_TEST="xvfb-run -a make test ARGS=-V"

if $WITH_CMAKE; then
  mkdir -p $BUILD_DIR
  cd $BUILD_DIR
  CMAKE_BUILD_ARGS=""
  if $PREBUILD_ITKVTK; then
    CMAKE_BUILD_ARGS="$CMAKE_BUILD_ARGS -DSimVascular_USE_SYSTEM_VTK:BOOL=1 -DSimVascular_USE_SYSTEM_ITK:BOOL=1 -DVTK_DIR:PATH=$VTK_DIR -DITK_DIR:PATH=$ITK_DIR"
  fi
  if $BUILD_SOLVER; then
    CMAKE_BUILD_ARGS="$CMAKE_BUILD_ARGS -DBUILD_ThreeDSolver=1 -DSimVascular_USE_DUMMY_MPICH2=1"
  fi
  if $BUILD_TEST; then
     CMAKE_BUILD_ARGS="$CMAKE_BUILD_ARGS -DBUILD_TESTING:BOOL=1  -DSimVascular_TEST_DIR:PATH=$SV_TEST_DIR/automated_tests -DSimVascular_RUN_GUI_TESTS:BOOL=0 " 
  fi
  echo CMAKE_BUILD_ARGS: $CMAKE_BUILD_ARGS
  cmake $CMAKE_BUILD_ARGS ../Code
  $MAKE
  if $BUILD_TEST; then
    $MAKE_TEST
  fi
  $MAKE clean
  cd -

else
    echo "Not supported yet"

fi
