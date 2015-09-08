#!/bin/bash

set -e
MAKE="make --jobs=$NUM_THREADS --keep-going"


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

  echo CMAKE_BUILD_ARGS: $CMAKE_BUILD_ARGS
  cmake $CMAKE_BUILD_ARGS ../Code
  $MAKE  
  $MAKE clean
  cd -

else
    echo "Not supported yet"

fi
