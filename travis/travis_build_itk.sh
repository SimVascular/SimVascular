#!/bin/bash

set -e
MAKE="make --jobs=$NUM_THREADS --keep-going"
if $PREBUILD_ITKVTK; then
    if [ "$ITK_VERSION" == "4.5" ]; then
        itk_repo_str=" --branch v4.5.0 https://github.com/SimVascular/ITK.git "
        cmake_arg_str=" -DModule_ITKVtkGlue=1 -DVTK_DIR=$VTK_DIR -DBUILD_SHARED_LIBS=1 -DBUILD_TESTING=0 -DBUILD_EXAMPLES=0 "
    elif [ "$ITK_VERSION" == "4.8" ]; then
        itk_repo_str=" --branch v4.8.0 https://github.com/SimVascular/ITK.git "
        cmake_arg_str=" -DModule_ITKVtkGlue=1 -DVTK_DIR=$VTK_DIR -DBUILD_SHARED_LIBS=1 -DBUILD_TESTING=0 -DBUILD_EXAMPLES=0 "
    fi

    if [ -d $ITK_SOURCE_DIR ]; then
        echo $ITK_SOURCE_DIR exists
        if [ ! -f $ITK_SOURCE_DIR/CMakeLists.txt ]; then
            echo $ITK_SOURCE_DIR does not contain CMakeList.txt
            rm -rf $ITK_SOURCE_DIR
        fi
    fi
    if [ ! -d $ITK_SOURCE_DIR ]; then
        git clone $itk_repo_str $ITK_SOURCE_DIR
    fi
    mkdir -p $ITK_DIR
    cd $ITK_DIR
    cmake $cmake_arg_str $ITK_SOURCE_DIR
    $MAKE
    cd -
fi
cd $cwd
