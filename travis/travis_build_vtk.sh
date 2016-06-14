#!/bin/bash

set -e
MAKE="make --jobs=$NUM_THREADS --keep-going"

if $PREBUILD_ITKVTK; then
    cmake_arg_str=" -DBUILD_TESTING=0 -DBUILD_SHARED_LIBS=1  -DBUILD_EXAMPLES=0 "
    vtk_repo_str=""
    if [ "$VTK_VERSION" == "6.0" ]; then
        vtk_repo_str=" --branch v6.0.0 https://github.com/SimVascular/VTK.git "
        cmake_arg_str=" $cmake_arg_str -DVTK_WRAP_TCL=1 -DVTK_Group_Tk=1 "
    elif [ "$VTK_VERSION" == "6.2" ]; then
        vtk_repo_str=" --branch simvascular-patch-6.2 https://github.com/SimVascular/VTK.git "
        cmake_arg_str=" $cmake_arg_str -DVTK_WRAP_TCL=1 -DVTK_Group_Imaging=1 -DVTK_Group_Tk=1 "
    fi
    if [ -d $VTK_SOURCE_DIR ]; then
        echo $VTK_SOURCE_DIR exists
        if [ ! -f $VTK_SOURCE_DIR/CMakeLists.txt ]; then
            echo $VTK_SOURCE_DIR does not contain CMakeList.txt
            rm -rf $VTK_SOURCE_DIR
        fi
    fi
    if [ ! -d "$VTK_SOURCE_DIR" ]; then
        git clone $vtk_repo_str $VTK_SOURCE_DIR
    fi
    mkdir -p $VTK_DIR
    cd $VTK_DIR
    cmake $cmake_arg_str $VTK_SOURCE_DIR
    $MAKE
fi
