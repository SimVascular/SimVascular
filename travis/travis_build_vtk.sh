#!/bin/bash

set -e
MAKE="make --jobs=$NUM_THREADS --keep-going"

if $PREBUILD_ITKVTK; then
    if $VTK60; then
        if [ -d $VTK_SOURCE_DIR ]; then
            echo $VTK_SOURCE_DIR exists
            if [ ! -f $VTK_SOURCE_DIR/CMakeLists.txt ]; then
                echo $VTK_SOURCE_DIR does not contain CMakeList.txt
                rm -rf $VTK_SOURCE_DIR
            fi
        fi
        if [ ! -d "$VTK_SOURCE_DIR" ]; then
            git clone --branch v6.0.0 https://github.com/Kitware/VTK.git $VTK_SOURCE_DIR
        fi
        mkdir -p $VTK_DIR
        cd $VTK_DIR
        cmake -DVTK_WRAP_TCL=1 -DVTK_Group_Tk=1 -DBUILD_SHARED_LIBS=0 -DBUILD_TESTING=0 -DBUILD_EXAMPLES=0 $VTK_SOURCE_DIR
        $MAKE
        cd -
    fi
fi