#!/bin/bash

set -e
MAKE="make --jobs=$NUM_THREADS --keep-going"

if $PREBUILD_ITKVTK; then
    if $ITK450; then
        if [ -d $ITK_SOURCE_DIR ]; then
            echo $ITK_SOURCE_DIR exists
            if [ ! -f $ITK_SOURCE_DIR/CMakeLists.txt ]; then
                echo $ITK_SOURCE_DIR does not contain CMakeList.txt
                rm -rf $ITK_SOURCE_DIR
            fi
        fi
        if [ ! -d $ITK_SOURCE_DIR ]; then
            git clone --branch v4.5.0 https://github.com/SimVascular/ITK.git $ITK_SOURCE_DIR
        fi
        mkdir -p $ITK_DIR
        cd $ITK_DIR
        cmake -DModule_ITKVtkGlue=1 -DVTK_DIR=$VTK_DIR \
              -DModule_ITKReview=1 -DBUILD_SHARED_LIBS=0 \
              -DBUILD_TESTING=0 -DBUILD_EXAMPLES=0 $ITK_SOURCE_DIR
        $MAKE
        cd -
    fi
fi
cd $cwd