#!/bin/bash

build_dir=build
rm -rf build
mkdir $build_dir

export CXX=/usr/bin/g++
export CC=/usr/bin/gcc

# Python libraries are not found by the interpreter
export LD_LIBRARY_PATH=$(pwd)/$build_dir/Externals-build/svExternals/bin/python-3.9.10/lib

cd $build_dir
cmake ..
make -j 4
