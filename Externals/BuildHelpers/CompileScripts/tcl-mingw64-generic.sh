
#/bin/bash  -f

# Copyright (c) 2015 Open Source Medical Software Corporation.
# All Rights Reserved.

export SV_TCL_VERSION=$1
export SV_COMPILER_DIR=$2

export SV_TOPLEVEL_SRCDIR=/usr/local/sv/ext/src
export SV_TOPLEVEL_BINDIR=/usr/local/sv/ext/bin

export SV_ARCH_DIR=x64

export SV_TCL_INSTALL_SHORT_DIR_NAME=tcltk-$SV_TCL_VERSION
export SV_TCL_INSTALL_FULL_DIR_NAME=${SV_TOPLEVEL_BINDIR}/${SV_COMPILER_DIR}/${SV_ARCH_DIR}/${SV_TCL_INSTALL_SHORT_DIR_NAME}

mkdir -p $SV_TCL_INSTALL_FULL_DIR_NAME

chmod a+rx  $SV_TCL_INSTALL_FULL_DIR_NAME
cd ../tcl-$SV_TCL_VERSION/win
./configure --prefix=${SV_TCL_INSTALL_FULL_DIR_NAME} --enable-threads --enable-shared --enable-64bit

make clean
make binaries libraries
make install-binaries install-libraries

chmod -R a+rx $SV_TCL_INSTALL_FULL_DIR_NAME
cd ../..
cd tk-$SV_TCL_VERSION/win
./configure --prefix=${SV_TCL_INSTALL_FULL_DIR_NAME} \
            --with-tcl=${SV_TCL_INSTALL_FULL_DIR_NAME}/lib \
            --enable-threads --enable-shared --enable-64bit
make clean
make binaries libraries
make install-binaries install-libraries
make clean
#chmod -R a+rx $SV_TCL_INSTALL_FULL_DIR_NAME
cd ../..
cd tcl-$SV_TCL_VERSION/win
make clean

cd ../../BuildHelpers

cd ../tcllib-1.17
$SV_TCL_INSTALL_FULL_DIR_NAME/bin/tclsh8?.exe installer.tcl
cd ../BuildHelpers
#chmod -R a+rx $SV_TCL_INSTALL_FULL_DIR_NAME

cd ../tklib-0.6
$SV_TCL_INSTALL_FULL_DIR_NAME/bin/tclsh8?.exe installer.tcl
cd ../BuildHelpers
#chmod -R a+rx $SV_TCL_INSTALL_FULL_DIR_NAME




