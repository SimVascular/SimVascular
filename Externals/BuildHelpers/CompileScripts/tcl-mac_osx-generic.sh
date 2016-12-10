#/bin/bash  -f

# Copyright (c) 2015 Open Source Medical Software Corporation.
# All Rights Reserved.

REPLACEME_SV_SPECIAL_COMPILER_SCRIPT

export CC=clang

rm -Rf REPLACEME_SV_TOP_BIN_DIR_TCL
mkdir -p REPLACEME_SV_TOP_BIN_DIR_TCL
chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_TCL


cd ../REPLACEME_SV_TCL_DIR/unix
./configure --prefix=REPLACEME_SV_TOP_BIN_DIR_TCL --enable-threads --enable-shared \
	    --enable-corefoundation
make -j8 clean
make -j8 release
make -j8 install

cd ../..
cd REPLACEME_SV_TK_DIR/unix
./configure --prefix=REPLACEME_SV_TOP_BIN_DIR_TK \
            --with-tcl=REPLACEME_SV_TOP_BIN_DIR_TCL/lib \
            --enable-threads --enable-shared --enable-corefoundation \
	    --enable-aqua
make -j8 clean
make -j8 release
make -j8 install
make -j8 clean

cd ../..
cd REPLACEME_SV_TCL_DIR/unix
make -j8 clean

cd ../../BuildHelpers

cd ../tcllib-1.17
REPLACEME_SV_TOP_BIN_DIR_TCL/bin/tclsh8.? installer.tcl
cd ../BuildHelpers

cd ../tklib-0.6
REPLACEME_SV_TOP_BIN_DIR_TCL/bin/tclsh8.? installer.tcl
cd ../BuildHelpers

chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_TCL/lib/REPLACEME_SV_TCL_LIB_NAME
install_name_tool -id REPLACEME_SV_TCL_LIB_NAME  REPLACEME_SV_TOP_BIN_DIR_TCL/lib/REPLACEME_SV_TCL_LIB_NAME

chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_TCL/bin/REPLACEME_SV_TCLSH_EXECUTABLE
install_name_tool -change  REPLACEME_SV_TOP_BIN_DIR_TCL/lib/REPLACEME_SV_TCL_LIB_NAME  REPLACEME_SV_TCL_LIB_NAME REPLACEME_SV_TOP_BIN_DIR_TCL/bin/REPLACEME_SV_TCLSH_EXECUTABLE

chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_TCL/lib/REPLACEME_SV_TK_LIB_NAME
install_name_tool -id REPLACEME_SV_TK_LIB_NAME  REPLACEME_SV_TOP_BIN_DIR_TCL/lib/REPLACEME_SV_TK_LIB_NAME

chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_TCL/bin/REPLACEME_SV_WISH_EXECUTABLE
install_name_tool -change  REPLACEME_SV_TOP_BIN_DIR_TCL/lib/REPLACEME_SV_TCL_LIB_NAME  REPLACEME_SV_TCL_LIB_NAME REPLACEME_SV_TOP_BIN_DIR_TCL/bin/REPLACEME_SV_WISH_EXECUTABLE
install_name_tool -change  REPLACEME_SV_TOP_BIN_DIR_TCL/lib/REPLACEME_SV_TK_LIB_NAME  REPLACEME_SV_TK_LIB_NAME REPLACEME_SV_TOP_BIN_DIR_TCL/bin/REPLACEME_SV_WISH_EXECUTABLE




