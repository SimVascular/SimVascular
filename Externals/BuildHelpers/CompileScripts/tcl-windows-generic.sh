#/bin/bash  -f

# Copyright (c) 2015 Open Source Medical Software Corporation.
# All Rights Reserved.

REPLACEME_SV_SPECIAL_COMPILER_SCRIPT

export CC=CL

rm -Rf REPLACEME_SV_TOP_BIN_DIR_TCL
mkdir -p REPLACEME_SV_TOP_BIN_DIR_TCL
chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_TCL


cd ../REPLACEME_SV_TCL_DIR/win
nmake -f makefile.vc MACHINE=AMD64 INSTALLDIR="REPLACEME_SV_TOP_BIN_DIR_TCL" OPTS=msvcrt,threads hose
nmake -f makefile.vc MACHINE=AMD64 INSTALLDIR="REPLACEME_SV_TOP_BIN_DIR_TCL" OPTS=msvcrt,threads release
nmake -f makefile.vc MACHINE=AMD64 INSTALLDIR="REPLACEME_SV_TOP_BIN_DIR_TCL" OPTS=msvcrt,threads install
chmod -R a+rwx REPLACEME_SV_TOP_BIN_DIR_TCL
cd ../..
cd REPLACEME_SV_TK_DIR/win
nmake -f makefile.vc MACHINE=AMD64 TCLDIR="REPLACEME_SV_TOP_SRC_DIR_TCL" INSTALLDIR="REPLACEME_SV_TOP_BIN_DIR_TK" OPTS=msvcrt,threads hose
nmake -f makefile.vc MACHINE=AMD64 TCLDIR="REPLACEME_SV_TOP_SRC_DIR_TCL" INSTALLDIR="REPLACEME_SV_TOP_BIN_DIR_TK" OPTS=msvcrt,threads release
nmake -f makefile.vc MACHINE=AMD64 TCLDIR="REPLACEME_SV_TOP_SRC_DIR_TCL" INSTALLDIR="REPLACEME_SV_TOP_BIN_DIR_TK" OPTS=msvcrt,threads install
chmod -R a+rwx REPLACEME_SV_TOP_BIN_DIR_TK
nmake -f makefile.vc MACHINE=AMD64 TCLDIR="REPLACEME_SV_TOP_SRC_DIR_TCL" INSTALLDIR= OPTS=msvcrt,threads hose
cd ../..
cd REPLACEME_SV_TCL_DIR/win
nmake -f makefile.vc MACHINE=AMD64 INSTALLDIR="REPLACEME_SV_TOP_BIN_DIR_TCL" OPTS=msvcrt,threads hose

cd ../../BuildHelpers

export SV_TOPLEVEL_BINDIR_CYGWIN=`cygpath "REPLACEME_SV_TOP_BIN_DIR_TCL"`

cd ../tcllib-1.17
$SV_TOPLEVEL_BINDIR_CYGWIN/bin/tclsh8?t.exe installer.tcl
cd ../BuildHelpers
chmod -R a+rx $SV_TOPLEVEL_BINDIR_CYGWIN

cd ../tklib-0.6
$SV_TOPLEVEL_BINDIR_CYGWIN/bin/tclsh8?t.exe installer.tcl
cd ../BuildHelpers
chmod -R a+rx $SV_TOPLEVEL_BINDIR_CYGWIN

# copy names required for opencascade
cp -f $SV_TOPLEVEL_BINDIR_CYGWIN/bin/tclsh86t.exe  $SV_TOPLEVEL_BINDIR_CYGWIN/bin/tclsh86.exe
cp -f $SV_TOPLEVEL_BINDIR_CYGWIN/bin/wish86t.exe $SV_TOPLEVEL_BINDIR_CYGWIN/bin/wish86.exe
cp -f $SV_TOPLEVEL_BINDIR_CYGWIN/bin/tcl86t.dll  $SV_TOPLEVEL_BINDIR_CYGWIN/bin/tcl86.dll
cp -f $SV_TOPLEVEL_BINDIR_CYGWIN/bin/tk86t.dll $SV_TOPLEVEL_BINDIR_CYGWIN/bin/tk86.dll
cp -f $SV_TOPLEVEL_BINDIR_CYGWIN/lib/tcl86t.lib  $SV_TOPLEVEL_BINDIR_CYGWIN/lib/tcl86.lib
cp -f $SV_TOPLEVEL_BINDIR_CYGWIN/lib/tk86t.lib $SV_TOPLEVEL_BINDIR_CYGWIN/lib/tk86.lib



