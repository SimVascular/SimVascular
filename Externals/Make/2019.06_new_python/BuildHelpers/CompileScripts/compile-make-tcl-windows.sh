#!/bin/bash -f

REPLACEME_SV_SPECIAL_COMPILER_SCRIPT

export CC=REPLACEME_CC
export CXX=REPLACEME_CXX

rm -Rf REPLACEME_SV_TOP_BIN_DIR_TCL
mkdir -p REPLACEME_SV_TOP_BIN_DIR_TCL
chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_TCL

cmd /C tmp\\compile.tcl.msvc.bat

export SV_TOPLEVEL_BINDIR_CYGWIN=`cygpath "REPLACEME_SV_TOP_BIN_DIR_TCL"`

cd ../tcllib-1.17
$SV_TOPLEVEL_BINDIR_CYGWIN/bin/tclsh86t.exe installer.tcl -no-gui -no-wait
cd ../BuildHelpers
chmod -R a+rx $SV_TOPLEVEL_BINDIR_CYGWIN

cd ../tklib-0.6
$SV_TOPLEVEL_BINDIR_CYGWIN/bin/tclsh86t.exe installer.tcl -no-gui -no-wait
cd ../BuildHelpers
chmod -R a+rx $SV_TOPLEVEL_BINDIR_CYGWIN

# copy names required for opencascade
cp -f $SV_TOPLEVEL_BINDIR_CYGWIN/bin/tclsh86t.exe  $SV_TOPLEVEL_BINDIR_CYGWIN/bin/tclsh86.exe
cp -f $SV_TOPLEVEL_BINDIR_CYGWIN/bin/wish86t.exe $SV_TOPLEVEL_BINDIR_CYGWIN/bin/wish86.exe
cp -f $SV_TOPLEVEL_BINDIR_CYGWIN/bin/tcl86t.dll  $SV_TOPLEVEL_BINDIR_CYGWIN/bin/tcl86.dll
cp -f $SV_TOPLEVEL_BINDIR_CYGWIN/bin/tk86t.dll $SV_TOPLEVEL_BINDIR_CYGWIN/bin/tk86.dll
cp -f $SV_TOPLEVEL_BINDIR_CYGWIN/lib/tcl86t.lib  $SV_TOPLEVEL_BINDIR_CYGWIN/lib/tcl86.lib
cp -f $SV_TOPLEVEL_BINDIR_CYGWIN/lib/tk86t.lib $SV_TOPLEVEL_BINDIR_CYGWIN/lib/tk86.lib

REPLACEME_SV_SPECIAL_COMPILER_END_SCRIPT
