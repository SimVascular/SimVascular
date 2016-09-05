
#/bin/bash  -f

# Copyright (c) 2015 Open Source Medical Software Corporation.
# All Rights Reserved.

export SV_TCL_VERSION=$1
export SV_COMPILER_DIR=$2
export SV_TCL_MAJOR_VERSION=$3

export SV_TOPLEVEL_SRCDIR=/usr/local/sv/ext/src
export SV_TOPLEVEL_BINDIR=/usr/local/sv/ext/bin

export SV_ARCH_DIR=x64

export SV_TCL_INSTALL_SHORT_DIR_NAME=tcltk-$SV_TCL_VERSION
export SV_TCL_INSTALL_FULL_DIR_NAME=${SV_TOPLEVEL_BINDIR}/${SV_COMPILER_DIR}/${SV_ARCH_DIR}/${SV_TCL_INSTALL_SHORT_DIR_NAME}

mkdir -p $SV_TCL_INSTALL_FULL_DIR_NAME

chmod a+rx  $SV_TCL_INSTALL_FULL_DIR_NAME

# builds in hardcoded directory name
cd ..
rm -Rf build

# embedded creates a template of a wish app for release with tcl/tk embedded, but
# we want the a more standard release for now and will build our own app structure later.
# note: this does a nice relative link to the shared libs using @executable_path,
# so we need to use install_name_tool to replace the faulty paths for tcl/tk created
# when building tclsh and wish using normal build.

#make -C tcl-$SV_TCL_VERSION/macosx -f GNUmakefile embedded INSTALL_ROOT=${SV_TCL_INSTALL_FULL_DIR_NAME}
#make -C tcl-$SV_TCL_VERSION/macosx -f GNUmakefile install-embedded INSTALL_ROOT=${SV_TCL_INSTALL_FULL_DIR_NAME}
#make -C tk-$SV_TCL_VERSION/macosx -f GNUmakefile embedded INSTALL_ROOT=${SV_TCL_INSTALL_FULL_DIR_NAME}
#make -C tk-$SV_TCL_VERSION/macosx -f GNUmakefile install-embedded INSTALL_ROOT=${SV_TCL_INSTALL_FULL_DIR_NAME}

make -C tcl-$SV_TCL_VERSION/macosx -f GNUmakefile INSTALL_ROOT=${SV_TCL_INSTALL_FULL_DIR_NAME}
make -C tcl-$SV_TCL_VERSION/macosx -f GNUmakefile install INSTALL_ROOT=${SV_TCL_INSTALL_FULL_DIR_NAME}
make -C tk-$SV_TCL_VERSION/macosx -f GNUmakefile INSTALL_ROOT=${SV_TCL_INSTALL_FULL_DIR_NAME}
make -C tk-$SV_TCL_VERSION/macosx -f GNUmakefile install INSTALL_ROOT=${SV_TCL_INSTALL_FULL_DIR_NAME}

mkdir ${SV_TCL_INSTALL_FULL_DIR_NAME}/bin
cp build/tcl/tclsh8.6 ${SV_TCL_INSTALL_FULL_DIR_NAME}/bin/tclsh${SV_TCL_MAJOR_VERSION}
install_name_tool -change /Library/Frameworks/Tcl.framework/Versions/${SV_TCL_MAJOR_VERSION}/Tcl \
		  ${SV_TCL_INSTALL_FULL_DIR_NAME}/Library/Frameworks/Tcl.framework/Versions/${SV_TCL_MAJOR_VERSION}/Tcl \
                  ${SV_TCL_INSTALL_FULL_DIR_NAME}/bin/tclsh${SV_TCL_MAJOR_VERSION}	  
cp build/tk/wish8.6 ${SV_TCL_INSTALL_FULL_DIR_NAME}/bin/wish${SV_TCL_MAJOR_VERSION}
install_name_tool -change /Library/Frameworks/Tcl.framework/Versions/${SV_TCL_MAJOR_VERSION}/Tcl \
		  ${SV_TCL_INSTALL_FULL_DIR_NAME}/Library/Frameworks/Tcl.framework/Versions/${SV_TCL_MAJOR_VERSION}/Tcl \
                  ${SV_TCL_INSTALL_FULL_DIR_NAME}/bin/wish${SV_TCL_MAJOR_VERSION}	  
install_name_tool -change /Library/Frameworks/Tk.framework/Versions/${SV_TCL_MAJOR_VERSION}/Tk \
		  ${SV_TCL_INSTALL_FULL_DIR_NAME}/Library/Frameworks/Tk.framework/Versions/${SV_TCL_MAJOR_VERSION}/Tk \
                  ${SV_TCL_INSTALL_FULL_DIR_NAME}/bin/wish${SV_TCL_MAJOR_VERSION}

cd BuildHelpers

cd ../tcllib-1.17
$SV_TCL_INSTALL_FULL_DIR_NAME/bin/tclsh8.? installer.tcl \
    -html \
    -pkg-path ${SV_TCL_INSTALL_FULL_DIR_NAME}/Library/Frameworks/Tcl.framework/Versions/${SV_TCL_MAJOR_VERSION}/Resources/Scripts/tcllib \
    -app-path ${SV_TCL_INSTALL_FULL_DIR_NAME}/Library/Frameworks/Tcl.framework/Versions/${SV_TCL_MAJOR_VERSION}/Resources/Documentation/Reference/tcllib_applications \
    -nroff-path ${SV_TCL_INSTALL_FULL_DIR_NAME}/Library/Frameworks/Tcl.framework/Versions/${SV_TCL_MAJOR_VERSION}/Resources/Documentation/Reference/tcllib_man \
    -html-path ${SV_TCL_INSTALL_FULL_DIR_NAME}/Library/Frameworks/Tcl.framework/Versions/${SV_TCL_MAJOR_VERSION}/Resources/Documentation/Reference/tcllib_html \
    -example-path ${SV_TCL_INSTALL_FULL_DIR_NAME}/Library/Frameworks/Tcl.framework/Versions/${SV_TCL_MAJOR_VERSION}/Resources/Documentation/Reference/tcllib_examples \
    -no-nroff \
    -no-gui

cd ../BuildHelpers

cd ../tklib-0.6
$SV_TCL_INSTALL_FULL_DIR_NAME/bin/tclsh8.? installer.tcl \
    -html \
    -pkg-path ${SV_TCL_INSTALL_FULL_DIR_NAME}/Library/Frameworks/Tk.framework/Versions/${SV_TCL_MAJOR_VERSION}/Resources/Scripts/tklib \
    -nroff-path ${SV_TCL_INSTALL_FULL_DIR_NAME}/Library/Frameworks/Tk.framework/Versions/${SV_TCL_MAJOR_VERSION}/Resources/Documentation/Reference/tklib_man \
    -app-path ${SV_TCL_INSTALL_FULL_DIR_NAME}/Library/Frameworks/Tk.framework/Versions/${SV_TCL_MAJOR_VERSION}/Resources/Documentation/Reference/tklib_applications \
    -html-path ${SV_TCL_INSTALL_FULL_DIR_NAME}/Library/Frameworks/Tk.framework/Versions/${SV_TCL_MAJOR_VERSION}/Resources/Documentation/Reference/tklib_html \
    -example-path ${SV_TCL_INSTALL_FULL_DIR_NAME}/Library/Frameworks/Tk.framework/Versions/${SV_TCL_MAJOR_VERSION}/Resources/Documentation/Reference/tklib_examples \
    -no-nroff \
    -no-gui
cd ../BuildHelpers




