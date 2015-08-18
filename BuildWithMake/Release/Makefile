# Hey emacs, this is a -*- makefile -*-

# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Portions of the code Copyright (c) 2009-2011 Open Source Medical 
# Software Corporation, University of California, San Diego.
# All rights reserved.
#
# Portions copyright (c) 1999-2007 Stanford University,
#   Nathan Wilson, Ken Wang, Charles Taylor.
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including 
# without limitation the rights to use, copy, modify, merge, publish, 
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
# 
# The above copyright notice and this permission notice shall be included 
# in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

TOP = ..

include $(TOP)/include.mk

#
#  The following are helper scripts to create PC
#  distributions.  Note that some of the paths are hardcoded.
#

SIMVASCULAR_CLUSTER = $(CLUSTER)

ifneq ($(CLUSTER),x64_linux)
#WIXUI_PATH := $(shell cygpath -m $(OPEN_SOFTWARE_PRECOMPILED_TOPLEVEL)/wix-2.0.5805.0)
WIXUI_PATH := /cygdrive/c/Program\ Files\ \(x86\)/WiX\ Toolset\ v3.9/bin
WIXUI_WIN_PATH = C:/Program\ Files\ \(x86\)/WiX\ Toolset\ v3.9/bin
endif

DIST_DIR_TOP = package/$(SIMVASCULAR_VERSION)
DIST_DIR := $(DIST_DIR_TOP)/$(shell echo "puts \"[clock seconds]\"" | tclsh)

REMOTE_DIR_TOP = remote_install_$(SIMVASCULAR_VERSION)

ifneq ($(CLUSTER),x64_linux)
  all: windows-distribution msi
else
  all: linux-distribution taritup taritup-meshsim-and-parasolid
endif

#
#  Create msi installer for simvascular meshsim-and-parasolid
#

msiclean-meshsim-and-parasolid:
	-rm *~
	-rm package-release-meshsim-and-parasolid
	-rm simvascular-meshsim-and-parasolid.msi
	-rm tmp/simvascular-meshsim-and-parasolid.wixobj
	-rm tmp/simvascular-meshsim-and-parasolid.wxs
	-rm tmp/simvascular-meshsim-and-parasolid.wixpdb
	-rm tmp/uuidgen.exe*
	-rm tmp/uuidgen.obj*

msi-meshsim-and-parasolid:	msiclean-meshsim-and-parasolid
ifeq ($(CXX_COMPILER_VERSION),mingw-gcc)
	x86_64-w64-mingw32-gcc -o tmp/uuidgen.exe windows_msi_helpers/uuidgen.c -lole32
else
	CL /Fetmp/uuidgen.exe /Fotmp/uuidgen.obj windows_msi_helpers/uuidgen.c ole32.lib
endif
	-rm -Rf package-release-meshsim-and-parasolid
	-mkdir -p package-release-meshsim-and-parasolid
	-date > package-release-meshsim-and-parasolid/release-date
	-chmod -R a+rwx package-release-meshsim-and-parasolid
	-echo "puts \"[file tail $(DIST_DIR)]\"" | tclsh > tmp/timestamp
	-cp -f tmp/timestamp package-release-meshsim-and-parasolid/
	-cp -f $(TOP)/Lib/*.$(SOEXT)* package-release-meshsim-and-parasolid
ifeq ($(MAKE_WITH_PARASOLID),1)
	-cp -f $(PARASOLID_DLLS) package-release-meshsim-and-parasolid
	-cp -Rf $(P_SCHEMA) package-release-meshsim-and-parasolid/schema
endif
	-cp -f $(TOP)/Bin/adaptor.exe package-release-meshsim-and-parasolid/adaptor-bin.exe
	-cp -fR $(TCL_BASE)/lib package-release-meshsim-and-parasolid
	-cp -fR $(TCL_BASE)/bin package-release-meshsim-and-parasolid
	-cp -fR $(TCL_BASE)/bin/*.$(SOEXT)* package-release-meshsim-and-parasolid
	-cp -fR $(TCL_BASE)/lib/*.$(SOEXT)* package-release-meshsim-and-parasolid
	-echo "Create WXS file..."
	-tclsh windows_msi_helpers/makewxs-meshsim-and-parasolid.tcl $(SIMVASCULAR_VERSION) $(SIMVASCULAR_PLATFORM) $(DIST_DIR_TOP) $(SIMVASCULAR_MAJOR_VER_NO) ../Meshsim-And-Parasolid/MeshSim/license.dat
	-echo "Run candle..."
	-$(WIXUI_PATH)/candle.exe -ext WixBalExtension -ext WixUIExtension -ext WixUtilExtension -out tmp/simvascular-meshsim-and-parasolid.wixobj tmp/simvascular-meshsim-and-parasolid.wxs
	-echo "Run light..."
	-$(WIXUI_PATH)/light.exe -ext WixBalExtension -ext WixUIExtension -ext WixUtilExtension -out simvascular-meshsim-and-parasolid.msi tmp/simvascular-meshsim-and-parasolid.wixobj

#
#  Create msi installer for SimVascular
#

msiclean:
	-rm *~
	-rm $(SIMVASCULAR_VERSION).msi
	-rm tmp/sv.wixobj
	-rm tmp/sv.wxs
	-rm tmp/uuidgen.exe*
	-rm tmp/uuidgen.obj*

msi-simvascular:	msiclean
ifeq ($(CXX_COMPILER_VERSION),mingw-gcc)
	x86_64-w64-mingw32-gcc -o tmp/uuidgen.exe windows_msi_helpers/uuidgen.c -lole32
else
	CL /Fetmp/uuidgen.exe /Fotmp/uuidgen.obj windows_msi_helpers/uuidgen.c ole32.lib
endif
	-echo "Create WXS file..."
	-tclsh windows_msi_helpers/makewxs.tcl $(SIMVASCULAR_VERSION) $(SIMVASCULAR_PLATFORM) $(DIST_DIR_TOP) $(SIMVASCULAR_MAJOR_VER_NO) Releases$(SIMVASCULAR_POSTFIX) simvascular-bin.exe $(SIMVASCULAR_VERSION) $(SIMVASCULAR_FULL_VER_NO)
	-echo "Run candle..."
	-$(WIXUI_PATH)/candle.exe -ext WixUIExtension -ext WixUtilExtension -out tmp/sv.wixobj tmp/sv.wxs
	-echo "Run light..."
	-$(WIXUI_PATH)/light.exe -ext WixUIExtension -ext WixUtilExtension -out $(SIMVASCULAR_VERSION).msi tmp/sv.wixobj

msi-bundle:
	-rm -f tmp/bundle.wixobj
	-rm -f simvascular-bundle.wixpdb
	-rm -f simvascular-bundle.exe
	-echo "Create WXS file..."
	-echo "Run candle..."
	-$(WIXUI_PATH)/candle.exe -ext WixBalExtension -ext WixUIExtension -ext WixUtilExtension -out tmp/bundle.wixobj windows_installer_packages/wix-simvascular-bundle.wxs
	-echo "Run light..."
	-$(WIXUI_PATH)/light.exe tmp/bundle.wixobj -ext WixBalExtension -ext WixUIExtension -ext WixUtilExtension -out simvascular-bundle.exe

msi: msiclean msi-simvascular msiclean-meshsim-and-parasolid msi-meshsim-and-parasolid msi-bundle

remote-linux-distribution: mksedscript linux-distribution
	-rm -Rf $(REMOTE_DIR_TOP)
	-mkdir -p $(REMOTE_DIR_TOP)/dist_files
	-cp tar_files/simvascular-$(SIMVASCULAR_OS)-$(SIMVASCULAR_PLATFORM)*.tar.gz $(REMOTE_DIR_TOP)/dist_files
	-sed -f tmp/sedscript linux_installer_scripts/linux-remote-clean-install.sh > $(REMOTE_DIR_TOP)/simvascular-clean-install-$(SIMVASCULAR_PLATFORM).sh
	-sed -f tmp/sedscript linux_installer_scripts/linux-remote-update-install.sh > $(REMOTE_DIR_TOP)/simvascular-update-install-$(SIMVASCULAR_PLATFORM).sh
	-sed -f tmp/sedscript platform_independent_scripts/generic-launch-script > $(REMOTE_DIR_TOP)/dist_files/generic_launch_script
	-tar --preserve -cvf tar_files/simvascular-$(SIMVASCULAR_OS)-$(SIMVASCULAR_PLATFORM)-remote-install.`awk '{print $$2"-"$$3"-"$$6}' tmp/release-date`.tar $(REMOTE_DIR_TOP)
	-chmod a+rx $(REMOTE_DIR_TOP)/*.sh
	-chmod a+rx $(REMOTE_DIR_TOP)/dist_files/generic_launch_script
	-gzip tar_files/simvascular-$(SIMVASCULAR_OS)-$(SIMVASCULAR_PLATFORM)-remote-install.`awk '{print $$2"-"$$3"-"$$6}' tmp/release-date`.tar 

remote-windows-distribution: mksedscript windows-distribution
	-rm -Rf $(REMOTE_DIR_TOP)
	-mkdir -p $(REMOTE_DIR_TOP)/dist_files
	-cp tar_files/simvascular-$(SIMVASCULAR_OS)-$(SIMVASCULAR_PLATFORM)*.tar.gz $(REMOTE_DIR_TOP)/dist_files
	-sed -f tmp/sedscript linux_installer_scripts/linux-remote-clean-install.sh > $(REMOTE_DIR_TOP)/simvascular-clean-install-$(SIMVASCULAR_PLATFORM).sh
	-sed -f tmp/sedscript linux_installer_scripts/linux-remote-update-install.sh > $(REMOTE_DIR_TOP)/simvascular-update-install-$(SIMVASCULAR_PLATFORM).sh
	-chmod a+rx $(REMOTE_DIR_TOP)/*.sh
	-tar --preserve -cvf tar_files/simvascular-$(SIMVASCULAR_OS)-$(SIMVASCULAR_PLATFORM)-remote-install.`awk '{print $$2"-"$$3"-"$$6}' tmp/release-date`.tar $(REMOTE_DIR_TOP)
	-gzip tar_files/simvascular-$(SIMVASCULAR_OS)-$(SIMVASCULAR_PLATFORM)-remote-install.`awk '{print $$2"-"$$3"-"$$6}' tmp/release-date`.tar 


mksedscript:
	-echo "puts \"s/REPLACE_TIMESTAMP/[file tail $(DIST_DIR)]/g\"" | tclsh | cat > tmp/sedscript
	-echo "puts \"s/REPLACE_SIMVASCULAR_TIMESTAMP/[file tail $(DIST_DIR)]/g\"" | tclsh | cat >> tmp/sedscript
	-echo "puts \"s/REPLACE_SIMVASCULAR_OS/$(SIMVASCULAR_OS)/g\"" | tclsh | cat >> tmp/sedscript
	-echo "puts \"s/REPLACE_SIMVASCULAR_VERSION/$(SIMVASCULAR_VERSION)/g\"" | tclsh | cat >> tmp/sedscript
	-echo "puts \"s/REPLACE_SIMVASCULAR_PLATFORM/$(SIMVASCULAR_PLATFORM)/g\"" | tclsh | cat >> tmp/sedscript
	-echo "puts \"s/REPLACE_SIMVASCULAR_POSTFIX/$(SIMVASCULAR_POSTFIX)/g\"" | tclsh | cat >> tmp/sedscript
	-echo "puts \"s/REPLACE_SIMVASCULAR_MAJOR_VER_NO/$(SIMVASCULAR_MAJOR_VER_NO)/g\"" | tclsh | cat >> tmp/sedscript
	-echo "puts \"s/REPLACE_SIMVASCULAR_FULL_VER_NO/$(SIMVASCULAR_FULL_VER_NO)/g\"" | tclsh | cat >> tmp/sedscript
	-echo "puts \"s/REPLACE_SIMVASCULAR_CLUSTER/$(CLUSTER)/g\"" | tclsh | cat >> tmp/sedscript
	-echo "puts \"s/REPLACE_TCL_VERSION_NO/$(notdir $(TCL_LIBRARY))/g\"" | tclsh | cat >> tmp/sedscript
	-echo "puts \"s/REPLACE_TK_VERSION_NO/$(notdir $(TK_LIBRARY))/g\"" | tclsh | cat >> tmp/sedscript
	-echo "puts \"s/REPLACE_TCLSH/$(notdir $(TCLSH))/g\"" | tclsh | cat >> tmp/sedscript

common-pre-distribution: tmp/sedscript
	-date > tmp/release-date
	-rm -f tar_files/*.tar
	-rm -f tar_files/*.tar.gz
	-rm -Rf $(DIST_DIR_TOP)
	-mkdir -p $(DIST_DIR)
	-date > $(DIST_DIR)/release-date
	-chmod -R a+rwx $(DIST_DIR_TOP)/../../package
	-echo "puts \"[file tail $(DIST_DIR)]\"" | tclsh > tmp/timestamp
	-cp -f License.rtf $(DIST_DIR)

copy-sv-executables:
	-cp -f $(TOP)/Bin/simvascular.exe $(DIST_DIR)/simvascular-bin.exe
	-cp -f $(TOP)/Bin/presolver.exe $(DIST_DIR)/presolver-bin.exe
	-cp -f $(TOP)/Bin/postsolver.exe $(DIST_DIR)/postsolver-bin.exe
	-cp -f $(TOP)/Bin/flowsolver.exe $(DIST_DIR)/flowsolver-bin.exe
	-cp -f $(TOP)/Bin/tetadaptor.exe $(DIST_DIR)/tetadaptor-bin.exe
	-cp -f $(TOP)/../Code/Modules/ThreeDSolver/IncompressibleSolver/Solver/default.inp $(DIST_DIR)
#	-cp -f $(TOP)/Lib/*.$(SOEXT)* $(DIST_DIR)

copy-sv-tcl-code:
	-mkdir -p $(DIST_DIR)/Tcl
	-svn export --force $(TOP)/../Tcl $(DIST_DIR)/UnTcl
         # relace the full version number in the tcl launch script
	-sed -f tmp/sedscript $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular_startup.tcl > $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular_startup.tcl.new
	-mv $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular_startup.tcl.new $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular_startup.tcl
	-sed -f tmp/sedscript $(DIST_DIR)/UnTcl/SimVascular_2.0/GUI/splash.tcl > $(DIST_DIR)/UnTcl/SimVascular_2.0/GUI/splash.tcl.new
	-mv $(DIST_DIR)/UnTcl/SimVascular_2.0/GUI/splash.tcl.new $(DIST_DIR)/UnTcl/SimVascular_2.0/GUI/splash.tcl
	-tclsh windows_msi_helpers/prepare_simvascular_tcl_release.tcl $(DIST_DIR)
	-cp -f $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular_startup.tcl $(DIST_DIR)/Tcl/SimVascular_2.0
	-cp -f $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular_vtk_init.tcl $(DIST_DIR)/Tcl/SimVascular_2.0
	-cp -f $(DIST_DIR)/UnTcl/SimVascular_2.0/launch_xf.tcl $(DIST_DIR)/Tcl/SimVascular_2.0
	-cp -R $(DIST_DIR)/UnTcl/External $(DIST_DIR)/Tcl
	-cp -R $(DIST_DIR)/UnTcl/Help $(DIST_DIR)/Tcl
	-cp $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular_logo.jpg $(DIST_DIR)/Tcl/SimVascular_2.0
	-cp $(DIST_DIR)/UnTcl/SimVascular_2.0/splash.gif $(DIST_DIR)/Tcl/SimVascular_2.0
	-cp $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular.rc $(DIST_DIR)
	-rm -Rf $(DIST_DIR)/UnTcl
	-tclsh tcl_helper_scripts/create_tclIndex.tcl $(DIST_DIR)/Tcl/SimVascular_2.0

copy-tcl-libs:
	-cp -fR $(TCL_BASE)/lib $(DIST_DIR)
	-cp -fR $(TCL_BASE)/bin $(DIST_DIR)
	-cp -fR $(TCL_BASE)/bin/*.$(SOEXT)* $(DIST_DIR)
	-cp -fR $(TCL_BASE)/lib/*.$(SOEXT)* $(DIST_DIR)
	-cp -fR $(TKCXIMAGE_DLL) $(DIST_DIR)

copy-external-libs:
	-mkdir -p $(DIST_DIR)/intel_compiler_runtime_libs
	-cp -f -p $(INTEL_COMPILER_SO_PATH)/*.$(SOEXT)* $(DIST_DIR)/intel_compiler_runtime_libs
#ifeq ($(MAKE_WITH_PARASOLID),1)
#	-cp -f $(PARASOLID_DLLS) $(DIST_DIR)
#	-cp -Rf $(P_SCHEMA) $(DIST_DIR)/schema
#endif
#ifeq ($(MAKE_WITH_MESHSIM),1)
#	-cp -f ../Licenses/MeshSim/license.dat $(DIST_DIR)/meshsim-license.dat
#endif
ifeq ($(MAKE_WITH_LESLIB),1)
	-cp -f ../Licenses/LesLib/license.dat $(DIST_DIR)/license.dat
endif
	-cp -fd $(VTK_LIBDIRS)/*.$(SOEXT)* $(DIST_DIR)
ifeq ($(MAKE_WITH_ZLIB),1)
	-cp -f $(ZLIB_LIBDIR)/zlib.$(SOEXT)* $(DIST_DIR)
endif
ifeq ($(MAKE_WITH_ITK),1)
	-cp -f $(ITK_LIBDIRS)/*.$(SOEXT)* $(DIST_DIR)
endif

copy-external-libs-windows-only:
	-cp -f dicom2/dicom2-win32.exe $(DIST_DIR)/dicom2.exe
	-cp -f dcmtk/dcmdump-win32.exe $(DIST_DIR)/dcmdump.exe
	-cp -f dcmtk/dcmodify-win32.exe $(DIST_DIR)/dcmodify.exe
	-cp $(GLIB_DLLS) $(DIST_DIR)
ifeq ($(FORTRAN_COMPILER_VERSION),mingw-gfortran)
	-cp -f /usr/x86_64-w64-mingw32/sys-root/mingw/bin/*.dll $(DIST_DIR)
endif

copy-external-libs-linux-only:
	-cp -f dicom2/dicom2-linux.exe $(DIST_DIR)/dicom2
	-cp -f dcmtk/dcmdump-linux.exe $(DIST_DIR)/dcmdump
	-cp -f dcmtk/dcmodify-linux.exe $(DIST_DIR)/dcmodify

generate-wrapper-scripts:
	-sed -f tmp/sedscript $(SIMVASCULAR_OS)_wrapper_scripts/$(SIMVASCULAR_OS)-dist-core-script > $(DIST_DIR)/$(SIMVASCULAR_VERSION)
	-sed -f tmp/sedscript $(SIMVASCULAR_OS)_wrapper_scripts/$(SIMVASCULAR_OS)-dist-core-script > $(DIST_DIR)/adaptor$(SIMVASCULAR_POSTFIX)
	-sed -f tmp/sedscript $(SIMVASCULAR_OS)_wrapper_scripts/$(SIMVASCULAR_OS)-dist-core-script > $(DIST_DIR)/tetadaptor$(SIMVASCULAR_POSTFIX)
	-sed -f tmp/sedscript $(SIMVASCULAR_OS)_wrapper_scripts/$(SIMVASCULAR_OS)-dist-core-script > $(DIST_DIR)/flowsolver$(SIMVASCULAR_POSTFIX)
	-sed -f tmp/sedscript $(SIMVASCULAR_OS)_wrapper_scripts/$(SIMVASCULAR_OS)-dist-core-script > $(DIST_DIR)/presolver$(SIMVASCULAR_POSTFIX)
	-sed -f tmp/sedscript $(SIMVASCULAR_OS)_wrapper_scripts/$(SIMVASCULAR_OS)-dist-core-script > $(DIST_DIR)/postsolver$(SIMVASCULAR_POSTFIX)
	-sed -f tmp/sedscript $(SIMVASCULAR_OS)_wrapper_scripts/$(SIMVASCULAR_OS)-dist-core-script > $(DIST_DIR)/xfsv$(SIMVASCULAR_POSTFIX)
	-sed -f tmp/sedscript $(SIMVASCULAR_OS)_wrapper_scripts/$(SIMVASCULAR_OS)-dist-simvascular-script >> $(DIST_DIR)/$(SIMVASCULAR_VERSION)
	-sed -f tmp/sedscript $(SIMVASCULAR_OS)_wrapper_scripts/$(SIMVASCULAR_OS)-dist-adaptor-script >> $(DIST_DIR)/adaptor$(SIMVASCULAR_POSTFIX)
	-sed -f tmp/sedscript $(SIMVASCULAR_OS)_wrapper_scripts/$(SIMVASCULAR_OS)-dist-tetadaptor-script >> $(DIST_DIR)/tetadaptor$(SIMVASCULAR_POSTFIX)
	-sed -f tmp/sedscript $(SIMVASCULAR_OS)_wrapper_scripts/$(SIMVASCULAR_OS)-dist-flowsolver-script >> $(DIST_DIR)/flowsolver$(SIMVASCULAR_POSTFIX)
	-sed -f tmp/sedscript $(SIMVASCULAR_OS)_wrapper_scripts/$(SIMVASCULAR_OS)-dist-presolver-script >> $(DIST_DIR)/presolver$(SIMVASCULAR_POSTFIX)
	-sed -f tmp/sedscript $(SIMVASCULAR_OS)_wrapper_scripts/$(SIMVASCULAR_OS)-dist-postsolver-script >> $(DIST_DIR)/postsolver$(SIMVASCULAR_POSTFIX)
	-sed -f tmp/sedscript $(SIMVASCULAR_OS)_wrapper_scripts/$(SIMVASCULAR_OS)-dist-xfsv-script >> $(DIST_DIR)/xfsv$(SIMVASCULAR_POSTFIX)
	-cp -f $(SIMVASCULAR_OS)_wrapper_scripts/$(SIMVASCULAR_OS)-simvascular-cmdline-script $(DIST_DIR)/$(SIMVASCULAR_VERSION)-cmdline
	-cat $(DIST_DIR)/$(SIMVASCULAR_VERSION) >> $(DIST_DIR)/$(SIMVASCULAR_VERSION)-cmdline

common-post-distribution:
	-sed -f tmp/sedscript platform_independent_scripts/post-install.sh > $(DIST_DIR)/post-install.sh
	-sed -f tmp/sedscript platform_independent_scripts/post-solver-install.sh > $(DIST_DIR)/post-solver-install.sh
	-sed -f tmp/sedscript platform_independent_scripts/generic-launch-script > $(DIST_DIR)/generic_launch_script
	-chmod -f a+rx $(DIST_DIR)/*.$(SOEXT)
	-chmod -f a+rx $(DIST_DIR)/*.sh
	-chmod -f a+rx $(DIST_DIR)/generic_launch_script
	-chmod -f a+rx $(DIST_DIR)/$(SIMVASCULAR_VERSION)
	-chmod -f a+rx $(DIST_DIR)/$(SIMVASCULAR_VERSION)-cmdline
	-chmod -f a+rx $(DIST_DIR)/adaptor$(SIMVASCULAR_POSTFIX)
	-chmod -f a+rx $(DIST_DIR)/tetadaptor$(SIMVASCULAR_POSTFIX)
	-chmod -f a+rx $(DIST_DIR)/flowsolver$(SIMVASCULAR_POSTFIX)
	-chmod -f a+rx $(DIST_DIR)/presolver$(SIMVASCULAR_POSTFIX)
	-chmod -f a+rx $(DIST_DIR)/postsolver$(SIMVASCULAR_POSTFIX)
	-chmod -f a+rx $(DIST_DIR)/*.$(SOEXT)*
	-chmod -f -R a+rx $(DIST_DIR)
	-chmod -f -R u+w $(DIST_DIR)

linux-distribution: mksedscript common-pre-distribution copy-sv-executables copy-sv-tcl-code copy-tcl-libs copy-external-libs copy-external-libs-linux-only generate-wrapper-scripts common-post-distribution

windows-distribution: mksedscript common-pre-distribution copy-sv-executables copy-sv-tcl-code copy-tcl-libs copy-external-libs copy-external-libs-windows-only generate-wrapper-scripts common-post-distribution

taritup-meshsim-and-parasolid:
	-mkdir tar_files
	-rm -Rf package-release-meshsim-and-parasolid
	-mkdir -p package-release-meshsim-and-parasolid/package/$(SIMVASCULAR_VERSION)/xxxxxxxx
	-date > package-release-meshsim-and-parasolid/release-date
	-chmod -R a+rwx package-release-meshsim-and-parasolid
	-echo "puts \"[file tail $(DIST_DIR)]\"" | tclsh > tmp/timestamp
	-cp -f tmp/timestamp package-release-meshsim-and-parasolid/
	-cp -f $(TOP)/Lib/*.$(SOEXT)* package-release-meshsim-and-parasolid
ifeq ($(MAKE_WITH_PARASOLID),1)
	-cp -f $(PARASOLID_DLLS) package-release-meshsim-and-parasolid
	-cp -Rf $(P_SCHEMA) package-release-meshsim-and-parasolid/schema
endif
	-cp -f $(TOP)/Bin/adaptor.exe package-release-meshsim-and-parasolid/adaptor-bin.exe
	-tar --preserve-permissions --transform 's,package-release-meshsim-and-parasolid/,,g'  -cvf tar_files/simvascular-meshsim-and-parasolid-$(SIMVASCULAR_OS)-$(SIMVASCULAR_PLATFORM).`awk '{print $$2"-"$$3"-"$$6}' tmp/release-date`.tar package-release-meshsim-and-parasolid
	-gzip tar_files/simvascular-meshsim-and-parasolid-$(SIMVASCULAR_OS)-$(SIMVASCULAR_PLATFORM).`awk '{print $$2"-"$$3"-"$$6}' tmp/release-date`.tar

taritup:
	-mkdir tar_files
	-tar --preserve-permissions -cvf tar_files/simvascular-$(SIMVASCULAR_OS)-$(SIMVASCULAR_PLATFORM).`awk '{print $$2"-"$$3"-"$$6}' tmp/release-date`.tar package
	-gzip tar_files/simvascular-$(SIMVASCULAR_OS)-$(SIMVASCULAR_PLATFORM).`awk '{print $$2"-"$$3"-"$$6}' tmp/release-date`.tar
