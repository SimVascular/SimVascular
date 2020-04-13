# Hey emacs, this is a -*- makefile -*-

# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
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
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

TOP = ..

include $(TOP)/include.mk

#
#  The following are helper scripts to create PC
#  distributions.  Note that some of the paths are hardcoded.
#

SV_CLUSTER = $(CLUSTER)

ifeq ($(CLUSTER),x64_cygwin)
  include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/wix-3.14.x64_cygwin.mk
endif

ifeq ($(CLUSTER),x64_cygwin)
  GCHMOD=chmod
  GCP=cp
  GDATE=date
  GECHO=echo
  GFIND=find
  GMKDIR=mkdir
  GMV=mv
  GRM=rm
  GSED=sed
  GTAR=tar
  GZIP=gzip
endif

ifeq ($(CLUSTER),x64_linux)
  GCHMOD=chmod
  GCP=cp
  GDATE=date
  GECHO=echo
  GFIND=find
  GMKDIR=mkdir
  GMV=mv
  GRM=rm
  GSED=sed
  GTAR=tar
  GZIP=gzip
endif

ifeq ($(CLUSTER),x64_macosx)
  GCHMOD=gchmod
  GCP=gcp
  GDATE=gdate
  GECHO=gecho
  GFIND=gfind
  GMKDIR=gmkdir
  GMV=gmv
  GRM=grm
  GSED=gsed
  GTAR=gtar
  GZIP=gzip
endif

SV_TIMESTAMP ?= $(shell $(GDATE) +"%Y-%m-%d")
DIST_DIR_TOP = package/$(SV_VERSION)
DIST_DIR := $(DIST_DIR_TOP)/$(SV_TIMESTAMP)

REAL_DIST_DIR_TOP = package/$(SV_VERSION)
REAL_DIST_DIR := $(DIST_DIR_TOP)/$(SV_TIMESTAMP)

ifeq ($(CLUSTER), x64_cygwin)
  DIST_DIR_TOP := /cygdrive/p/package/$(SV_VERSION)
  DIST_DIR := /cygdrive/p/package/$(SV_VERSION)/$(SV_TIMESTAMP)
else
  DIST_DIR_TOP := $(REAL_DIST_DIR_TOP)
  DIST_DIR := $(REAL_DIST_DIR)
endif

ifeq ($(CLUSTER),x64_cygwin)
  all: use_subst windows-distribution msi msi-bundle sv-bundle-for-plugins
else
  all: linux-distribution taritup
endif

use_subst:
ifeq ($(CLUSTER),x64_cygwin)
	-subst P: /D
	-subst P: $(shell cygpath -w `pwd`)
endif

veryclean: use_subst msiclean msi-bundle-clean
	-$(GRM) -f tar_files/*.tar
	-$(GRM) -f tar_files/*.tar.gz
	-$(GRM) -Rf $(DIST_DIR_TOP)
	-$(GRM) -Rf package
	-$(GRM) -Rf SimVascularLibsAndHeaders
	-$(GRM) -Rf tmp/*

#
# create script for sv
#
sv-startup-configure:
	-$(GRM) tmp/startup_configure.tcl
	-$(GECHO) "set SV_BUILD_ID     $(SV_TIMESTAMP)" > tmp/startup_configure.tcl
	-$(GECHO) "set SV_FULL_VER_NO  $(SV_FULL_VER_NO)" >> tmp/startup_configure.tcl
	-$(GECHO) "set SV_MAJOR_VER_NO $(SV_MAJOR_VER_NO)" >> tmp/startup_configure.tcl
	-$(GECHO) "set SV_TIMESTAMP    $(SV_TIMESTAMP)" >> tmp/startup_configure.tcl
	-$(GECHO) "set SV_PLATFORM     $(SV_PLATFORM)" >> tmp/startup_configure.tcl
	-$(GECHO) "set SV_VERSION      $(SV_VERSION)" >> tmp/startup_configure.tcl
ifeq ($(SV_USE_PYTHON),1)
	-$(GECHO) "set SV_USE_PYTHON   ON" >> tmp/startup_configure.tcl
else
	-$(GECHO) "set SV_USE_PYTHON   OFF" >> tmp/startup_configure.tcl
endif

#
#  Create msi installer for SimVascular
#

msiclean:
	-$(GRM) -f *~
	-$(GRM) -f $(SV_VERSION)*.msi
	-$(GRM) -f tmp/simvascular.wixpdb
	-$(GRM) -f simvascular.wixpdb
	-$(GRM) -f tmp/sv.wixobj
	-$(GRM) -f tmp/sv.wxs
	-$(GRM) -f tmp/uuidgen.exe*
	-$(GRM) -f tmp/uuidgen.obj*

msi-simvascular: use_subst msiclean
	-$(GFIND) $(DIST_DIR) -name "*.pyc" -delete
	-$(GFIND) $(DIST_DIR) -name "__pycache__" -delete
ifeq ($(CXX_COMPILER_VERSION),mingw-gcc)
	x86_64-w64-mingw32-gcc -o tmp/uuidgen.exe windows_msi_helpers/uuidgen.c -lole32
else
	CL /Fetmp/uuidgen.exe /Fotmp/uuidgen.obj windows_msi_helpers/uuidgen.c ole32.lib
endif
	-$(GECHO) "Create WXS file..."
	-tclsh windows_msi_helpers/makewxs.tcl $(SV_VERSION) $(SV_PLATFORM) $(DIST_DIR_TOP) $(SV_MAJOR_VER_NO) Releases$(SV_POSTFIX) sv.bat $(SV_VERSION) $(SV_FULL_VER_NO) $(TCL_LIBRARY) $(TK_LIBRARY) $(PYTHON_MAJOR_VERSION) $(PYTHON_MINOR_VERSION) $(SV_CMAKE_BUILD_TYPE)
	-$(GECHO) "Run candle..."
	-$(WIXUI_PATH)/candle.exe -ext WixUIExtension -ext WixUtilExtension -out tmp/sv.wixobj tmp/sv.wxs
	-$(GECHO) "Run light..."
	-$(WIXUI_PATH)/light.exe -ext WixUIExtension -ext WixUtilExtension -sice:ICE60 -out $(SV_VERSION)-$(SV_TIMESTAMP)-Windows-64bit.msi tmp/sv.wixobj

msi-bundle-clean:
	-$(GRM) -f tmp/bundle.wixobj
	-$(GRM) -f simvascular-*-bundle.wixpdb
	-$(GRM) -f simvascular-*-bundle.exe
	-$(GRM) -f tmp/wix-simvascular-bundle.wxs

msi-bundle: msi-bundle-clean mksedscript
	-$(GECHO) "Create WXS file..."
	-tclsh windows_msi_helpers/simvascular-bundle-make-wxs.tcl $(SV_VERSION) $(SV_FULL_VER_NO) $(SV_TIMESTAMP) tmp/wix-simvascular-bundle.wxs
	-$(GECHO) "Run candle..."
	-$(WIXUI_PATH)/candle.exe -ext WixBalExtension -ext WixUIExtension -ext WixUtilExtension -out tmp/bundle.wixobj tmp/wix-simvascular-bundle.wxs
	-$(GECHO) "Run light..."
	-$(WIXUI_PATH)/light.exe tmp/bundle.wixobj -ext WixBalExtension -ext WixUIExtension -ext WixUtilExtension -out $(SV_VERSION)-$(SV_TIMESTAMP)-Windows-64bit-bundle.exe

msi: msiclean msi-simvascular

mksedscript:
	-$(GMKDIR) -p tmp
	-$(GECHO) "puts \"s/REPLACE_TIMESTAMP/[file tail $(DIST_DIR)]/g\"" | tclsh | cat > tmp/sedscript
	-$(GECHO) "puts \"s/REPLACE_SV_TIMESTAMP/[file tail $(DIST_DIR)]/g\"" | tclsh | cat >> tmp/sedscript
	-$(GECHO) "puts \"s/REPLACE_SV_OS/$(SV_OS)/g\"" | tclsh | cat >> tmp/sedscript
	-$(GECHO) "puts \"s/REPLACE_SV_VERSION/$(SV_VERSION)/g\"" | tclsh | cat >> tmp/sedscript
	-$(GECHO) "puts \"s/REPLACE_SV_PLATFORM/$(SV_PLATFORM)/g\"" | tclsh | cat >> tmp/sedscript
	-$(GECHO) "puts \"s/REPLACE_SV_POSTFIX/$(SV_POSTFIX)/g\"" | tclsh | cat >> tmp/sedscript
	-$(GECHO) "puts \"s/REPLACE_SV_MAJOR_VER_NO/$(SV_MAJOR_VER_NO)/g\"" | tclsh | cat >> tmp/sedscript
	-$(GECHO) "puts \"s/REPLACE_SV_FULL_VER_NO/$(SV_FULL_VER_NO)/g\"" | tclsh | cat >> tmp/sedscript
	-$(GECHO) "puts \"s/REPLACE_SV_CLUSTER/$(CLUSTER)/g\"" | tclsh | cat >> tmp/sedscript
	-$(GECHO) "puts \"s/REPLACE_TCL_VERSION_NO/$(notdir $(TCL_LIBRARY))/g\"" | tclsh | cat >> tmp/sedscript
	-$(GECHO) "puts \"s/REPLACE_TK_VERSION_NO/$(notdir $(TK_LIBRARY))/g\"" | tclsh | cat >> tmp/sedscript
	-$(GECHO) "puts \"s/REPLACE_TCLSH/$(notdir $(TCLSH))/g\"" | tclsh | cat >> tmp/sedscript
	-$(GECHO) "puts \"s/REPLACE_PYTHON_MAJOR_VERSION/$(PYTHON_MAJOR_VERSION)/g\"" | tclsh | cat >> tmp/sedscript
	-$(GECHO) "puts \"s/REPLACE_PYTHON_MINOR_VERSION/$(PYTHON_MINOR_VERSION)/g\"" | tclsh | cat >> tmp/sedscript
	-$(GECHO) "puts \"s/REPLACEME_QT_WEBENGINE_PROC/$(QT_WEBENGINE_PROC)/g\"" | tclsh | cat >> tmp/sedscript

common-pre-distribution: tmp/sedscript
	-$(GDATE) > tmp/release-date
	-$(GRM) -f tar_files/*.tar
	-$(GRM) -f tar_files/*.tar.gz
	-$(GRM) -Rf $(DIST_DIR_TOP)
	-$(GMKDIR) -p $(DIST_DIR)
	-$(GDATE) > $(DIST_DIR)/release-date
	-$(GCHMOD) -R a+rwx $(DIST_DIR_TOP)/../../package
	-$(GECHO) "puts \"[file tail $(DIST_DIR)]\"" | tclsh > tmp/timestamp
	-$(GCP) -f License.rtf $(DIST_DIR)

copy-sv-executables:
	-$(GCP) -f $(TOP)/Bin/simvascular.exe $(DIST_DIR)/simvascular-bin.exe
	-$(GCP) -f $(TOP)/Lib/$(LIB_BUILD_DIR)/*simvascular_globals*.$(SOEXT)* $(DIST_DIR)
ifeq ($(SV_USE_PYTHON),1)
	-$(GCP) -f $(TOP)/Lib/$(LIB_BUILD_DIR)/*simvascular_python_interp*.$(SOEXT)* $(DIST_DIR)
endif
	-$(GCP) -f $(TOP)/Lib/$(LIB_BUILD_DIR)/*simvascular_*.$(SOEXT)* $(DIST_DIR)
	-$(GCP) -fR $(TOP)/Bin/plugins $(DIST_DIR)
	-$(GCP) -fR $(TOP)/Bin/QtWebEngine $(DIST_DIR)
ifeq ($(CLUSTER), x64_cygwin)
	-$(GCP) -f svOneDSolver/windows/svOneDSolver.exe $(DIST_DIR)/svOneDSolver.exe
endif

copy-sv-tcl-code:
	-$(GMKDIR) -p $(DIST_DIR)/Tcl
	-$(GCP) -Rf $(TOP)/../Tcl $(DIST_DIR)/UnTcl
         # relace the full version number in the tcl launch script
	-$(GSED) -f tmp/sedscript $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular_startup.tcl > $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular_startup.tcl.new
	-$(GMV) $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular_startup.tcl.new $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular_startup.tcl
	-$(GSED) -f tmp/sedscript $(DIST_DIR)/UnTcl/SimVascular_2.0/GUI/splash.tcl > $(DIST_DIR)/UnTcl/SimVascular_2.0/GUI/splash.tcl.new
	-$(GMV) $(DIST_DIR)/UnTcl/SimVascular_2.0/GUI/splash.tcl.new $(DIST_DIR)/UnTcl/SimVascular_2.0/GUI/splash.tcl
	-tclsh windows_msi_helpers/prepare_simvascular_tcl_release.tcl $(DIST_DIR)
	-$(GCP) -f $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular_startup.tcl $(DIST_DIR)/Tcl/SimVascular_2.0
	-$(GCP) -f $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular_vtk_init.tcl $(DIST_DIR)/Tcl/SimVascular_2.0
	-$(GCP) -f $(DIST_DIR)/UnTcl/SimVascular_2.0/launch_xf.tcl $(DIST_DIR)/Tcl/SimVascular_2.0
	-$(GCP) -f $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular_find_external_execs.tcl $(DIST_DIR)/Tcl/SimVascular_2.0
	-$(GCP) -R $(DIST_DIR)/UnTcl/External $(DIST_DIR)/Tcl
	-$(GCP) -R $(DIST_DIR)/UnTcl/Help $(DIST_DIR)/Tcl
	-$(GCP) $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular_logo.jpg $(DIST_DIR)/Tcl/SimVascular_2.0
	-$(GCP) $(DIST_DIR)/UnTcl/SimVascular_2.0/splash.gif $(DIST_DIR)/Tcl/SimVascular_2.0
	-$(GCP) $(DIST_DIR)/UnTcl/SimVascular_2.0/simvascular.rc $(DIST_DIR)
	-$(GRM) -Rf $(DIST_DIR)/UnTcl
	-tclsh tcl_helper_scripts/create_tclIndex.tcl $(DIST_DIR)/Tcl/SimVascular_2.0
	-$(GCP) -f tmp/startup_configure.tcl $(DIST_DIR)/Tcl

copy-tcl-libs:
	-$(GCP) -fR $(TCL_BASE)/lib $(DIST_DIR)
	-$(GCP) -fR $(TCL_BASE)/bin $(DIST_DIR)
	-$(GCP) -fR $(TCL_BASE)/bin/*.$(SOEXT)* $(DIST_DIR)
	-$(GCP) -fR $(TCL_BASE)/lib/*.$(SOEXT)* $(DIST_DIR)
	-$(GCP) -fR $(TKCXIMAGE_DLL) $(DIST_DIR)

copy-external-libs:
ifeq ($(SV_USE_VTK),1)
	-$(GCP) -fd $(VTK_LIBDIRS)/*.$(SOEXT)* $(DIST_DIR)
	-$(GCP) -fd $(VTK_BINDIRS)/*.$(SOEXT)* $(DIST_DIR)
	-$(GCP) -fR $(VTK_TCL_LIB_PATH) $(DIST_DIR)/lib/vtk
endif
#ifeq ($(SV_USE_ZLIB),1)
#	-$(GCP) -f $(ZLIB_LIBDIR)/zlib.$(SOEXT)* $(DIST_DIR)
#endif
ifeq ($(SV_USE_ITK),1)
	-$(GCP) -f $(ITK_BIN_LIBS)/*.$(SOEXT)* $(DIST_DIR)
endif
ifeq ($(SV_USE_HDF5),1)
	-$(GCP) -f $(SV_HDF5_DLLS)/*.$(SOEXT)* $(DIST_DIR)
endif
ifeq ($(SV_USE_GDCM),1)
	-$(GCP) -f $(SV_GDCM_DLLS)/*gdcm*.$(SOEXT)* $(DIST_DIR)
ifeq ($(CLUSTER),x64_cygwin)
	-$(GCP) -f $(SV_GDCM_DLLS)/gdcm*.exe* $(DIST_DIR)
else
	-$(GCP) -fR $(SV_GDCM_DLLS)/gdcm* $(DIST_DIR)
endif
	-$(GCP) -f $(SV_GDCM_DLLS)/socketxx.$(SOEXT)* $(DIST_DIR)
endif
ifeq ($(SV_USE_PYTHON),1)
#	-$(GCP) -fR $(PYTHON_HOME)/Lib $(DIST_DIR)/python-packages
#	-$(GCP) -fR $(VTK_PYTHON_PACKAGES) $(DIST_DIR)/python-packages
	-$(GCP) -fR $(PYTHON_HOME) $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)
ifeq ($(CLUSTER), x64_cygwin)
	-$(GCP) -fR $(VTK_PYTHON_PACKAGES)/* $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/Lib/site-packages
else
	-$(GCP) -fR $(VTK_PYTHON_PACKAGES)/* $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/lib/python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/site-packages
endif
ifeq ($(CLUSTER), x64_cygwin)
	-$(GCP) -fR $(TOP)/../Python/site-packages/* $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/Lib/site-packages
else
	-$(GCP) -fR $(TOP)/../Python/site-packages/* $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/lib/python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/site-packages
endif
	-$(GCP) -f $(SV_PYTHON_SO_PATH)/*.$(SOEXT)* $(DIST_DIR)
	for fn in $(notdir $(wildcard $(PYTHON_HOME)/bin-relocate/*)); do \
	  $(GECHO) "File -> $$fn"; \
	  $(GSED) -e "s+#!/usr/local/sv/svpython/bin/python+#!/usr/local/sv/svpython/bin/svpython+g;s+/usr/local/sv/svpython+/usr/local/$(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)+g" $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/bin-relocate/$$fn > $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/bin/$$fn; \
	done
	# undo damage to real executable from sed above
	-$(GCP) -f $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/bin-relocate/python $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/bin/python
ifeq ($(CLUSTER), x64_cygwin)
	-$(GCP) -fR Jupyter/windows/share/* $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/share
	-$(GCP) -fR Jupyter/windows/site-packages/* $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/Lib/site-packages
ifeq ($(CLUSTER), x64_cygwin)
	-$(GCP) -f Jupyter/windows/Scripts/* $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/Scripts
endif
	-$(GCP) -f $(PYTHON_HOME)/bin/* $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/Scripts
	-$(GSED) -f tmp/sedscript Jupyter/windows/site-packages/simvascular_tcl_kernel/kernel.py > $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/Lib/site-packages/simvascular_tcl_kernel/kernel.py
	-$(GSED) -f tmp/sedscript Jupyter/windows/site-packages/simvascular_python_kernel/kernel.py > $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/Lib/site-packages/simvascular_python_kernel/kernel.py
else
	-$(GSED) -f tmp/sedscript Jupyter/linux/bin/sv_jupyter_launch.sh > $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/bin/sv_jupyter_launch.sh
	-$(GCHMOD) a+rx $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/bin/sv_jupyter_launch.sh
	-$(GMKDIR) -p $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/share/jupyter/kernels/echo
	-$(GMKDIR) -p $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/share/jupyter/kernels/simvascular_python
	-$(GMKDIR) -p $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/share/jupyter/kernels/simvascular_tcl
	-$(GSED) -f tmp/sedscript Jupyter/linux/share/jupyter/kernels/echo/kernel.json > $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/share/jupyter/kernels/echo/kernel.json
	-$(GSED) -f tmp/sedscript Jupyter/linux/share/jupyter/kernels/simvascular_python/kernel.json > $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/share/jupyter/kernels/simvascular_python/kernel.json
	-$(GSED) -f tmp/sedscript Jupyter/linux/share/jupyter/kernels/simvascular_tcl/kernel.json > $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/share/jupyter/kernels/simvascular_tcl/kernel.json
	-$(GCP) -fR Jupyter/linux/site-packages/* $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/lib/python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/site-packages
	-$(GSED) -f tmp/sedscript Jupyter/linux/site-packages/simvascular_tcl_kernel/kernel.py > $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/lib/python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/site-packages/simvascular_tcl_kernel/kernel.py
	-$(GSED) -f tmp/sedscript Jupyter/linux/site-packages/simvascular_python_kernel/kernel.py > $(DIST_DIR)/Python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/lib/python$(PYTHON_MAJOR_VERSION).$(PYTHON_MINOR_VERSION)/site-packages/simvascular_python_kernel/kernel.py
endif
#	-$(GCP) -Rf $(TOP)/../Python $(DIST_DIR)/Python
        # need to delete byte-code compiled code if it exists since this can break
        # installation
	-$(GFIND) $(DIST_DIR) -name "*.pyc" -delete
	-$(GFIND) $(DIST_DIR) -name "__pycache__" -delete
endif
ifeq ($(SV_USE_QT),1)
  ifeq ($(CLUSTER), x64_cygwin)
  # need dummy exec since windeployqt no longer recognizes are exe as a qt app...
  # also need QtWebEngine in the dir with exe unless we set env variable!
	-$(GCP) -f $(QT_WEBENGINE_PATH) $(DIST_DIR)
	-$(QT_TOP_DIR)/bin/windeployqt $(QT_DEPLOY_FLAGS) \
                                       -plugindir `cygpath -m $(DIST_DIR)/qt-plugins` \
                                       `cygpath -m $(DIST_DIR)/$(QT_WEBENGINE_PROC)`
	-$(GCP) -fR $(QT_QPA_FONTDIR) $(DIST_DIR)/fonts
	-$(GCP) -fR $(QT_QPA_FONTDIR) $(DIST_DIR)/resources
	-$(GECHO) "[Paths]" > $(DIST_DIR)/qt.conf
	-$(GECHO) "Prefix=\".\"" >> $(DIST_DIR)/qt.conf
  endif
  ifeq ($(CLUSTER), x64_macosx)
  # need dummy exec since maceployqt no longer recognizes are exe as a qt app...
  # also need QtWebEngine in the dir with exe unless we set env variable!
	-$(GCP) -f $(QT_WEBENGINE_PATH) $(DIST_DIR)
	-$(GCP) -fR $(QT_QPA_FONTDIR)/* $(DIST_DIR)/QtWebEngine
	-$(GCP) -fd $(QT_LIBDIRS)/*.$(SOEXT)* $(DIST_DIR)
	-$(GCP) -fR $(QT_PLUGIN_PATH) $(DIST_DIR)/qt-plugins
	-$(GECHO) "[Paths]" > $(DIST_DIR)/qt.conf
	-$(GECHO) "Prefix=\".\"" >> $(DIST_DIR)/qt.conf
	-$(GECHO) "Data=\"QtWebEngine\"" >> $(DIST_DIR)/qt.conf
	-$(GECHO) "Translations=\"QtWebEngine\"" >> $(DIST_DIR)/qt.conf
  endif
  ifeq ($(CLUSTER), x64_linux)
	-$(GCP) -fR $(QT_QPA_FONTDIR) $(DIST_DIR)/qt-fonts
	-$(GCP) -fd $(QT_LIBDIRS)/*.$(SOEXT)* $(DIST_DIR)
	-$(GCP) -fR $(QT_PLUGIN_PATH) $(DIST_DIR)/qt-plugins
	-$(GECHO) "[Paths]" > $(DIST_DIR)/qt.conf
	-$(GECHO) "Prefix=\".\"" >> $(DIST_DIR)/qt.conf
	-$(GECHO) "Data=\"QtWebEngine\"" >> $(DIST_DIR)/qt.conf
	-$(GECHO) "Translations=\"QtWebEngine\"" >> $(DIST_DIR)/qt.conf
  endif
endif
ifeq ($(SV_USE_OPENCASCADE),1)
	-$(GCP) -fd $(SV_OPENCASCADE_SO_PATH)/*.$(SOEXT)* $(DIST_DIR)
endif
ifeq ($(SV_USE_FREETYPE),1)
	-$(GCP) -fd $(SV_FREETYPE_SO_PATH)/*.$(SOEXT)* $(DIST_DIR)
endif
ifeq ($(SV_USE_HDF5),1)
	-$(GCP) -fd $(SV_HDF5_SO_PATH)/*.$(SOEXT)* $(DIST_DIR)
endif
ifeq ($(SV_USE_MITK),1)
	-$(GMKDIR) -p $(DIST_DIR)/mitk
ifeq ($(CLUSTER), x64_cygwin)
	-$(GCP) -fRd $(MITK_BINDIRS) $(DIST_DIR)/mitk
#	-$(GMKDIR) -p $(DIST_DIR)/mitk/bin/plugins
#	-$(GCP) -fR $(SV_MITK_PLUGIN_PATH) $(DIST_DIR)/mitk/bin/plugins
#	-$(GCP) -f $(SV_MITK_SO_PATH)/liborg*.$(SOEXT)* $(DIST_DIR)/mitk/bin/plugins/RelWithDebInfo
else
	-$(GMKDIR) -p $(DIST_DIR)/mitk/bin
	-$(GMKDIR) -p $(DIST_DIR)/mitk/lib/plugins
	-$(GMKDIR) -p $(DIST_DIR)/mitk/lib64
	-$(GCP) -fR $(MITK_BINDIRS)/dcm* $(DIST_DIR)
	-$(GCP) -f $(MITK_LIBDIRS)/*.$(SOEXT)* $(DIST_DIR)/mitk/lib
	-$(GCP) -f $(MITK_LIB64DIR)/*.$(SOEXT)* $(DIST_DIR)/mitk/lib64
	-$(GCP) -fR $(MITK_LIBDIRS)/MitkCore $(DIST_DIR)/mitk/lib
	-$(GCP) -fR $(MITK_LIBDIRS)/MitkPython $(DIST_DIR)/mitk/lib
	-$(GCP) -fR $(MITK_LIBDIRS)/Python $(DIST_DIR)/mitk/lib
	-$(GCP) -f $(SV_MITK_PLUGIN_PATH)/* $(DIST_DIR)/mitk/lib/plugins
	-$(GCP) -f $(SV_MITK_SO_PATH)/liborg*.$(SOEXT)* $(DIST_DIR)/mitk/lib/plugins
        # need to delete byte-code compiled code if it exists since this can break
        # installation
	-$(GFIND) $(DIST_DIR) -name "*.pyc" -delete
	-$(GFIND) $(DIST_DIR) -name "__pycache__" -delete
endif
endif

copy-external-libs-windows-only:
	-$(GCP) -f dicom2/dicom2-win32.exe $(DIST_DIR)/dicom2.exe
ifeq ($(USE_GLIB),1)
	-$(GCP) $(GLIB_DLLS) $(DIST_DIR)
endif
ifeq ($(FORTRAN_COMPILER_VERSION),mingw-gfortran)
	-$(GCP) -f /usr/x86_64-w64-mingw32/sys-root/mingw/bin/*.dll $(DIST_DIR)
endif

copy-external-libs-linux-only:
	-$(GCP) -f dicom2/dicom2-linux.exe $(DIST_DIR)/dicom2

generate-wrapper-scripts:
	-$(GSED) -f tmp/sedscript $(SV_OS)_wrapper_scripts/$(SV_OS)-dist-core-script > $(DIST_DIR)/$(SV_VERSION)
	-$(GSED) -f tmp/sedscript $(SV_OS)_wrapper_scripts/$(SV_OS)-dist-core-script > $(DIST_DIR)/xfsv$(SV_POSTFIX)
	-$(GSED) -f tmp/sedscript $(SV_OS)_wrapper_scripts/$(SV_OS)-dist-simvascular-script >> $(DIST_DIR)/$(SV_VERSION)
	-$(GSED) -f tmp/sedscript $(SV_OS)_wrapper_scripts/$(SV_OS)-dist-xfsv-script >> $(DIST_DIR)/xfsv$(SV_POSTFIX)
ifeq ($(CLUSTER),x64_cygwin)
	-$(GSED) s/SV_CMAKE_BUILD_TYPE/$(SV_CMAKE_BUILD_TYPE)/g $(SV_OS)_wrapper_scripts/sv.bat > $(DIST_DIR)/sv.bat
	-$(GCHMOD) a+rx $(DIST_DIR)/sv.bat
endif

sv-bundle-for-plugins:
	-$(GRM) -Rf SimVascularLibsAndHeaders
	-$(GRM) tar_files/simvascular-make-libs-headers-$(SV_OS)-$(SV_PLATFORM).$(SV_TIMESTAMP).tar*
# copy required make files
	-$(GMKDIR) -p SimVascularLibsAndHeaders/$(SV_TIMESTAMP)/BuildWithMake
	-$(GCP) ../*.mk SimVascularLibsAndHeaders/$(SV_TIMESTAMP)/BuildWithMake
	-$(GCP) -R ../MakeHelpers SimVascularLibsAndHeaders/$(SV_TIMESTAMP)/BuildWithMake
	-$(GCP) -R ../TclHelpers SimVascularLibsAndHeaders/$(SV_TIMESTAMP)/BuildWithMake
# copy libs
	-$(GCP) -R ../Lib SimVascularLibsAndHeaders/$(SV_TIMESTAMP)/BuildWithMake
# copy ThirdParty
	-$(GMKDIR) -p SimVascularLibsAndHeaders/$(SV_TIMESTAMP)/Code
	-$(GCP) -R ../../Code/ThirdParty SimVascularLibsAndHeaders/$(SV_TIMESTAMP)/Code
#  copy headers
	-$(GMKDIR) -p SimVascularLibsAndHeaders/$(SV_TIMESTAMP)/Code/Source/Include
	-for i in $(shell $(GFIND) ../../Code/Source -name "*.h"); do \
	     echo "$$i  $(shell $(GBASENAME) $$i)" ; \
	    $(GCP) -fl $$i SimVascularLibsAndHeaders/$(SV_TIMESTAMP)/Code/Source/Include; \
	 done
	-for i in $(shell $(GFIND) ../../Code/Source -name "*.hxx"); do \
	     echo "$$i  $(shell $(GBASENAME) $$i)" ; \
	    $(GCP) -fl $$i SimVascularLibsAndHeaders/$(SV_TIMESTAMP)/Code/Source/Include; \
	 done
	-$(GMKDIR) tar_files
	-$(GTAR) --preserve-permissions -cvf tar_files/simvascular-make-libs-headers-$(SV_OS)-$(SV_PLATFORM).$(SV_TIMESTAMP).tar SimVascularLibsAndHeaders
	-$(GZIP) tar_files/simvascular-make-libs-headers-$(SV_OS)-$(SV_PLATFORM).$(SV_TIMESTAMP).tar

common-post-distribution:
	-$(GSED) -f tmp/sedscript platform_independent_scripts/post-install.sh > $(DIST_DIR)/post-install.sh
	-$(GSED) -f tmp/sedscript platform_independent_scripts/generic-launch-script > $(DIST_DIR)/generic_launch_script
	-$(GCHMOD) -f a+rx $(DIST_DIR)/*.$(SOEXT)
	-$(GCHMOD) -f a+rx $(DIST_DIR)/*.sh
	-$(GCHMOD) -f a+rx $(DIST_DIR)/generic_launch_script
	-$(GCHMOD) -f a+rx $(DIST_DIR)/$(SV_VERSION)
	-$(GCHMOD) -f a+rx $(DIST_DIR)/xfsv
	-$(GCHMOD) -f a+rx $(DIST_DIR)/*.$(SOEXT)*
	-$(GCHMOD) -f -R a+rx $(DIST_DIR)
	-$(GCHMOD) -f -R u+w $(DIST_DIR)

linux-distribution: mksedscript common-pre-distribution copy-sv-executables sv-startup-configure copy-tcl-libs copy-sv-tcl-code copy-external-libs copy-external-libs-linux-only generate-wrapper-scripts common-post-distribution

windows-distribution: mksedscript common-pre-distribution copy-sv-executables sv-startup-configure copy-tcl-libs copy-sv-tcl-code copy-external-libs copy-external-libs-windows-only generate-wrapper-scripts common-post-distribution

taritup:
	-$(GMKDIR) tar_files
	-$(GTAR) --preserve-permissions -cvf tar_files/simvascular-$(SV_OS)-$(SV_PLATFORM).`awk '{print $$2"-"$$3"-"$$6}' tmp/release-date`.tar package
	-$(GZIP) tar_files/simvascular-$(SV_OS)-$(SV_PLATFORM).`awk '{print $$2"-"$$3"-"$$6}' tmp/release-date`.tar
