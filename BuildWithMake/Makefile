# Hey emacs, this is a -*- makefile -*-

# Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                         University of California, San Diego.
# 
# All rights reserved. 
#
# Portions copyright (c) 1999-2007 Stanford University,
# Nathan Wilson, Ken Wang, Charles Taylor.
# 
# See SimVascular Acknowledgements file for additional
# contributors to the source code. 
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

TOP = .

include $(TOP)/include.mk

all-shared: shared-pre-build all-shared-fast

all-shared-fast: static-build shared-build exec-shared-build

all: mixed

mixed: static-pre-build static-build shared-build exec-build

mksedscript:
	if [ -e tclscript ];then /bin/rm -f tclscript;fi
	if [ -e sedscript ];then /bin/rm -f sedscript;fi
	if [ -e sv ];then /bin/rm -f sv;fi
	if [ -e xfsv ];then /bin/rm -f xfsv;fi
	-echo "puts \"s+REPLACEME_SOFTWARE_PLATFORM+$(CLUSTER)+g\"" > tclscript 
	-echo "puts \"s+REPLACEME_CXX_COMPILER_VERSION+$(CXX_COMPILER_VERSION)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_FORTRAN_COMPILER_VERSION+$(FORTRAN_COMPILER_VERSION)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SV_HOME+[file rootname `pwd`]/..+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SV_VERSION+$(SIMVASCULAR_VERSION)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_LIB_BUILD_DIR+$(LIB_BUILD_DIR)+g\"" >> tclscript
ifeq ($(CLUSTER),x64_cygwin)
	-echo "puts \"s+REPLACEME_SV_OS+windows+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_OPEN_SOFTWARE_BINARIES_TOPLEVEL+`cygpath -u $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_LICENSED_SOFTWARE_TOPLEVEL+`cygpath -u $(LICENSED_SOFTWARE_TOPLEVEL)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCL_LIBRARY+`cygpath -u $(TCL_LIBRARY)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TK_LIBRARY+`cygpath -u $(TK_LIBRARY)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCLTK_SO_PATH+`cygpath -u $(TCLTK_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCLSH+`cygpath -u $(TCLSH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TKCXIMAGE_DLL+`cygpath -u $(TKCXIMAGE_DLL)`+g\"" >> tclscript
# leave intel compilers blank for now on windows
#	-echo "puts \"s+REPLACEME_INTEL_COMPILER_SO_PATH+`cygpath -u $(INTEL_COMPILER_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_INTEL_COMPILER_SO_PATH++g\"" >> tclscript
ifeq ($(SV_USE_PARASOLID),1)
	-echo "puts \"s+REPLACEME_PARASOLID_SO_PATH+`cygpath -u $(PARASOLID_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_P_SCHEMA+`cygpath -u $(P_SCHEMA)`+g\"" >> tclscript
endif
ifeq ($(SV_USE_MESHSIM),1)
	-echo "puts \"s+REPLACEME_MESHSIM_SO_PATH+`cygpath -u $(MESHSIM_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SIM_LICENSE_FILE+$(SIM_LICENSE_FILE)+g\"" >> tclscript
endif
ifeq ($(SV_USE_VTK),1)
	-echo "puts \"s+REPLACEME_VTK_SO_PATH+`cygpath -u $(VTK_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_BIN_PATH+`cygpath -u $(VTK_BINDIRS)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_LIB_PATH+`cygpath -u $(VTK_LIBDIRS)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_TCL_LIB_PATH+`cygpath -u $(VTK_TCL_LIB_PATH)`+g\"" >> tclscript
endif
ifeq ($(SV_USE_GDCM),1)
	-echo "puts \"s+REPLACEME_SV_GDCM_SO_PATH+`cygpath -u $(SV_GDCM_SO_PATH)`+g\"" >> tclscript
endif
ifeq ($(SV_USE_ITK),1)
	-echo "puts \"s+REPLACEME_ITK_SO_PATH+`cygpath -u $(ITK_BIN_LIBS)`+g\"" >> tclscript
endif

ifeq ($(SV_USE_FREETYPE),1)
	-echo "puts \"s+REPLACEME_SV_FREETYPE_SO_PATH+`cygpath -u $(SV_FREETYPE_SO_PATH)`+g\"" >> tclscript
endif
ifeq ($(SV_USE_PYTHON),1)
	-echo "puts \"s+REPLACEME_PYTHON_HOME+`cygpath -u $(PYTHON_HOME)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SV_PYTHON_SO_PATH+`cygpath -u $(SV_PYTHON_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_PYTHON_SITE_PACKAGES+$(PYTHON_SITE_PACKAGES)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_PYTHON_PACKAGES+$(VTK_PYTHON_PACKAGES)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_PYTHON_PACKAGES+$(PYTHON_SITE_PACKAGES)\;$(VTK_PYTHON_PACKAGES);$(VTK_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SV_USE_PYTHON+ON+g\"" >> tclscript
else
	-echo "puts \"s+REPLACEME_SV_USE_PYTHON+OFF+g\"" >> tclscript
endif

ifeq ($(SV_USE_QT),1)
	-echo "puts \"s+REPLACEME_QT_SO_PATH+`cygpath -u $(QT_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_QT_PLUGIN_PATH+`cygpath -u $(QT_PLUGIN_PATH)`+g\"" >> tclscript
endif
ifeq ($(SV_USE_OPENCASCADE),1)
	-echo "puts \"s+REPLACEME_SV_OPENCASCADE_SO_PATH+`cygpath -u $(SV_OPENCASCADE_SO_PATH)`+g\"" >> tclscript
endif
ifeq ($(SV_USE_MITK),1)
	-echo "puts \"s+REPLACEME_SV_MITK_SO_PATH+`cygpath -u $(SV_MITK_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SV_MITK_PLUGIN_PATH+`cygpath -u $(SV_MITK_PLUGIN_PATH)`+g\"" >> tclscript
endif
endif
ifeq ($(CLUSTER),x64_linux)
	-echo "puts \"s+MPIEXEC+$(MPIEXEC)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SV_OS+linux+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_OPEN_SOFTWARE_BINARIES_TOPLEVEL+$(OPEN_SOFTWARE_BINARIES_TOPLEVEL)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_LICENSED_SOFTWARE_TOPLEVEL+$(LICENSED_SOFTWARE_TOPLEVEL)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCL_LIBRARY+$(TCL_LIBRARY)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TK_LIBRARY+$(TK_LIBRARY)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCLTK_SO_PATH+$(TCLTK_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCLSH+$(TCLSH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TKCXIMAGE_DLL+$(TKCXIMAGE_DLL)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_TCL_LIB_PATH+$(VTK_TCL_LIB_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SIM_LICENSE_FILE+$(SIM_LICENSE_FILE)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_MESHSIM_SO_PATH+$(MESHSIM_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_PARASOLID_SO_PATH+$(PARASOLID_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_P_SCHEMA+$(P_SCHEMA)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_SO_PATH+$(VTK_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_BIN_PATH+$(VTK_BINDIRS)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_LIB_PATH+$(VTK_LIBDIRS)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_INTEL_COMPILER_SO_PATH+$(INTEL_COMPILER_SO_PATH)+g\"" >> tclscript
ifeq ($(SV_USE_GDCM),1)
	-echo "puts \"s+REPLACEME_SV_GDCM_SO_PATH+$(SV_GDCM_SO_PATH)+g\"" >> tclscript
endif
ifeq ($(SV_USE_ITK),1)
	-echo "puts \"s+REPLACEME_ITK_SO_PATH+$(ITK_BIN_LIBS)+g\"" >> tclscript
endif
ifeq ($(SV_USE_QT),1)
	-echo "puts \"s+REPLACEME_QT_SO_PATH+$(QT_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_QT_PLUGIN_PATH+$(QT_PLUGIN_PATH)+g\"" >> tclscript
endif
ifeq ($(SV_USE_OPENCASCADE),1)
	-echo "puts \"s+REPLACEME_SV_OPENCASCADE_SO_PATH+$(SV_OPENCASCADE_SO_PATH)+g\"" >> tclscript
endif
ifeq ($(SV_USE_MITK),1)
	-echo "puts \"s+REPLACEME_SV_MITK_SO_PATH+$(SV_MITK_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SV_MITK_PLUGIN_PATH+$(SV_MITK_PLUGIN_PATH)+g\"" >> tclscript
endif
ifeq ($(SV_USE_FREETYPE),1)
	-echo "puts \"s+REPLACEME_SV_FREETYPE_SO_PATH+$(SV_FREETYPE_SO_PATH)+g\"" >> tclscript
endif
ifeq ($(SV_USE_PYTHON),1)
	-echo "puts \"s+REPLACEME_PYTHON_HOME+$(PYTHON_HOME)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SV_PYTHON_SO_PATH+$(SV_PYTHON_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_PYTHON_SITE_PACKAGES+$(PYTHON_SITE_PACKAGES)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_PYTHON_PACKAGES+$(VTK_PYTHON_PACKAGES)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_PYTHON_PACKAGES+$(PYTHON_SITE_PACKAGES):$(VTK_PYTHON_PACKAGES):$(VTK_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SV_USE_PYTHON+ON+g\"" >> tclscript
else
	-echo "puts \"s+REPLACEME_SV_USE_PYTHON+OFF+g\"" >> tclscript
endif

endif

ifeq ($(CLUSTER),x64_macosx)
	-echo "puts \"s+MPIEXEC+$(MPIEXEC)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SV_OS+macosx+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_OPEN_SOFTWARE_BINARIES_TOPLEVEL+$(OPEN_SOFTWARE_BINARIES_TOPLEVEL)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_LICENSED_SOFTWARE_TOPLEVEL+$(LICENSED_SOFTWARE_TOPLEVEL)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCL_LIBRARY+$(TCL_LIBRARY)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TK_LIBRARY+$(TK_LIBRARY)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCLTK_SO_PATH+$(TCLTK_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCLSH+$(TCLSH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TKCXIMAGE_DLL+$(TKCXIMAGE_DLL)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_TCL_LIB_PATH+$(VTK_TCL_LIB_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SIM_LICENSE_FILE+`cygpath -u $(SIM_LICENSE_FILE)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_MESHSIM_SO_PATH+$(MESHSIM_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_PARASOLID_SO_PATH+$(PARASOLID_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_P_SCHEMA+$(P_SCHEMA)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_SO_PATH+$(VTK_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_BIN_PATH+$(VTK_BINDIR)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_LIB_PATH+$(VTK_LIBDIRS)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_INTEL_COMPILER_SO_PATH+$(INTEL_COMPILER_SO_PATH)+g\"" >> tclscript
ifeq ($(SV_USE_GDCM),1)
	-echo "puts \"s+REPLACEME_SV_GDCM_SO_PATH+$(SV_GDCM_SO_PATH)+g\"" >> tclscript
endif
ifeq ($(SV_USE_ITK),1)
	-echo "puts \"s+REPLACEME_ITK_SO_PATH+$(ITK_BIN_LIBS)+g\"" >> tclscript
endif
ifeq ($(SV_USE_OPENCASCADE),1)
	-echo "puts \"s+REPLACEME_SV_OPENCASCADE_SO_PATH+$(SV_OPENCASCADE_SO_PATH)+g\"" >> tclscript
endif
ifeq ($(SV_USE_QT),1)
	-echo "puts \"s+REPLACEME_QT_SO_PATH+$(QT_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_QT_PLUGIN_PATH+$(QT_PLUGIN_PATH)+g\"" >> tclscript
endif
ifeq ($(SV_USE_MITK),1)
	-echo "puts \"s+REPLACEME_SV_MITK_SO_PATH+$(SV_MITK_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SV_MITK_PLUGIN_PATH+$(SV_MITK_PLUGIN_PATH)+g\"" >> tclscript
endif
ifeq ($(SV_USE_FREETYPE),1)
	-echo "puts \"s+REPLACEME_SV_FREETYPE_SO_PATH+$(SV_FREETYPE_SO_PATH)+g\"" >> tclscript
endif
ifeq ($(SV_USE_PYTHON),1)
	-echo "puts \"s+REPLACEME_PYTHON_HOME+$(PYTHON_HOME)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SV_PYTHON_SO_PATH+$(SV_PYTHON_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_PYTHON_SITE_PACKAGES+$(PYTHON_SITE_PACKAGES)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_PYTHON_PACKAGES+$(VTK_PYTHON_PACKAGES)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_PYTHON_PACKAGES+$(PYTHON_SITE_PACKAGES):$(VTK_PYTHON_PACKAGES):$(VTK_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SV_USE_PYTHON+ON+g\"" >> tclscript
else
	-echo "puts \"s+REPLACEME_SV_USE_PYTHON+OFF+g\"" >> tclscript
endif
endif
	-/usr/bin/tclsh tclscript > sedscript
	-rm -f tclscript

common-pre-build: mksedscript post-build
	mkdir -p Bin
	mkdir -p Lib
	mkdir -p Bin/plugins
	mkdir -p ../Code/Source/Include/Make
	touch ../Code/Source/Include/Make/simvascular_options.h
	touch ../Code/Source/Include/Make/simvascular_version.h

shared-pre-build: common-pre-build
	cd ../Code/Source/Plugins;$(MAKE) create_plugin_export_h create_manifest_qrc create_cached_qrc
	cd ../Code/Source/Plugins;$(MAKE) moc qrc ui
	cd ../Code/Source/Modules;$(MAKE) moc qrc ui
	cd ../Code/Source/Modules;$(MAKE) create_exports_h us-init-module
	cd ../Code/Source/UI;$(MAKE) moc

static-pre-build: common-pre-build
	cd ../Code/Source/UI;$(MAKE) SV_STATIC_BUILD=1 pre
	cd ../Code/Source/Plugins;$(MAKE) create_plugin_export_h create_manifest_qrc create_cached_qrc
	cd ../Code/Source/Plugins;$(MAKE) moc qrc ui
	cd ../Code/Source/Modules;$(MAKE) moc qrc ui
	cd ../Code/Source/Modules;$(MAKE) create_exports_h us-init-module
	cd ../Code/Source/UI;$(MAKE) moc

shared-build:
	for i in ${SHARED_LIBDIRS}; do ( \
	  echo "building shared directory ($$i)"; \
	  if [ -e $$i ];then \
	  cd $$i; \
	  $(MAKE) shared;fi ) ; done

static-third-party-build:
	for i in ${THIRD_PARTY_LIBDIRS}; do ( \
	  echo "static building directory ($$i)"; \
	  if [ -e $$i ];then \
	  cd $$i; \
	  $(MAKE) SV_STATIC_BUILD=1 lib;fi ) ; done

static-build: static-third-party-build
	for i in ${LIBDIRS}; do ( \
	  echo "static building directory ($$i)"; \
	  if [ -e $$i ];then \
	  cd $$i; \
	  $(MAKE) SV_STATIC_BUILD=1 lib;fi ) ; done

exec-build:
	for i in ${EXECDIRS}; do ( \
	  echo "building execs ($$i)"; \
	  if [ -e $$i ];then \
	  cd $$i; \
	  $(MAKE) SV_STATIC_BUILD=1 static;fi ) ; done

exec-shared-build:
	for i in ${EXECDIRS}; do ( \
	  echo "building execs ($$i)"; \
	  if [ -e $$i ];then \
	  cd $$i; \
	  $(MAKE) shared;fi ) ; done

post-build: mksedscript
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-core-script > sv
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-simvascular-script >> sv
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-core-script > xfsv
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-xfsv-script >> xfsv
	-chmod a+rx sv
	-chmod a+rx xfsv
#	-rm -f sedscript

clean:  veryclean

veryclean:
	for i in ${THIRD_PARTY_LIBDIRS}; do ( \
	  echo "super cleaning directory ($$i)"; \
	  if [ -e $$i ];then \
	  cd $$i; \
	  $(MAKE) NO_DEPEND=1 veryclean;fi ) ;done
	for i in ${LIBDIRS}; do ( \
	  echo "super cleaning directory ($$i)"; \
	  if [ -e $$i ];then \
	  cd $$i; \
	  $(MAKE) NO_DEPEND=1 veryclean;fi ) ; done
	for i in ${SHARED_LIBDIRS}; do ( \
	  echo "super cleaning directory ($$i)"; \
	  if [ -e $$i ];then \
	  cd $$i; \
	  $(MAKE) NO_DEPEND=1 veryclean;fi ) ; done
	for i in ${EXECDIRS}; do ( \
	  echo "super cleaning directory ($$i)"; \
	  if [ -e $$i ];then \
	  cd $$i; \
	  $(MAKE) NO_DEPEND=1 SV_STATIC_BUILD=1 veryclean;fi ) ; done
	for fn in $(TOP)/*~; do /bin/rm -f $$fn;done
	for fn in $(TOP)/Lib/*; do /bin/rm -f -r $$fn; done
	for fn in $(TOP)/Bin/*; do /bin/rm -f -r $$fn; done
	if [ -e sv ];then /bin/rm -f sv;fi
	if [ -e xfsv ];then /bin/rm -f xfsv;fi
	$(RM) $(TOP)/../Tcl/Common/Visualization/tclIndex
	$(RM) $(TOP)/../Tcl/Common/Vis/tclIndex
	$(RM) $(TOP)/../Tcl/Common/General/tclIndex
	$(RM) $(TOP)/../Tcl/SimVascular_2.0/GUI/tclIndex
	$(RM) $(TOP)/../Tcl/SimVascular_2.0/Core/tclIndex
	$(RM) $(TOP)/../Tcl/SimVascular_2.0/Plugins/tclIndex
	/bin/rm -f  ../Code/Source/Include/Make/simvascular_options.h
	/bin/rm -f  ../Code/Source/Include/Make/simvascular_version.h
	/bin/rm -fR ../Code/Source/Include/Make
	/bin/rm -f  sedscript

