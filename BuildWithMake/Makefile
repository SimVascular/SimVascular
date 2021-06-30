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

TOP = .

include $(TOP)/include.mk

all-shared: shared-pre-build all-shared-build-only

all-shared-build-only: static-build shared-build exec-shared-build

all: mixed

mixed: static-pre-build static-build shared-build exec-build

fast:
	-make shared-pre-build
	-make -j6 all-shared-build-only
	-make all-shared-build-only
	-make all-shared-build-only
	make all-shared-build-only

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
	-echo "puts \"s+REPLACEME_MAJOR_VERSION+$(SV_MAJOR_VERSION)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_MINOR_VERSION+$(SV_MINOR_VERSION)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_PATCH_VERSION+$(SV_PATCH_VERSION)+g\"" >> tclscript
ifeq ($(CLUSTER),x64_cygwin)
	-echo "puts \"s+REPLACEME_SV_OS+windows+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_OPEN_SOFTWARE_BINARIES_TOPLEVEL+`cygpath -u $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCL_LIBRARY+`cygpath -u $(TCL_LIBRARY)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TK_LIBRARY+`cygpath -u $(TK_LIBRARY)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCLTK_SO_PATH+`cygpath -u $(TCLTK_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCLSH+`cygpath -u $(TCLSH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TKCXIMAGE_DLL+`cygpath -u $(TKCXIMAGE_DLL)`+g\"" >> tclscript
# leave intel compilers blank for now on windows
#	-echo "puts \"s+REPLACEME_INTEL_COMPILER_SO_PATH+`cygpath -u $(INTEL_COMPILER_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_INTEL_COMPILER_SO_PATH++g\"" >> tclscript
ifeq ($(SV_USE_VTK),1)
	-echo "puts \"s+REPLACEME_VTK_SO_PATH+`cygpath -u $(VTK_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_BIN_PATH+`cygpath -u $(VTK_BINDIRS)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_LIB_PATH+`cygpath -u $(VTK_LIBDIRS)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_TCL_LIB_PATH+`cygpath -u $(VTK_TCL_LIB_PATH)`+g\"" >> tclscript
endif
ifeq ($(SV_USE_GDCM),1)
	-echo "puts \"s+REPLACEME_SV_GDCM_SO_PATH+`cygpath -u $(SV_GDCM_SO_PATH)`+g\"" >> tclscript
endif
ifeq ($(SV_USE_HDF5),1)
	-echo "puts \"s+REPLACEME_SV_HDF5_SO_PATH+`cygpath -u $(SV_HDF5_SO_PATH)`+g\"" >> tclscript
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
	-echo "puts \"s+REPLACEME_QT_PLUGIN_PATH+$(QT_PLUGIN_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_QT_QPA_FONTDIR+$(QT_QPA_FONTDIR)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_QT_WEBENGINE_PROC+$(QT_WEBENGINE_PROC)+g\"" >> tclscript
endif
ifeq ($(SV_USE_OPENCASCADE),1)
	-echo "puts \"s+REPLACEME_SV_OPENCASCADE_SO_PATH+`cygpath -u $(SV_OPENCASCADE_SO_PATH)`+g\"" >> tclscript
endif
ifeq ($(SV_USE_MITK),1)
	-echo "puts \"s+REPLACEME_SV_MITK_SO_PATH+`cygpath -u -p $(SV_MITK_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SV_MITK_PLUGIN_PATH+$(SV_MITK_PLUGIN_PATH)+g\"" >> tclscript
endif
endif
ifeq ($(CLUSTER),x64_linux)
	-echo "puts \"s+MPIEXEC+$(MPIEXEC)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SV_OS+linux+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_OPEN_SOFTWARE_BINARIES_TOPLEVEL+$(OPEN_SOFTWARE_BINARIES_TOPLEVEL)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCL_LIBRARY+$(TCL_LIBRARY)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TK_LIBRARY+$(TK_LIBRARY)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCLTK_SO_PATH+$(TCLTK_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCLSH+$(TCLSH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TKCXIMAGE_DLL+$(TKCXIMAGE_DLL)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_TCL_LIB_PATH+$(VTK_TCL_LIB_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SIM_LICENSE_FILE+$(SIM_LICENSE_FILE)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_SO_PATH+$(VTK_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_BIN_PATH+$(VTK_BINDIRS)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_LIB_PATH+$(VTK_LIBDIRS)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_INTEL_COMPILER_SO_PATH+$(INTEL_COMPILER_SO_PATH)+g\"" >> tclscript
ifeq ($(SV_USE_GDCM),1)
	-echo "puts \"s+REPLACEME_SV_GDCM_SO_PATH+$(SV_GDCM_SO_PATH)+g\"" >> tclscript
endif
ifeq ($(SV_USE_HDF5),1)
	-echo "puts \"s+REPLACEME_SV_HDF5_SO_PATH+$(SV_HDF5_SO_PATH)+g\"" >> tclscript
endif
ifeq ($(SV_USE_ITK),1)
	-echo "puts \"s+REPLACEME_ITK_SO_PATH+$(ITK_BIN_LIBS)+g\"" >> tclscript
endif
ifeq ($(SV_USE_QT),1)
	-echo "puts \"s+REPLACEME_QT_SO_PATH+$(QT_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_QT_PLUGIN_PATH+$(QT_PLUGIN_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_QT_QPA_FONTDIR+$(QT_QPA_FONTDIR)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_QT_WEBENGINE_PROC+$(QT_WEBENGINE_PROC)+g\"" >> tclscript
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
	-echo "puts \"s+REPLACEME_TCL_LIBRARY+$(TCL_LIBRARY)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TK_LIBRARY+$(TK_LIBRARY)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCLTK_SO_PATH+$(TCLTK_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCLSH+$(TCLSH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TKCXIMAGE_DLL+$(TKCXIMAGE_DLL)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_TCL_LIB_PATH+$(VTK_TCL_LIB_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SIM_LICENSE_FILE+$(SIM_LICENSE_FILE)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_SO_PATH+$(VTK_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_BIN_PATH+$(VTK_BINDIR)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_LIB_PATH+$(VTK_LIBDIRS)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_INTEL_COMPILER_SO_PATH+$(INTEL_COMPILER_SO_PATH)+g\"" >> tclscript
ifeq ($(SV_USE_GDCM),1)
	-echo "puts \"s+REPLACEME_SV_GDCM_SO_PATH+$(SV_GDCM_SO_PATH)+g\"" >> tclscript
endif
ifeq ($(SV_USE_HDF5),1)
	-echo "puts \"s+REPLACEME_SV_HDF5_SO_PATH+$(SV_HDF5_SO_PATH)+g\"" >> tclscript
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
	-echo "puts \"s+REPLACEME_QT_QPA_FONTDIR+$(QT_QPA_FONTDIR)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_QT_WEBENGINE_PROC+$(QT_WEBENGINE_PROC)+g\"" >> tclscript
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
	-/usr/bin/tclsh tclscript >> sedscript
	-rm -f tclscript

mk_simvascular_options_h:
	mkdir -p ../Code/Source/Include/Make
	-echo "#ifndef _simvascular_options_h" > ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define _simvascular_options_h" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define Qt5_MAJOR_VERSION $(Qt5_MAJOR_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define TCL_MAJOR_VERSION $(TCL_MAJOR_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define PYTHON_MAJOR_VERSION $(PYTHON_MAJOR_VERSION) " >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define FREETYPE_MAJOR_VERSION $(FREETYPE_MAJOR_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define MMG_MAJOR_VERSION $(MMG_MAJOR_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define GDCM_MAJOR_VERSION $(GDCM_MAJOR_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define ITK_MAJOR_VERSION $(ITK_MAJOR_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define OpenCASCADE_MAJOR_VERSION $(OpenCASCADE_MAJOR_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define MITK_MAJOR_VERSION $(MITK_MAJOR_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define Qt5_MINOR_VERSION $(Qt5_MINOR_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define TCL_MINOR_VERSION $(TCL_MINOR_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define PYTHON_MINOR_VERSION $(PYTHON_MINOR_VERSION) " >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define FREETYPE_MINOR_VERSION $(FREETYPE_MINOR_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define MMG_MINOR_VERSION $(MMG_MINOR_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define GDCM_MINOR_VERSION $(GDCM_MINOR_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define ITK_MINOR_VERSION $(ITK_MINOR_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define OpenCASCADE_MINOR_VERSION $(OpenCASCADE_MINOR_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define MITK_MINOR_VERSION $(MITK_MINOR_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define Qt5_PATCH_VERSION $(Qt5_PATCH_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define TCL_PATCH_VERSION $(TCL_PATCH_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define PYTHON_PATCH_VERSION $(PYTHON_PATCH_VERSION) " >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define FREETYPE_PATCH_VERSION $(FREETYPE_PATCH_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define MMG_PATCH_VERSION $(MMG_PATCH_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define GDCM_PATCH_VERSION $(GDCM_PATCH_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define ITK_PATCH_VERSION $(ITK_PATCH_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define OpenCASCADE_PATCH_VERSION $(OpenCASCADE_PATCH_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#define MITK_PATCH_VERSION $(MITK_PATCH_VERSION)" >> ../Code/Source/Include/Make/simvascular_options.h
	-echo "#endif" >> ../Code/Source/Include/Make/simvascular_options.h

machine-learning-network:
	if [[ ! -e ../Python/site-packages/sv_ml/results ]];  then \
	  /bin/rm -Rf results; \
	  /bin/rm -Rf ../Python/site-packages/sv_ml/results; \
	  /bin/rm -f networks.tar; \
	  wget http://simvascular.stanford.edu/downloads/public/machine_learning/networks.tar .; \
	  tar xvf ./networks.tar; \
	  /bin/mv results ../Python/site-packages/sv_ml/results; \
	  /bin/rm -f networks.tar; \
	fi

#	-/bin/rm -f networks.tar

#clone-zeroDsolver:
#	if [[ ! -e ../Python/site-packages/svZeroDSolver/svzerodsolver ]];  then \
#	  /bin/rm -Rf ../Python/site-packages/svZeroDSolver; \
#	  git clone https://github.com/SimVascular/svZeroDSolver.git ../Python/site-packages/svZeroDSolver; \
#	fi

clone-zeroDsolver:
	if [[ ! -e ../Python/site-packages/svZeroDSolver/svzerodsolver ]];  then \
	  git submodule update --init --recursive --remote; \
	fi

common-pre-build: mksedscript mk_simvascular_options_h machine-learning-network clone-zeroDsolver post-build 
	mkdir -p Bin
	mkdir -p Lib
	mkdir -p Bin/plugins
	mkdir -p ../Code/Source/Include/Make
	touch ../Code/Source/Include/Make/simvascular_version.h
	touch ../Code/Source/Include/Make/delaunay_options.h

shared-pre-build: common-pre-build
ifeq ($(SV_USE_MITK),1)
ifeq ($(SV_USE_SV4_GUI),1)
	cd ../Code/Source/sv4gui/Plugins;$(MAKE) create_plugin_export_h create_manifest_qrc create_cached_qrc
	cd ../Code/Source/sv4gui/Plugins;$(MAKE) moc qrc ui
endif
	cd ../Code/Source/sv4gui/Modules;$(MAKE) moc qrc ui
	cd ../Code/Source/sv4gui/Modules;$(MAKE) create_exports_h us-init-module create_cppmicroservices_file
endif
	cd ../Code/Source/vtkSV;$(MAKE) create_exports_outname_h
	for i in ${SHARED_LIBDIRS}; do ( \
	  echo "creating exports for shared directory ($$i)"; \
	  if [ -e $$i ];then \
	  cd $$i; \
	  $(MAKE) create_exports_cv_h;fi ) ; done

static-pre-build: common-pre-build
	cd ../Code/Source/Application;$(MAKE) SV_STATIC_BUILD=1 pre
ifeq ($(SV_USE_MITK),1)
ifeq ($(SV_USE_SV4_GUI),1)
	cd ../Code/Source/sv4gui/Plugins;$(MAKE) create_plugin_export_h create_manifest_qrc create_cached_qrc
	cd ../Code/Source/sv4gui/Plugins;$(MAKE) moc qrc ui
	cd ../Code/Source/sv4gui/Modules;$(MAKE) moc qrc ui
	cd ../Code/Source/sv4gui/Modules;$(MAKE) create_exports_h us-init-module
endif
endif
	cd ../Code/Source/vtkSV;$(MAKE) create_exports_outname_h

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
	-mkdir -p Bin
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-core-script > sv
ifeq ($(CLUSTER),x64_macosx)
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-simvascular-mac_osx-script >> sv
else
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-simvascular-script >> sv
endif
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-core-script > xfsv
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-xfsv-script >> xfsv
	-chmod a+rx sv
	-chmod a+rx xfsv
ifeq ($(SV_USE_QT),1)
	-echo "[Paths]" > Bin/qt.conf
	-echo "Prefix=\"$(QT_TOP_DIR)\"" >> Bin/qt.conf
	-mkdir -p Bin/QtWebEngine
	-/bin/cp -f $(QT_WEBENGINE_PATH) Bin/QtWebEngine
	-/bin/cp -r $(QT_TOP_DIR)/resources/* Bin/QtWebEngine
	-/bin/cp -Rf $(QT_TOP_DIR)/translations/* Bin/QtWebEngine
ifeq ($(CLUSTER),x64_macosx)
	-install_name_tool -add_rpath $(QT_SO_PATH) Bin/QtWebEngine/$(notdir $(QT_WEBENGINE_PATH))
endif
ifeq ($(CLUSTER),x64_macosx)
	-mkdir -p Bin/sv.app/Contents/MacOS
	-ln -s ../../../simvascular.exe Bin/sv.app/Contents/MacOS/sv
endif
endif
#	-rm -f sedscript

clean:
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
	if [ -e Bin/qt.conf ];then /bin/rm -Rf Bin/qt.conf;fi
	$(RM) $(TOP)/../Tcl/Common/Visualization/tclIndex
	$(RM) $(TOP)/../Tcl/Common/Vis/tclIndex
	$(RM) $(TOP)/../Tcl/Common/General/tclIndex
	$(RM) $(TOP)/../Tcl/External/tclxml3.2/tclIndex
	$(RM) $(TOP)/../Tcl/SimVascular_2.0/GUI/tclIndex
	$(RM) $(TOP)/../Tcl/SimVascular_2.0/Core/tclIndex
	$(RM) $(TOP)/../Tcl/SimVascular_2.0/Plugins/tclIndex
	/bin/rm -f  ../Code/Source/Include/Make/simvascular_options.h
	/bin/rm -f  ../Code/Source/Include/Make/simvascular_version.h
	/bin/rm -f  ../Code/Source/Include/Make/delaunay_options.h
	/bin/rm -fR ../Code/Source/Include/Make
	/bin/rm -f  sedscript

veryclean:  clean
	if [ -e ext ];then /bin/rm -Rf ext;fi
	if [ -e ../Python/site-packages/sv_ml/results ];then /bin/rm -Rf ../Python/site-packages/sv_ml/results;fi
