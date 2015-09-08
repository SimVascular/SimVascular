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

all:	mixed

mixed: mksedscript pre-build static-build shared-build exec-build post-build

mksedscript:
	if [ -e tclscript ];then /bin/rm -f tclscript;fi
	if [ -e sedscript ];then /bin/rm -f sedscript;fi
	if [ -e myadaptor ];then /bin/rm -f myadaptor;fi
	if [ -e mytetadaptor ];then /bin/rm -f mytetadaptor;fi
	if [ -e sv ];then /bin/rm -f sv;fi
	if [ -e xfsv ];then /bin/rm -f xfsv;fi
	if [ -e mysolver ];then /bin/rm -f mysolver;fi
	if [ -e sv-shared ];then /bin/rm -f sv;fi
	if [ -e mypost ];then /bin/rm -f mypost;fi
	-echo "puts \"s+REPLACEME_SOFTWARE_PLATFORM+$(CLUSTER)+g\"" > tclscript 
	-echo "puts \"s+REPLACEME_COMPILER_VERSION+$(COMPILER_VERSION)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SIMVASCULAR_HOME+[file rootname `pwd`]/..+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SIMVASCULAR_VERSION+$(SIMVASCULAR_VERSION)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_LES_LICENSE_SERVER+$(LES_LICENSE_SERVER)+g\"" >> tclscript
	-echo "puts \"s+MPIEXEC+$(MPIEXEC)+g\"" >> tclscript
ifneq ($(CLUSTER),x64_linux)
	-echo "puts \"s+REPLACEME_SIMVASCULAR_OS+windows+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_OPEN_SOFTWARE_BINARIES_TOPLEVEL+`cygpath -u $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_LICENSED_SOFTWARE_TOPLEVEL+`cygpath -u $(LICENSED_SOFTWARE_TOPLEVEL)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SIM_LICENSE_FILE+`cygpath -u $(SIM_LICENSE_FILE)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCL_LIBRARY+`cygpath -u $(TCL_LIBRARY)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TK_LIBRARY+`cygpath -u $(TK_LIBRARY)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCLTK_SO_PATH+`cygpath -u $(TCLTK_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCLSH+`cygpath -u $(TCLSH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TKCXIMAGE_DLL+`cygpath -u $(TKCXIMAGE_DLL)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_GLIB_SO_PATH+`cygpath -u $(GLIB_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_MPICH_SO_PATH+`cygpath -u $(MPI_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_MPIEXEC_PATH+`cygpath -u $(MPIEXEC_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_PARASOLID_SO_PATH+`cygpath -u $(PARASOLID_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_P_SCHEMA+`cygpath -u $(P_SCHEMA)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_SO_PATH+`cygpath -u $(VTK_SO_PATH)`+g\"" >> tclscript
# leave intel compilers blank for now on windows
#	-echo "puts \"s+REPLACEME_INTEL_COMPILER_SO_PATH+`cygpath -u $(INTEL_COMPILER_SO_PATH)`+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_INTEL_COMPILER_SO_PATH++g\"" >> tclscript
else
	-echo "puts \"s+REPLACEME_SIMVASCULAR_OS+linux+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_OPEN_SOFTWARE_BINARIES_TOPLEVEL+$(OPEN_SOFTWARE_BINARIES_TOPLEVEL)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_LICENSED_SOFTWARE_TOPLEVEL+$(LICENSED_SOFTWARE_TOPLEVEL)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_SIM_LICENSE_FILE+$(SIM_LICENSE_FILE)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCL_LIBRARY+$(TCL_LIBRARY)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TK_LIBRARY+$(TK_LIBRARY)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCLTK_SO_PATH+$(TCLTK_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TCLSH+$(TCLSH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_TKCXIMAGE_DLL+$(TKCXIMAGE_DLL)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_GLIB_SO_PATH+$(GLIB_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_MPICH_SO_PATH+$(MPI_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_MPIEXEC_PATH+$(MPIEXEC_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_PARASOLID_SO_PATH+$(PARASOLID_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_P_SCHEMA+$(P_SCHEMA)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_VTK_SO_PATH+$(VTK_SO_PATH)+g\"" >> tclscript
	-echo "puts \"s+REPLACEME_INTEL_COMPILER_SO_PATH+$(INTEL_COMPILER_SO_PATH)+g\"" >> tclscript
endif
	-/usr/bin/tclsh tclscript > sedscript
	-rm -f tclscript

pre-build:
	cd ../Code/Source/UI;$(MAKE) MAKE_STATIC_BUILD=1 pre
	mkdir -p Bin
	mkdir -p Lib
	touch ../Code/Source/Include/simvascular_options.h
	touch ../Code/Source/Include/simvascular_version.h
	touch ../Code/FlowSolvers/ThreeDSolver/cvFlowsolverOptions.h

shared-build:
	for i in ${SHARED_LIBDIRS}; do ( \
	  echo "building lib directory ($$i)"; \
	  if [ -e $$i ];then \
	  cd $$i; \
	  $(MAKE) lib;fi ) ; done
	for i in ${SHARED_LIBDIRS}; do ( \
	  echo "building shared directory ($$i)"; \
	  if [ -e $$i ];then \
	  cd $$i; \
	  $(MAKE) shared;fi ) ; done

static-build:
	for i in ${THIRD_PARTY_LIBDIRS}; do ( \
	  echo "static building directory ($$i)"; \
	  if [ -e $$i ];then \
	  cd $$i; \
	  $(MAKE) MAKE_STATIC_BUILD=1 lib;fi ) ; done
	for i in ${LIBDIRS}; do ( \
	  echo "static building directory ($$i)"; \
	  if [ -e $$i ];then \
	  cd $$i; \
	  $(MAKE) MAKE_STATIC_BUILD=1 lib;fi ) ; done

exec-build:
	for i in ${EXECDIRS}; do ( \
	  echo "building execs ($$i)"; \
	  if [ -e $$i ];then \
	  cd $$i; \
	  $(MAKE) MAKE_STATIC_BUILD=1 static;fi ) ; done

post-build: mksedscript
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-core-script > sv
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-simvascular-script >> sv
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-core-script > xfsv
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-xfsv-script >> xfsv
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-core-script > myadaptor
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-adaptor-script >> myadaptor
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-core-script > mytetadaptor
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-tetadaptor-script >> mytetadaptor
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-core-script > mypost
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-postsolver-script >> mypost
ifeq ($(CLUSTER),x64_linux)
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-flowsolver-linux-script > mysolver
else
	-sed -f sedscript $(TOP)/Release/developer_wrapper_scripts/developer-flowsolver-windows-script > mysolver
endif
	-chmod a+rx sv
	-chmod a+rx xfsv
	-chmod a+rx mysolver
	-chmod a+rx myadaptor
	-chmod a+rx mytetadaptor
	-chmod a+rx mypost
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
	  $(MAKE) NO_DEPEND=1 MAKE_STATIC_BUILD=1 veryclean;fi ) ; done
	for fn in $(TOP)/*~; do /bin/rm -f $$fn;done
	for fn in $(TOP)/Lib/*; do /bin/rm -f $$fn; done
	for fn in $(TOP)/Bin/*; do /bin/rm -f $$fn; done
	if [ -e myadaptor ];then /bin/rm -f myadaptor;fi
	if [ -e mytetadaptor ];then /bin/rm -f mytetadaptor;fi
	if [ -e sv ];then /bin/rm -f sv;fi
	if [ -e xfsv ];then /bin/rm -f xfsv;fi
	if [ -e mysolver ];then /bin/rm -f mysolver;fi
	if [ -e sv-shared ];then /bin/rm -f sv-shared;fi
	if [ -e mypost ];then /bin/rm -f mypost;fi
	$(RM) $(TOP)/../Tcl/Common/Visualization/tclIndex
	$(RM) $(TOP)/../Tcl/Common/Vis/tclIndex
	$(RM) $(TOP)/../Tcl/Common/General/tclIndex
	$(RM) $(TOP)/../Tcl/SimVascular_2.0/GUI/tclIndex
	$(RM) $(TOP)/../Tcl/SimVascular_2.0/Core/tclIndex
	$(RM) $(TOP)/../Tcl/SimVascular_2.0/Plugins/tclIndex
	/bin/rm -f  ../Code/Source/Include/simvascular_options.h
	/bin/rm -f  ../Code/Source/Include/simvascular_version.h
	/bin/rm -f  ../Code/FlowSolvers/ThreeDSolver/cvFlowsolverOptions.h
	/bin/rm -f  sedscript

