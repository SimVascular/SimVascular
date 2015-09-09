# Hey emacs, this is a -*- makefile -*-

#===========================================================================
#    
# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Portions of the code Copyright (c) 2009-2013 Open Source Medical
# Software Corporation, University of California, San Diego.
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
#
#===========================================================================    

TOP=../../../../BuildWithMake

include $(TOP)/include.mk

# --------------
# Compiler flags
# --------------

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(LOCAL_INCDIR) $(SOLVERIO_INCDIR) $(THREEDSOLVER_INCDIR) $(ZLIB_INCDIR) $(VTK_INCDIRS)

HDRS	=

CXXSRCS = post_solver.cxx

SRCS	= $(CXXSRCS)

DEPS	= $(CXXSRCS:.cxx=.d)

OBJS	= $(CXXSRCS:.cxx=.$(OBJECTEXT))

TARGET_EXE = postsolver.exe

static: createOptionFile $(TOP)/Bin/$(TARGET_EXE)

$(TOP)/Bin/$(TARGET_EXE): $(OBJS)
	$(LINK_EXE)$(TARGET_EXE) $(GLOBAL_LFLAGS) \
             $(OBJS) $(ZLIB_LIBS) $(VTK_LIBS) \
             ../../../../BuildWithMake/Lib/lib_lib_simvascular_solverio.$(STATICEXT) \
             $(CXX_LIBS)
	for fn in $(TOP)/Bin/$(TARGET_EXE)*; do /bin/rm -f $$fn; done
	mv $(TARGET_EXE)* $(TOP)/Bin

ifndef NO_DEPEND
-include $(DEPS)
endif

createOptionFile:
	@echo "#define VER_CORONARY $(FLOWSOLVER_VERSION_CORONARY_ACTIVATE)" > $(TOP)/../Code/FlowSolvers/ThreeDSolver/svPost/cvFlowsolverOptions.h
	@echo "#define VER_CLOSEDLOOP $(FLOWSOLVER_VERSION_CLOSEDLOOP_ACTIVATE)" >> $(TOP)/../Code/FlowSolvers/ThreeDSolver/svPost/cvFlowsolverOptions.h
	@echo "#define VER_VARWALL $(FLOWSOLVER_VERSION_VARWALL_ACTIVATE)" >> $(TOP)/../Code/FlowSolvers/ThreeDSolver/svPost/cvFlowsolverOptions.h
	@echo "#define VER_USE_VTK $(FLOWSOLVER_VERSION_USE_VTK_ACTIVATE)" >> $(TOP)/../Code/FlowSolvers/ThreeDSolver/svPost/cvFlowsolverOptions.h

clean: veryclean

veryclean:
	for fn in *.$(OBJECTEXT); do /bin/rm -f $$fn; done
	for fn in *~; do /bin/rm -f $$fn;done
	for fn in *.mod; do /bin/rm -f $$fn;done
	for fn in *.d; do /bin/rm -f $$fn; done
	for fn in *.pdb; do /bin/rm -f $$fn; done
	for fn in *.exp; do /bin/rm -f $$fn; done
	for fn in $(TOP)/Bin/$(TARGET_EXE)*; do /bin/rm -f $$fn; done
	/bin/rm -f cvFlowsolverOptions.h

