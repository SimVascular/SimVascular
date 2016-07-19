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

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(LOCAL_INCDIR) $(SOLVERIO_INCDIR) $(THREEDSOLVER_INCDIR) $(ZLIB_INCDIR) $(VTK_INCDIRS) -I../../Include/Make

HDRS	=

CXXSRCS = post_solver.cxx

SRCS	= $(CXXSRCS)

DEPS	= $(CXXSRCS:.cxx=.d)

OBJS    = $(addprefix $(BUILD_DIR)/,$(CXXSRCS:.cxx=.$(OBJECTEXT))) \
          $(addprefix $(BUILD_DIR)/,$(CSRCS:.c=.$(OBJECTEXT))) \
          $(addprefix $(BUILD_DIR)/,$(FSRCS:.f=.$(OBJECTEXT)))

TARGET_EXE_FULL = $(TOP)/Bin/postsolver-$(CXX_COMPILER_VERSION)-$(FORTRAN_COMPILER_VERSION).exe
TARGET_EXE = $(TOP)/Bin/postsolver.exe

static: directories $(TARGET_EXE_FULL)

shared: directories $(TARGET_EXE_FULL) 

directories:
	-mkdir -p $(BUILD_DIR)

$(TARGET_EXE_FULL): $(OBJS)
	for fn in $(TARGET_EXE); do /bin/rm -f $$fn; done
	for fn in $(basename $(TARGET_EXE)).pdb; do /bin/rm -f $$fn; done
	for fn in $(TARGET_EXE_FULL); do /bin/rm -f $$fn; done
	for fn in $(basename $(TARGET_EXE_FULL)).pdb; do /bin/rm -f $$fn; done
	$(LINK_EXE)$(TARGET_EXE_FULL) $(GLOBAL_LFLAGS) \
             $(OBJS) $(ZLIB_LIBS) $(VTK_LIBS) \
             $(SVLIBFLAG)_lib_simvascular_solverio$(LIBLINKEXT) \
             $(CXX_LIBS)
	if [ -e $(notdir $(TARGET_EXE_FULL)).pdb ];then mv $(basename $(TARGET_EXE_FULL)).pdb $(TOP)/Bin;fi
	ln -s $(notdir $(TARGET_EXE_FULL)) $(TARGET_EXE)

ifndef NO_DEPEND
-include $(DEPS)
endif

clean:
	for fn in $(BUILD_DIR); do /bin/rm -f -r $$fn;done
	for fn in *~; do /bin/rm -f $$fn;done
	for fn in $(TOP)/Bin/$(TARGET_EXE)*; do /bin/rm -f $$fn; done
	/bin/rm -f $(TOP)/../Code/FlowSolvers/ThreeDSolver/svPost/Make/cvFlowsolverOptions.h
	/bin/rm -fR $(TOP)/../Code/FlowSolvers/ThreeDSolver/svPost/Make

veryclean: clean
	if [ -e obj ];then /bin/rm -f -r obj;fi

