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

TOP=../../../../BuildWithMake

include $(TOP)/include.mk

# --------------
# Compiler flags
# --------------

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(LOCAL_INCDIR) $(TCLTK_INCDIR) \
	   $(VTK_INCDIRS)

HDRS	= cvLevelSet.h cvLevelSetNode.h \
	  cvLevelSetStructuredGrid.h cvLevelSetDenseGrid.h \
          cvLevelSetSparseGrid.h \
	  cvLevelSetVelocity.h cvLevelSetVelocityKGI.h \
          cvLevelSetVelocityConstant.h \
	  cvLevelSetVelocityThreshold.h cvLevelSetVelocityPotential.h \
          cvLevelSetVelocityExponentialDecay.h cvLevelSetVelocitySmooth.h \
          cvLevelSetVelocityImage.h

CXXSRCS	= cvLevelSet.cxx cvLevelSetNode.cxx \
	  cvLevelSetStructuredGrid.cxx cvLevelSetDenseGrid.cxx \
          cvLevelSetSparseGrid.cxx \
	  cvLevelSetVelocity.cxx cvLevelSetVelocityKGI.cxx \
          cvLevelSetVelocityConstant.cxx \
	  cvLevelSetVelocityThreshold.cxx cvLevelSetVelocityPotential.cxx \
          cvLevelSetVelocityExponentialDecay.cxx \
          cvLevelSetVelocitySmooth.cxx cvLevelSetVelocityImage.cxx

SRCS	= $(CXXSRCS)

DEPS	= $(CXXSRCS:.cxx=.d)

OBJS	= $(CXXSRCS:.cxx=.$(OBJECTEXT))

TARGET_LIB = lib_lib_simvascular_lset.$(STATICEXT)

DLLHDRS = $(HDRS) cv_LsetCore_init.h
DLLSRCS = $(CXXSRCS) cv_LsetCore_init.cxx
DLLOBJS = $(DLLSRCS:.cxx=.$(OBJECTEXT))
ifeq ($(CLUSTER),x64_linux)
DLLLIBS = -l_simvascular_globals
else
DLLLIBS = lib_simvascular_globals.$(STATICEXT)
endif
TARGET_SHARED = lib_simvascular_lsetcore.$(SOEXT)

DLLHDRS2 = $(HDRS) cv_LsetV_init.h
DLLSRCS2 = $(CXXSRCS) cv_LsetV_init.cxx
DLLOBJS2 = $(DLLSRCS2:.cxx=.$(OBJECTEXT))
TARGET_SHARED2 = lib_simvascular_lsetv.$(SOEXT)
ifeq ($(CLUSTER),x64_linux)
DLLLIBS2 = -l_simvascular_globals
else
DLLLIBS2 = lib_simvascular_globals.$(STATICEXT)
endif

# -------
# Targets
# -------

include $(TOP)/targetlib.mk

