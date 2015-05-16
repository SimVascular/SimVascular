# Hey emacs, this is a -*- makefile -*-

# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Portions of the code Copyright (c) 2009-2011 Open Source Medical 
# Software Corporation, University of California, San Diego.
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

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(LOCAL_INCDIR) \
	   $(TCLTK_INCDIR) \
	   $(VTK_INCDIRS)


HDRS	= cvRepository.h cvRepositoryData.h cvDataObject.h \
	  cvPolyData.h cvStrPts.h cvUnstructuredGrid.h \
          cv_repos_init.h \
          cvDataSet.h

CXXSRCS	= cvRepository.cxx cvRepositoryData.cxx cvDataObject.cxx \
	  cvPolyData.cxx cvStrPts.cxx cvUnstructuredGrid.cxx \
	  cv_repos_init.cxx cvDataSet.cxx

SRCS	= $(CXXSRCS)

DEPS	= $(CXXSRCS:.cxx=.d)

OBJS	= $(CXXSRCS:.cxx=.$(OBJECTEXT))

TARGET_LIB = lib_lib_simvascular_repository.$(STATICEXT)

DLLHDRS = $(HDRS)
DLLSRCS = $(CXXSRCS)
DLLOBJS = $(OBJS)
ifeq ($(CLUSTER),x64_linux)
DLLLIBS = -l_lib_simvascular_utils -l_simvascular_globals $(TCLTK_LIBS) $(VTK_LIBS)
else
DLLLIBS = lib_lib_simvascular_utils.$(STATICEXT) \
          lib_simvascular_globals.$(STATICEXT) \
          $(TCLTK_LIBS) $(VTK_LIBS)
endif
TARGET_SHARED = lib_simvascular_repository.$(SOEXT)

# -------
# Targets
# -------

include $(TOP)/targetlib.mk
