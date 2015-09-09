# Hey emacs, this is a -*- makefile -*-

# =========================================================================
#
# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved.
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
# =========================================================================

TOP=../../../../BuildWithMake

include $(TOP)/include.mk

# --------------
# Compiler flags
# --------------

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(LOCAL_INCDIR) $(TCLTK_INCDIR) $(VTK_INCDIRS) \
	   $(SHAPES_INCDIR) $(GLIB_INCDIR) $(GTS_INCDIR)


HDRS	= cvPolyDataSolid.h \
          cv_polydatasolid_init.h \
          cv_polydatasolid_utils.h \
          vtkGetBoundaryFaces.h

CXXSRCS	= cvPolyDataSolid.cxx \
          cv_polydatasolid_init.cxx \
          cv_polydatasolid_utils.cxx \
          vtkGetBoundaryFaces.cxx

SRCS	= $(CXXSRCS)

DEPS	= $(CXXSRCS:.cxx=.d)

OBJS	= $(CXXSRCS:.cxx=.$(OBJECTEXT))

TARGET_LIB = lib_lib_simvascular_polydatasolid.$(STATICEXT)

DLLHDRS = $(HDRS) cv_polydatasolid_init.h
DLLSRCS = $(CXXSRCS) cv_polydatasolid_utils.cxx
DLLOBJS = $(DLLSRCS:.cxx=.$(OBJECTEXT))
ifeq ($(CLUSTER),x64_linux)
DLLLIBS = -l_simvascular_globals
else
DLLLIBS = lib_simvascular_globals.$(STATICEXT)
endif

TARGET_SHARED = lib_simvascular_polydatasolid.$(SOEXT)

# -------
# Targets
# -------

include $(TOP)/targetlib.mk
