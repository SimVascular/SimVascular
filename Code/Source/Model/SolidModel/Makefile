# Hey emacs, this is a -*- makefile -*-

# =========================================================================
#
# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                         University of California, San Diego.
#
# Portions of the code Copyright (c) 1998-2007 Stanford University,
# Charles Taylor, Nathan Wilson, Ken Wang.
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
# =========================================================================

TOP=../../../../BuildWithMake

include $(TOP)/include.mk

# --------------
# Compiler flags
# --------------

CXXFLAGS = $(GLOBAL_CXXFLAGS) \
	   $(LOCAL_INCDIR) \
	   $(VTK_INCDIRS) \
	   $(TCLTK_INCDIR)

# Because this SolidModel should be encapsulating all concrete implementations,
# we should *not* have to include directories from concrete classes.
# TO DO -- comment out the following include dependencies.
ifeq ($(EXCLUDE_SOLID_MODEL),0)

    ifeq ($(MAKE_WITH_PARASOLID),1)
        CXXFLAGS += $(PARASOLID_INCDIR)
    endif

endif

ifeq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL),1)
  CXXFLAGS += $(MESHSIM_INCDIR) $(TCLTK_INCDIR)
endif

HDRS	= cvSolidModel.h 
CXXSRCS	= cvSolidModel.cxx 

SRCS	= $(CXXSRCS)

DEPS	= $(CXXSRCS:.cxx=.d)

OBJS	= $(CXXSRCS:.cxx=.$(OBJECTEXT))

DEPLIBS = $(TOP)/lib/lib_simvascular_utils.$(STATICEXT)

TARGET_LIB = lib_lib_simvascular_solid.$(STATICEXT)

DLLHDRS = $(HDRS) cv_solid_init.h 
DLLSRCS = $(CXXSRCS) cv_solid_init.cxx 
DLLOBJS = $(DLLSRCS:.cxx=.$(OBJECTEXT))
ifeq ($(CLUSTER),x64_linux)
DLLLIBS = -l_lib_simvascular_utils -l_simvascular_globals
else
DLLLIBS = lib_lib_simvascular_utils.$(STATICEXT) lib_simvascular_globals.$(STATICEXT)
endif
TARGET_SHARED = lib_simvascular_solid.$(SOEXT)

# -------
# Targets
# -------

include $(TOP)/targetlib.mk
