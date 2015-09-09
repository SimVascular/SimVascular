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

TOP=../../../BuildWithMake

include $(TOP)/include.mk


# --------------
# Compiler flags
# --------------

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(LOCAL_INCDIR) $(TCLTK_INCDIR) $(VTK_INCDIRS)


HDRS	= cv_image.h cv_img_offsets.h cv_decode.h \
	  cv_calc_correction_eqn.h cv_img_threshold.h \
	  cvDistanceMap.h cv_mask_image_in_place.h
CXXSRCS	= cv_image.cxx cv_read_header.cxx cv_decode.cxx \
	  cv_calc_correction_eqn.cxx \
	  cv_img_threshold.cxx cvDistanceMap.cxx \
	  cv_mask_image_in_place.cxx

SRCS	= $(CXXSRCS)

DEPS	= $(CXXSRCS:.cxx=.d)

OBJS	= $(CXXSRCS:.cxx=.$(OBJECTEXT))

TARGET_LIB = lib_lib_simvascular_image.$(STATICEXT)

DLLHDRS = $(HDRS) cv_image_init.h
DLLSRCS = $(CXXSRCS) cv_image_init.cxx
DLLOBJS = $(DLLSRCS:.cxx=.$(OBJECTEXT))
ifeq ($(CLUSTER),x64_linux)
DLLLIBS = -l_simvascular_globals
else
DLLLIBS = lib_simvascular_globals.$(STATICEXT)
endif
TARGET_SHARED = lib_simvascular_image.$(SOEXT)

# -------
# Targets
# -------

include $(TOP)/targetlib.mk

