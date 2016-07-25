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
# =========================================================================*/

TOP=../../../../BuildWithMake

include $(TOP)/include.mk

# --------------
# Compiler flags
# --------------

CXXFLAGS = $(GLOBAL_CXXFLAGS)

CXXFLAGS += -DSV_EXPORT_MMG_COMPILE

CXXFLAGS += $(LOCAL_INCDIR) \
	    $(SV_MMG_INCDIR) $(VTK_INCDIRS) $(TCLTK_INCDIR)

HDRS	= cv_mmg_mesh_utils.h

CXXSRCS	= cv_mmg_mesh_utils.cxx

DLLHDRS = cv_mmg_mesh_init.h

DLLSRCS = cv_mmg_mesh_init.cxx

TARGET_LIB_NAME = simvascular_mmg_mesh

DLLLIBS = $(SVLIBFLAG)_simvascular_polydata_solid$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_geom$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_repository$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_utils$(LIBLINKEXT) \
	  $(SV_MMG_LIBS)

include $(TOP)/targetlib.mk




