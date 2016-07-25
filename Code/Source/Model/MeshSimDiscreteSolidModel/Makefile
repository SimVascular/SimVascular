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

CXXFLAGS = $(GLOBAL_CXXFLAGS) \
	   $(LOCAL_INCDIR) \
	   $(VTK_INCDIRS) \
	   $(TCLTK_INCDIR) \
	   $(MESHSIM_DEFS) \
           $(MESHSIM_INCDIR) \
	   $(ZLIB_INCDIR)

CXXFLAGS += -DSV_EXPORT_DISCRETE_COMPILE

HDRS	= cvMeshSimDiscreteSolidModel.h cv_discrete_utils.h

CXXSRCS	= cvMeshSimDiscreteSolidModel.cxx cv_discrete_utils.cxx

DLLHDRS = cv_discrete_init.h

DLLSRCS = cv_discrete_init.cxx

DLLLIBS = $(SVLIBFLAG)_simvascular_solid$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_repository$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_geom$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_utils$(LIBLINKEXT)

ifeq ($(SV_USE_VMTK),1)
	DLLLIBS += $(SVLIBFLAG)_simvascular_thirdparty_vmtk$(LIBLINKEXT)
endif

DLLLIBS += $(VTK_LIBS) $(MESHSIM_LIBS) $(PARASOLID_LIBS)

TARGET_LIB_NAME = simvascular_meshsim_discrete_solid

include $(TOP)/targetlib.mk
