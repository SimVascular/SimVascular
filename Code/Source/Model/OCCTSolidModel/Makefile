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

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(LOCAL_INCDIR) $(TCLTK_INCDIR) $(VTK_INCDIRS) \
	   $(OPENCASCADE_INCDIR) $(OPENCASCADE_DEFS) $(PYTHON_INCDIR)

CREATE_EXPORTS_CV_ALL_CAPS=SV_OPENCASCADE
CREATE_EXPORTS_CV_EXPORT_NAME=SV_EXPORT_OPENCASCADE
CREATE_EXPORTS_CV_FILE_H=svOpenCASCADEExports.h

CXXFLAGS += -DSV_EXPORT_OPENCASCADE_COMPILE

HDRS	= cvOCCTSolidModel.h cv_occtsolid_utils.h
CXXSRCS	= cvOCCTSolidModel.cxx cv_occtsolid_utils.cxx

DLLHDRS = cv_occt_init.h cv_occt_init_py.h
DLLSRCS = cv_occt_init.cxx cv_occt_init_py.cxx

TARGET_LIB_NAME = $(SV_LIB_OpenCASCADE_SOLID_NAME)

DLLLIBS = $(SVLIBFLAG)$(SV_LIB_SOLID_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
	  $(VMTK_LIBS) $(OPENCASCADE_LIBS)

include $(TOP)/targetlib.mk
