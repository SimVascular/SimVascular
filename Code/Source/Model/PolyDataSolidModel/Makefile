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

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(LOCAL_INCDIR) $(VMTK_INCDIR) $(TCLTK_INCDIR) $(VTK_INCDIRS)

CREATE_EXPORTS_CV_ALL_CAPS=SV_POLYDATASOLID
CREATE_EXPORTS_CV_EXPORT_NAME=SV_EXPORT_POLYDATASOLID
CREATE_EXPORTS_CV_FILE_H=svPolyDataSolidExports.h

CXXFLAGS += -DSV_EXPORT_POLYDATASOLID_COMPILE

ifeq ($(SV_USE_PYTHON),1)
   CXXFLAGS += $(PYTHON_INCDIR)
endif

HDRS	= cvPolyDataSolid.h \
          cv_polydatasolid_utils.h \
          vtkGetBoundaryFaces.h

CXXSRCS	= cvPolyDataSolid.cxx \
          cv_polydatasolid_utils.cxx \
          vtkGetBoundaryFaces.cxx

DLLHDRS = cv_polydatasolid_init.h
DLLSRCS = cv_polydatasolid_init.cxx

DLLLIBS = $(SVLIBFLAG)$(SV_LIB_SOLID_NAME)$(LIBLINKEXT) \
	  $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_VMTK_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_VTKSVBOOLEAN_NAME)$(LIBLINKEXT) \
	  $(VMTK_LIBS)

TARGET_LIB_NAME = $(SV_LIB_POLYDATA_SOLID_NAME)

include $(TOP)/targetlib.mk
