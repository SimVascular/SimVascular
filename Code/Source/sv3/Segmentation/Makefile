# Hey emacs, this is a -*- makefile -*-

# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
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
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

TOP=../../../../BuildWithMake

include $(TOP)/include.mk

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(ITK_DEFS) $(LOCAL_INCDIR) $(TCLTK_INCDIR) $(PYTHON_INCDIR) $(VTK_INCDIRS) $(ITK_INCDIRS)

CREATE_EXPORTS_CV_ALL_CAPS=SV_SEGMENTATION
CREATE_EXPORTS_CV_EXPORT_NAME=SV_EXPORT_SEGMENTATION
CREATE_EXPORTS_CV_FILE_H=sv3SegmentationExports.h

CXXFLAGS += -DSV_EXPORT_SEGMENTATION_COMPILE

HDRS	= sv3_Contour.h sv3_LevelSetContour.h sv3_SegmentationUtils.h sv3_CircleContour.h sv3_PolygonContour.h sv3_SplinePolygonContour.h sv3_ThresholdContour.h
CXXSRCS	= sv3_Contour.cxx sv3_LevelSetContour.cxx sv3_SegmentationUtils.cxx sv3_CircleContour.cxx sv3_PolygonContour.cxx sv3_SplinePolygonContour.cxx sv3_ThresholdContour.cxx


SVPYHDRS = sv3_Contour_init_py.h sv3_LevelSetContour_init_py.h sv3_CircleContour_init_py.h sv3_PolygonContour_init_py.h sv3_SplinePolygonContour_init_py.h sv3_ThresholdContour_init_py.h
SVPYSRCS = sv3_Contour_init_py.cxx sv3_LevelSetContour_init_py.cxx sv3_CircleContour_init_py.cxx sv3_PolygonContour_init_py.cxx sv3_SplinePolygonContour_init_py.cxx sv3_ThresholdContour_init_py.cxx

DLLLIBS = $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_SOLID_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_COMMON_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_PATH_NAME)$(LIBLINKEXT)

ifeq ($(SV_USE_ITK),1)
  DLLLIBS += $(SVLIBFLAG)$(SV_LIB_ITK_LSET_NAME)$(LIBLINKEXT) \
	           $(ITK_LIBS)
endif

TARGET_LIB_NAME = $(SV_LIB_SEGMENTATION_NAME)

SV_PYTHON_MODULE_NAME = pyContour

include $(TOP)/targetlib.mk

