# Hey emacs, this is a -*- makefile -*-

# Copyright (c) 2009-2011, Open Source Medical Software Corporation,
#              University of California, San Diego.
#
# All rights reserved.
#
# Portions copyright (c) 1999-2007 Stanford University,
# Nathan Wilson, Ken Wang, Charles Taylor.
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

CXXFLAGS = $(GLOBAL_CXXFLAGS) \
	   $(LOCAL_INCDIR) \
	   $(VTK_INCDIRS) \
	   $(TCLTK_INCDIR) \
           $(PYTHON_INCDIR) \
           $(QT_INCDIRS) $(QT_DEFS) \
	   $(ITK_INCDIRS) \
           $(MITK_INCDIRS) $(MITK_DEFS)

MODULE_NAME = svSegmentation
MODULE_NAME_ALL_CAPS = SVSEGMENTATION
CXXFLAGS += -D$(MODULE_NAME)_EXPORTS

CXXFLAGS += -DUS_MODULE_NAME="$(MODULE_NAME)"

HDRS	= \
    svSegmentationUtils.h \
    svContour.h \
    svContourCircle.h \
    svContourEllipse.h \
    svContourPolygon.h \
    svContourTensionPolygon.h \
    svContourSplinePolygon.h \
    svContourOperation.h \
    svContourModel.h \
    svContourModelVtkMapper2D.h \
    svContourModelThresholdInteractor.h \
    svContourGroup.h \
    svContourGroupVtkMapper2D.h \
    svContourGroupVtkMapper3D.h \
    svContourGroupDataInteractor.h \
    svContourGroupIO.h \
    svSegmentationLegacyIO.h \
    svSurface.h \
    svSurfaceVtkMapper3D.h \
    svSeg3D.h \
    svMitkSeg3D.h \
    svMitkSeg3DOperation.h \
    svMitkSeg3DIO.h \
    svMitkSeg3DVtkMapper3D.h \
    svMitkSeg3DDataInteractor.h \
    svSegmentationObjectFactory.h \
    svSeg3DUtils.h

CXXSRCS	= \
    svSegmentationUtils.cxx \
    svContour.cxx \
    svContourCircle.cxx \
    svContourEllipse.cxx \
    svContourPolygon.cxx \
    svContourTensionPolygon.cxx \
    svContourSplinePolygon.cxx \
    svContourOperation.cxx \
    svContourModel.cxx \
    svContourModelVtkMapper2D.cxx \
    svContourModelThresholdInteractor.cxx \
    svContourGroup.cxx \
    svContourGroupVtkMapper2D.cxx \
    svContourGroupVtkMapper3D.cxx \
    svContourGroupDataInteractor.cxx \
    svContourGroupIO.cxx \
    svSegmentationLegacyIO.cxx \
    svSurface.cxx \
    svSurfaceVtkMapper3D.cxx \
    svSeg3D.cxx \
    svMitkSeg3D.cxx \
    svMitkSeg3DOperation.cxx \
    svMitkSeg3DIO.cxx \
    svMitkSeg3DVtkMapper3D.cxx \
    svMitkSeg3DDataInteractor.cxx \
    svSegmentationObjectFactory.cxx \
    svSeg3DUtils.cxx

CXXSRCS += us_init.cxx

UIFILES = 

SV_APPEND_CPPMICROSERVICES_TO_DLL = 1
RCFILES = \
    resource/Interactions/svContourGroupInteraction.xml \
    resource/Interactions/svContourModelThresholdInteraction.xml \
    resource/Interactions/svMitkSeg3DInteraction.xml \
    resource/Interactions/svSegmentationConfig.xml

QRCFILES = 

CXXSRCS += $(addprefix moc_,$(HDRS:.h=.cxx))

CXXSRCS += $(addprefix rcc_,$(notdir $(QRCFILES:.qrc=.cxx)))

DLLHDRS =

DLLSRCS =

DLLLIBS += $(PYTHON_LIB) $(VTK_LIBS) $(ITK_LIBS) $(QT_LIBS) $(MITK_LIBS)

DLLLIBS += \
          $(SVLIBFLAG)$(SV_LIB_MODULE_COMMON_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_ITK_LSET_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GLOBALS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_VTKSVBOOLEAN_NAME)$(LIBLINKEXT)

TARGET_LIB_NAME = $(SV_LIB_MODULE_SEGMENTATION_NAME)

include $(TOP)/targetlib.mk
