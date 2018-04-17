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
    sv3gui_SegmentationUtils.h \
    sv3gui_Contour.h \
    sv3gui_ContourCircle.h \
    sv3gui_ContourEllipse.h \
    sv3gui_ContourPolygon.h \
    sv3gui_ContourTensionPolygon.h \
    sv3gui_ContourSplinePolygon.h \
    sv3gui_ContourOperation.h \
    sv3gui_ContourModel.h \
    sv3gui_ContourModelVtkMapper2D.h \
    sv3gui_ContourModelThresholdInteractor.h \
    sv3gui_ContourGroup.h \
    sv3gui_ContourGroupVtkMapper2D.h \
    sv3gui_ContourGroupVtkMapper3D.h \
    sv3gui_ContourGroupDataInteractor.h \
    sv3gui_ContourGroupIO.h \
    sv3gui_SegmentationLegacyIO.h \
    sv3gui_Surface.h \
    sv3gui_SurfaceVtkMapper3D.h \
    sv3gui_Seg3D.h \
    sv3gui_MitkSeg3D.h \
    sv3gui_MitkSeg3DOperation.h \
    sv3gui_MitkSeg3DIO.h \
    sv3gui_MitkSeg3DVtkMapper3D.h \
    sv3gui_MitkSeg3DDataInteractor.h \
    sv3gui_SegmentationObjectFactory.h \
    sv3gui_Seg3DUtils.h

CXXSRCS	= \
    sv3gui_SegmentationUtils.cxx \
    sv3gui_Contour.cxx \
    sv3gui_ContourCircle.cxx \
    sv3gui_ContourEllipse.cxx \
    sv3gui_ContourPolygon.cxx \
    sv3gui_ContourTensionPolygon.cxx \
    sv3gui_ContourSplinePolygon.cxx \
    sv3gui_ContourOperation.cxx \
    sv3gui_ContourModel.cxx \
    sv3gui_ContourModelVtkMapper2D.cxx \
    sv3gui_ContourModelThresholdInteractor.cxx \
    sv3gui_ContourGroup.cxx \
    sv3gui_ContourGroupVtkMapper2D.cxx \
    sv3gui_ContourGroupVtkMapper3D.cxx \
    sv3gui_ContourGroupDataInteractor.cxx \
    sv3gui_ContourGroupIO.cxx \
    sv3gui_SegmentationLegacyIO.cxx \
    sv3gui_Surface.cxx \
    sv3gui_SurfaceVtkMapper3D.cxx \
    sv3gui_Seg3D.cxx \
    sv3gui_MitkSeg3D.cxx \
    sv3gui_MitkSeg3DOperation.cxx \
    sv3gui_MitkSeg3DIO.cxx \
    sv3gui_MitkSeg3DVtkMapper3D.cxx \
    sv3gui_MitkSeg3DDataInteractor.cxx \
    sv3gui_SegmentationObjectFactory.cxx \
    sv3gui_Seg3DUtils.cxx

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
