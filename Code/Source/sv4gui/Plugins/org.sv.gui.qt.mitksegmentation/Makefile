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

TOP=../../../../../BuildWithMake

include $(TOP)/include.mk

CXXFLAGS = $(GLOBAL_CXXFLAGS) \
	   $(LOCAL_INCDIR) \
	   $(VTK_INCDIRS) \
	   $(TCLTK_INCDIR) \
           $(PYTHON_INCDIR) \
           $(MESHSIM_INCDIR) \
           $(PARASOLID_INCDIR) \
           $(QT_INCDIRS) $(QT_DEFS) \
	   $(ITK_INCDIRS) \
           $(SV_TINYXML_INCDIR) $(SV_TINYXML2_INCDIR) \
           $(MITK_INCDIRS) $(MITK_DEFS) \
           -Dorg_sv_gui_qt_mitksegmentation_EXPORTS \
           -DUS_MODULE_NAME=sv4guiModuleMitkSegmentation

ifeq ($(SV_USE_OPENCASCADE),1)
    CXXFLAGS += $(OPENCASCADE_INCDIR)
endif

HDRS	= \
	sv4gui_mitkPluginActivator.h \
	sv4gui_QmitkAutocropAction.h \
	sv4gui_QmitkBooleanOperationsWidget.h \
	sv4gui_QmitkContourModelToImageWidget.h \
	sv4gui_QmitkCreatePolygonModelAction.h \
	sv4gui_QmitkDataSelectionWidget.h \
	sv4gui_QmitkDeformableClippingPlaneView.h \
	sv4gui_QmitkImageMaskingWidget.h \
	sv4gui_QmitkMorphologicalOperationsWidget.h \
	sv4gui_QmitkSegmentationPreferencePage.h \
	sv4gui_QmitkSegmentationUtilitiesView.h \
	sv4gui_QmitkSegmentationUtilityWidget.h \
	sv4gui_QmitkSegmentationView.h \
	sv4gui_QmitkSurfaceToImageWidget.h \
	sv4gui_QmitkThresholdAction.h \
	sv4gui_segmentation.h

CXXSRCS	= \
	sv4gui_mitkPluginActivator.cxx \
	sv4gui_QmitkAutocropAction.cxx \
	sv4gui_QmitkBooleanOperationsWidget.cxx \
	sv4gui_QmitkContourModelToImageWidget.cxx \
	sv4gui_QmitkCreatePolygonModelAction.cxx \
	sv4gui_QmitkDataSelectionWidget.cxx \
	sv4gui_QmitkDeformableClippingPlaneView.cxx \
	sv4gui_QmitkImageMaskingWidget.cxx \
	sv4gui_QmitkMorphologicalOperationsWidget.cxx \
	sv4gui_QmitkSegmentationPreferencePage.cxx \
	sv4gui_QmitkSegmentationUtilitiesView.cxx \
	sv4gui_QmitkSegmentationUtilityWidget.cxx \
	sv4gui_QmitkSegmentationView.cxx \
	sv4gui_QmitkSurfaceToImageWidget.cxx \
	sv4gui_QmitkThresholdAction.cxx

UIFILES = \
	sv4gui_QmitkBooleanOperationsWidgetControls.ui \
	sv4gui_QmitkContourModelToImageWidgetControls.ui \
	sv4gui_QmitkDataSelectionWidgetControls.ui \
	sv4gui_QmitkDeformableClippingPlaneViewControls.ui \
	sv4gui_QmitkImageMaskingWidgetControls.ui \
	sv4gui_QmitkMorphologicalOperationsWidgetControls.ui \
	sv4gui_QmitkSegmentationControls.ui \
	sv4gui_QmitkSegmentationUtilitiesViewControls.ui \
	sv4gui_QmitkSurfaceToImageWidgetControls.ui

QRCFILES = org_sv_gui_qt_mitksegmentation_manifest.qrc \
           org_sv_gui_qt_mitksegmentation_cached.qrc \
           resources/segmentation.qrc \
           resources/SegmentationUtilities.qrc \
           resources/BooleanOperationsWidget.qrc \
           resources/MorphologicalOperationsWidget.qrc

RCFILES = \
  resources/segmentation.svg \
  resources/deformablePlane.png \
  resources/clipping_plane_translate_48x48.png \
  resources/clipping_plane_rotate48x48.png \
  resources/clipping_plane_deform48x48.png \
  resources/segmentation_utilities.svg \
  plugin.xml

CXXSRCS += $(addprefix moc_,$(notdir $(HDRS:.h=.cxx)))
CXXSRCS += $(addprefix qrc_,$(notdir $(QRCFILES:.qrc=.cxx)))

DLLHDRS =

DLLSRCS =

DLLLIBS += $(PYTHON_LIB) $(VTK_LIBS) $(ITK_LIBS) $(QT_LIBS) $(MITK_LIBS)

DLLLIBS +=  \
          $(SVLIBFLAG)$(SV_PLUGIN_DATAMANAGER_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_COMMON_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_SEGMENTATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_MACHINELEARNING_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_MODEL_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PATH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PROJECTMANAGEMENT_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_QTWIDGETS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_ITK_LSET_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_PATH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GLOBALS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_SEGMENTATION_NAME)$(LIBLINKEXT)

ifeq ($(SV_USE_MESHSIM_DISCRETE_MODEL),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_MESHSIM_DISCRETE_SOLID_NAME)$(LIBLINKEXT)
endif
ifeq ($(SV_USE_MESHSIM_SOLID_MODEL),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_MESHSIM_SOLID_NAME)$(LIBLINKEXT)
endif
ifeq ($(SV_USE_PARASOLID),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_PARASOLID_SOLID_NAME)$(LIBLINKEXT)
endif
ifeq ($(SV_USE_OPENCASCADE),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_OpenCASCADE_SOLID_NAME)$(LIBLINKEXT)
endif

DLLLIBS += $(PARASOLID_LIBS) $(MESHSIM_LIBS)

ifeq ($(SV_USE_OPENCASCADE),1)
    DLLLIBS += $(OPENCASCADE_LIBS)
endif

ifeq ($(SV_USE_TINYXML),1)
  DLLLIBS += $(SV_TINYXML_LIBS)
endif

ifeq ($(SV_USE_TINYXML2),1)
  DLLLIBS += $(SV_TINYXML2_LIBS)
endif

EXTRA_MOC_INCDIRS = $(MITK_PLUGIN_INCDIRS) -I../org.sv.gui.qt.datamanager

TARGET_LIB_NAME = $(SV_PLUGIN_MITKSEGMENTATION_NAME)

PLUGIN_SYMBOLIC_NAME = org_sv_gui_qt_mitksegmentation
PLUGIN_EXPORTS_NAME = org_sv_gui_qt_mitksegmentation
PLUGIN_EXPORTS_PREFIX = SV_QT_
PLUGIN_NAME = MITKSEGMENTATION

SV_COPY_DLL_TO_BIN_PLUGINS = 1

include $(TOP)/targetlib.mk
