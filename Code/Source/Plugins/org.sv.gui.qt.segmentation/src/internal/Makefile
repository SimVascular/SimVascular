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

TOP=../../../../../../BuildWithMake

include $(TOP)/include.mk

CXXFLAGS = $(GLOBAL_CXXFLAGS) \
	   $(LOCAL_INCDIR) \
	   $(VTK_INCDIRS) \
	   $(TCLTK_INCDIR) \
           $(PYTHON_INCDIR) \
           $(OPENCASCADE_INCDIR) \
           $(QT_INCDIRS) $(QT_DEFS) \
	   $(ITK_INCDIRS) \
           $(MITK_INCDIRS) $(MITK_DEFS) \
           -Dorg_sv_gui_qt_segmentation_EXPORTS \
           -DUS_MODULE_NAME=svSegmentation

HDRS	= \
    svContourGroupCreate.h \
    svContourGroupCreateAction.h \
    svSegmentationLegacyLoadAction.h \
    svSegmentationLegacySaveAction.h \
    svLevelSet2DWidget.h \
    svLoftParamWidget.h \
    svSegmentation2D.h \
    svSegmentationPluginActivator.h

CXXSRCS	= \
    svContourGroupCreate.cxx \
    svContourGroupCreateAction.cxx \
    svSegmentationLegacyLoadAction.cxx \
    svSegmentationLegacySaveAction.cxx \
    svLevelSet2DWidget.cxx \
    svLoftParamWidget.cxx \
    svSegmentation2D.cxx \
    svSegmentationPluginActivator.cxx

UIFILES = \
    svContourGroupCreate.ui \
    svLevelSet2DWidget.ui \
    svLoftParamWidget.ui \
    svSegmentation2D.ui

QRCFILES = org_sv_gui_qt_segmentation_manifest.qrc \
           org_sv_gui_qt_segmentation_cached.qrc

RCFILES = \
  ../../plugin.xml \
  ../../resources/contourgroup.png

CXXSRCS += $(addprefix moc_,$(notdir $(HDRS:.h=.cxx)))
CXXSRCS += $(addprefix qrc_,$(notdir $(QRCFILES:.qrc=.cxx)))

DLLHDRS =

DLLSRCS =

DLLLIBS += $(PYTHON_LIB) $(VTK_LIBS) $(ITK_LIBS) $(QT_LIBS) $(MITK_LIBS)

DLLLIBS +=  \
          $(SVLIBFLAG)_simvascular_qtgui_module_common$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_qtgui_module_segmentation$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_qtgui_module_model$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_qtgui_module_pathplanning$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_qtgui_module_qtwidgets$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_itk_lset$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_repository$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_geom$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_utils$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_globals$(LIBLINKEXT)

ifeq ($(SV_USE_OPENCASCADE),1)
	DLLLIBS += $(SVLIBFLAG)_simvascular_opencascade_solid$(LIBLINKEXT)
	DLLLIBS += $(OPENCASCADE_LIBS)
endif

EXTRA_MOC_INCDIRS = $(MITK_PLUGIN_INCDIRS)

TARGET_LIB_NAME = org_sv_gui_qt_segmentation

PLUGIN_SYMBOLIC_NAME = org_sv_gui_qt_segmentation
PLUGIN_EXPORTS_NAME = org_sv_gui_qt_segmentation
PLUGIN_EXPORTS_PREFIX = SV_QT_
PLUGIN_NAME = SEGMENTATION

SV_COPY_DLL_TO_BIN_PLUGINS = 1

include $(TOP)/targetlib.mk
