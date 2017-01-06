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

TOP= ../../../../../../BuildWithMake

include $(TOP)/include.mk

CXXFLAGS = $(GLOBAL_CXXFLAGS) \
	   $(LOCAL_INCDIR) \
	   $(VTK_INCDIRS) \
	   $(TCLTK_INCDIR) \
           $(PYTHON_INCDIR) \
           $(ZLIB_INCDIR) \
           $(TETGEN_INCDIR) \
           $(OPENCASCADE_INCDIR) \
           $(QT_INCDIRS) $(QT_DEFS) \
	   $(ITK_INCDIRS) \
           $(MITK_INCDIRS) $(MITK_DEFS) \
           -Dorg_sv_gui_qt_modeling_EXPORTS \
          -DUS_MODULE_NAME=svModeling

HDRS	= \
    svModelCreate.h \
    svModelCreateAction.h \
    svModelLegacyLoadAction.h \
    svModelLegacySaveAction.h \
    svModelExtractPathsAction.h \
    svSegSelectionWidget.h \
    svModelEdit.h \
    svFaceListDelegate.h \
    svModelingPluginActivator.h

CXXSRCS	= \
    svModelCreate.cxx \
    svModelCreateAction.cxx \
    svModelLegacyLoadAction.cxx \
    svModelLegacySaveAction.cxx \
    svModelExtractPathsAction.cxx \
    svSegSelectionWidget.cxx \
    svModelEdit.cxx \
    svFaceListDelegate.cxx \
    svModelingPluginActivator.cxx

UIFILES =  \
    svModelCreate.ui \
    svSegSelectionWidget.ui \
    svModelEdit.ui

QRCFILES =  ../../resources/modeling.qrc

QRCFILES += org_sv_gui_qt_modeling_manifest.qrc \
           org_sv_gui_qt_modeling_cached.qrc

RCFILES = \
  ../../plugin.xml \
  ../../resources/model.png

CXXSRCS += $(addprefix moc_,$(notdir $(HDRS:.h=.cxx)))
CXXSRCS += $(addprefix qrc_,$(notdir $(QRCFILES:.qrc=.cxx)))

DLLHDRS =

DLLSRCS =

DLLLIBS += $(PYTHON_LIB) $(VTK_LIBS) $(ITK_LIBS) $(QT_LIBS) $(MITK_LIBS)

DLLLIBS +=  \
          $(SVLIBFLAG)_simvascular_qtgui_module_common$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_qtgui_module_model$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_qtgui_module_segmentation$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_qtgui_module_pathplanning$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_polydata_solid$(LIBLINKEXT) \
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

TARGET_LIB_NAME = org_sv_gui_qt_modeling

PLUGIN_SYMBOLIC_NAME = org_sv_gui_qt_modeling
PLUGIN_EXPORTS_NAME = org_sv_gui_qt_modeling
PLUGIN_EXPORTS_PREFIX = SV_QT_
PLUGIN_NAME = MODELING

SV_COPY_DLL_TO_BIN_PLUGINS = 1

include $(TOP)/targetlib.mk
