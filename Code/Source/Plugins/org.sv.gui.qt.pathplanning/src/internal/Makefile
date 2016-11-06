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
           $(QT_INCDIRS) $(QT_DEFS) \
	   $(ITK_INCDIRS) \
           $(MITK_INCDIRS) $(MITK_DEFS) \
           -Dorg_sv_gui_qt_pathplanning_EXPORTS \
           -I../../../PluginFramework \
           -I../../../org.mitk.gui.common/src \
           -I../../../org.mitk.gui.qt.datamanager/src \
           -I../../../org.mitk.gui.qt.common/src \
           -I../../../org.mitk.gui.qt.common.legacy/src \
           -I../../../org.blueberry.ui.qt/src \
           -I../../../org.blueberry.core.runtime/src \
           -I../../../org.blueberry.core.runtime/src/registry \
           -I../../../org.mitk.core.services/src \
           -I../../.. \
          -DUS_MODULE_NAME=svPathPlanning
HDRS	= \
    svPathCreate.h \
    svPathCreateAction.h \
    svPathLegacyLoadAction.h \
    svPathLegacySaveAction.h \
    svPathSmooth.h \
    svPathEdit.h \
    svPathPlanningPluginActivator.h

CXXSRCS	=  \
    svPathCreate.cxx \
    svPathCreateAction.cxx \
    svPathLegacyLoadAction.cxx \
    svPathLegacySaveAction.cxx \
    svPathSmooth.cxx \
    svPathEdit.cxx \
    svPathPlanningPluginActivator.cxx

UIFILES = \
    svPathCreate.ui \
    svPathSmooth.ui \
    svPathEdit.ui

QRCFILES = \
    ../../resources/pathplanning.qrc

RCFILES = \
    ../../plugin.xml \
    ../../resources/pathedit.png

CXXSRCS += $(addprefix moc_,$(HDRS:.h=.cxx))

#CXXSRCS += $(addprefix ui_,$(UIFILES:.ui=.cxx))

CXXSRCS += $(addprefix rcc_,$(notdir $(QRCFILES:.qrc=.cxx)))

DLLHDRS =

DLLSRCS =

DLLLIBS += $(PYTHON_LIB) $(VTK_LIBS) $(ITK_LIBS) $(QT_LIBS) $(MITK_LIBS)

PLUGINLIBSDIR = C:/sv/mitk-2016.03/MITK-build/lib/plugins/RelWithDebInfo
DLLLIBS += $(PLUGINLIBSDIR)/org_mitk_gui_common.lib \
           $(PLUGINLIBSDIR)/org_mitk_gui_qt_datamanager.lib \
           $(PLUGINLIBSDIR)/org_mitk_gui_qt_common.lib \
           $(PLUGINLIBSDIR)/org_mitk_gui_qt_common_legacy.lib \
           $(PLUGINLIBSDIR)/org_blueberry_ui_qt.lib \
           $(PLUGINLIBSDIR)/org_blueberry_core_runtime.lib \
           $(PLUGINLIBSDIR)/org_mitk_core_services.lib \

DLLLIBS +=  \
          $(SVLIBFLAG)_simvascular_qtgui_module_common$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_qtgui_module_segmentation$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_qtgui_module_pathplanning$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_qtgui_module_qtwidgets$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_itk_lset$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_repository$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_geom$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_utils$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_globals$(LIBLINKEXT)

EXTRA_MOC_INCDIRS = -I../../../PluginFramework \
          -I../../../org.mitk.gui.qt.datamanager/src

TARGET_LIB_NAME = simvascular_qtgui_plugin_pathplanning

PLUGIN_DIR_NAME = org_sv_gui_qt_pathplanning

PLUGIN_NAME = PATHPLANNING

SV_COPY_DLL_TO_BIN_PLUGINS = 1

include $(TOP)/targetlib.mk
