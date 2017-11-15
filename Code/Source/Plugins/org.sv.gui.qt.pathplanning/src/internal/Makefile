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
          -DUS_MODULE_NAME=svPathPlanning

HDRS	= \
    svPathCreate.h \
    svPathCreateAction.h \
    svPathLoadAction.h \
    svPathLegacySaveAction.h \
    svPathSmooth.h \
    svPathEdit.h \
    svPathPoint2DSizeAction.h \
    svPathPoint3DSizeAction.h \
    svPathPlanningPluginActivator.h

CXXSRCS	=  \
    svPathCreate.cxx \
    svPathCreateAction.cxx \
    svPathLoadAction.cxx \
    svPathLegacySaveAction.cxx \
    svPathSmooth.cxx \
    svPathEdit.cxx \
    svPathPoint2DSizeAction.cxx \
    svPathPoint3DSizeAction.cxx \
    svPathPlanningPluginActivator.cxx

UIFILES = \
    svPathCreate.ui \
    svPathSmooth.ui \
    svPathEdit.ui

QRCFILES =

QRCFILES += org_sv_gui_qt_pathplanning_manifest.qrc \
            org_sv_gui_qt_pathplanning_cached.qrc

RCFILES = \
    ../../plugin.xml \
    ../../resources/pathedit.png

CXXSRCS += $(addprefix moc_,$(HDRS:.h=.cxx))

CXXSRCS += $(addprefix qrc_,$(notdir $(QRCFILES:.qrc=.cxx)))

DLLHDRS =

DLLSRCS =

DLLLIBS += $(PYTHON_LIB) $(VTK_LIBS) $(ITK_LIBS) $(QT_LIBS) $(MITK_LIBS)

DLLLIBS +=  \
          $(SVLIBFLAG)$(SV_PLUGIN_DATAMANAGER_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_COMMON_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_SEGMENTATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PATH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PROJECTMANAGEMENT_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_QTWIDGETS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_ITK_LSET_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GLOBALS_NAME)$(LIBLINKEXT)

EXTRA_MOC_INCDIRS = $(MITK_PLUGIN_INCDIRS) -I../../../org.sv.gui.qt.datamanager/src/internal

TARGET_LIB_NAME = $(SV_PLUGIN_PATHPLANNING_NAME)

PLUGIN_SYMBOLIC_NAME = org_sv_gui_qt_pathplanning
PLUGIN_EXPORTS_NAME = org_sv_gui_qt_pathplanning
PLUGIN_EXPORTS_PREFIX = SV_QT_
PLUGIN_NAME = PATHPLANNING

SV_COPY_DLL_TO_BIN_PLUGINS = 1

include $(TOP)/targetlib.mk
