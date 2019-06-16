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
           $(ZLIB_INCDIR) \
           $(TETGEN_INCDIR) \
           $(MESHSIM_INCDIR) \
           $(PARASOLID_INCDIR) \
           $(QT_INCDIRS) $(QT_DEFS) \
	   $(ITK_INCDIRS) \
           $(SV_TINYXML_INCDIR) $(SV_TINYXML2_INCDIR) \
           $(MITK_INCDIRS) $(MITK_DEFS) \
           -Dorg_sv_gui_qt_modeling_EXPORTS \
          -DUS_MODULE_NAME=sv4guiModuleModeling

ifeq ($(SV_USE_OPENCASCADE),1)
    CXXFLAGS += $(OPENCASCADE_INCDIR)
endif

HDRS	= \
    sv4gui_ModelCreate.h \
    sv4gui_ModelCreateAction.h \
    sv4gui_ModelLoadAction.h \
    sv4gui_ModelLegacySaveAction.h \
    sv4gui_ModelExtractPathsAction.h \
    sv4gui_SegSelectionWidget.h \
    sv4gui_ModelEdit.h \
    sv4gui_FaceListDelegate.h \
    sv4gui_ModelFaceInfoExportAction.h \
    sv4gui_ModelingPluginActivator.h \
    sv4gui_LoftingPreferencePage.h \
    sv4gui_CapSelectionWidget.h

CXXSRCS	= \
    sv4gui_ModelCreate.cxx \
    sv4gui_ModelCreateAction.cxx \
    sv4gui_ModelLoadAction.cxx \
    sv4gui_ModelLegacySaveAction.cxx \
    sv4gui_ModelExtractPathsAction.cxx \
    sv4gui_SegSelectionWidget.cxx \
    sv4gui_ModelEdit.cxx \
    sv4gui_FaceListDelegate.cxx \
    sv4gui_ModelFaceInfoExportAction.cxx \
    sv4gui_ModelingPluginActivator.cxx \
    sv4gui_LoftingPreferencePage.cxx \
    sv4gui_CapSelectionWidget.cxx

UIFILES =  \
    sv4gui_ModelCreate.ui \
    sv4gui_SegSelectionWidget.ui \
    sv4gui_ModelEdit.ui \
    sv4gui_CapSelectionWidget.ui

QRCFILES =  resources/modeling.qrc

QRCFILES += org_sv_gui_qt_modeling_manifest.qrc \
           org_sv_gui_qt_modeling_cached.qrc

RCFILES = \
  plugin.xml \
  resources/model.png

CXXSRCS += $(addprefix moc_,$(notdir $(HDRS:.h=.cxx)))
CXXSRCS += $(addprefix qrc_,$(notdir $(QRCFILES:.qrc=.cxx)))

DLLHDRS =

DLLSRCS =

DLLLIBS += $(PYTHON_LIB) $(VTK_LIBS) $(ITK_LIBS) $(QT_LIBS) $(MITK_LIBS)

DLLLIBS +=  \
          $(SVLIBFLAG)$(SV_PLUGIN_DATAMANAGER_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_COMMON_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_MODEL_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_SEGMENTATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PATH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PROJECTMANAGEMENT_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_PLUGIN_SEGMENTATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_POLYDATA_SOLID_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_ITK_LSET_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_PATH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GLOBALS_NAME)$(LIBLINKEXT)

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

TARGET_LIB_NAME = $(SV_PLUGIN_MODELING_NAME)

PLUGIN_SYMBOLIC_NAME = org_sv_gui_qt_modeling
PLUGIN_EXPORTS_NAME = org_sv_gui_qt_modeling
PLUGIN_EXPORTS_PREFIX = SV_QT_
PLUGIN_NAME = MODELING

SV_COPY_DLL_TO_BIN_PLUGINS = 1

include $(TOP)/targetlib.mk
