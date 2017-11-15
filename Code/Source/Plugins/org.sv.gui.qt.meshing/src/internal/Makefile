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
           $(ZLIB_INCDIR) \
           $(TETGEN_INCDIR) \
           $(QT_INCDIRS) $(QT_DEFS) \
	   $(ITK_INCDIRS) \
           $(MITK_INCDIRS) $(MITK_DEFS) \
           $(MESHSIM_INCDIR) \
           $(PARASOLID_INCDIR) \
           -Dorg_sv_gui_qt_meshing_EXPORTS \
           -DUS_MODULE_NAME=svMeshing

ifeq ($(SV_USE_OPENCASCADE_QT_GUI),1)
    CXXFLAGS += $(OPENCASCADE_INCDIR)
endif

HDRS	= \
    svMeshCreate.h \
    svMeshCreateAction.h \
    svMeshEdit.h \
    svLocalTableDelegate.h \
    svMeshLegacySaveAction.h \
    svMeshLoadSurfaceAction.h \
    svMeshLoadVolumeAction.h \
    svMeshingPluginActivator.h

CXXSRCS	= \
    svMeshCreate.cxx \
    svMeshCreateAction.cxx \
    svMeshEdit.cxx \
    svVtkMeshSphereWidget.cxx \
    svLocalTableDelegate.cxx \
    svMeshLegacySaveAction.cxx \
    svMeshLoadSurfaceAction.cxx \
    svMeshLoadVolumeAction.cxx \
    svMeshingPluginActivator.cxx

UIFILES = \
    svMeshCreate.ui \
    svMeshEdit.ui

QRCFILES = org_sv_gui_qt_meshing_manifest.qrc \
           org_sv_gui_qt_meshing_cached.qrc

RCFILES = \
  ../../plugin.xml \
  ../../resources/mesh.png

CXXSRCS += $(addprefix moc_,$(notdir $(HDRS:.h=.cxx)))
CXXSRCS += $(addprefix qrc_,$(notdir $(QRCFILES:.qrc=.cxx)))

DLLHDRS =

DLLSRCS =

DLLLIBS += $(PYTHON_LIB) $(VTK_LIBS) $(ITK_LIBS) $(QT_LIBS) $(MITK_LIBS)

DLLLIBS +=  \
          $(SVLIBFLAG)$(SV_PLUGIN_DATAMANAGER_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_COMMON_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_SEGMENTATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_MESH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_MODEL_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PATH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PROJECTMANAGEMENT_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_QTWIDGETS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_ITK_LSET_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GLOBALS_NAME)$(LIBLINKEXT)

ifeq ($(SV_USE_MESHSIM_DISCRETE_MODEL_QT_GUI),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_MESHSIM_DISCRETE_SOLID_NAME)$(LIBLINKEXT)
endif
ifeq ($(SV_USE_MESHSIM_SOLID_MODEL_QT_GUI),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_MESHSIM_SOLID_NAME)$(LIBLINKEXT)
endif
ifeq ($(SV_USE_PARASOLID_QT_GUI),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_PARASOLID_SOLID_NAME)$(LIBLINKEXT)
endif
ifeq ($(SV_USE_OPENCASCADE_QT_GUI),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_OpenCASCADE_SOLID_NAME)$(LIBLINKEXT)
endif

DLLLIBS += $(PARASOLID_LIBS) $(MESHSIM_LIBS)

ifeq ($(SV_USE_OPENCASCADE_QT_GUI),1)
    DLLLIBS += $(OPENCASCADE_LIBS)
endif

EXTRA_MOC_INCDIRS = $(MITK_PLUGIN_INCDIRS) -I../../../org.sv.gui.qt.datamanager/src/internal

TARGET_LIB_NAME = $(SV_PLUGIN_MESHING_NAME)

PLUGIN_SYMBOLIC_NAME = org_sv_gui_qt_meshing
PLUGIN_EXPORTS_NAME = org_sv_gui_qt_meshing
PLUGIN_EXPORTS_PREFIX = SV_QT_
PLUGIN_NAME = MESHING

SV_COPY_DLL_TO_BIN_PLUGINS = 1

include $(TOP)/targetlib.mk
