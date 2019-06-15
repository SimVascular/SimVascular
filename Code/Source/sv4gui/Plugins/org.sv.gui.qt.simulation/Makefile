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
           $(QT_INCDIRS) $(QT_DEFS) \
	   $(ITK_INCDIRS) \
           $(SV_TINYXML_INCDIR) $(SV_TINYXML2_INCDIR) \
           $(MITK_INCDIRS) $(MITK_DEFS) \
           -Dorg_sv_gui_qt_simulation_EXPORTS \
           -DUS_MODULE_NAME=sv4guiModuleSimulation

HDRS	= \
    sv4gui_SimJobCreate.h \
    sv4gui_SimJobCreateAction.h \
    sv4gui_CapBCWidget.h \
    sv4gui_SplitBCWidget.h \
    sv4gui_TableCapDelegate.h \
    sv4gui_TableSolverDelegate.h \
    sv4gui_SimulationView.h \
    sv4gui_SimulationPreferencePage.h \
    sv4gui_MPIPreferencePage.h \
    sv4gui_SimulationPreferences.h \
    sv4gui_MPIPreferences.h \
    sv4gui_SimJobStopAction.h \
    sv4gui_SimJobExportAction.h \
    sv4gui_SimulationPluginActivator.h \
    sv4gui_ProcessHandler.h \
    sv4gui_SolverProcessHandler.h

CXXSRCS	= \
    sv4gui_SimJobCreate.cxx \
    sv4gui_SimJobCreateAction.cxx \
    sv4gui_CapBCWidget.cxx \
    sv4gui_SplitBCWidget.cxx \
    sv4gui_TableCapDelegate.cxx \
    sv4gui_TableSolverDelegate.cxx \
    sv4gui_SimulationView.cxx \
    sv4gui_SimulationPreferencePage.cxx \
    sv4gui_MPIPreferencePage.cxx \
    sv4gui_SimulationPreferences.cxx \
    sv4gui_MPIPreferences.cxx \
    sv4gui_SimJobStopAction.cxx \
    sv4gui_SimJobExportAction.cxx \
    sv4gui_SimulationPluginActivator.cxx \
    sv4gui_ProcessHandler.cxx \
    sv4gui_SolverProcessHandler.cxx

ifeq ($(SV_USE_WIN32_REGISTRY),1)
  HDRS += sv4gui_win32_use_registry.h
  CXXSRCS += sv4gui_win32_use_registry.cxx
endif

UIFILES = \
    sv4gui_SimJobCreate.ui \
    sv4gui_CapBCWidget.ui \
    sv4gui_SplitBCWidget.ui \
    sv4gui_SimulationPreferencePage.ui \
    sv4gui_SimulationView.ui \
    sv4gui_MPIPreferencePage.ui

QRCFILES = resources/simulation.qrc

QRCFILES += org_sv_gui_qt_simulation_manifest.qrc \
            org_sv_gui_qt_simulation_cached.qrc

RCFILES = \
  plugin.xml \
  resources/simulation.png

CXXSRCS += $(addprefix moc_,$(notdir $(HDRS:.h=.cxx)))
CXXSRCS += $(addprefix qrc_,$(notdir $(QRCFILES:.qrc=.cxx)))

DLLHDRS =

DLLSRCS =

DLLLIBS += $(PYTHON_LIB) $(VTK_LIBS) $(ITK_LIBS) $(QT_LIBS) $(MITK_LIBS)

DLLLIBS +=  \
          $(SVLIBFLAG)$(SV_PLUGIN_DATAMANAGER_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_COMMON_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_SEGMENTATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_SIMULATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_MODEL_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_MESH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PATH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PROJECTMANAGEMENT_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_QTWIDGETS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_ITK_LSET_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GLOBALS_NAME)$(LIBLINKEXT)

ifeq ($(SV_USE_TINYXML),1)
  DLLLIBS += $(SV_TINYXML_LIBS)
endif

ifeq ($(SV_USE_TINYXML2),1)
  DLLLIBS += $(SV_TINYXML2_LIBS)
endif

EXTRA_MOC_INCDIRS = $(MITK_PLUGIN_INCDIRS) -I../org.sv.gui.qt.datamanager

TARGET_LIB_NAME = $(SV_PLUGIN_SIMULATION_NAME)

PLUGIN_SYMBOLIC_NAME = org_sv_gui_qt_simulation
PLUGIN_EXPORTS_NAME = org_sv_gui_qt_simulation
PLUGIN_EXPORTS_PREFIX = SV_QT_
PLUGIN_NAME = SIMULATION

SV_COPY_DLL_TO_BIN_PLUGINS = 1

include $(TOP)/targetlib.mk
