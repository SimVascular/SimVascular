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
           -Dorg_sv_gui_qt_romsimulation_EXPORTS \
           -DUS_MODULE_NAME=sv4guiModuleROMSimulation

HDRS	= \
    sv4gui_ConvertProcessHandlerROM.h \
    sv4gui_ConvertWorkerROM.h \
    sv4gui_ROMSimJobCreate.h \
    sv4gui_ROMSimJobCreateAction.h \
    sv4gui_CapBCWidgetROM.h \
    sv4gui_SplitBCWidgetROM.h \
    sv4gui_TableCapDelegateROM.h \
    sv4gui_TableSolverDelegateROM.h \
    sv4gui_ROMSimulationExtractCenterlines.h \
    sv4gui_ROMSimulationLinesContainer.h \
    sv4gui_ROMSimulationLinesMapper.h \
    sv4gui_ROMSimulationView.h \
    sv4gui_ROMSimulationPreferencePage.h \
    sv4gui_ROMSimulationPython.h \
    sv4gui_ROMSimulationPythonConvert.h \
    sv4gui_ROMSimJobStopAction.h \
    sv4gui_ROMSimJobExportAction.h \
    sv4gui_ROMSimulationPluginActivator.h \
    sv4gui_ProcessHandlerROM.h \
    sv4gui_SolverProcessHandlerROM.h

CXXSRCS	= \
    sv4gui_ConvertProcessHandlerROM.cxx \
    sv4gui_ConvertWorkerROM.cxx \
    sv4gui_ROMSimJobCreate.cxx \
    sv4gui_ROMSimJobCreateAction.cxx \
    sv4gui_CapBCWidgetROM.cxx \
    sv4gui_SplitBCWidgetROM.cxx \
    sv4gui_TableCapDelegateROM.cxx \
    sv4gui_TableSolverDelegateROM.cxx \
    sv4gui_ROMSimulationExtractCenterlines.cxx \
    sv4gui_ROMSimulationLinesContainer.cxx \
    sv4gui_ROMSimulationLinesMapper.cxx \
    sv4gui_ROMSimulationView.cxx \
    sv4gui_ROMSimulationPreferencePage.cxx \
    sv4gui_ROMSimulationPython.cxx \
    sv4gui_ROMSimulationPythonConvert.cxx \
    sv4gui_ROMSimJobStopAction.cxx \
    sv4gui_ROMSimJobExportAction.cxx \
    sv4gui_ROMSimulationPluginActivator.cxx \
    sv4gui_ProcessHandlerROM.cxx \
    sv4gui_SolverProcessHandlerROM.cxx

UIFILES = \
    sv4gui_ROMSimJobCreate.ui \
    sv4gui_CapBCWidgetROM.ui \
    sv4gui_SplitBCWidgetROM.ui \
    sv4gui_ROMSimulationPreferencePage.ui \
    sv4gui_ROMSimulationView.ui

QRCFILES = resources/romsimulation.qrc

QRCFILES += org_sv_gui_qt_romsimulation_manifest.qrc \
            org_sv_gui_qt_romsimulation_cached.qrc

RCFILES = \
  plugin.xml \
  resources/romsimulation.png

CXXSRCS += $(addprefix moc_,$(notdir $(HDRS:.h=.cxx)))
CXXSRCS += $(addprefix qrc_,$(notdir $(QRCFILES:.qrc=.cxx)))

DLLHDRS =

DLLSRCS =

DLLLIBS += $(PYTHON_LIB) $(VTK_LIBS) $(ITK_LIBS) $(QT_LIBS) $(MITK_LIBS)

DLLLIBS +=  \
          $(SVLIBFLAG)$(SV_PLUGIN_MODELING_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_PLUGIN_DATAMANAGER_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_COMMON_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_SEGMENTATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_SIMULATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_ROM_SIMULATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_MODEL_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_MESH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PATH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PROJECTMANAGEMENT_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_QTWIDGETS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_POLYDATA_SOLID_NAME)$(LIBLINKEXT) \
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

TARGET_LIB_NAME = $(SV_PLUGIN_ROM_SIMULATION_NAME)

PLUGIN_SYMBOLIC_NAME = org_sv_gui_qt_romsimulation
PLUGIN_EXPORTS_NAME = org_sv_gui_qt_romsimulation
PLUGIN_EXPORTS_PREFIX = SV_QT_
PLUGIN_NAME = ROMSIMULATION

SV_COPY_DLL_TO_BIN_PLUGINS = 1

include $(TOP)/targetlib.mk
