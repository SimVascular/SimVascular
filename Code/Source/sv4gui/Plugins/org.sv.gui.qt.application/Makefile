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
           -Dorg_sv_gui_qt_application_EXPORTS \
           -DUS_MODULE_NAME=svApplication

HDRS	= \
  sv4gui_Application.h \
  sv4gui_ApplicationPluginActivator.h \
  sv4gui_FileCreateProjectAction.h \
  sv4gui_FileOpenProjectAction.h \
  sv4gui_FileSaveProjectAction.h \
  sv4gui_FileSaveProjectAsAction.h \
  sv4gui_CloseProjectAction.h \
  sv4gui_ProjectCreate.h \
  sv4gui_WorkbenchWindowAdvisor.h \
  sv4gui_WorkbenchWindowAdvisorHack.h \
  sv4gui_AboutDialog.h \
  sv4gui_WorkbenchIntroPart.h \
  sv4gui_DefaultPerspective.h \
  sv4gui_MitkApp.h \
  sv4gui_Main.cxx

CXXSRCS	= \
  sv4gui_Application.cxx \
  sv4gui_ApplicationPluginActivator.cxx \
  sv4gui_FileCreateProjectAction.cxx \
  sv4gui_FileOpenProjectAction.cxx \
  sv4gui_FileSaveProjectAction.cxx \
  sv4gui_FileSaveProjectAsAction.cxx \
  sv4gui_CloseProjectAction.cxx \
  sv4gui_ProjectCreate.cxx \
  sv4gui_WorkbenchWindowAdvisor.cxx \
  sv4gui_AppWorkbenchAdvisor.cxx \
  sv4gui_AboutDialog.cxx \
  sv4gui_WorkbenchIntroPart.cxx \
  sv4gui_DefaultPerspective.cxx \
  sv4gui_WorkbenchWindowAdvisorHack.cxx \
  sv4gui_MitkApp.cxx \
  sv4gui_Main.cxx

UIFILES = \
  sv4gui_ProjectCreate.ui \
  sv4gui_AboutDialog.ui \
  sv4gui_WelcomeScreenViewControls.ui

QRCFILES = \
  resources/svApplication.qrc \
  resources/welcome/svWelcomeScreenView.qrc

QRCFILES += org_sv_gui_qt_application_manifest.qrc \
            org_sv_gui_qt_application_cached.qrc

RCFILES = \
  plugin.xml \
  resources/icon.png

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
          $(SVLIBFLAG)$(SV_LIB_MODULE_ROM_SIMULATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_SVFSI_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_MODEL_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PATH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_QTWIDGETS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_MESH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PROJECTMANAGEMENT_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_ITK_LSET_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GLOBALS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_PATH_NAME)$(LIBLINKEXT)

ifeq ($(SV_USE_TINYXML),1)
  DLLLIBS += $(SV_TINYXML_LIBS)
endif

ifeq ($(SV_USE_TINYXML2),1)
  DLLLIBS += $(SV_TINYXML2_LIBS)
endif

EXTRA_MOC_INCDIRS = $(MITK_PLUGIN_INCDIRS) -I../org.sv.gui.qt.datamanager

TARGET_LIB_NAME = $(SV_PLUGIN_APPLICATION_NAME)

PLUGIN_SYMBOLIC_NAME = org_sv_gui_qt_application
PLUGIN_EXPORTS_NAME = org_sv_gui_qt_application
PLUGIN_EXPORTS_PREFIX = SV_QT_
PLUGIN_NAME = APPLICATION

SV_COPY_DLL_TO_BIN_PLUGINS = 1

include $(TOP)/targetlib.mk
