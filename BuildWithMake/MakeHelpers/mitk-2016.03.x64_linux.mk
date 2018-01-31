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

# ----
# MITK
# ----

MITK_BINDIR = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/mitk-2016.03

MITK_US_RESOURCE_COMPILER = $(MITK_BINDIR)/bin/usResourceCompiler

MITK_LIBDIRS = $(MITK_BINDIR)/lib
MITK_PLUGIN_DIR = $(MITK_BINDIR)/lib/plugins
SV_MITK_PLUGIN_PATH = $(MITK_BINDIR)/lib/plugins
MITK_BINDIRS = $(MITK_BINDIR)/bin

SV_MITK_SO_PATH = "$(MITK_LIBDIRS):$(MITK_BINDIRS):$(SV_MITK_PLUGIN_PATH)"
MITK_DLLS    = $(MITK_BINDIRS)/*.$(SOEXT)

MITK_SYS_LIBS  =

# Poco needs?

MITK_DEFS = -DSV_NO_PYTHONQT_ALL

ifeq ($(SV_USE_SHARED),0)
  MITK_DEFS += -DUS_STATIC_MODULE
endif

MITK_INCDIRS = \
           -I$(MITK_BINDIR)/include \
           -I$(MITK_BINDIR)/include/ctk \
           -I$(MITK_BINDIR)/include/eigen3 \
           -I$(MITK_BINDIR)/include/mitk \
           -I$(MITK_BINDIR)/include/mitk/configs \
           -I$(MITK_BINDIR)/include/mitk/exports \
           -I$(MITK_BINDIR)/include/mitk/ui_files \
           -I$(MITK_BINDIR)/include/mitk/AlgorithmsExt \
           -I$(MITK_BINDIR)/include/mitk/AlgorithmsExt/include \
           -I$(MITK_BINDIR)/include/mitk/AppUtil/include \
           -I$(MITK_BINDIR)/include/mitk/Core \
           -I$(MITK_BINDIR)/include/mitk/Core/include \
           -I$(MITK_BINDIR)/include/mitk/CppMicroServices/core \
           -I$(MITK_BINDIR)/include/mitk/CppMicroServices/core/include \
           -I$(MITK_BINDIR)/include/mitk/DataTypesExt \
           -I$(MITK_BINDIR)/include/mitk/DataTypesExt/include \
           -I$(MITK_BINDIR)/include/mitk/MapperExt \
           -I$(MITK_BINDIR)/include/mitk/MapperExt/include \
           -I$(MITK_BINDIR)/include/mitk/PlanarFigure \
           -I$(MITK_BINDIR)/include/mitk/PlanarFigure/include \
           -I$(MITK_BINDIR)/include/mitk/QtWidgets \
           -I$(MITK_BINDIR)/include/mitk/QtWidgets/include \
           -I$(MITK_BINDIR)/include/mitk/QtWidgetsExt \
           -I$(MITK_BINDIR)/include/mitk/QtWidgetsExt/include \
           -I$(MITK_BINDIR)/include/mitk/SceneSerialization \
           -I$(MITK_BINDIR)/include/mitk/SceneSerialization/include \
           -I$(MITK_BINDIR)/include/mitk/Utilities/mbilog \
           -I$(MITK_BINDIR)/include/PythonQt \
           -I$(MITK_BINDIR)/include/tinyxml \
           -I$(MITK_BINDIR)/include/mitk/Modules/ContourModel/DataManagement \
           -I$(MITK_BINDIR)/include/mitk/Modules/CppMicroServices/core/src/module \
           -I$(MITK_BINDIR)/include/mitk/Modules/CppMicroServices/core/src/service \
           -I$(MITK_BINDIR)/include/mitk/Modules/CppMicroServices/core/src/util \
           -I$(MITK_BINDIR)/include/mitk/Modules/ImageDenoising \
           -I$(MITK_BINDIR)/include/mitk/Modules/LegacyGL \
           -I$(MITK_BINDIR)/include/mitk/Modules/Multilabel \
           -I$(MITK_BINDIR)/include/mitk/Modules/Overlays \
           -I$(MITK_BINDIR)/include/mitk/Modules/Segmentation/Algorithms \
           -I$(MITK_BINDIR)/include/mitk/Modules/Segmentation/Controllers \
           -I$(MITK_BINDIR)/include/mitk/Modules/Segmentation/Interactions \
           -I$(MITK_BINDIR)/include/mitk/Modules/SegmentationUI/Qmitk \
           -I$(MITK_BINDIR)/include/mitk/Modules/SurfaceInterpolation \
           -I$(MITK_BINDIR)/include/mitk/Modules/ContourModel \
           -I$(MITK_BINDIR)/include/mitk/Modules/ImageDenoising \
           -I$(MITK_BINDIR)/include/mitk/Modules/LegacyGL \
           -I$(MITK_BINDIR)/include/mitk/Modules/Multilabel \
           -I$(MITK_BINDIR)/include/mitk/Modules/Overlays \
           -I$(MITK_BINDIR)/include/mitk/Modules/QtWidgets \
           -I$(MITK_BINDIR)/include/mitk/Modules/Segmentation \
           -I$(MITK_BINDIR)/include/mitk/Modules/Segmentation/Interactions \
           -I$(MITK_BINDIR)/include/mitk/Modules/SegmentationUI \
           -I$(MITK_BINDIR)/include/mitk/Modules/SurfaceInterpolation \
           -I$(MITK_BINDIR)/include/mitk/Utilities/mbilog

# Plugin Includes (exports give in include/mitk/exports)
MITK_PLUGIN_INCDIRS = \
           -I$(MITK_BINDIR)/include/ctk/PluginFramework \
           -I$(MITK_BINDIR)/include/mitk/plugins/org.mitk.core.services \
           -I$(MITK_BINDIR)/include/mitk/plugins/org.mitk.gui.common \
           -I$(MITK_BINDIR)/include/mitk/plugins/org.mitk.gui.qt.application \
           -I$(MITK_BINDIR)/include/mitk/plugins/org.mitk.gui.qt.common \
           -I$(MITK_BINDIR)/include/mitk/plugins/org.mitk.gui.qt.common.legacy \
           -I$(MITK_BINDIR)/include/mitk/plugins/org.mitk.gui.qt.datamanager \
           -I$(MITK_BINDIR)/include/mitk/plugins/org.mitk.gui.qt.stdmultiwidgeteditor \
           -I$(MITK_BINDIR)/include/mitk/plugins/org.mitk.gui.qt.ext \
           -I$(MITK_BINDIR)/include/mitk/plugins/org.blueberry.ui.qt \
           -I$(MITK_BINDIR)/include/mitk/plugins/org.blueberry.ui.qt/application \
           -I$(MITK_BINDIR)/include/mitk/plugins/org.blueberry.ui.qt/intro \
           -I$(MITK_BINDIR)/include/mitk/plugins/org.blueberry.core.runtime \
           -I$(MITK_BINDIR)/include/mitk/plugins/org.blueberry.core.runtime/application \
           -I$(MITK_BINDIR)/include/mitk/plugins/org.blueberry.core.runtime/registry

EXTRA_MOC_INCDIRS := $(MITK_PLUGIN_INCDIRS)

MITK_INCDIRS += $(MITK_PLUGIN_INCDIRS)

MITK_PLUGIN_LIBS += \
             $(LIBFLAG)org_mitk_gui_qt_application$(LIBLINKEXT) \
             $(LIBFLAG)org_mitk_gui_qt_datamanager$(LIBLINKEXT) \
             $(LIBFLAG)org_mitk_gui_qt_stdmultiwidgeteditor$(LIBLINKEXT) \
             $(LIBFLAG)org_mitk_gui_qt_ext$(LIBLINKEXT) \
             $(LIBFLAG)org_mitk_gui_common$(LIBLINKEXT) \
             $(LIBFLAG)org_mitk_gui_qt_common$(LIBLINKEXT) \
             $(LIBFLAG)org_mitk_gui_qt_application$(LIBLINKEXT) \
             $(LIBFLAG)org_mitk_gui_qt_common_legacy$(LIBLINKEXT) \
             $(LIBFLAG)org_blueberry_ui_qt$(LIBLINKEXT) \
             $(LIBFLAG)org_blueberry_core_runtime$(LIBLINKEXT) \
             $(LIBFLAG)org_mitk_core_services$(LIBLINKEXT)

### this library doesn't seem to be on linux
##         $(LIBFLAG)CTKVisualizationVTKCorePythonQt$(LIBLINKEXT)

MITK_LIBS += \
           $(LIBPATH_COMPILER_FLAG)$(MITK_LIBDIRS) \
           $(LIBPATH_COMPILER_FLAG)$(MITK_BINDIRS) \
           $(LIBPATH_COMPILER_FLAG)$(MITK_PLUGIN_DIR) \
           $(MITK_PLUGIN_LIBS) \
           $(LIBFLAG)CppMicroServices$(LIBLINKEXT) \
           $(LIBFLAG)CTKCommandLineModulesBackendLocalProcess$(LIBLINKEXT) \
           $(LIBFLAG)CTKCommandLineModulesCore$(LIBLINKEXT) \
           $(LIBFLAG)CTKCommandLineModulesFrontendQtGui$(LIBLINKEXT) \
           $(LIBFLAG)CTKCore$(LIBLINKEXT) \
           $(LIBFLAG)CTKDICOMCore$(LIBLINKEXT) \
           $(LIBFLAG)CTKDICOMWidgets$(LIBLINKEXT) \
           $(LIBFLAG)CTKPluginFramework$(LIBLINKEXT) \
           $(LIBFLAG)CTKScriptingPythonCore$(LIBLINKEXT) \
           $(LIBFLAG)CTKScriptingPythonWidgets$(LIBLINKEXT) \
           $(LIBFLAG)CTKVisualizationVTKCore$(LIBLINKEXT) \
           $(LIBFLAG)CTKWidgets$(LIBLINKEXT) \
           $(LIBFLAG)CTKXNATCore$(LIBLINKEXT) \
           $(LIBFLAG)mbilog$(LIBLINKEXT) \
           $(LIBFLAG)qtsingleapplication$(LIBLINKEXT) \
           $(LIBFLAG)MitkAlgorithmsExt$(LIBLINKEXT) \
           $(LIBFLAG)MitkAppUtil$(LIBLINKEXT) \
           $(LIBFLAG)MitkCore$(LIBLINKEXT) \
           $(LIBFLAG)MitkDataTypesExt$(LIBLINKEXT) \
           $(LIBFLAG)MitkImageDenoising$(LIBLINKEXT) \
           $(LIBFLAG)MitkMapperExt$(LIBLINKEXT) \
           $(LIBFLAG)MitkQtWidgets$(LIBLINKEXT) \
           $(LIBFLAG)MitkQtWidgetsExt$(LIBLINKEXT) \
           $(LIBFLAG)MitkSceneSerialization$(LIBLINKEXT) \
           $(LIBFLAG)MitkSegmentation$(LIBLINKEXT) \
           $(LIBFLAG)MitkSegmentationUI$(LIBLINKEXT) \
           $(LIBFLAG)MitkSurfaceInterpolation$(LIBLINKEXT) \
           $(LIBFLAG)PythonQt$(LIBLINKEXT) \
           $(LIBFLAG)tinyxml$(LIBLINKEXT) \
           $(LIBFLAG)PocoFoundation$(LIBLINKEXT) \
           $(LIBFLAG)PocoJSON$(LIBLINKEXT) \
           $(LIBFLAG)PocoNet$(LIBLINKEXT) \
           $(LIBFLAG)PocoXML$(LIBLINKEXT) \
           $(LIBFLAG)PocoZip$(LIBLINKEXT) \
           $(LIBFLAG)PocoUtil$(LIBLINKEXT) \
           $(LIBFLAG)CppMicroServices$(LIBLINKEXT)

MITK_LIBS += $(MITK_PLUGIN_LIBS)
