# ----
# MITK
# ----

MITK_BINDIR = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/mitk-2016.03

MITK_LIBDIRS = $(MITK_BINDIR)/lib
MITK_BINDIRS = $(MITK_BINDIR)/bin
MITK_SO_PATH = $(MITK_BINDIRS)
MITK_DLLS    = $(MITK_BINDIRS)/*.$(SOEXT)

MITK_SYS_LIBS  = 

# Poco requires the _WIN32_WINNT to be set!  Set the value to Windows 10
# should probably use #define _WIN32_WINNT_WIN10                  0x0A00 // Windows 10

MITK_DEFS = -D_WIN32_WINNT=0x0A00 -DUS_MODULE_NAME=svlib

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

MITK_LIBS = \
           $(LIBPATH_COMPILER_FLAG)$(MITK_BINDIR)/lib \
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
           $(LIBFLAG)CTKVisualizationVTKCorePythonQt$(LIBLINKEXT) \
           $(LIBFLAG)CTKVisualizationVTKCore$(LIBLINKEXT) \
           $(LIBFLAG)CTKWidgets$(LIBLINKEXT) \
           $(LIBFLAG)CTKXNATCore$(LIBLINKEXT) \
           $(LIBFLAG)mbilog$(LIBLINKEXT) \
           $(LIBFLAG)MitkAlgorithmsExt$(LIBLINKEXT) \
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
           $(LIBFLAG)tinyxml$(LIBLINKEXT)
