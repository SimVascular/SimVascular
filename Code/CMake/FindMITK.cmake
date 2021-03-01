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

# - Find Parasolid Libraries
#
# === Variables ===
#
#  MITK_LIBRARIES, library search path
#  MITK_INCLUDE_DIRS, include search path
#  MITK_FOUND, If false, do not try to use this library.

set(proj MITK)

include(CMakeFindFrameworks)
include(FindPackageHandleStandardArgs)
include(GetPrerequisites)

if(NOT ${proj}_DIR)
  set(${proj}_DIR "${proj}_DIR-NOTFOUND" CACHE PATH "Path of toplevel ${proj} dir. Specify this if ${proj} cannot be found.")
  message(FATAL_ERROR "${proj}_DIR was not specified. Set ${proj}_DIR to the toplevel ${proj} dir that contains bin, lib, include")
endif()

#-----------------------------------------------------------------------------
# Set what we need to find
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Libraries
set(${proj}_LIBNAMES CppMicroServices
                     CTKCommandLineModulesBackendLocalProcess
                     CTKCommandLineModulesCore
                     CTKCommandLineModulesFrontendQtGui
                     CTKCore
                     CTKDICOMCore
                     CTKDICOMWidgets
                     CTKPluginFramework
                     CTKScriptingPythonCore
                     CTKScriptingPythonWidgets
                     #CTKVisualizationVTKCore
                     CTKWidgets
                     CTKXNATCore
                     mbilog
                     MitkAlgorithmsExt
                     MitkAppUtil
                     MitkCore
                     MitkContourModel
                     MitkDataTypesExt
                     MitkImageDenoising
                     MitkMapperExt
                     MitkQtWidgets
                     MitkQtWidgetsExt
                     MitkSceneSerialization
                     MitkSegmentation
                     MitkSegmentationUI
                     MitkSurfaceInterpolation
                     PythonQt
                     tinyxml
                     mbilog
                     PocoFoundation
                     PocoUtil
                     dcmjpeg
                     dcmjpls
                     dcmwlm
                     dcmpstat
                     dcmrt
                     dcmseg
                     dcmtract
                     dcmpmap
                     dcmtls
                     dcmsr
                     dcmimage
                     dcmdsig
                     dcmqrdb
                     dcmnet
                     dcmimgle
                     dcmfg
                     dcmiod
                     dcmdata
                     )
if(SV_USE_MITK_SEGMENTATION)
    list(APPEND ${proj}_LIBNAMES MitkMultilabel)
endif()

set(${proj}_PLUGIN_LIBNAMES org_mitk_core_services
                            org_mitk_gui_common
                            org_mitk_gui_qt_application
                            org_mitk_gui_qt_common
                            org_mitk_gui_qt_common_legacy
                            org_mitk_gui_qt_datamanager
                            org_mitk_gui_qt_stdmultiwidgeteditor
                            org_mitk_gui_qt_ext
                            org_blueberry_ui_qt
                            org_blueberry_core_runtime)

# Add requestion components
set(${proj}_LIBNAMES ${${proj}_LIBNAMES} ${${proj}_FIND_COMPONENTS})

#-----------------------------------------------------------------------------
# Header
set(${proj}_HEADERS "ctkAbstractFactory.h"                           #ctk
                    "ctkPluginFrameworkLauncher.h"                   #ctk/PluginFramework
                    "signature_of_eigen3_matrix_library"             #eigen3
                    "mitkVersion.h"                                  #mitk
                    "usGlobalConfig.h"                               #mitk/configs
                    "MitkAlgorithmsExtExports.h"                     #mitk/exports
                    "ui_QmitkAboutDialogGUI.h"                       #mitk/ui_files
                    "itkIntelligentBinaryClosingFilter.h"            #mitk/AlgorithmsExt/include
                    "mitkBaseApplication.h"                          #mitk/AppUtil/include
                    "itkImportMitkImageContainer.h"                  #mitk/Core/include
                    "usAny.h"                                        #mitk/CppMicroServices/core/include
                    "mitkAffineBaseDataInteractor3D.h"               #mitk/DataTypesExt/include
                    "mitkEnhancedPointSetVtkMapper3D.h"              #mitk/MapperExt/include
                    "mitkExtrudePlanarFigureFilter.h"                #mitk/PlanarFigure/include
                    "ui_QmitkFileReaderOptionsDialog.h"              #mitk/QtWidgets
                    "QmitkApplicationCursor.h"                       #mitk/QtWidgets/include
                    "ui_QmitkAboutDialogGUI.h"                       #mitk/QtWidgetsExt
                    "QmitkAboutDialog.h"                             #mitk/QtWidgetsExt/include
                    "mitkSceneIO.h"                                  #mitk/SceneSerialization/include
                    "mbilog.h"                                       #mitk/Utilities/mbilog
                    "mitkContourElement.h"                           #mitk/Modules/ContourModel/DataManagement
                    "usCoreModuleContext_p.h"                        #mitk/Modules/CppMicroServices/core/src/module
                    "usLDAPExpr_p.h"                                 #mitk/Modules/CppMicroServices/core/src/service
                    "usAtomicInt_p.h"                                #mitk/Modules/CppMicroServices/core/src/util
                    "itkLocalVariationImageFilter.h"                 #mitk/Modules/ImageDenoising
                    "mitkGL.h"                                       #mitk/Modules/LegacyGL
                    "mitkLabel.h"                                    #mitk/Modules/Multilabel
                    "itkAdaptiveThresholdIterator.h"                 #mitk/Modules/Segmentation/Algorithms
                    "mitkSegmentationInterpolationController.h"      #mitk/Modules/Segmentation/Controllers
                    "mitkAdaptiveRegionGrowingTool.h"                #mitk/Modules/Segmentation/Interactions
                    "ui_QmitkAdaptiveRegionGrowingToolGUIControls.h" #mitk/Modules/SegmentationUI
                    "QmitkAdaptiveRegionGrowingToolGUI.h"            #mitk/Modules/SegmentationUI/Qmitk
                    "mitkSurfaceInterpolationController.h"           #mitk/Modules/SurfaceInterpolation
                    "ui_QmitkFileReaderOptionsDialog.h"              #mitk/Modules/QtWidgets
                    "PythonQt.h"                                     #PythonQt
                    "tinyxml.h"                                      #tinyxml
                    "mitkIDataStorageService.h"                      #mitk/plugins/org.mitk.core.services
                    "mitkDataNodeSelection.h"                        #mitk/plugins/org.mitk.gui.common
                    "QmitkFileOpenAction.h"                          #mitk/plugins/org.mitk.gui.qt.application
                    "QmitkAbstractView.h"                            #mitk/plugins/org.mitk.gui.qt.common
                    "QmitkFunctionality.h"                           #mitk/plugins/org.mitk.gui.qt.common.legacy
                    "mitkIContextMenuAction.h"                       #mitk/plugins/org.mitk.gui.qt.datamanager
                    "QmitkStdMultiWidgetEditor.h"                    #mitk/plugins/org.mitk.gui.qt.stdmultiwidgeteditor
                    "QmitkExtActionBarAdvisor.h"                     #mitk/plugins/org.mitk.gui.qt.ext
                    "berryMacros.h"                                  #mitk/plugins/org.blueberry.core.runtime
                    "berryIApplication.h"                            #mitk/plugins/org.blueberry.core.runtime/application
                    "berryIConfigurationElement.h"                   #mitk/plugins/org.blueberry.core.runtime/registry
                    "berryQtViewPart.h"                              #mitk/plugins/org.blueberry.ui.qt
                    "berryQtWorkbenchAdvisor.h"                      #mitk/plugins/org.blueberry.ui.qt/application
                    "berryIntroPart.h"                               #mitk/plugins/org.blueberry.ui.qt/intro
                    )

if(${proj}_VERSION STREQUAL "2016.03")
  list(APPEND ${proj}_HEADERS
    "mitkLogoOverlay.h"                              #mitk/Modules/Overlays
    )
elseif(${proj}_VERSION VERSION_GREATER "2018.01")
  list(APPEND ${proj}_HEADERS
    "mitkLogoAnnotation.h"                           #mitk/Modules/Annotation
    )
endif()
if(SV_USE_MITK_SEGMENTATION)
    list(APPEND ${proj}_HEADERS
        "mitkLabelSetImage.h"                            #mitk/Modules/Multilabel
        "mitkBooleanOperation.h"                         #mitk/Modules/Segmentation/SegmentationUtilities/BooleanOperations
        "mitkMorphologicalOperations.h"                  #mitk/Modules/Segmentation/SegmentationUtilities/MorphologicalOperations
        )
endif()
#-----------------------------------------------------------------------------
# Find Libraries
#-----------------------------------------------------------------------------
set(${proj}_POSSIBLE_PATHS ${${proj}_DIR})
set(lib_sub_path "lib")
set(lib64_sub_path "lib64")

foreach(p ${${proj}_POSSIBLE_PATHS})
  list(APPEND ${proj}_POSSIBLE_LIB_PATHS "${p}/${lib_sub_path}")
  list(APPEND ${proj}_POSSIBLE_LIB_PATHS "${p}/${lib64_sub_path}")
endforeach()

set(${proj}_POSSIBLE_LIB_PATHS ${${proj}_POSSIBLE_LIB_PATHS}
  ${${proj}_DIR}
  ${${proj}_DIR}/shared_object
  ${${proj}_DIR}/dll
  ${${proj}_DIR}/lib/${CMAKE_BUILD_TYPE}
  ${${proj}_DIR}/lib64
  ${${proj}_DIR}/lib64/${CMAKE_BUILD_TYPE}
  )

set(${proj}_LIBS_MISSING ${${proj}_LIBNAMES})
list(REMOVE_DUPLICATES ${proj}_LIBS_MISSING)
set(${proj}_LIBRARIES_WORK "")
foreach(lib ${${proj}_LIBNAMES})
	#find library
  unset(${proj}_${lib}_LIBRARY CACHE)
	find_library(${proj}_${lib}_LIBRARY
		NAMES
		${lib}
    ${lib}d
		PATHS
		${${proj}_POSSIBLE_LIB_PATHS}
		NO_DEFAULT_PATH
    )
	mark_as_advanced(${proj}_${lib}_LIBRARY)
	set(${proj}_LIB_FULLNAMES ${${proj}_LIB_FULLNAMES} ${proj}_${lib}_LIBRARY)
	if(${proj}_${lib}_LIBRARY)
		set(${proj}_LIBRARIES_WORK ${${proj}_LIBRARIES_WORK} "${${proj}_${lib}_LIBRARY}")
		list(REMOVE_ITEM ${proj}_LIBS_MISSING ${lib})
	endif()
endforeach()

#message("${proj}_LIBRARIES_WORK: ${${proj}_LIBRARIES_WORK}")

list(LENGTH ${proj}_LIBRARIES_WORK ${proj}_NUMLIBS)
list(LENGTH ${proj}_LIBNAMES ${proj}_NUMLIBS_EXPECTED)
#message("${${proj}_NUMLIBS} ${${proj}_NUMLIBS_EXPECTED}")
if (NOT ${proj}_NUMLIBS EQUAL ${proj}_NUMLIBS_EXPECTED)
	set(${proj}_LIBRARIES_WORK "${proj}_LIBRARIES-NOTFOUND")
        message(FATAL_ERROR "${proj}_LIBS_MISSING: ${${proj}_LIBS_MISSING}")
endif()

set(${proj}_LIBRARIES  ${${proj}_LIBRARIES_WORK} CACHE STRING
	"${proj} libraries to link against" FORCE)

# Clean up.  If all libraries were found remove cache entries.
if(${proj}_LIBRARIES)
	foreach(lib ${${proj}_LIBNAMES})
    unset(${proj}_${lib}_LIBRARY CACHE)
	endforeach()
	if(${proj}_NUMLIBS_EXPECTED EQUAL 1)
		set(temp_path ${${proj}_LIBRARIES})
	else()
		list(GET ${proj}_LIBRARIES 1 temp_path)
	endif()
        get_filename_component(${proj}_LIBRARY_DIR ${temp_path} PATH)
endif()

#-----------------------------------------------------------------------------
# Find Plugins
#-----------------------------------------------------------------------------
set(${proj}_POSSIBLE_PATHS ${${proj}_DIR})
set(lib_sub_path "lib")

foreach(p ${${proj}_POSSIBLE_PATHS})
  list(APPEND ${proj}_POSSIBLE_PLUGIN_LIB_PATHS "${p}/${lib_sub_path}/plugins")
endforeach()

set(${proj}_POSSIBLE_PLUGIN_LIB_PATHS ${${proj}_POSSIBLE_PLUGIN_LIB_PATHS}
    ${${proj}_DIR}
    ${${proj}_DIR}/shared_object
    ${${proj}_DIR}/dll
    ${${proj}_DIR}/lib/plugins/RelWithDebInfo
    ${${proj}_DIR}/bin/plugins/RelWithDebInfo
    ${${proj}_DIR}/lib/plugins/Release
    ${${proj}_DIR}/bin/plugins/Release
    ${${proj}_DIR}/lib/plugins/Debug
    ${${proj}_DIR}/bin/plugins/Debug
    ${${proj}_DIR}/lib/plugins/MinSizeRel
    ${${proj}_DIR}/bin/plugins/MinSizeRel
  )

set(${proj}_PLUGIN_LIBS_MISSING ${${proj}_PLUGIN_LIBNAMES})
list(REMOVE_DUPLICATES ${proj}_PLUGIN_LIBS_MISSING)
set(${proj}_PLUGIN_LIBRARIES_WORK "")
foreach(lib ${${proj}_PLUGIN_LIBNAMES})
	#find library
  unset(${proj}_${lib}_PLUGIN_LIBRARY CACHE)
  find_library(${proj}_${lib}_PLUGIN_LIBRARY
    NAMES
    ${lib}
    PATHS
    ${${proj}_POSSIBLE_PLUGIN_LIB_PATHS}
    NO_DEFAULT_PATH
    )

  mark_as_advanced(${proj}_${lib}_PLUGIN_LIBRARY)
  set(${proj}_PLUGIN_LIB_FULLNAMES ${${proj}_PLUGIN_LIB_FULLNAMES} ${proj}_${lib}_PLUGIN_LIBRARY)
  if(${proj}_${lib}_PLUGIN_LIBRARY)
    set(${proj}_PLUGIN_LIBRARIES_WORK ${${proj}_PLUGIN_LIBRARIES_WORK} "${${proj}_${lib}_PLUGIN_LIBRARY}")
    list(REMOVE_ITEM ${proj}_PLUGIN_LIBS_MISSING ${lib})
	endif()
endforeach()

#message("${proj}_PLUGIN_LIBRARIES_WORK: ${${proj}_PLUGIN_LIBRARIES_WORK}")

list(LENGTH ${proj}_PLUGIN_LIBRARIES_WORK ${proj}_NUMPLUGINS)
list(LENGTH ${proj}_PLUGIN_LIBNAMES ${proj}_NUMPLUGINS_EXPECTED)
#message("${${proj}_NUMPLUGINS} ${${proj}_NUMPLUGINS_EXPECTED}")
if (NOT ${proj}_NUMPLUGINS EQUAL ${proj}_NUMPLUGINS_EXPECTED)
  set(${proj}_PLUGIN_LIBRARIES_WORK "${proj}_PLUGIN_LIBRARIES-NOTFOUND")
  message(FATAL_ERROR "${proj}_PLUGIN_LIBS_MISSING: ${${proj}_PLUGIN_LIBS_MISSING}")
endif()

set(${proj}_PLUGIN_LIBRARIES  ${${proj}_PLUGIN_LIBRARIES_WORK} CACHE STRING
	"${proj} libraries to link against" FORCE)

# Clean up.  If all libraries were found remove cache entries.
if(${proj}_PLUGIN_LIBRARIES)
  foreach(lib ${${proj}_PLUGIN_LIBNAMES})
    unset(${proj}_${lib}_PLUGIN_LIBRARY CACHE)
	endforeach()
        if(${proj}_NUMPLUGINS_EXPECTED EQUAL 1)
          set(temp_path ${${proj}_PLUGIN_LIBRARIES})
	else()
          list(GET ${proj}_PLUGIN_LIBRARIES 1 temp_path)
	endif()
        get_filename_component(${proj}_PLUGIN_LIBRARY_DIR ${temp_path} PATH)
endif()

#-----------------------------------------------------------------------------
# Find Include Directory
#-----------------------------------------------------------------------------

set(${proj}_POSSIBLE_INCLUDE_PATHS
  "${${proj}_DIR}/*"
  "${${proj}_DIR}/include/*"
  "${${proj}_DIR}/include/*/*"
  "${${proj}_DIR}/include/*/*/*"
  "${${proj}_DIR}/include/*/*/*/*"
  "${${proj}_DIR}/include/*/*/*/*/*"
  "${${proj}_DIR}/include/*/*/*/*/*/*"
  )

#-----------------------------------------------------------------------------
# Search for header
set(${proj}_HEADERS_MISSING ${${proj}_HEADERS})
list(REMOVE_DUPLICATES ${proj}_HEADERS_MISSING)
set(${proj}_HEADERS_WORK "")
foreach(header ${${proj}_HEADERS})
  unset(${proj}_${header}_HEADER CACHE)
  find_path(${proj}_${header}_HEADER
    NAMES
    ${header}
    PATHS
    ${${proj}_POSSIBLE_INCLUDE_PATHS}
    NO_DEFAULT_PATH
    )

  mark_as_advanced(${proj}_${header}_HEADER)
  set(${proj}_HEADER_FULLNAMES ${${proj}_HEADER_FULLNAMES} ${proj}_${header}_HEADER)
  if(${proj}_${header}_HEADER)
    set(${proj}_HEADERS_WORK ${${proj}_HEADERS_WORK} "${${proj}_${header}_HEADER}")
    list(REMOVE_ITEM ${proj}_HEADERS_MISSING ${header})
  endif()
endforeach()

list(LENGTH ${proj}_HEADERS_WORK ${proj}_NUMHEADERS)
list(LENGTH ${proj}_HEADERS ${proj}_NUMHEADERS_EXPECTED)
#message("${${proj}_NUMHEADERS} ${${proj}_NUMHEADERS_EXPECTED}")
if (NOT ${proj}_NUMHEADERS EQUAL ${proj}_NUMHEADERS_EXPECTED)
  set(${proj}_HEADERS_WORK "${proj}_HEADERS-NOTFOUND")
  message(FATAL_ERROR "${proj}_HEADERS_MISSING: ${${proj}_HEADERS_MISSING}")
endif()

set(${proj}_HEADERS_WORK ${${proj}_DIR}/include ${${proj}_HEADERS_WORK})
set(${proj}_INCLUDE_DIRS  ${${proj}_HEADERS_WORK} CACHE STRING
	"${proj} include directories" FORCE)
list(GET ${proj}_INCLUDE_DIRS 0 ${proj}_INCLUDE_DIR)

#-----------------------------------------------------------------------------
# Handle Standard Args
find_package_handle_standard_args(${proj}
	FOUND_VAR ${proj}_FOUND
  REQUIRED_VARS ${proj}_INCLUDE_DIR ${proj}_LIBRARIES ${proj}_LIBRARY_DIR ${proj}_PLUGIN_LIBRARIES ${proj}_PLUGIN_LIBRARY_DIR
  VERSION_VAR ${proj}_VERSION_STRING
  FAIL_MESSAGE "Could NOT find ${proj} missing component: ${${proj}_LIBS_MISSING} and ${${proj}_HEADERS_MISSING}")

set(${proj}_LIBRARY ${${proj}_LIBRARIES})

mark_as_advanced(
  ${proj}_INCLUDE_DIR
  ${proj}_LIBRARIES
  ${proj}_LIBRARY_DIR
  ${proj}_PLUGIN_LIBRARIES
  ${proj}_PLUGIN_LIBRARY_DIR
  )
