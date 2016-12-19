# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved.
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

#-----------------------------------------------------------------------------
# QT
if(SV_USE_QT_GUI)
    set(CMAKE_PREFIX_PATH "${CMAKE_PREFIX_PATH}" CACHE PATH "")
    set(SimVascular_QT5_COMPONENTS
      Concurrent
      Core
      Designer
      Gui
      Help
      OpenGL
      PrintSupport
      Script
      Sql
      Svg
      WebKitWidgets
      WebKit
      Widgets
      Xml
      XmlPatterns
      UiTools)
    find_package(Qt5 COMPONENTS ${SimVascular_QT5_COMPONENTS} REQUIRED)
    if(Qt5_DIR)
      get_filename_component(_Qt5_DIR "${Qt5_DIR}/../../../" ABSOLUTE)
      list(FIND CMAKE_PREFIX_PATH "${_Qt5_DIR}" _result)
      if(_result LESS 0)
        set(CMAKE_PREFIX_PATH "${_Qt5_DIR};${CMAKE_PREFIX_PATH}" CACHE PATH "" FORCE)
      endif()
    endif()
    if(NOT SV_USE_MITK_CONFIG)
      set(QT_LIBRARIES "")
      set(QT_INCLUDE_DIRS "")
      foreach(comp ${SimVascular_QT5_COMPONENTS})
        if(Qt5${comp}_LIBRARIES)
          set(QT_LIBRARIES ${QT_LIBRARIES} ${Qt5${comp}_LIBRARIES})
        endif()
        if(Qt5${comp}_INCLUDE_DIRS)
          set(QT_INCLUDE_DIRS ${QT_INCLUDE_DIRS} ${Qt5${comp}_INCLUDE_DIRS})
        endif()
      endforeach()
      include_directories(${QT_INCLUDE_DIRS})

      #CppMicroServices
      find_package(CppMicroServices)
    endif()

    set(f svConfigure.h)
    configure_file(
      ${SV_SOURCE_DIR}/Source/Include/${f}.in
      ${SV_BINARY_DIR}/Source/Include/${f}
      )
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# TCL
if(SV_EXTERNALS_USE_TOPLEVEL_DIR)
  set(TCL_DIR ${SV_TCL_DIR} CACHE PATH "Force TCL dir to externals" FORCE)
endif()
simvascular_external(TCL)
set(SV_TCL_DIR ${TCL_DIR})

STRING(REGEX REPLACE
	"^.*libtcl([0-9]\\.*[0-9]).*$" "\\1" TCL_VERSION "${TCL_LIBRARY}")
get_filename_component(TCL_LIBRARY_PATH "${TCL_LIBRARY}" PATH)
link_directories(${TCL_LIBRARY_PATH})
# TCL has two include directories, the macro only includes one.
include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})
if(WIN32)
	get_filename_component(TCL_DLL_PATH "${TCL_TCLSH}" PATH)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Tkcximage (Legacy)
if(WIN32)
	if(SV_USE_TKCXIMAGE)
		find_library(TKCXIMAGE_DLL tkcximage)
		if(TKCXIMAGE_DLL)
			set(TKCXIMAGE_DLL_LIBRARY ${TKCXIMAGE_DLL})
			get_filename_component(TKCXIMAGE_DLL_PATH ${TKCXIMAGE_DLL} DIRECTORY CACHE)
			set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} "TKCXIMAGE")
		endif()
	endif()
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# PYTHON
if(SV_USE_PYTHON)
  if(SV_EXTERNALS_USE_TOPLEVEL_DIR)
    set(PYTHON_DIR ${SV_PYTHON_DIR} CACHE PATH "Force PYTHON dir to externals" FORCE)
  endif()
  simvascular_external(PYTHON SHARED_LIB)
  set(SV_PYTHON_DIR ${PYTHON_DIR})

  if(PYTHONLIBS_FOUND)
    if(NOT WIN32)
      string(REPLACE "." ";" PYTHONLIBS_VERSION_LIST "${PYTHONLIBS_VERSION_STRING}")
      list(GET PYTHONLIBS_VERSION_LIST 0 PYTHONLIBS_MAJOR_VERSION)
      list(GET PYTHONLIBS_VERSION_LIST 1 PYTHONLIBS_MINOR_VERSION)
    else()
      set (PYTHONLIBS_MAJOR_VERSION 2)
      set (PYTHONLIBS_MINOR_VERSION 7)
    endif()
    include_directories(${PYTHON_INCLUDE_DIR})
  else()
    message(STATUS "")
    message(STATUS "Python Libs NOT FOUND")
    message(STATUS "Make sure you have python installed on your system.")
  endif()
  set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_PYTHON")
  set(VTK_PYTHON_MODULES vtkWrappingPythonCore)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# FREETYPE
if(SV_USE_FREETYPE)
  if(SV_EXTERNALS_USE_TOPLEVEL_DIR)
    set(FREETYPE_DIR ${SV_FREETYPE_DIR} CACHE PATH "Force FREETYPE dir to externals" FORCE)
  endif()
  simvascular_external(FREETYPE)
  set(USE_FREETYPE ON)
  set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_FREETYPE")
  if(SV_USE_FREETYPE_SHARED)
    set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} FREETYPE)
    set(SV_INSTALL_EXTERNALS ON)
  endif()
  set(SV_FREETYPE_DIR ${FREETYPE_DIR})
  link_directories(${FREETYPE_DIR}/lib)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# GDCM
if(SV_USE_GDCM)
  if(SV_EXTERNALS_USE_TOPLEVEL_DIR)
    set(GDCM_DIR ${SV_GDCM_DIR}/lib/gdcm-${GDCM_MAJOR_VERSION}.${GDCM_MINOR_VERSION} CACHE PATH "Force GDCM dir to externals" FORCE)
  endif()
  simvascular_external(GDCM)
  set(USE_GDCM ON)
  set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_GDCM")
  if(SV_USE_GDCM_SHARED)
    set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} GDCM)
    set(SV_INSTALL_EXTERNALS ON)
  endif()
  simvascular_get_external_path_from_include_dir(GDCM)
endif()
#-----------------------------------------------------------------------------

# VTK
#-----------------------------------------------------------------------------
# VTK must come after ITK because ITK_USE_FILE resets vtk components and
# messes up everything
if(NOT SV_INSTALL_VTK_TCL_DIR)
  set(SV_INSTALL_VTK_TCL_DIR ${SV_EXT_VTK_BIN_DIR}/lib/tcltk/vtk-${VTK_MAJOR_VERSION}.${VTK_MINOR_VERSION})
endif()
if(SV_EXTERNALS_USE_TOPLEVEL_DIR)
  set(VTK_DIR ${SV_VTK_DIR}/lib/cmake/vtk-${VTK_MAJOR_VERSION}.${VTK_MINOR_VERSION} CACHE PATH "Force VTK dir to externals" FORCE)
endif()
if(SV_USE_VTK_SHARED)
  set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} VTK)
  set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_VTK_SHARED -DVTK_BUILD_SHARED_LIBS")
  set(SV_INSTALL_EXTERNALS ON)
endif()
simvascular_external(VTK
  COMPONENTS
  vtkFiltersFlowPaths
  vtkWrappingTcl
  vtkRenderingTk
  vtkCommonDataModel
  vtkCommonCore
  vtkChartsCore
  vtkCommonExecutionModel
  vtkFiltersCore
  vtkFiltersVerdict
  vtkIOLegacy
  vtkIOPLY
  vtkIOXML
  vtkImagingStencil
  vtktiff
  ${VTK_PYTHON_MODULES}
  NO_DEFAULT_PATH)
include(${VTK_USE_FILE})
if(SV_USE_PYTHON)
  list(REMOVE_AT VTK_LIBRARIES -1)
  get_target_property(VTK_PYTHON_INTERFACE vtkWrappingPythonCore INTERFACE_LINK_LIBRARIES)
  simvascular_list_replace(VTK_PYTHON_INTERFACE 1 ${PYTHON_LIBRARY})
  set_target_properties(vtkWrappingPythonCore PROPERTIES INTERFACE_LINK_LIBRARIES "${VTK_PYTHON_INTERFACE}")
endif()                             
get_target_property(VTK_TCL_INTERFACE vtkCommonCoreTCL INTERFACE_LINK_LIBRARIES)
simvascular_list_replace(VTK_TCL_INTERFACE 2 ${TCL_LIBRARY})
set_target_properties(vtkCommonCoreTCL PROPERTIES INTERFACE_LINK_LIBRARIES "${VTK_TCL_INTERFACE}")
set(VTK_LIBRARIES ${VTK_LIBRARIES} vtkCommonCoreTCL)
simvascular_get_external_path_from_include_dir(VTK)
set(CMAKE_PREFIX_PATH "${VTK_DIR}" "${CMAKE_PREFIX_PATH}")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# ITK
# ITK resets the vtk dir and variables (very annoying), must set a temp
# vtk dir to reset at the end
if(SV_USE_ITK)
  set(TEMP_VTK_DIR ${VTK_DIR})
  set(TEMP_VTK_LIBRARIES ${VTK_LIBRARIES})
  if(SV_EXTERNALS_USE_TOPLEVEL_DIR)
    set(ITK_DIR ${SV_ITK_DIR}/lib/cmake/ITK-${ITK_MAJOR_VERSION}.${ITK_MINOR_VERSION} CACHE PATH "Force ITK dir to externals" FORCE)
  endif()
  if(SV_USE_ITK_SHARED)
    set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} ITK)
    set(SV_INSTALL_EXTERNALS ON)
  endif()
  simvascular_external(ITK)
  include(${ITK_USE_FILE})

  set(USE_ITK ON)
  include_directories("${SV_SOURCE_DIR}/Source/Segmentation/ITK/Include")
  include_directories("${SV_SOURCE_DIR}/Source/Segmentation/ITK/ITKCode")
  simvascular_get_external_path_from_include_dir(ITK)
  set(VTK_DIR ${TEMP_VTK_DIR} CACHE PATH "Must reset VTK dir after processing ITK" FORCE)
  set(VTK_LIBRARIES ${TEMP_VTK_LIBRARIES})
endif()
#-----------------------------------------------------------------------------


#-----------------------------------------------------------------------------
# OpenCASCADE
# If using extern directory, set dir with Config file!
if(SV_USE_OpenCASCADE)
  if(SV_EXTERNALS_USE_TOPLEVEL_DIR)
    set(OpenCASCADE_DIR ${SV_OpenCASCADE_DIR}/lib/cmake/opencascade CACHE PATH "Force OpenCASCADE dir to externals" FORCE)
  endif()
  simvascular_external(OpenCASCADE)
  set(USE_OpenCASCADE ON)
  set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_OPENCASCADE")
  if(SV_USE_OpenCASCADE_SHARED)
    set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} OpenCASCADE)
    set(SV_INSTALL_EXTERNALS ON)
  endif()
  simvascular_get_external_path_from_include_dir(OpenCASCADE)
endif()

#-----------------------------------------------------------------------------
# MMG
if(SV_USE_MMG)
  if(SV_EXTERNALS_USE_TOPLEVEL_DIR)
    set(MMG_DIR ${SV_MMG_DIR} CACHE PATH "Force MMG dir to externals" FORCE)
  endif()
  simvascular_external(MMG)
  set(USE_MMG ON)
  if(SV_USE_MMG_SHARED)
    set(SV_INSTALL_EXTERNALS ON)
    set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} "MMG")
    set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_MMG_SHARED")
  endif()
  simvascular_get_external_path_from_include_dir(MMG)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# MITK
if(SV_USE_MITK)
  if(SV_EXTERNALS_USE_TOPLEVEL_DIR)
    set(MITK_DIR ${SV_MITK_DIR} CACHE PATH "Force MITK dir to externals" FORCE)
  endif()
  if(SV_USE_MITK_CONFIG)
    find_package(MITK NO_MODULE)
    set(MITK_LIBRARIES MitkCore
                       MitkAppUtil
                       MitkQtWidgets
                       MitkQtWidgetsExt
                       MitkMapperExt
                       MitkImageDenoising
                       MitkSegmentationUI
                       MitkSegmentation
                       MitkSceneSerialization)
  else()
    simvascular_external(MITK)
  endif()
  if(SV_USE_MITK_SHARED)
    set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} MITK)
    set(SV_INSTALL_EXTERNALS ON)
  endif()
  include_directories(${MITK_INCLUDE_DIRS})
  include_directories(${CTK_INCLUDE_DIRS})
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Install
foreach(proj ${SV_EXTERNALS_LIST})
  if(SV_USE_${proj}_SHARED AND SV_USE_EXTERNALS_TOPLEVEL_DIR)
    simvascular_install_external(${proj})
  endif()
endforeach()
#-----------------------------------------------------------------------------
