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
# File to use externals cmake to download and use sv externals
#-----------------------------------------------------------------------------
if(SimVascular_USE_FILE_INCLUDED)
  return()
endif()
set(SimVascular_USE_FILE_INCLUDED 1)

list(APPEND CMAKE_MODULE_PATH "${SimVascular_CMAKE_DIR}")
list(APPEND CMAKE_MODULE_PATH "${SimVascular_EXTERNALS_DIR}")
list(APPEND CMAKE_MODULE_PATH "${SimVascular_EXTERNALS_CMAKE_DIR}")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# CMake Includes
include(CheckLibraryExists)
include(GetPrerequisites)
include(GenerateExportHeader)
include(ExternalProject)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# SimVascular Includes
include(SimVascularMacros)
include(SimVascularSystemSetup)
include(SimVascularFunctionCheckCompilerFlags)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# RPATH handling
# No objects built directly with project. Not needed!
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# First process special Qt stuff
#Find Qt!
if(WIN32)
  set(SV_Qt5_search_paths C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/lib/cmake/Qt5
                          C:/OpenSource/Qt5.4.2/5.4/msvc2013_64_opengl/lib/cmake/Qt5
                          C:/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/lib/cmake/Qt5
			  C:/Qt5.4.2/5.4/msvc2013_64_opengl/lib/cmake/Qt5)
elseif(LINUX)
  set(SV_Qt5_search_paths /opt/Qt5.4.2/5.4/gcc_64/lib/cmake/Qt5
                          /usr/local/package/Qt5.4.2/5.4/clang_64/lib/cmake/Qt5)
elseif(APPLE)
  set(SV_Qt5_search_paths /usr/local/package/Qt5.4.2/5.4/clang_64/lib/cmake/Qt5
                          /opt/Qt5.4.2/5.4/gcc_64/lib/cmake/Qt5)
endif()

set(SV_Qt5_COMPONENTS
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
find_package(Qt5 PATHS ${SV_Qt5_search_paths} COMPONENTS ${SV_Qt5_COMPONENTS} REQUIRED)

# need toplevel Qt dir path
if(Qt5_DIR)
  get_filename_component(_Qt5_DIR "${Qt5_DIR}/../../../" ABSOLUTE)
  list(FIND CMAKE_PREFIX_PATH "${_Qt5_DIR}" _result)
  if(_result LESS 0)
    set(CMAKE_PREFIX_PATH "${_Qt5_DIR};${CMAKE_PREFIX_PATH}" CACHE PATH "" FORCE)
  endif()
endif()
# Need to set include dirs and libraries of Qt from individual components
if(NOT SV_USE_MITK_CONFIG)
  set(QT_LIBRARIES "")
  set(QT_INCLUDE_DIRS "")
  foreach(comp ${SV_Qt5_COMPONENTS})
    if(Qt5${comp}_LIBRARIES)
      set(QT_LIBRARIES ${QT_LIBRARIES} ${Qt5${comp}_LIBRARIES})
    endif()
    if(Qt5${comp}_INCLUDE_DIRS)
      set(QT_INCLUDE_DIRS ${QT_INCLUDE_DIRS} ${Qt5${comp}_INCLUDE_DIRS})
    endif()
  endforeach()
  include_directories(${QT_INCLUDE_DIRS})
endif()

#-----------------------------------------------------------------------------
## Special variable to inform about externals download
option(ACCEPT_DOWNLOAD_EXTERNALS "Turn to ON to download externals" OFF)
if(NOT ACCEPT_DOWNLOAD_EXTERNALS)
  message(FATAL_ERROR "To download externals used by SimVascular, switch ACCEPT_DOWNLOAD_EXTERNALS to ON and reconfigure. Be patient as this can take some time depending on internet connection and operating system")
endif()
#-----------------------------------------------------------------------------

##-----------------------------------------------------------------------------
## Externals!
configure_file("${SimVascular_CMAKE_DIR}/simvascular_download_externals.sh.in" "${CMAKE_BINARY_DIR}/simvascular_download_externals.sh" @ONLY)
set(SV_EXTERNALS_ADDITIONAL_CMAKE_ARGS "" CACHE STRING "If more options want to be provided to the sv_externals build, they can be with this string")
list(APPEND SV_EXTERNALS_ADDITIONAL_CMAKE_ARGS -DCMAKE_MODULE_PATH:PATH="${CMAKE_MODULE_PATH}")
#execute_process(COMMAND bash "-c"
#  "${CMAKE_COMMAND} \
#  -B${CMAKE_CURRENT_BINARY_DIR}/Externals-build \
#  -H${SimVascular_EXTERNALS_DIR} \
#  -DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER} \
#  -DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER} \
#  -DCMAKE_CXX_FLAGS:STRING=\"${CMAKE_CXX_FLAGS}\" \
#  -DCMAKE_C_FLAGS:STRING=\"${CMAKE_C_FLAGS}\" \
#  -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE} \
#  -DCMAKE_MACOSX_RPATH:BOOL=ON \
#  -DQt5_DIR:PATH=${Qt5_DIR} \
#  ${SV_EXTERNALS_ADDITIONAL_CMAKE_ARGS}"
#  OUTPUT_VARIABLE EXTERNALS_OUTPUT
#  RESULT_VARIABLE EXTERNALS_RESULT
#  ERROR_VARIABLE EXTERNALS_ERROR)
execute_process(COMMAND bash "-c" "source ${CMAKE_BINARY_DIR}/simvascular_download_externals.sh"
  OUTPUT_VARIABLE EXTERNALS_OUTPUT
  RESULT_VARIABLE EXTERNALS_RESULT
  ERROR_VARIABLE EXTERNALS_ERROR)
message("CONFIGURING EXTERNALS DOWNLOAD OUTPUT: ${EXTERNALS_OUTPUT}")
if(EXTERNALS_RESULT)
  message(FATAL_ERROR "Error configuring download of externals. ERROR: ${EXTERNALS_ERROR}")
endif()

execute_process(COMMAND bash "-c" "make"
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/Externals-build"
  OUTPUT_VARIABLE MAKE_OUTPUT
  RESULT_VARIABLE MAKE_RESULT
  ERROR_VARIABLE  MAKE_ERROR)
if(MAKE_RESULT)
  message(FATAL_ERROR "Error downloading externals. OUTPUT: ${MAKE_OUTPUT}. ERROR: ${MAKE_ERROR}")
else()
  message(STATUS "DOWNLOADED EXTERNALS")
endif()
##-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Setup the externals now
set(SV_EXTERNALS_USE_TOPLEVEL_DIR ON)
set(SV_EXTERNALS_TOPLEVEL_DIR "${CMAKE_CURRENT_BINARY_DIR}/Externals-build/sv_externals")
set(SV_EXTERNALS_SRC_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/src")
set(SV_EXTERNALS_BLD_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/build")
set(SV_EXTERNALS_PFX_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/prefix")
set(SV_EXTERNALS_BIN_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/bin/${SV_COMPILER_DIR}/${SV_COMPILER_VERSION_DIR}/${SV_ARCH_DIR}")
# TCL
simvascular_add_new_external(TCL 8.6.4 ON ON tcltk)

#PYTHON
simvascular_add_new_external(PYTHON 2.7.11 ON ON python)

#FREETYPE
simvascular_add_new_external(FREETYPE 2.6.3 ON ON freetype)

# MMG
simvascular_add_new_external(MMG 5.1.0 ON OFF mmg)

# VTK
simvascular_add_new_external(VTK 6.2.0 ON ON vtk)

# GDCM
simvascular_add_new_external(GDCM 2.6.1 ON ON gdcm)

# ITK
simvascular_add_new_external(ITK 4.7.1 ON ON itk)

# OpenCASCADE
simvascular_add_new_external(OpenCASCADE 7.0.0 ON ON opencascade)

# MITK
simvascular_add_new_external(MITK 2016.03 ON ON mitk)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Process each external in the order they were added to SV_EXTERNALS_LIST
# using simvascular_add_new_external in SimVascularOptions.cmake
foreach(proj ${SV_EXTERNALS_LIST})
  if(SV_USE_${proj})
    if(EXISTS "${SimVascular_CMAKE_DIR}/Externals/${proj}.cmake")
      include("${SimVascular_CMAKE_DIR}/Externals/${proj}.cmake")
    endif()
  endif()
endforeach()
#-----------------------------------------------------------------------------
