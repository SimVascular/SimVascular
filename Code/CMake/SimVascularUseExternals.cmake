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
## Special variable to inform about externals download
option(ACCEPT_DOWNLOAD_EXTERNALS "Turn to ON to download externals" OFF)
if(NOT ACCEPT_DOWNLOAD_EXTERNALS)
  message(FATAL_ERROR "To download externals used by SimVascular, switch ACCEPT_DOWNLOAD_EXTERNALS to ON and reconfigure. Be patient as this can take some time depending on internet connection and operating system")
endif()
#-----------------------------------------------------------------------------

##-----------------------------------------------------------------------------
## Externals!
# Setup the externals now
set(SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR ON)
set(SV_EXTERNALS_TOPLEVEL_DIR "${CMAKE_CURRENT_BINARY_DIR}/Externals-build/sv_externals")
set(SV_EXTERNALS_TOPLEVEL_BIN_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/bin")
set(SV_USE_QT ON)
set(SV_USE_QT_GUI ON)

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
# Qt5
simvascular_add_new_external(Qt5 5.4.2 ON ON qt)

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
