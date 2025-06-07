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
set(SV_EXTERNALS_TOPLEVEL_DIR "${CMAKE_CURRENT_BINARY_DIR}/Externals-build/svExternals")
set(SV_EXTERNALS_TOPLEVEL_BIN_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/bin")
set(SV_USE_QT ON)
set(SV_USE_SV4_GUI ON)

configure_file("${SimVascular_CMAKE_DIR}/simvascular_download_externals.sh.in" "${CMAKE_BINARY_DIR}/simvascular_download_externals.sh" @ONLY)
set(SV_EXTERNALS_ADDITIONAL_CMAKE_ARGS "" CACHE STRING "If more options want to be provided to the svExternals build, they can be with this string")
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

if(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2019.06")
    simvascular_add_new_external(Qt5 5.11.3 ON ON qt)
    simvascular_add_new_external(HDF5 1.10.1 ON ON hdf5)
    simvascular_add_new_external(TCL 8.6.4 ON ON tcltk)
    simvascular_add_new_external(PYTHON 3.5.5 ON ON python)
    simvascular_add_new_external(FREETYPE 2.6.3 ON ON freetype)
    simvascular_add_new_external(MMG 5.3.9 ON OFF mmg)
    simvascular_add_new_external(GDCM 2.6.3 ON ON gdcm)
    simvascular_add_new_external(VTK 8.1.1 ON ON vtk)
    simvascular_add_new_external(ITK 4.13.2 ON ON itk)
    simvascular_add_new_external(OpenCASCADE 7.3.0 ON ON opencascade)
    simvascular_add_new_external(MITK 2018.04.2 ON ON mitk)

elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2022.10")
    simvascular_add_new_external(Qt5 5.11.3 ON ON qt)
    simvascular_add_new_external(HDF5 1.10.1 ON ON hdf5)
    simvascular_add_new_external(PYTHON 3.5.5 ON ON python)
    simvascular_add_new_external(FREETYPE 2.6.3 ON ON freetype)
    simvascular_add_new_external(MMG 5.3.9 ON OFF mmg)
    simvascular_add_new_external(GDCM 2.6.3 ON ON gdcm)
    simvascular_add_new_external(VTK 8.1.1 ON ON vtk)
    simvascular_add_new_external(ITK 4.13.2 ON ON itk)
    simvascular_add_new_external(OpenCASCADE 7.3.0 ON ON opencascade)
    simvascular_add_new_external(MITK 2018.04.2 ON ON mitk)

# [DaveP] is this used?
#
elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2024.05")
    #simvascular_add_new_external(Qt6 6.7.0 ON ON qt)

    simvascular_add_new_external(HDF5 1.14.3 ON ON hdf5)
    #simvascular_add_new_external(HDF5 1.12.1 ON ON hdf5)

    simvascular_add_new_external(PYTHON 3.9.10 ON ON python)
    simvascular_add_new_external(FREETYPE 2.13.0 ON ON freetype)
    simvascular_add_new_external(GDCM 3.0.10 ON ON gdcm)
    simvascular_add_new_external(MMG 5.3.9 ON OFF mmg)

    simvascular_add_new_external(VTK 9.3.0 ON ON vtk)
    #simvascular_add_new_external(VTK 9.1.0 ON ON vtk)

    simvascular_add_new_external(ITK 5.4.0 ON ON itk)

    simvascular_add_new_external(OpenCASCADE 7.6.0 ON ON opencascade)
    simvascular_add_new_external(MITK 2022.10 ON ON mitk)

endif()

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
