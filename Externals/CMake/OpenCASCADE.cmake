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
#
#-----------------------------------------------------------------------------
# OpenCASCADE
set(proj OpenCASCADE)

set(SV_OPEN_CASCADE_DIR /Users/parkerda/software/ktbolt/svExternals/install/opencascade)
      
set(msg "[Externals/CMake/OpenCASCADE.cmake] ")
message(STATUS "${msg} ")
message(STATUS "${msg} -------------------------------------------------------------------------------------")
message(STATUS "${msg} +++++                              OpenCASCADE.cmake                                 ")
message(STATUS "${msg} -------------------------------------------------------------------------------------")
message(STATUS "${msg} proj: ${proj}")
message(STATUS "${msg} SV_OPEN_CASCADE_DIR: ${SV_OPEN_CASCADE_DIR}")

# Dependencies
set(${proj}_DEPENDENCIES "VTK")
if(${SV_EXTERNALS_ENABLE_TCL})
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "TCL")
endif()
if(${SV_EXTERNALS_ENABLE_TK})
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "TK")
endif()
if(${SV_EXTERNALS_ENABLE_FREETYPE})
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "FREETYPE")
endif()

# Git info
set(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL "" CACHE STRING "Manual specification of ${proj}, can be web address or local path to tar file")
mark_as_advanced(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)
if(NOT SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_ORIGINALS_URL}/opencascade/opencascade-${SV_EXTERNALS_${proj}_VERSION}.tgz")
else()
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL}")
endif()

# Set FREETYPE arguments to someting ...
#
if(SV_EXTERNALS_ENABLE_FREETYPE)
  list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
    -D3RDPARTY_FREETYPE_DIR:PATH=${SV_EXTERNALS_FREETYPE_BIN_DIR}
    -D3RDPARTY_FREETYPE_DLL:FILEPATH=${SV_EXTERNALS_FREETYPE_LIBRARY}
    -D3RDPARTY_FREETYPE_DLL_DIR:PATH=${SV_EXTERNALS_FREETYPE_LIBRARY_DIR}
    -D3RDPARTY_FREETYPE_INCLUDE_DIR_freetype2:PATH=${SV_EXTERNALS_FREETYPE_INCLUDE_DIR}
    -D3RDPARTY_FREETYPE_INCLUDE_DIR_ft2build:PATH=${SV_EXTERNALS_FREETYPE_INCLUDE_DIR}
    -D3RDPARTY_FREETYPE_LIBRARY:FILEPATH=${SV_EXTERNALS_FREETYPE_LIBRARY}
    -D3RDPARTY_FREETYPE_LIBRARY_DIR:PATH=${SV_EXTERNALS_FREETYPE_LIBRARY_DIR}
    )
endif()

# Patch if vtk greater than 8.0
#
if(SV_EXTERNALS_ENABLE_VTK)
  if(SV_EXTERNALS_VTK_VERSION VERSION_GREATER "8.0")
    set(SV_EXTERNALS_${proj}_CUSTOM_PATCH ${SV_EXTERNALS_${proj}_CUSTOM_PATCH}
      COMMAND patch -N -p1 -i ${SV_EXTERNALS_SOURCE_DIR}/Patches/2018.05/patch-opencascade-vtk-greater-8.0.patch)
  endif()
endif()

if(APPLE)
  if(SV_EXTERNALS_${proj}_VERSION VERSION_EQUAL "7.3.0")
    set(SV_EXTERNALS_${proj}_CUSTOM_PATCH ${SV_EXTERNALS_${proj}_CUSTOM_PATCH}
      COMMAND patch -N -p1 -i ${SV_EXTERNALS_SOURCE_DIR}/Patches/2018.05/patch-opencascade-7.3.0-macos.patch)
  endif()
endif()

# Add external project


if(SV_OPEN_CASCADE_DIR)
#if(SV_EXTERNALS_DOWNLOAD_${proj})

  message(STATUS "${msg} +++++ Use installed OpenCASCADE")
  find_package(OpenCASCADE REQUIRED PATHS ${SV_OPEN_CASCADE_DIR} NO_DEFAULT_PATH)

  #ExternalProject_Add(${proj}
    #URL ${SV_EXTERNALS_${proj}_BINARIES_URL}
    #PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
    #SOURCE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}
    #BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}
    #DEPENDS ${${proj}_DEPENDENCIES}
    #CONFIGURE_COMMAND ""
    #BUILD_COMMAND ""
    #INSTALL_COMMAND ""
    #UPDATE_COMMAND ""
    #)

  message(STATUS "${msg} OpenCASCADE_INCLUDE_DIR: ${OpenCASCADE_INCLUDE_DIR}") 
  message(STATUS "${msg} OpenCASCADE_LIBRARY_DIR: ${OpenCASCADE_LIBRARY_DIR}") 

else()
  ExternalProject_Add(${proj}
    URL ${SV_EXTERNALS_${proj}_SOURCE_URL}
    PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
    SOURCE_DIR ${SV_EXTERNALS_${proj}_SRC_DIR}
    BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}
    DEPENDS ${${proj}_DEPENDENCIES}
    PATCH_COMMAND ${SV_EXTERNALS_${proj}_CUSTOM_PATCH}
    UPDATE_COMMAND ""
    CMAKE_CACHE_ARGS
      -DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
      -DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
      -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
      -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
      -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
      -DCMAKE_MACOSX_RPATH:BOOL=ON
      -DUSE_VTK:BOOL=${SV_EXTERNALS_ENABLE_VTK}
      -DBUILD_MODULE_Draw:BOOL=OFF
      -D3RDPARTY_DIR:PATH=${SV_EXTERNALS_TOPLEVEL_DIR}
      -D3RDPARTY_VTK_DIR:PATH=${SV_EXTERNALS_VTK_BIN_DIR}
      -D3RDPARTY_VTK_DLL_DIR:PATH=${SV_EXTERNALS_VTK_LIBRARY_DIR}
      -D3RDPARTY_VTK_INCLUDE_DIR:PATH=${SV_EXTERNALS_VTK_INCLUDE_DIR}
      -D3RDPARTY_VTK_LIBRARY_DIR:PATH=${SV_EXTERNALS_VTK_LIBRARY_DIR}
      -DINSTALL_DIR:PATH=${SV_EXTERNALS_${proj}_BIN_DIR}
      -DINSTALL_DIR_BIN:PATH=bin
      -DINSTALL_DIR_LIB:PATH=lib
      ${SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS}
    )
endif()
