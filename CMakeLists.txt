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
# This toplevel CMakeLists.txt should be used as the first attempt to build
# SimVascular.
# This CMakeLists.txt uses ExternalProject_Add to do two main things:
# 1. ExternalProject_Add(Externals ---> Gets all of SimVascular's externals
# 2. ExternalProject_Add(SimVascular ---> Builds SimVascular
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
cmake_minimum_required(VERSION 3.6.0)

if (POLICY CMP0040)
  cmake_policy(SET CMP0040 OLD)
endif()
if (POLICY CMP0046)
  cmake_policy(SET CMP0046 OLD)
endif()
if (POLICY CMP0053)
  cmake_policy(SET CMP0053 OLD)
endif()
if (POLICY CMP0054)
  cmake_policy(SET CMP0054 OLD)
endif()
if (POLICY CMP0025) # Compiler ID when using clang set to clang not appleclang
  cmake_policy(SET CMP0025 OLD)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
Project(SV_TOP)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# CMake module path inside of true simvascular source
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/Code/CMake"
	"${CMAKE_MODULE_PATH}")
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
# Externals!
set(SV_EXTERNALS_TOPLEVEL_BIN_DIR "${CMAKE_BINARY_DIR}/Externals-build/sv_externals/bin")
set(SV_EXTERNALS_ADDITIONAL_CMAKE_ARGS "" CACHE STRING "If more options want to be provided to the sv_externals build, they can be with this string")
ExternalProject_Add(Externals
  SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Externals
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/Externals-build
  DOWNLOAD_COMMAND ""
  UPDATE_COMMAND ""
  INSTALL_COMMAND ""
  CMAKE_CACHE_ARGS
    -DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
    -DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
    -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
    -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    -DCMAKE_MACOSX_RPATH:BOOL=ON
    -DBUILD_TESTING:BOOL=OFF
    -DBUILD_EXAMPLES:BOOL=OFF
    -DSV_EXTERNALS_TOPLEVEL_BIN_DIR:PATH=${SV_EXTERNALS_TOPLEVEL_BIN_DIR}
    ${SV_EXTERNALS_ADDITIONAL_CMAKE_ARGS}
    )
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# SimVascular!

if(WIN32)
  set(freetype_shared_option "-DSV_USE_FREETYPE_SHARED:BOOL=OFF")
else()
  set(freetype_shared_option "-DSV_USE_FREETYPE_SHARED:BOOL=ON")
endif()

set(SV_ADDITIONAL_CMAKE_ARGS "" CACHE STRING "If more options want to be provided to the sv_externals build, they can be with this string")
ExternalProject_Add(SimVascular
  SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Code
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/SimVascular-build
  DEPENDS Externals
  DOWNLOAD_COMMAND ""
  UPDATE_COMMAND ""
  INSTALL_COMMAND ""
  CMAKE_CACHE_ARGS
    -DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
    -DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
    -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
    -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    -DCMAKE_MACOSX_RPATH:BOOL=ON
    -DCMAKE_INSTALL_PREFIX:STRING=${CMAKE_INSTALL_PREFIX}
    -DBUILD_TESTING:BOOL=OFF
    -DBUILD_EXAMPLES:BOOL=OFF
    -DBUILD_SHARED_LIBS:BOOL=ON
    -DSV_USE_FREETYPE:BOOL=ON
    ${freetype_shared_option}
    -DSV_USE_GDCM:BOOL=ON
    -DSV_USE_ITK:BOOL=ON
    -DSV_USE_MITK:BOOL=ON
    -DSV_USE_MITK_CONFIG:BOOL=OFF
    -DSV_USE_MMG:BOOL=ON
    -DSV_USE_MPICH2:BOOL=OFF
    -DSV_USE_OpenCASCADE:BOOL=ON
    -DSV_USE_PYTHON:BOOL=ON
    -DSV_USE_Qt5:BOOL=ON
    -DSV_USE_QT:BOOL=ON
    -DSV_USE_QT_GUI:BOOL=ON
    -DSV_USE_SOLVERIO:BOOL=ON
    -DSV_USE_TETGEN_ADAPTOR:BOOL=ON
    -DSV_EXTERNALS_USE_TOPLEVEL_BIN_DIR:BOOL=ON
    -DSV_EXTERNALS_TOPLEVEL_BIN_DIR:PATH=${SV_EXTERNALS_TOPLEVEL_BIN_DIR}
    -DQT_DLL_PATH:PATH=${qt5_installed_prefix}/bin
    -DQT_PLUGIN_PATH:PATH=${qt5_installed_prefix}/plugins
    ${SV_ADDITIONAL_CMAKE_ARGS}
    )
#-----------------------------------------------------------------------------
