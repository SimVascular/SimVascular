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
cmake_minimum_required(VERSION 3.10.0)
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

# Optionally pull in the svZeroDSolver code 
option(SV_GIT_PULL_SUBMODULES "Check submodules during build" OFF)

# Optionally build the 1D solver plugin.
option(SV_USE_ONEDSOLVER "Enable building the 1D solver plugin" ON)

# Optionally build the MITK segmentation plugin
option(SV_USE_MITK_SEGMENTATION "Enable building the MITK segmentation plugin" ON)
#-----------------------------------------------------------------------------
# Externals!

set(SV_EXTERNALS_VERSION_NUMBER "2019.06" CACHE STRING "SimVascular Externals Release Version")
mark_as_advanced(SV_EXTERNALS_VERSION_NUMBER)

# Distribution
option(SV_ENABLE_DISTRIBUTION "Enable Installer Build for SimVascular" OFF)

# sv4gui
option(SV_USE_SV4_GUI "Enable SV MITK/Qt Based GUI" ON)

if(APPLE)
  option(SV_EXTERNALS_USE_PREBUILT_QT "Use Externally Installed Qt5" OFF)
  set(SV_EXTERNALS_PREBUILT_QT_PATH "" CACHE PATH "Qt5 Cmake Config File Path" FORCE)
elseif(LINUX)
  option(SV_EXTERNALS_USE_PREBUILT_QT "Use Externally Installed Qt5" OFF)
  if(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.01")
    set(SV_EXTERNALS_PREBUILT_QT_PATH "/opt/Qt5.4.2/5.4/gcc_64/lib/cmake/Qt5" CACHE PATH "Qt5 Cmake Config File Path" FORCE)
  elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.05")
    set(SV_EXTERNALS_PREBUILT_QT_PATH "/opt/Qt5.6.3/5.6.3/gcc_64/lib/cmake/Qt5" CACHE PATH "Qt5 Cmake Config File Path" FORCE)
  elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2019.02")
    set(SV_EXTERNALS_PREBUILT_QT_PATH "/opt/Qt5.6.3/5.6.3/gcc_64/lib/cmake/Qt5" CACHE PATH "Qt5 Cmake Config File Path" FORCE)
  elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_GREATER_EQUAL "2019.06")
    set(SV_EXTERNALS_PREBUILT_QT_PATH "/opt/Qt5.11.3/5.11.3/gcc_64/lib/cmake/Qt5" CACHE PATH "Qt5 Cmake Config File Path" FORCE)
  else()
    set(SV_EXTERNALS_PREBUILT_QT_PATH "" FORCE)
  endif()
elseif(WIN32)
  option(SV_EXTERNALS_USE_PREBUILT_QT "Use Externally Installed Qt5" OFF)
  if(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.01")
    set(SV_EXTERNALS_PREBUILT_QT_PATH "C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/lib/cmake/Qt5" CACHE PATH "Qt5 Cmake Config File Path")
  elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.05")
    set(SV_EXTERNALS_PREBUILT_QT_PATH "C:/OpenSource/Qt/Qt5.6.3/5.6.3/msvc2015_64/lib/cmake/Qt5" CACHE PATH "Qt5 Cmake Config File Path")
  elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2019.02")
    set(SV_EXTERNALS_PREBUILT_QT_PATH "C:/OpenSource/Qt/Qt5.6.3/5.6.3/msvc2015_64/lib/cmake/Qt5" CACHE PATH "Qt5 Cmake Config File Path")
  elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_GREATER_EQUAL "2019.06")
    set(SV_EXTERNALS_PREBUILT_QT_PATH "C:/OpenSource/Qt/Qt5.11.3/5.11.3/msvc2017_64/lib/cmake/Qt5" CACHE PATH "Qt5 Cmake Config File Path")
  else()
    set(SV_EXTERNALS_PREBUILT_QT_PATH "" FORCE)
  endif()
endif()

mark_as_advanced(SV_EXTERNALS_USE_PREBUILT_QT)
mark_as_advanced(SV_EXTERNALS_PREBUILT_QT_PATH)

set(SV_EXTERNALS_TOPLEVEL_BIN_DIR "${CMAKE_BINARY_DIR}/Externals-build/svExternals/bin")
set(SV_EXTERNALS_ADDITIONAL_CMAKE_ARGS "" CACHE STRING "If more options want to be provided to the sv_externals build, they can be with this string")

# Note: only download mitk if needed, but need to download Qt
# because of VTK dependence regardless
set(SV_CMAKE_DOWLOAD_MITK_EXTERNAL "ON" CACHE BOOL "Force ON" FORCE)
if(DEFINED SV_USE_SV4_GUI)
  if(NOT SV_USE_SV4_GUI)
    set(SV_CMAKE_DOWLOAD_MITK_EXTERNAL "OFF" CACHE BOOL "Force OFF" FORCE)
  endif()
endif()

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
    -DSV_USE_MITK:BOOL=${SV_CMAKE_DOWLOAD_MITK_EXTERNAL}
    -DSV_GIT_PULL_SUBMODULES:BOOL=${SV_GIT_PULL_SUBMODULES}
    -DSV_EXTERNALS_VERSION_NUMBER:STRING=${SV_EXTERNALS_VERSION_NUMBER}
    -DSV_EXTERNALS_USE_PREBUILT_QT:BOOL=${SV_EXTERNALS_USE_PREBUILT_QT}
    -DSV_EXTERNALS_PREBUILT_QT_PATH:PATH=${SV_EXTERNALS_PREBUILT_QT_PATH}
    -DQt5_DIR:PATH=${SV_EXTERNALS_PREBUILT_QT_PATH}
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
    -DSV_EXTERNALS_VERSION_NUMBER:STRING=${SV_EXTERNALS_VERSION_NUMBER}
    -DSV_ENABLE_DISTRIBUTION:BOOL=${SV_ENABLE_DISTRIBUTION}
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
    -DSV_USE_SV4_GUI:BOOL=${SV_USE_SV4_GUI}
    -DSV_USE_MITK_SEGMENTATION:BOOL=${SV_USE_MITK_SEGMENTATION}
    -DSV_USE_SOLVERIO:BOOL=ON
    -DSV_USE_TETGEN_ADAPTOR:BOOL=ON
    -DSV_EXTERNALS_USE_TOPLEVEL_BIN_DIR:BOOL=ON
    -DSV_EXTERNALS_TOPLEVEL_BIN_DIR:PATH=${SV_EXTERNALS_TOPLEVEL_BIN_DIR}
    -DQT_DLL_PATH:PATH=${qt5_installed_prefix}/bin
    -DQT_PLUGIN_PATH:PATH=${qt5_installed_prefix}/plugins
    -DSV_EXTERNALS_USE_PREBUILT_QT:BOOL=${SV_EXTERNALS_USE_PREBUILT_QT}
    -DSV_EXTERNALS_PREBUILT_QT_PATH:PATH=${SV_EXTERNALS_PREBUILT_QT_PATH}
    -DSV_EXTERNALS_VERSION_NUMBER:STRING=${SV_EXTERNALS_VERSION_NUMBER}
    -DQt5_DIR:PATH=${SV_EXTERNALS_PREBUILT_QT_PATH}
    ${SV_ADDITIONAL_CMAKE_ARGS}
    )
#-----------------------------------------------------------------------------
