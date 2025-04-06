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

cmake_minimum_required(VERSION 3.18.0)

Project(SV_TOP)

# CMake module path inside of true simvascular source
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/Code/CMake" "${CMAKE_MODULE_PATH}")

# CMake Includes
include(CheckLibraryExists)
include(GetPrerequisites)
include(GenerateExportHeader)
include(ExternalProject)

# SimVascular Includes
include(SimVascularMacros)
include(SimVascularSystemSetup)
include(SimVascularFunctionCheckCompilerFlags)

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

# Define varaibles storing external paths.
#
# These will be set manually during the build.
#
# Note that these names must match those given in Code/CMake/Externals/*.cmake files.
#
set(SV_EXTERNALS_DIR "" CACHE STRING "SimVascular externals directory")
mark_as_advanced(SV_EXTERNALS_DIR)

set(SV_FREETYPE_DIR "" CACHE PATH "FREETYPE install directory containing a configuration file")
set(SV_GDCM_DIR "" CACHE PATH "GDCM install directory containing a configuration file")
set(SV_HDF5_DIR "" CACHE PATH "hdf5 install directory containing a configuration file")
set(SV_ITK_DIR "" CACHE PATH "ITK install directory containing a configuration file")
set(SV_ML_DIR "" CACHE PATH "ML install directory containing machine learning weights")
set(SV_MMG_DIR "" CACHE PATH "MMG install directory containing a configuration file")
set(SV_OpenCASCADE_DIR "" CACHE PATH "OpenCASCADE install directory containing a configuration file")
set(SV_PYTHON_DIR"" CACHE PATH "PYTHON install directory containing a configuration file")
set(SV_Qt6_DIR "" CACHE PATH "Qt6 install directory containing a configuration file")
set(SV_TINYXML2_DIR "" CACHE PATH "TINYXML2 install directory containing a configuration file")
set(SV_VTK_DIR "" CACHE PATH "VTK install directory containing a configuration file")

set(SV_EXTERNALS_VERSION_NUMBER "2024.05" CACHE STRING "SimVascular Externals Release Version")
mark_as_advanced(SV_EXTERNALS_VERSION_NUMBER)

# Distribution
option(SV_ENABLE_DISTRIBUTION "Enable Installer Build for SimVascular" OFF)

# sv4gui
option(SV_USE_SV4_GUI "Enable SV MITK/Qt Based GUI" ON)

if(APPLE)
  option(SV_EXTERNALS_USE_PREBUILT_QT "Use Externally Installed Qt6" OFF)
  #set(SV_EXTERNALS_PREBUILT_QT_PATH "" CACHE PATH "Qt6 Cmake Config File Path" FORCE)
elseif(LINUX)
  option(SV_EXTERNALS_USE_PREBUILT_QT "Use Externally Installed Qt6" OFF)

  #if(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.01")
    #set(SV_EXTERNALS_PREBUILT_QT_PATH "/opt/Qt5.4.2/5.4/gcc_64/lib/cmake/Qt5" CACHE PATH "Qt5 Cmake Config File Path" FORCE)
  #elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.05")
    #set(SV_EXTERNALS_PREBUILT_QT_PATH "/opt/Qt5.6.3/5.6.3/gcc_64/lib/cmake/Qt5" CACHE PATH "Qt5 Cmake Config File Path" FORCE)
  #elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2019.02")
    #set(SV_EXTERNALS_PREBUILT_QT_PATH "/opt/Qt5.6.3/5.6.3/gcc_64/lib/cmake/Qt5" CACHE PATH "Qt5 Cmake Config File Path" FORCE)
  #elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_GREATER_EQUAL "2019.06")
    #set(SV_EXTERNALS_PREBUILT_QT_PATH "/opt/Qt5.11.3/5.11.3/gcc_64/lib/cmake/Qt5" CACHE PATH "Qt5 Cmake Config File Path" FORCE)
  #else()
    #set(SV_EXTERNALS_PREBUILT_QT_PATH "" FORCE)
  #endif()

elseif(WIN32)
  option(SV_EXTERNALS_USE_PREBUILT_QT "Use Externally Installed Qt6" OFF)
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

#mark_as_advanced(SV_EXTERNALS_USE_PREBUILT_QT)
#mark_as_advanced(SV_EXTERNALS_PREBUILT_QT_PATH)

#set(SV_EXTERNALS_TOPLEVEL_BIN_DIR "${CMAKE_BINARY_DIR}/Externals-build/svExternals/bin")
#set(SV_EXTERNALS_ADDITIONAL_CMAKE_ARGS "" CACHE STRING "If more options want to be provided to the sv_externals build, they can be with this string")

# Note: only download mitk if needed, but need to download Qt
# because of VTK dependence regardless
#
set(SV_CMAKE_DOWLOAD_MITK_EXTERNAL "ON" CACHE BOOL "Force ON" FORCE)
if(DEFINED SV_USE_SV4_GUI)
  if(NOT SV_USE_SV4_GUI)
    set(SV_CMAKE_DOWLOAD_MITK_EXTERNAL "OFF" CACHE BOOL "Force OFF" FORCE)
  endif()
endif()

message(STATUS "[root] ")
message(STATUS "[root] ++++ Add Externals project ... ")
message(STATUS "[root] SV_EXTERNALS_DIR: ${SV_EXTERNALS_DIR}")
message(STATUS "[root] SV_EXTERNALS_VERSION_NUMBER: ${SV_EXTERNALS_VERSION_NUMBER}")
message(STATUS "[root] DSV_FREETYPE_DIR: ${DSV_FREETYPE_DIR}")
message(STATUS "[root] SV_GDCM_DIR: ${SV_GDCM_DIR}")
message(STATUS "[root] SV_PYTHON_DIR: ${SV_PYTHON_DIR}")
message(STATUS "[root] SV_Qt6_DIR: ${SV_Qt6_DIR}")
message(STATUS "[root] PYTHON_DIR: ${PYTHON_DIR}")
message(STATUS "[root] SV_TINYXML2_DIR: ${SV_TINYXML2_DIR}")
#message(FATAL_ERROR "[root] PYTHON_DIR: ${PYTHON_DIR}")

# [DaveP] I commented this out and SV build fine, does not seem 
# to affect installer build.
#
#[[
#
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
    # External packages directories
    -DSV_FREETYPE_DIR:PATH=${SV_FREETYPE_DIR}
    -DSV_GDCM_DIR:PATH=${SV_GDCM_DIR}
    -DSV_HDF5_DIR:PATH=${SV_HDF5_DIR}
    -DSV_ITK_DIR:PATH=${SV_ITK_DIR}
    -DSV_MMG_DIR:PATH=${SV_MMG_DIR}
    -DSV_MITK_DIR:PATH=${SV_MITK_DIR}
    -DSV_ML_DIR:PATH=${SV_ML_DIR}
    -DSV_OpenCASCADE_DIR:PATH=${SV_OpenCASCADE_DIR}
    -DSV_PYTHON_DIR:PATH=${SV_PYTHON_DIR}
    -DSV_TINYXML2_DIR:PATH=${SV_TINYXML2_DIR}
    -DSV_Qt6_DIR:PATH=${SV_Qt6_DIR}
    -DSV_VTK_DIR:PATH=${SV_VTK_DIR}
    #
    -DSV_EXTERNALS_USE_PREBUILT_QT:BOOL=${SV_EXTERNALS_USE_PREBUILT_QT}
    -DSV_EXTERNALS_PREBUILT_QT_PATH:PATH=${SV_EXTERNALS_PREBUILT_QT_PATH}
    -DQt6_DIR:PATH=${SV_EXTERNALS_PREBUILT_QT_PATH}
    -DSV_EXTERNALS_TOPLEVEL_BIN_DIR:PATH=${SV_EXTERNALS_TOPLEVEL_BIN_DIR}
    ${SV_EXTERNALS_ADDITIONAL_CMAKE_ARGS}
    )
#]]

message(STATUS "[root] ---- Done add Externals project ... ")

# Create SimVascular external project ?
#
message(STATUS "[root] ")
message(STATUS "[root] ++++ Add SimVascular external project ... ")
message(STATUS "[root] SV_EXTERNALS_DIR: ${SV_EXTERNALS_DIR}")

if(WIN32)
  set(freetype_shared_option "-DSV_USE_FREETYPE_SHARED:BOOL=OFF")
else()
  set(freetype_shared_option "-DSV_USE_FREETYPE_SHARED:BOOL=ON")
endif()

variable_watch(SV_USE_ZLIB)

set(SV_ADDITIONAL_CMAKE_ARGS "" CACHE STRING "If more options want to be provided to the sv_externals build, they can be with this string")

ExternalProject_Add(SimVascular
  SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Code
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/SimVascular-build
  # Davep DEPENDS Externals
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
    -DSV_FREETYPE_DIR:PATH=${SV_FREETYPE_DIR}
    ${freetype_shared_option}
    -DSV_USE_GDCM:BOOL=ON
    -DSV_GDCM_DIR:PATH=${SV_GDCM_DIR}
    -DSV_HDF5_DIR:PATH=${SV_HDF5_DIR}
    -DSV_USE_ITK:BOOL=ON
    -DSV_ITK_DIR:PATH=${SV_ITK_DIR}
    -DSV_USE_MITK:BOOL=ON
    -DSV_USE_MITK_CONFIG:BOOL=OFF
    -DSV_MITK_DIR:PATH=${SV_MITK_DIR}
    -DSV_ML_DIR:PATH=${SV_ML_DIR}
    -DSV_USE_MMG:BOOL=ON
    -DSV_MMG_DIR:PATH=${SV_MMG_DIR}
    -DSV_USE_MPICH2:BOOL=OFF
    -DSV_USE_OpenCASCADE:BOOL=ON
    -DSV_OpenCASCADE_DIR:PATH=${SV_OpenCASCADE_DIR}
    -DSV_USE_PYTHON:BOOL=ON
    -DSV_USE_Qt6:BOOL=ON
    -DSV_USE_QT:BOOL=ON
    -DPython_DIR:PATH=${SV_PYTHON_DIR}
    -DPYTHON_DIR:PATH=${SV_PYTHON_DIR}
    -DSV_PYTHON_DIR:PATH=${SV_PYTHON_DIR}
    -DSV_Qt6_DIR:PATH=${SV_Qt6_DIR}
    -DSV_VTK_DIR:PATH=${SV_VTK_DIR}
    -DSV_TINYXML2_DIR:PATH=${SV_TINYXML2_DIR}
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
    -DQt6_DIR:PATH=${SV_EXTERNALS_PREBUILT_QT_PATH}
    ${SV_ADDITIONAL_CMAKE_ARGS}
    )

message(STATUS "[root] ---- Done add SimVascular external project ... ")

