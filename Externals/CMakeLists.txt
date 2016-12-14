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
cmake_minimum_required(VERSION 2.8.12)

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


#-----------------------------------------------------------------------------
Project(SV_EXTERNALS)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# CMake functions
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/CMake"
  "${CMAKE_MODULE_PATH}")
set(SV_EXTERNALS_CMAKE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/CMake")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Set a default build type (if none was specified)
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'RelWithDebInfo' as none was specified.")
  set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Choose the type of build." FORCE)
  mark_as_advanced(CMAKE_BUILD_TYPE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
    "MinSizeRel" "RelWithDebInfo")
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Includes
include(ExternalProject)
include(SvExtMacros)
#include(SvExtFunctions)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Initial options
include(SvExtOptions)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# First process special Qt stuff, external but not being built
if(SV_EXTERNALS_USE_QT)
  #Find Qt!
  set(SV_EXTERNALS_Qt5_COMPONENTS
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
  find_package(Qt5 COMPONENTS ${SV_EXTERNALS_Qt5_COMPONENTS} REQUIRED)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Add each external to be built. Full list and initial options are specified in SvExtOptions.cmake
foreach(proj ${SV_EXTERNALS_LIST})
  if(SV_EXTERNALS_BUILD_${proj})
    if(EXISTS "${SV_EXTERNALS_CMAKE_DIR}/${proj}.cmake")
      include("${SV_EXTERNALS_CMAKE_DIR}/${proj}.cmake")
    endif()
  endif()
endforeach()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Tar up everything
set(SV_EXTERNALS_EVERYTHING}_TAR_INSTALL_NAME ${SV_EXTERNALS_PLATFORM_DIR}.${SV_EXTERNALS_COMPILER_DIR}.${SV_EXTERNALS_ARCH_DIR}.${${proj}_VERSION_DIR}-BUILD${TODAY})
if(EXISTS "${SV_EXTERNALS_TAR_INSTALL_DIR}")
  install(CODE "execute_process(COMMAND ${CMAKE_COMMAND} -E tar -czvf ${SV_EXTERNALS_TAR_INSTALL_DIR}/${SV_EXTERNALS_EVERYTING_TAR_INSTALL_NAME}.tar.gz ${SV_EXTERNALS_TOPLEVEL_BIN_DIR}
    WORKING_DIRECTORY ${SV_EXTERNALS_TOPLEVEL_BIN_DIR}/../)")
endif()
#-----------------------------------------------------------------------------
