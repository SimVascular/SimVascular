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
# This is SimVascular's CMakeLists.txt for downloading/building the externals
# needed for SimVacular. Pre-built binaries are only available for certain
# versions of the libraries on certain operating systems. To build your own,
# you must turn off downloading of the specific external. In this case,
# you should be able to build another version of the external so long that
# the external is being cloned from git and the version provided is an
# existing tag.
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
#-----------------------------------------------------------------------------


#-----------------------------------------------------------------------------
Project(SV_EXTERNALS)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# CMake module path for both us and the real simvascular
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/CMakeExternals"
  "${CMAKE_MODULE_PATH}")
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../Code/CMake"
  "${CMAKE_MODULE_PATH}")
set(SV_EXTERNALS_CMAKE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/CMakeExternals")
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
# Initial options
set(SV_EXTERNALS_LIST "")
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
  if(SV_EXTERNALS_ENABLE_${proj})
    if(EXISTS "${SV_EXTERNALS_CMAKE_DIR}/${proj}.cmake")
      include("${SV_EXTERNALS_CMAKE_DIR}/${proj}.cmake")
    endif()
  endif()
endforeach()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Tar up everything
simvascular_today(YEAR MONTH DAY)
set(SV_EXTERNALS_EVERYTHING_TAR_INSTALL_NAME ${SV_PLATFORM_DIR}.${SV_PLATFORM_VERSION_DIR}.${SV_COMPILER_DIR}.${SV_COMPILER_VERSION_DIR}.${SV_ARCH_DIR}.${SV_BUILD_TYPE_DIR}.${YEAR}.${MONTH}.${DAY}.everything)
if(EXISTS "${SV_EXTERNALS_TAR_INSTALL_DIR}")
  install(CODE "execute_process(COMMAND ${CMAKE_COMMAND} -E tar -czvf ${SV_EXTERNALS_TAR_INSTALL_DIR}/${SV_EXTERNALS_EVERYTHING_TAR_INSTALL_NAME}.tar.gz ${SV_EXTERNALS_TOPLEVEL_BIN_DIR}
    WORKING_DIRECTORY ${SV_EXTERNALS_TOPLEVEL_BIN_DIR}/../)")
endif()
#-----------------------------------------------------------------------------
