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
# This file contains important cmake system setup variables for simvascular
# Sets the version number, determines the architecture, compiler, etc.
# System architecture
if("${CMAKE_SYSTEM_PROCESSOR}" MATCHES "64" AND NOT APPLE)
	set(ARCH "x64")
	set(IS64 TRUE)
elseif(APPLE)
	#uname -p does not work correctly on OS X, we are going to assume its x64
	SET(ARCH "x64")
	set(IS64 TRUE)
else()
	SET(ARCH "x32")
  set(IS64 FALSE)
endif()
set(SV_ARCH_DIR "${ARCH}" CACHE STRING "The architecture being used.")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# OS
set(SV_OS "${CMAKE_SYSTEM_NAME}")
IF("${CMAKE_SYSTEM}" MATCHES "Linux")
	SET(LINUX TRUE)
elseif(WIN32)
	set(WINDOWS TRUE)
endif()

if(WIN32)
	if("${CMAKE_SIZEOF_VOID_P}" EQUAL 8)
		set(WIN64 TRUE)
	else()
		set(WIN64 FALSE)
	endif()
	set(ENV_SET_COMMAND "set")
	set(ENV_PATH_VARIABLE "PATH")
	set(ENV_LIBRARY_PATH_VARIABLE "PATH")
	set(ENV_SEPERATOR ";")
	set(DIR_SEPERATOR "\\")
endif()

if(UNIX)
	set(ENV_SET_COMMAND "export")
	set(ENV_PATH_VARIABLE "PATH")
	set(ENV_SEPERATOR ":")
	set(DIR_SEPERATOR "/")
	if(APPLE)
		set(ENV_LIBRARY_PATH_VARIABLE "DYLD_LIBRARY_PATH")
		set(DYLD "DYLD")
	endif()
	if(UNIX AND NOT APPLE)
		set(ENV_LIBRARY_PATH_VARIABLE "LD_LIBRARY_PATH")
		set(DYLD "LD")
	endif()
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Cluster
set(CLUSTER "${ARCH}_${SV_OS}")
if(SV_DEVELOPER_OUTPUT)
	message(STATUS "${CLUSTER}")
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# These are SimVascular build and version that are presented to the user to modify
set(SV_RELEASE_TYPE "Release" CACHE STRING "This specificies which install dir and GUIDs to use, is also used in header files")
set_property(CACHE SV_RELEASE_TYPE PROPERTY STRINGS Release Beta)
mark_as_advanced(SV_RELEASE_TYPE)

set(SV_VERSION "simvascular")
if(SV_RELEASE_TYPE MATCHES "^Beta$")
	set(SV_VERSION "simvascular-beta")
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Timestamp
string(TIMESTAMP DATE_IMESTAMP %y%m%d)
math(EXPR SV_VERSION_TIMESTAMP "${DATE_IMESTAMP}-140000")
string(TIMESTAMP SV_RELEASE_TIMESTAMP %y%m%d%H%M%S)
set(SV_PLATFORM ${ARCH})
simvascular_today(YEAR MONTH DAY)
set(SV_MAJOR_VERSION ${YEAR})   # YEAR
set(SV_MINOR_VERSION ${MONTH})   # MONTH
set(SV_PATCH_VERSION ${DAY})  # DAY
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Other helpful sv variables
set(SV_RELEASE_BUILD 0)
set(SV_MAJOR_VER_NO ${SV_MAJOR_VERSION})
set(SV_FULL_VER_NO
	"${SV_MAJOR_VERSION}.${SV_MINOR_VERSION}")
set(SV_FULL_VERSION
	"${SV_MAJOR_VERSION}.${SV_MINOR_VERSION}.${SV_PATCH_VERSION}")
set(SV_REGISTRY_TOPLEVEL "SV")

message(STATUS "SimVascular Version: ${SV_VERSION} ${SV_MAJOR_VERSION}-${SV_MINOR_VERSION}-${SV_PATCH_VERSION}")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Environment Home
SET(USER_HOME_DIR $ENV{HOME})
if(SV_DEVELOPER_OUTPUT)
	message(STATUS "Home dir: ${USER_HOME_DIR}")
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Install root dir
if(NOT SV_INSTALL_ROOT_DIR)
  set(SV_INSTALL_ROOT_DIR "SV")
endif()
if(NOT WIN32)
  if(NOT CMAKE_INSTALL_PREFIX MATCHES "${SV_INSTALL_ROOT_DIR}")
    set(CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX}/${SV_INSTALL_ROOT_DIR})
  endif()
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Set platforms directories
# Directory structure is SV_KERNEL_DIR/SV_PLATFORM_DIR/SV_PLATFORM_VERSION_DIR/SV_COMPILER_DIR/SV_COMPILER_VERSION_DIR/SV_ARCH_DIR/SV_BUILD_TYPE_DIR
string(TOLOWER "${CMAKE_SYSTEM_NAME}" _kernel_lower)
set(SV_KERNEL_DIR "${_kernel_lower}" CACHE STRING "The overall platform kernel being used.")

# Set the rest of the system variables
if(APPLE)

  # Assuming use mac os if APPLE
  set(SV_PLATFORM_DIR "mac_osx" CACHE STRING "The distribution platform being used.")

  # Get just major minor version of osx
  simvascular_get_major_minor_version(${CURRENT_OSX_VERSION} SV_OSX_MAJOR_VERSION SV_OSX_MINOR_VERSION)

  # Set the os version number
  set(SV_PLATFORM_VERSION_DIR "${SV_OSX_MAJOR_VERSION}.${SV_OSX_MINOR_VERSION}" CACHE STRING "The distribution platform version being used.")

elseif(LINUX)

  # To get the distriubtion and the version, we need to use lsb
  find_program(LSB_RELEASE lsb_release)
  execute_process(COMMAND ${LSB_RELEASE} -a
    OUTPUT_VARIABLE LSB_RELEASE_INFO
    OUTPUT_STRIP_TRAILING_WHITESPACE)

  # Get distribution name and version number from lsb_release output
  STRING(REGEX REPLACE "Distributor ID:[\t]*([^ \n\r]+).*$" "\\1" LSB_DISTRIB "${LSB_RELEASE_INFO}")
  STRING(REGEX REPLACE "^.*Release:[\t]*([^ \n\r]+).*$" "\\1" LSB_VERSION "${LSB_RELEASE_INFO}")
  string(TOLOWER "${LSB_DISTRIB}" _platform_lower)

  # Set the distrib and version
  set(SV_PLATFORM_DIR "${_platform_lower}" CACHE STRING "The distribution platform being used.")
  set(SV_PLATFORM_VERSION_DIR "${LSB_VERSION}" CACHE STRING "The distribution platform version being used.")

elseif(WIN64)

  # Windows the platform and kernel should be same
  set(SV_PLATFORM_DIR "${SV_KERNEL_DIR}" CACHE STRING "The distribution platform being used.")

  # Set the version of the platform
  set(SV_PLATFORM_VERSION_DIR "${CMAKE_SYSTEM_VERSION}" CACHE STRING "The distribution platform version being used.")
else()
  set(SV_PLATFORM_DIR "unsupported")
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Compiler
set(COMPILER_VERSION ${CMAKE_CXX_COMPILER_ID})
if (NOT CMAKE_CXX_COMPILER_VERSION)
  message(FATAL_ERROR "Compiler version does not exist; must specify the compiler
                       version with -DCMAKE_CXX_COMPILER_VERSION='major_version'.'minor_version'")
endif()
simvascular_get_major_minor_version(${CMAKE_CXX_COMPILER_VERSION} COMPILER_MAJOR_VERSION COMPILER_MINOR_VERSION)

string(TOLOWER "${COMPILER_VERSION}" _compiler_version_lower)
set(SV_COMPILER_DIR "${_compiler_version_lower}" CACHE STRING "The compiler being used.")
set(SV_COMPILER_VERSION_DIR "${COMPILER_MAJOR_VERSION}.${COMPILER_MINOR_VERSION}" CACHE STRING "The compiler version being used.")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Temp dir for TCL
if(NOT TEMP_DIR)
  set(TEMP_DIR ${SV_BINARY_DIR}/tmp)
  file(MAKE_DIRECTORY ${TEMP_DIR})
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# SV HOME
get_filename_component(SV_SOURCE_HOME ${SV_SOURCE_DIR}/../ ABSOLUTE)
dev_message("SimVascular Source Home: ${SV_SOURCE_HOME}")
if(NOT SV_BINARY_HOME)
	set(SV_BINARY_HOME ${SV_BINARY_DIR})
endif()

set(SV_HOME ${SV_BINARY_HOME})
set(SV_DISTRIBUTION_DIR ${SV_SOURCE_HOME}/Distribution)
set(SV_BINARY_DISTRIBUTION_DIR ${SV_BINARY_HOME}/Distribution)
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
set(SV_BUILD_TYPE_DIR "${CMAKE_BUILD_TYPE}" CACHE STRING "The compile type being used.")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Build type
set(SV_BUILD_TYPE "CMAKE" CACHE STRING "Designate CMAKE build" FORCE)
set_property(CACHE SV_BUILD_TYPE PROPERTY STRINGS CMAKE)
mark_as_advanced(SV_BUILD_TYPE)
#-----------------------------------------------------------------------------

