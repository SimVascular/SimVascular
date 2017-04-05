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
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Compiler
set(COMPILER_VERSION ${CMAKE_CXX_COMPILER_ID})
if (NOT CMAKE_CXX_COMPILER_VERSION)
  message(FATAL_ERROR "Compiler version does not exist; must specify the compiler
                       version with -DCMAKE_CXX_COMPILER_VERSION='major_version'.'minor_version'")
endif()
sv_externals_get_major_minor_version(${CMAKE_CXX_COMPILER_VERSION} COMPILER_MAJOR_VERSION COMPILER_MINOR_VERSION)
string(TOLOWER "${COMPILER_VERSION}" COMPILER_VERSION_LOWER)
set(SV_EXTERNALS_COMPILER_DIR "${COMPILER_VERSION_LOWER}-${COMPILER_MAJOR_VERSION}.${COMPILER_MINOR_VERSION}")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Architecture, only x64 supported
set(SV_EXTERNALS_ARCH_DIR "x64")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Set platforms directories
if(APPLE)
  set(SV_EXTERNALS_PLATFORM_DIR "mac_osx")
  set(SV_EXTERNALS_DOWNLOADS_PLATFORM_DIRS "${SV_EXTERNALS_PLATFORM_DIR}/10.10/${SV_EXTERNALS_COMPILER_DIR}")
  set(SV_EXTERNALS_DOWNLOADS_DEFAULT_DIR "${SV_EXTERNALS_PLATFORM_DIR}/10.10/clang-7.0/latest/${SV_EXTERNALS_PLATFORM_DIR}.clang-7.0.${SV_EXTERNALS_ARCH_DIR}")
elseif(LINUX)
  set(SV_EXTERNALS_PLATFORM_DIR "linux")
  set(SV_EXTERNALS_DOWNLOADS_PLATFORM_DIRS" ${SV_EXTERNALS_PLATFORM_DIR}/ubuntu_14/${SV_EXTERNALS_COMPILER_DIR}")
  set(SV_EXTERNALS_DOWNLOADS_DEFAULT_DIR "${SV_EXTERNALS_PLATFORM_DIR}/ubuntu_14/gnu-4.8/latest/${SV_EXTERNALS_PLATFORM_DIR}.gnu-4.8.${SV_EXTERNALS_ARCH_DIR}")
elseif(WIN64)
  set(SV_EXTERNALS_PLATFORM_DIR "windows")
  set(SV_EXTERNALS_DOWNLOADS_PLATFORM_DIRS "${SV_EXTERNALS_PLATFORM_DIR}/10/msvc_2013")
  set(SV_EXTERNALS_DOWNLOADS_DEFAULT_DIR "${SV_EXTERNALS_PLATFORM_DIR}/10/msvc_2013/latest/${SV_EXTERNALS_PLATFORM_DIR}.msvc-12.5.${SV_EXTERNALS_ARCH_DIR}")
else()
  set(SV_EXTERNALS_PLATFORM_DIR "unsupported")
  set(SV_EXTERNALS_DOWNLOADS_PLATFORM_DIRS "${SV_EXTERNALS_PLATFORM_DIR}")
endif()
#-----------------------------------------------------------------------------

