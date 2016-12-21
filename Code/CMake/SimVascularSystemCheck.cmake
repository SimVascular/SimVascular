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
set(SV_ARCH_DIR "${ARCH}")
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
# Compiler
set(COMPILER_VERSION ${CMAKE_CXX_COMPILER_ID})
if (NOT CMAKE_CXX_COMPILER_VERSION)
  message(FATAL_ERROR "Compiler version does not exist; must specify the compiler
                       version with -DCMAKE_CXX_COMPILER_VERSION='major_version'.'minor_version'")
endif()
simvascular_get_major_minor_version(${CMAKE_CXX_COMPILER_VERSION} COMPILER_MAJOR_VERSION COMPILER_MINOR_VERSION)

string(TOLOWER "${COMPILER_VERSION}" COMPILER_VERSION_LOWER)
set(SV_COMPILER_DIR "${COMPILER_VERSION_LOWER}-${COMPILER_MAJOR_VERSION}.${COMPILER_MINOR_VERSION}")
#-----------------------------------------------------------------------------
