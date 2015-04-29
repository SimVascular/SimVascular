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

if("${CMAKE_SYSTEM_PROCESSOR}" MATCHES "64" AND NOT APPLE)
	SET(ARCH "x64")
	set(IS64 TRUE)
elseif(APPLE)
	#uname -p does not work correctly on OS X, we are going to assume its x64
	SET(ARCH "x64")
	set(IS64 TRUE)
endif()

set(SIMVASCULAR_OS "${CMAKE_SYSTEM_NAME}")
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

set(CLUSTER "${ARCH}_${SIMVASCULAR_OS}")
if(SimVascular_DEV_OUTPUT)
	message(STATUS "${CLUSTER}")
endif()

set(COMPILER_VERSION ${CMAKE_CXX_COMPILER_ID})


SET(USER_HOME_DIR $ENV{HOME})
if(SimVascular_DEV_OUTPUT)
	message(STATUS "Home dir: ${USER_HOME_DIR}")
endif()

#-----
# System Macros
#
macro(env_variable_to_value_variable value_variable variable)
	if(WIN32 AND NOT UNIX)
		set(${value_variable} "%${variable}%")
	endif()
	if(UNIX)
		set(${value_variable} "$${variable}")
	endif()
endmacro()

function(append_env_string evn_var value output_variable)
	env_variable_to_value_variable(ENV_VALUE ${evn_var})
	set(${output_variable} "${ENV_SET_COMMAND} ${evn_var}=${ENV_VALUE}${ENV_SEPERATOR}${value}" PARENT_SCOPE)
endfunction()

function(set_env_string evn_var value output_variable)
	set(${output_variable} "${ENV_SET_COMMAND} ${evn_var}=${value}\n" PARENT_SCOPE)
endfunction()

macro(set_env_string_concat evn_var value output_variable)
	set_env_string(${evn_var} ${value} _tmp)
	set(${output_variable} "${${output_variable}}${_tmp}")
endmacro()

macro(append_env_string_concat evn_var value output_variable)
	append_env_string(${evn_var} ${value} _tmp)
	set(${output_variable} "${${output_variable}}${_tmp}\n")
endmacro()