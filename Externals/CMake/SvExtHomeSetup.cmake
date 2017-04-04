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
# Set platforms directories
if(APPLE)
  set(SV_EXTERNALS_PLATFORM_DIR "mac_osx/10.10")
elseif(LINUX)
  set(SV_EXTERNALS_PLATFORM_DIR "linux/ubuntu_14")
elseif(WIN64)
  set(SV_EXTERNALS_PLATFORM_DIR "windows/10")
else()
  set(SV_EXTERNALS_PLATFORM_DIR "unsupported")
endif()
#-----------------------------------------------------------------------------
