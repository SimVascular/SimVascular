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

set(proj GLIB)
set(${proj}_NEEDED_LIBS glib-2.0)
if(SimVascular_USE_SYSTEM_${proj})
	if(LINUX)		
		#set(GLIB_FULL_PATH "${OpenLibs_Bin_Directory}/${GLIB_PATH_PREFIX}")
		message(ERROR "GLIB not support on Linux Systems")
	endif()
	if(LINUX OR APPLE)
		set(${proj}_PATH_PREFIX "glib-2.0" CACHE TYPE PATH)
		set(${proj}_FULL_PATH "/opt/local/include/${${proj}_PATH_PREFIX}")
		set(${proj}_LIB_DIR "${SimVascular_SV_EXTERN_OpenLibs_BIN_DIR}/libs")
		set(${proj}_DLL_DIR "/opt/local/bin")
		set(${proj}_INCLUDE_DIR "${${proj}_FULL_PATH};/opt/local/lib/${${proj}_PATH_PREFIX}/include;${${proj}_FULL_PATH}/glib;${${proj}_FULL_PATH}/gio;${${proj}_FULL_PATH}/gobject;")
	endif()

	if(CYGWIN AND IS64)
		set(${proj}_PATH_PREFIX "glib-2.36.4" CACHE TYPE PATH)
		set(${proj}_FULL_PATH "${OpenLibs_Bin_Directory}/${${proj}_PATH_PREFIX}")
		set(${proj}_LIB_DIR "${${proj}_FULL_PATH}/lib")
		set(${proj}_DLL_DIR "${${proj}_FULL_PATH}/bin")
		set(${proj}_INCLUDE_DIR "${${proj}_FULL_PATH}/include/glib-2.0;${${proj}_FULL_PATH}/lib/glib-2.0/include;")
	endif()

	if(CYGWIN AND NOT IS64)
		message(ERROR "GLIB not supported on 32-bit Cygwin")
	endif()
else()
	message(STATUS "${proj} Superbuild!")
endif()

GENLIBS(${proj}_LIBRARY "${${proj}_NEEDED_LIBS}" "${proj}" "${${proj}_LIB_DIR}")

link_directories(${${proj}_LIB_DIR})
include_directories(${${proj}_INCLUDE_DIR})
