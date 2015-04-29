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

set(proj TETGEN)
set(${proj}_NEEDED_LIBS tet)
if(SimVascular_USE_SYSTEM_${proj})

	  #The two different available versions of tetgen
	  set(TETGEN150 false)
	  set(TETGEN143 false)
	
	  #Options are 1.5.0 or 1.4.3
	  if(${TETGEN_VERSION} STREQUAL "1.5.0")
	  	set(TETGEN150 true)
	 elseif(${TETGEN_VERSION} STREQUAL "1.4.3")
		set(TETGEN143 true)
	endif()
	
	if(TETGEN150)
		set(${proj}_PATH_PREFIX "tetgen-1.5.0" CACHE TYPE PATH)
	elseif(TETGEN143)
		set(${proj}_PATH_PREFIX "tetgen-1.4.3" CACHE TYPE PATH)
	endif()
	
	if(APPLE)
		set(${proj}_FULL_PATH "${OpenLibs_Bin_Directory}/${${proj}_PATH_PREFIX}")
		set(${proj}_LIB_DIR "${${proj}_FULL_PATH}")
		set(${proj}_INCLUDE_DIR "${${proj}_FULL_PATH}")
	endif()
	
	if(LINUX)
		set(${proj}_FULL_PATH "${OpenLibs_Bin_Directory}/${${proj}_PATH_PREFIX}")
		set(${proj}_LIB_DIR "${${proj}_FULL_PATH}")
		set(${proj}_INCLUDE_DIR "${${proj}_FULL_PATH}")
	endif()
	
	if(WIN32 OR (CYGWIN AND NOT IS64))
		set(${proj}_FULL_PATH "${OpenLibs_Bin_Directory}/${${proj}_PATH_PREFIX}")
		set(${proj}_LIB_DIR "${${proj}_FULL_PATH}" CACHE TYPE PATH)
		set(${proj}_INCLUDE_DIR "${${proj}_FULL_PATH}" CACHE TYPE PATH)
	endif()
	
	if(CYGWIN AND IS64)
		set(${proj}_FULL_PATH "${OpenLibs_Bin_Directory}/${${proj}_PATH_PREFIX}")
		set(${proj}_LIB_DIR "${${proj}_FULL_PATH}" CACHE TYPE PATH)
		set(${proj}_INCLUDE_DIR "${${proj}_FULL_PATH}" CACHE TYPE PATH)
	endif()
	
else()
	message(STATUS "${proj} Superbuild!")
endif()

GENLIBS(${proj}_LIBRARY "${${proj}_NEEDED_LIBS}" "${proj}" "${${proj}_LIB_DIR}")

link_directories(${${proj}_LIB_DIR})
include_directories(${${proj}_INCLUDE_DIR})

if(TETGEN150)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DTETGEN150 -DUSE_TETGEN")
elseif(TETGEN143)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DTETGEN143 -DUSE_TETGEN")
endif()

