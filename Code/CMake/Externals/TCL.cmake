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
# TCL
set(proj TCL)
# If using toplevel dir, foce TCL_DIR to be the SV_TCL_DIR set by the
# simvascular_add_new_external macro
if(SV_EXTERNALS_USE_TOPLEVEL_DIR)
  set(${proj}_DIR ${SV_${proj}_DIR} CACHE PATH "Force ${proj} dir to externals" FORCE)
endif()
# Find TCL
simvascular_external(${proj} SHARED_LIB ${SV_USE_${proj}_SHARED} VERSION ${${proj}_VERSION})
# Set SV_TCL_DIR to the directory that was found to contain TCL
set(SV_${proj}_DIR ${${proj}_DIR})

# Get found ${proj} version
get_filename_component(${proj}_LIBRARY_PATH "${${proj}_LIBRARY}" PATH)
link_directories(${${proj}_LIBRARY_PATH})
# TCL has two include directories, the macro only includes one.
include_directories(${${proj}_INCLUDE_PATH} ${TK_INCLUDE_PATH})
if(WIN32)
	get_filename_component(${proj}_DLL_PATH "${${proj}_TCLSH}" PATH)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Tkcximage (Legacy)
if(WIN32)
	if(SV_USE_TKCXIMAGE)
		find_library(TKCXIMAGE_DLL tkcximage)
		if(TKCXIMAGE_DLL)
			set(TKCXIMAGE_DLL_LIBRARY ${TKCXIMAGE_DLL})
			get_filename_component(TKCXIMAGE_DLL_PATH ${TKCXIMAGE_DLL} DIRECTORY CACHE)
			set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} "TKCXIMAGE")
		endif()
	endif()
endif()
#-----------------------------------------------------------------------------
