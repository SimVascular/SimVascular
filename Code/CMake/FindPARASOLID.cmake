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

# - Find Parasolid Libraries
#
# === Variables ===
#
#  PARASOLID_LIBRARIES, library search path
#  PARASOLID_INCLUDE_DIR, include search path
#  PARASOLID_FOUND, If false, do not try to use this library.

set(proj PARASOLID)
include(FindPackageHandleStandardArgs)
include(GetPrerequisites)

#-----------------------------------------------------------------------------
# Set what we need to find
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Libraries
set(${proj}_LIBNAMES pskernel)

# Add requestion components
set(${proj}_LIBNAMES ${${proj}_LIBNAMES} ${${proj}_FIND_COMPONENTS})

#-----------------------------------------------------------------------------
# Header
set(${proj}_HEADER "parasolid_kernel.h")

#-----------------------------------------------------------------------------
# Find Libraries
#-----------------------------------------------------------------------------
set(${proj}_POSSIBLE_PATHS ${${proj}_DIR} ${${proj}_DIR}/parasolid)
if(${PROJECT_NAME}_EXTERNAL_DIR AND IS_DIRECTORY ${${PROJECT_NAME}_EXTERNAL_DIR})
	set(${proj}_PATH "${SimVascular_SV_EXTERN_LicensedLibs_BIN_DIR}/parasolid-24.0/")
endif()
# Set paths to search for parasolid
if(LINUX)
	set(sub_path "intel_linux/base")
elseif(APPLE)
	set(sub_path "intel_macos/base")
elseif(WIN32 AND IS64)
	set(sub_path "x64_win/base")
elseif(WIN32 AND NOT IS64)
	set(sub_path "intel_nt/base")
endif()

foreach(p ${${proj}_POSSIBLE_PATHS})
	set(${proj}_POSSIBLE_PATHS ${${proj}_POSSIBLE_PATHS} 
		"${p}/${sub_path}")
endforeach()
#message("${proj}_POSSIBLE_PATHS: ${${proj}_POSSIBLE_PATHS}")

# Set paths to search for parasolid
if(LINUX)
	set(lib_sub_path "shared_object")
elseif(APPLE)
	set(lib_sub_path "shared_object")
elseif(WIN32 AND IS64)
	set(lib_sub_path "dll")
elseif(WIN32 AND NOT IS64)
	set(lib_sub_path "dll")
endif()


set(${proj}_POSSIBLE_LIB_PATHS )
foreach(p ${${proj}_POSSIBLE_PATHS})
	set(${proj}_POSSIBLE_LIB_PATHS ${${proj}_POSSIBLE_LIB_PATHS} 
		"${p}/${lib_sub_path}")
endforeach()
set(${proj}_POSSIBLE_LIB_PATHS ${${proj}_POSSIBLE_LIB_PATHS} ${${proj}_LIB_DIR} )

# add some more possible paths
set(${proj}_POSSIBLE_LIB_PATHS ${${proj}_POSSIBLE_LIB_PATHS}
	)
if(WIN32) #add some windows specific dirs/ registry stuff here
set(${proj}_POSSIBLE_LIB_PATHS ${${proj}_POSSIBLE_LIB_PATHS}
	)
endif()

#message("${proj}_POSSIBLE_LIB_PATHS: ${${proj}_POSSIBLE_LIB_PATHS}")
set(${proj}_LIBS_MISSING ${${proj}_LIBNAMES})
list(REMOVE_DUPLICATES ${proj}_LIBS_MISSING)
set(${proj}_LIBRARIES_WORK "")
foreach(lib ${${proj}_LIBNAMES})
	#find library
	find_library(${proj}_${lib}_LIBRARY
		NAMES
		${lib}
		PATHS
		${${proj}_POSSIBLE_LIB_PATHS}
		${${proj}_DIR} ${${proj}_DIR}/shared_object ${${proj}_DIR}/dll
		NO_DEFAULT_PATH)
	find_library(${proj}_${lib}_LIBRARY
		NAMES
		${lib}
		PATHS
		${${proj}_POSSIBLE_LIB_PATHS}
		${${proj}_DIR} ${${proj}_DIR}/shared_object ${${proj}_DIR}/dll)
	set(${proj}_LIB_FULLNAMES ${${proj}_LIB_FULLNAMES} ${proj}_${lib}_LIBRARY)
	mark_as_advanced(${proj}_${lib}_LIBRARY)
	if(${proj}_${lib}_LIBRARY)
		set(${proj}_LIBRARIES_WORK ${${proj}_LIBRARIES_WORK} "${${proj}_${lib}_LIBRARY}")
		list(REMOVE_ITEM ${proj}_LIBS_MISSING ${lib})
	endif()
endforeach()

#message("${proj}_LIBRARIES_WORK: ${${proj}_LIBRARIES_WORK}")

list(LENGTH ${proj}_LIBRARIES_WORK ${proj}_NUMLIBS)
list(LENGTH ${proj}_LIBNAMES ${proj}_NUMLIBS_EXPECTED)
#message("${${proj}_NUMLIBS} ${${proj}_NUMLIBS_EXPECTED}")
if (NOT ${proj}_NUMLIBS EQUAL ${proj}_NUMLIBS_EXPECTED)
	set(${proj}_LIBRARIES_WORK "${proj}_LIBRARIES-NOTFOUND")
endif()

set(${proj}_LIBRARIES  ${${proj}_LIBRARIES_WORK} CACHE STRING 
	"${proj} libraries to link against" FORCE)

# Clean up.  If all libraries were found remove cache entries.
if(${proj}_LIBRARIES)
	foreach(lib ${${proj}_LIBNAMES})
		unset(${proj}_${lib}_LIBRARY CACHE)
	endforeach()
	if(${proj}_NUMLIBS_EXPECTED EQUAL 1)
		set(temp_path ${${proj}_LIBRARIES})
	else()
		list(GET ${proj}_LIBRARIES 1 temp_path)
	endif()
	get_filename_component(PARASOLID_DLL_PATH ${temp_path} PATH)
	mark_as_superbuild(PARASOLID_DLL_PATH:PATH)
endif()

#-----------------------------------------------------------------------------
# Find Include Directory
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Setup search paths for header	

set(${proj}_POSSIBLE_INCLUDE_PATHS ${${proj}_POSSIBLE_PATHS})
foreach(p ${${proj}_POSSIBLE_PATHS})
	set(${proj}_POSSIBLE_INCLUDE_PATHS ${${proj}_POSSIBLE_INCLUDE_PATHS} "${p}/base")
endforeach()
if(${proj}_LIB_DIR)
	set(${proj}_POSSIBLE_INCLUDE_PATHS ${${proj}_POSSIBLE_INCLUDE_PATHS} ${${proj}_EXTRA_PATHS} ${${proj}_LIB_DIR}
		"${${proj}_LIB_DIR}/${inc_sub_path}")
endif()
#-----------------------------------------------------------------------------
# Add windows Specific Search Paths
if(WIN32)
	set(${proj}_POSSIBLE_INCLUDE_PATHS ${${proj}_POSSIBLE_INCLUDE_PATHS}
		)
endif()

#-----------------------------------------------------------------------------
# Search for header
#message("${proj}_POSSIBLE_INCLUDE_PATHS :${${proj}_POSSIBLE_INCLUDE_PATHS}")
FIND_PATH(${proj}_INCLUDE_DIR
	NAMES ${${proj}_HEADER}
	PATHS ${${proj}_POSSIBLE_INCLUDE_PATHS}
	${${proj}_DIR} ${${proj}_DIR}/include
	NO_DEFAULT_PATH
	)

FIND_PATH(${proj}_INCLUDE_DIR
	NAMES ${${proj}_HEADER}
	PATHS ${${proj}_POSSIBLE_INCLUDE_PATHS}
	${${proj}_DIR} ${${proj}_DIR}/include
	)

#-----
# Find Schema 
find_path(${proj}_SCHEMA_DIR
	NAMES sch_0_15.sch_txt sch_13006.sch_txt sch_18007.sch_txt
	PATHS ${${proj}_INCLUDE_DIR}/schema
	)


if(${proj}_INCLUDE_DIR)
	if(NOT ${proj}_INCLUDE_DIR MATCHES "^${${proj}_DIR}")
		message(WARNING "${proj}_INCLUDE_DIR and ${proj}_DIR mismatch!  This probably means you changes ${proj} directories. It is reccomend that you change the include directory the new instalation. Mismatched directories may cause strange behaviour.")
	endif()
	if(NOT ${proj}_SCHEMA_DIR MATCHES "^${${proj}_DIR}")
		message(WARNING "${proj}_SCHEMA_DIR and ${proj}_DIR mismatch!  This probably means you changes ${proj} directories. It is reccomend that you change the schema directory the new instalation. Mismatched directories may cause strange behaviour.")
	endif()
	if(NOT ${proj}_DLL_PATH MATCHES "^${${proj}_DIR}")
		message(WARNING "${proj}_DLL_PATH and ${proj}_DIR mismatch!  This probably means you changes ${proj} directories. It is reccomend that you change the dll path the new instalation. Mismatched directories may cause strange behaviour.")
	endif()
	string(REGEX REPLACE
		"^(.*parasolid[-|][0-9|]\\.*[0-9].*[0-9|])/*$" "\\1" ${proj}_DIR "${${proj}_INCLUDE_DIR}")
	string(REGEX REPLACE
		"^.*parasolid-([0-9]\\.*[0-9]).*[0-9]/*$" "\\1" ${proj}_VERSION_WORK "${${proj}_INCLUDE_DIR}")
	if(${proj}_VERSION_WORK MATCHES "^[0-9]\\.*[0-9]$")
		set(${proj}_VERSION ${${proj}_VERSION_WORK})
	else()
		set(${proj}_VERSION "Unknown")
	endif()
else()
	set(${proj}_DIR "" CACHE PATH "Path to top level libraries.  Specify this if ${proj} cannot be found.")
endif()

#-----------------------------------------------------------------------------
# Handle Standard Args
find_package_handle_standard_args(${proj} 
	FOUND_VAR ${proj}_FOUND
	REQUIRED_VARS ${proj}_DIR ${proj}_INCLUDE_DIR ${proj}_LIBRARIES ${proj}_DLL_PATH
	VERSION_VAR ${proj}_VERSION
	FAIL_MESSAGE "Could NOT find ${proj} missing component: ${${proj}_LIBS_MISSING} causing:")
set(${proj}_LIBRARY ${${proj}_LIBRARIES})