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

# - Find MeshSim Libraries
#
# === Variables ===
#
#  MESHSIM_LIBRARIES, library search path
#  MESHSIM_INCLUDE_DIR, include search path
#  MESHSIM_FOUND, If false, do not try to use this library.
#  If a particular library is missing it will show up in the GUI and the user may set it.

set(proj MESHSIM)
include(FindPackageHandleStandardArgs)
include(GetPrerequisites)

#-----------------------------------------------------------------------------
# Set what we need to find
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Default Libraries
set(${proj}_LIBNAMES SimAdvMeshing SimMeshing SimMeshTools SimModel 
	SimMeshTools SimModel SimPartitionWrapper SimPartitionedMesh SimExport)

# Add requestion components
message(STATUS "Adding requested meshsim components: ${${proj}_FIND_COMPONENTS}")
set(${proj}_LIBNAMES ${${proj}_LIBNAMES} ${${proj}_FIND_COMPONENTS} ${${proj}_LIBNAMES})

#-----------------------------------------------------------------------------
# Header
set(${proj}_HEADER "MeshSim.h")

#-----------------------------------------------------------------------------
# Find Libraries
#-----------------------------------------------------------------------------
set(${proj}_POSSIBLE_PATHS ${${proj}_DIR})
if(${PROJECT_NAME}_EXTERNAL_DIR AND IS_DIRECTORY ${${PROJECT_NAME}_EXTERNAL_DIR})
	set(${proj}_POSSIBLE_PATHS ${${proj}_POSSIBLE_PATHS} "${${PROJECT_NAME}_EXTERNAL_DIR}/licensed/meshsim-8.0-131007/")
endif()

# Set paths to search for parasolid
if(LINUX)
	set(lib_sub_path "lib/x64_rhel5_gcc41")
elseif(APPLE)
	set(lib_sub_path "lib/x64_rhel5_gcc41")
elseif(WIN32 AND IS64)
	set(lib_sub_path "lib/x64_win_vc10")
elseif(WIN32 AND NOT IS64)
	set(lib_sub_path "lib/x86_win_vc10")
endif()

set(${proj}_POSSIBLE_LIB_PATHS )
foreach(p ${${proj}_POSSIBLE_PATHS})
	set(${proj}_POSSIBLE_LIB_PATHS ${${proj}_POSSIBLE_LIB_PATHS} "${p}/${lib_sub_path}" "${p}/lib")
endforeach()
set(${proj}_POSSIBLE_LIB_PATHS ${${proj}_POSSIBLE_LIB_PATHS} ${${proj}_LIB_DIR} )

if(WIN32) #add some windows specific dirs/ registry stuff here
set(${proj}_POSSIBLE_LIB_PATHS ${${proj}_POSSIBLE_LIB_PATHS}
	)
endif()

#message("${proj}_POSSIBLE_LIB_PATHS: ${${proj}_POSSIBLE_LIB_PATHS}")
set(${proj}_LIBS_MISSING ${${proj}_LIBNAMES})
list(REMOVE_DUPLICATES ${proj}_LIBS_MISSING)
set(${proj}_LIBRARIES_WORK "")
if(${proj}_POSSIBLE_LIB_PATHS)
	foreach(lib ${${proj}_LIBNAMES})
		#find library
		find_library(${proj}_${lib}_LIBRARY
			NAMES
			${lib}
			PATHS
			${${proj}_POSSIBLE_LIB_PATHS}
			${${proj}_DIR} ${${proj}_DIR}/lib
			DOC "Path to MeshSim Library ${lib}"
			NO_DEFAULT_PATH)
		find_library(${proj}_${lib}_LIBRARY
			NAMES
			${lib}
			PATHS
			${${proj}_POSSIBLE_LIB_PATHS}
			${${proj}_DIR} ${${proj}_DIR}/lib
			DOC "Path to MeshSim Library ${lib}")
		set(${proj}_LIB_FULLNAMES ${${proj}_LIB_FULLNAMES} ${proj}_${lib}_LIBRARY)
		mark_as_advanced(${proj}_${lib}_LIBRARY)

		if(${proj}_${lib}_LIBRARY)
			set(${proj}_LIBRARIES_WORK ${${proj}_LIBRARIES_WORK} "${${proj}_${lib}_LIBRARY}")
			list(REMOVE_ITEM ${proj}_LIBS_MISSING ${lib})
		else()
			#message("MeshSim Library ${lib}")
		endif()
	endforeach()
endif()
#message("${proj}_LIBS_MISSING ${${proj}_LIBS_MISSING}")


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
endif()




#-----------------------------------------------------------------------------
# Find Include Directory
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Setup search paths for header
set(${proj}_POSSIBLE_INCLUDE_PATHS )
foreach(p ${${proj}_POSSIBLE_PATHS})
	set(${proj}_POSSIBLE_INCLUDE_PATHS ${${proj}_POSSIBLE_INCLUDE_PATHS} "${p}/include")
endforeach()

set(${proj}_POSSIBLE_INCLUDE_PATHS ${${proj}_POSSIBLE_INCLUDE_PATHS} ${${proj}_EXTRA_PATHS}
	"${${proj}_LIB_DIR}/include")	

#-----------------------------------------------------------------------------
# Add windows Specific Search Paths
if(WIN32)
	set(${proj}_POSSIBLE_INCLUDE_PATHS ${${proj}_POSSIBLE_INCLUDE_PATHS}
		)
endif()


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
if(${proj}_INCLUDE_DIR)
	if(NOT ${proj}_INCLUDE_DIR MATCHES "^${${proj}_DIR}")
		message(WARNING "${proj}_INCLUDE_DIR and ${proj}_DIR mismatch!  This probably means you changes ${proj} directories. It is reccomend that you change the include directory the new instalation. Mismatched directories may cause strange behaviour.")
	endif()
	string(REGEX REPLACE
		"^(.*meshsim[-|][0-9|]\\.*[0-9].*[0-9|])/include" "\\1" ${proj}_DIR "${${proj}_INCLUDE_DIR}")
	string(REGEX REPLACE
		"^.*meshsim-([0-9]\\.*[0-9]).*[0-9]/include" "\\1" MESHSIM_VERSION_WORK "${${proj}_INCLUDE_DIR}")
else()
	set(${proj}_DIR "" CACHE PATH "Path to top level libraries.  Specify this if meshsim cannot be found.")
endif()
if(MESHSIM_VERSION_WORK MATCHES "^[0-9]\\.*[0-9]$")
	set(MESHSIM_VERSION ${MESHSIM_VERSION_WORK})
else()
	set(MESHSIM_VERSION "Unknown")
endif()

##
#message("${LIB_FULLNAMES}")
find_package_handle_standard_args(${proj}
	FOUND_VAR ${proj}_FOUND
	REQUIRED_VARS ${proj}_DIR ${proj}_INCLUDE_DIR ${proj}_LIBRARIES
	VERSION_VAR ${proj}_VERSION
	)
if(NOT ${proj}_FOUND)
	message(STATUS "${proj}: could not find ${${proj}_LIBS_MISSING}")
endif()
set(${proj}_LIBRARY ${${proj}_LIBRARIES})

