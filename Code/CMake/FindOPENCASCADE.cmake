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

# - Find OpenCascade Libraries
#
# === Variables ===
#
#  OPENCASCADE_LIBRARIES, library search path
#  OPENCASCADE_INCLUDE_DIR, include search path
#  OPENCASCADE_FOUND, If false, do not try to use this library.

set(proj OPENCASCADE)
include(FindPackageHandleStandardArgs)
include(GetPrerequisites)

#-----------------------------------------------------------------------------
# Set what we need to find
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Libraries
set(${proj}_LIBNAMES TKBRep TKHLR)
set(${proj}_FIND_COMPONENTS TKTopAlgo TKCAF TKLCAF TKXCAF TKCDF
			    TKPrim TKMath TKIVtk TKG3d TKG2d
		            TKGeomBase TKGeomAlgo TKV3d
			    TKMesh TKOffset TKBool TKBO TKShHealing TKernel
			    TKIGES TKMeshVS TKSTEP TKSTEP209
			    TKSTEPAttr TKVRML TKSTEPBase TKSTL
			    TKBin TKBinL TKBinTObj TKBinXCAF TKFeat TKFillet
			    TKService
			    TKXDEIGES TKXDESTEP TKXMesh
			    TKXSBase TKXml TKXmlL TKXmlTObj TKXmlXCAF
			    FWOSPlugin)
if (APPLE)
set(${proj}_FIND_COMPONENTS ${${proj}_FIND_COMPONENTS} TKOpenGL)
endif()

# Add requestion components
set(${proj}_LIBNAMES ${${proj}_LIBNAMES} ${${proj}_FIND_COMPONENTS})

#-----------------------------------------------------------------------------
# Header
set(${proj}_HEADER "gp_Pnt.hxx")

#-----------------------------------------------------------------------------
# Find Libraries
#-----------------------------------------------------------------------------
set(${proj}_POSSIBLE_PATHS ${${proj}_DIR} ${${proj}_DIR}/simvascular/opencascade)
if(${PROJECT_NAME}_EXTERNAL_DIR AND IS_DIRECTORY ${${PROJECT_NAME}_EXTERNAL_DIR})
	set(${proj}_PATH "${SimVascular_SV_EXTERN_LicensedLibs_BIN_DIR}/opencascade-6.9.1/")
endif()
# Set paths to search for OpenCascade
if(LINUX)
	set(sub_path "lin64/gcc")
elseif(APPLE)
	set(sub_path "mac64/clang")
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

# Set paths to search for OpenCascade
if(NOT SimVascular_USE_SYSTEM_${proj})
  if(${CMAKE_BUILD_TYPE} STREQUAL "RelWithDebInfo")
    set(${proj}_LIB_INSTALL_EXT "i")
  elseif(${CMAKE_BUILD_TYPE} STREQUAL "Debug")
    set(${proj}_LIB_INSTALL_EXT "d")
  else()
    set(${proj}_LIB_INSTALL_EXT "")
  endif()
else()
  set(${proj}_LIB_INSTALL_EXT "")
endif()

if(WIN32)
  set(${proj}_LIB_INSTALL_DIR "bin${${proj}_LIB_INSTALL_EXT}")
elseif(APPLE)
  message("WARNING: OpenCASCADE must be built and installed as Debug")
  set(${proj}_LIB_INSTALL_DIR "libd")
else()
  set(${proj}_LIB_INSTALL_DIR "lib${${proj}_LIB_INSTALL_EXT}")
endif()
mark_as_superbuild(${${proj}_LIB_INSTALL_DIR})

set(lib_sub_path ${${proj}_LIB_INSTALL_DIR})

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
endif()

#-----------------------------------------------------------------------------
# Find Include Directory
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Setup search paths for header	

set(${proj}_POSSIBLE_INCLUDE_PATHS ${${proj}_POSSIBLE_PATHS})
foreach(p ${${proj}_POSSIBLE_PATHS})
	set(${proj}_POSSIBLE_INCLUDE_PATHS ${${proj}_POSSIBLE_INCLUDE_PATHS} "${p}/inc")
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
	${${proj}_DIR} ${${proj}_DIR}/inc
	NO_DEFAULT_PATH
	)

FIND_PATH(${proj}_INCLUDE_DIR
	NAMES ${${proj}_HEADER}
	PATHS ${${proj}_POSSIBLE_INCLUDE_PATHS}
	${${proj}_DIR} ${${proj}_DIR}/inc
	)

set(${proj}_DIR "" CACHE PATH "Path to top level libraries.  Specify this if ${proj} cannot be found.")
#-----------------------------------------------------------------------------
# Handle Standard Args
find_package_handle_standard_args(${proj} 
	FOUND_VAR ${proj}_FOUND
	REQUIRED_VARS ${proj}_DIR ${proj}_INCLUDE_DIR ${proj}_LIBRARIES
	VERSION_VAR ${proj}_VERSION
	FAIL_MESSAGE "Could NOT find ${proj} missing component: ${${proj}_LIBS_MISSING} causing:")
set(${proj}_LIBRARY ${${proj}_LIBRARIES})
