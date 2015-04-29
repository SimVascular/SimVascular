# === Variables ===
#
#  GTS_LIBRARIES, library search path
#  GTS_INCLUDE_DIR, include search path
#  GTS_{component}_LIBRARY, the library to link against
#  GTS_FOUND, If false, do not try to use this library.

set(proj GTS)
include(FindPackageHandleStandardArgs)
include(GetPrerequisites)

# Libraries
set(${proj}_LIBNAMES gts)

# Add requestion components
set(${proj}_LIBNAMES ${${proj}_LIBNAMES} ${${proj}_FIND_COMPONENTS})

#-----------------------------------------------------------------------------
# Header
set(${proj}_HEADER "gts.h")

set(${proj}_PATHS ${${proj}_LIB_DIR} ${${proj}_PATH})

if(LINUX)
	set(lib_sub_path "lib")
elseif(APPLE)
	set(lib_sub_path "lib")
elseif(WIN32 AND IS64)
	set(lib_sub_path "lib/")
	set(${proj}_PATH "${SimVascular_SV_EXTERN_LicensedLibs_BIN_DIR}/x64_win_vc10")
elseif(WIN32 AND NOT IS64)
	set(lib_sub_path "")
endif()

set(${proj}_POSSIBLE_LIB_PATHS ${${proj}_EXTRA_PATHS})
foreach(path ${${proj}_PATHS})
	set(${proj}_POSSIBLE_LIB_PATHS ${${proj}_POSSIBLE_LIB_PATHS}  ${path} "${path}/${lib_sub_path}")
endforeach()
# add some more possible paths
set(${proj}_POSSIBLE_LIB_PATHS ${${proj}_LIB_DIR} ${${proj}_POSSIBLE_LIB_PATHS})

if(WIN32) 
	#add some windows specific dirs/ registry stuff here
	set(${proj}_POSSIBLE_LIB_PATHS ${${proj}_POSSIBLE_LIB_PATHS})
endif()

set(${proj}_LIBRARIES_WORK "")
foreach(lib ${${proj}_LIBNAMES})
	#find library
	find_library(${proj}_${lib}_LIBRARY
		NAMES
		${lib} lib${lib}
		PATHS
		${${proj}_POSSIBLE_LIB_PATHS}
		NO_DEFAULT_PATH)
	find_library(${proj}_${lib}_LIBRARY
		NAMES
		${lib} lib${lib}
		PATHS
		${${proj}_POSSIBLE_LIB_PATHS})
	mark_as_advanced(${proj}_${lib}_LIBRARY)
	set(${proj}_LIB_FULLNAMES ${${proj}_LIB_FULLNAMES} ${proj}_${lib}_LIBRARY)
	if(${proj}_${lib}_LIBRARY)
		set(${proj}_LIBRARIES_WORK ${${proj}_LIBRARIES_WORK} "${${proj}_${lib}_LIBRARY}")
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

#-----------------------------------------------------------------------------
# Find Include Directory
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Setup search paths for header
if(LINUX)
	set(inc_sub_path "")
elseif(APPLE)
	set(inc_sub_path "libs")
elseif(WIN32 AND IS64)
	set(inc_sub_path "")
elseif(WIN32 AND NOT IS64)
	set(inc_sub_path "include")
endif()		

set(${proj}_POSSIBLE_INCLUDE_PATHS ${${proj}_EXTRA_PATHS}
	"${${proj}_PATH}/${inc_sub_path}"
	"/usr/local/include"
	)	

#-----------------------------------------------------------------------------
# Add windows Specific Search Paths
if(WIN32)
	set(${proj}_POSSIBLE_INCLUDE_PATHS ${${proj}_POSSIBLE_INCLUDE_PATHS}
		)
endif()

#-----------------------------------------------------------------------------
# Search for header
FIND_PATH(${proj}_INCLUDE_DIR
	NAMES ${${proj}_HEADER}
	PATHS ${${proj}_POSSIBLE_INCLUDE_PATHS}
	NO_DEFAULT_PATH
	)

FIND_PATH(${proj}_INCLUDE_DIR
	NAMES ${${proj}_HEADER}
	PATHS ${${proj}_POSSIBLE_INCLUDE_PATHS}
	)

#-----------------------------------------------------------------------------
# Handle Standard Args
find_package_handle_standard_args(${proj} DEFAULT_MSG 
	${proj}_LIBRARIES
	${${proj}_LIBRARIES_NAMES}
	${proj}_INCLUDE_DIR)
set(${proj}_LIBRARY ${${proj}_LIBRARIES})
