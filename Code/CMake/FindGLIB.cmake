# - Find glib Libraries
#
# === Variables ===
#
#  GLIB_LIBRARIES, library search path
#  GLIB_INCLUDE_DIR, include search path
#  GLIB_{component}_LIBRARY, the library to link against
#  GLIB_FOUND, If false, do not try to use this library.

set(proj GLIB)
include(FindPackageHandleStandardArgs)
include(GetPrerequisites)

# Libraries
set(${proj}_LIBNAMES glib-2.0)
if(APPLE)
	set(${proj}_LIBNAMES ${${proj}_LIBNAMES} iconv intl)
endif()

# Add requestion components
set(${proj}_LIBNAMES ${${proj}_LIBNAMES} ${${proj}_FIND_COMPONENTS})

#-----------------------------------------------------------------------------
# Header
set(${proj}_HEADER "glib.h" "glibconfig.h")

if(NOT ${proj}_PATH)
	if(LINUX)
		set(${proj}_PATH "/usr/lib/x86_64-linux-gnu")
	elseif(APPLE)
		set(${proj}_PATH "/opt/local/lib")
	elseif(WIN32 AND IS64)
		set(${proj}_PATH "${SimVascular_SV_EXTERN_OpenLibs_BIN_DIR}/glib-2.36.4")
	elseif(WIN32 AND NOT IS64)
		message(ERROR "GLIB not supported on 32-bit Cygwin")
	endif()
endif()
if(LINUX)
	set(lib_sub_path "")
elseif(APPLE)
	set(lib_sub_path "libs")
elseif(WIN32 AND IS64)
	set(lib_sub_path "lib")
elseif(WIN32 AND NOT IS64)
	message(ERROR "GLIB not supported on 32-bit Cygwin")
endif()

set(${proj}_POSSIBLE_LIB_PATHS ${${proj}_EXTRA_PATHS}
	"${${proj}_PATH}/${lib_sub_path}")

# add some more possible paths
set(${proj}_POSSIBLE_LIB_PATHS ${${proj}_LIB_DIR} ${${proj}_POSSIBLE_LIB_PATHS})

if(WIN32) #add some windows specific dirs/ registry stuff here
set(${proj}_POSSIBLE_LIB_PATHS ${${proj}_POSSIBLE_LIB_PATHS})

endif()

set(${proj}_LIBRARIES_WORK "")
foreach(lib ${${proj}_LIBNAMES})
	#find library
	IF(APPLE)
		set(LIBNAMES lib${lib}.a ${lib}.a ${lib})
	else()
		set(LIBNAMES ${lib})
	endif()
	find_library(${proj}_${lib}_LIBRARY
		NAMES
		${LIBNAMES}
		PATHS
		${${proj}_POSSIBLE_LIB_PATHS}
		NO_DEFAULT_PATH)
	find_library(${proj}_${lib}_LIBRARY
		NAMES
		${LIBNAMES}
		PATHS
		${${proj}_POSSIBLE_LIB_PATHS})
	mark_as_advanced(${proj}_${lib}_LIBRARY)
	set(${proj}_LIB_FULLNAMES ${${proj}_LIB_FULLNAMES} ${proj}_${lib}_LIBRARY)
	if(${proj}_${lib}_LIBRARY)
		set(${proj}_LIBRARIES_WORK ${${proj}_LIBRARIES_WORK} "${${proj}_${lib}_LIBRARY}")
	endif()
endforeach()

if(WIN32)
	find_file(GLIB_DLL_LIBRARY
		NAMES glib-2-vs10.dll
		PATHS
		${${proj}_POSSIBLE_LIB_PATHS}
		${${proj}_PATH}/bin
		)
	get_filename_component(GLIB_DLL_PATH ${GLIB_DLL_LIBRARY} PATH)
endif()

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
set(${proj}_POSSIBLE_INCLUDE_PATHS "")
if(LINUX)
	set(inc_sub_path "glib-2.0/include")
	set(${proj}_POSSIBLE_INCLUDE_PATHS ${${proj}_EXTRA_PATHS}
		"${${proj}_PATH}/${inc_sub_path}"
		"/usr/include/glib-2.0"
		)	
elseif(APPLE)
	set(inc_sub_path "glib-2.0/include")
	set(${proj}_POSSIBLE_INCLUDE_PATHS ${${proj}_EXTRA_PATHS}
		"${${proj}_PATH}/${inc_sub_path}"
		"/opt/local/include/glib-2.0"
		)	
elseif(WIN32 AND IS64)
	set(inc_sub_path "bin")
	set(${proj}_POSSIBLE_INCLUDE_PATHS ${${proj}_EXTRA_PATHS}
		"${${proj}_PATH}/${inc_sub_path}"
		"${${proj}_PATH}/include/glib-2.0"
		"${${proj}_PATH}/lib/glib-2.0/include"
		)	
elseif(WIN32 AND NOT IS64)
	message(ERROR "GLIB not supported on 32-bit Cygwin")
endif()		


#-----------------------------------------------------------------------------
# Add windows Specific Search Paths
if(WIN32)
	set(${proj}_POSSIBLE_INCLUDE_PATHS ${${proj}_POSSIBLE_INCLUDE_PATHS}
		)
endif()

#-----------------------------------------------------------------------------
# Search for headers
set(${proj}_INCLUDE_DIR "")
foreach(header ${${proj}_HEADER})
	find_path(${header}_path
		NAMES ${header}
		PATHS ${${proj}_POSSIBLE_INCLUDE_PATHS}
		)
	set(${proj}_INCLUDE_DIR ${${proj}_INCLUDE_DIR} ${${header}_path})
	unset(${header}_path CACHE)
endforeach()

#-----------------------------------------------------------------------------
# Handle Standard Args
find_package_handle_standard_args(${proj} DEFAULT_MSG 
	${proj}_LIBRARIES
	${${proj}_LIBRARIES_NAMES}
	${proj}_INCLUDE_DIR)
set(${proj}_LIBRARY ${${proj}_LIBRARIES})
