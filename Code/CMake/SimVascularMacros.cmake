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

include (CMakeParseArguments)

macro(dev_message string)
	if(SV_DEVELOPER_OUTPUT)
		message("DEV: ${string}")
	endif()
endmacro()
MACRO(LIST_CONTAINS var value)
	SET(${var})
	FOREACH (value2 ${ARGN})
		IF (${value} STREQUAL ${value2})
			SET(${var} TRUE)
		ENDIF (${value} STREQUAL ${value2})
	ENDFOREACH (value2)
ENDMACRO(LIST_CONTAINS)

MACRO(glob_dirs _newlist )
	set(${_newlist})
	foreach(value ${ARGN})
		if(IS_DIRECTORY ${value})
			list(APPEND ${_newlist} ${value})
		endif()
	endforeach()
endmacro()

MACRO(combine_files output_file)
	set(ACCUM_SCRIPT_STRING)
	set(TEMP_SCRIPT)
	foreach(file ${ARGN})
		dev_message("    Processing file: ${file}")
		file(READ ${file} TEMP_SCRIPT)
		set(ACCUM_SCRIPT_STRING "${ACCUM_SCRIPT_STRING}\n${TEMP_SCRIPT}")
	endforeach()
	set(${output_file} ${ACCUM_SCRIPT_STRING})
endmacro()

#-----------------------------------------------------------------------------
# simvascular_external - Macro to find libraries needed by simvascular 
# and create the necessary variables to load and link them.
# 
macro(simvascular_external _pkg)
	string(TOLOWER "${_pkg}" _lower)

	dev_message("Configuring ${_pkg}")

	set(options OPTIONAL VERSION_EXACT 
		DOWNLOADABLE SYSTEM_DEFAULT 
		SVEXTERN_CONFIG ADD_INSTALL SHARED_LIB NO_MODULE
		) 
	set(oneValueArgs VERSION)
	set(multiValueArgs PATHS HINTS COMPONENTS)

	CMAKE_PARSE_ARGUMENTS("simvascular_external" 
		"${options}"
		"${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

	set(EXTRA_ARGS)
	if(simvascular_external_COMPONENTS)
		set(EXTRA_ARGS COMPONENTS ${simvascular_external_COMPONENTS})
	endif()
	if(simvascular_external_NO_MODULE)
		set(EXTRA_ARGS ${EXTRA_ARGS} NO_MODULE)
	endif()
	#message("EXTRA_ARGS: ${EXTRA_ARGS}")
	set(${_pkg}_VERSION ${simvascular_external_VERSION})
	if(simvascular_external_VERSION_EXACT)
		set(${_pkg}_VERSION ${${_pkg}_VERSION} EXACT)
	endif()

	unset(ARG_STRING)
	set(_paths "${simvascular_external_PATHS}")
	if(NOT simvascular_external_PATHS)
		set(_paths "${CMAKE_MODULE_PATH}")
	endif()

	#message(STATUS "Search paths for ${_pkg}Config.cmake: ${_paths}")

	if(simvascular_external_SYSTEM_DEFAULT)
		option(SV_USE_SYSTEM_${_pkg} "Use system ${_pkg}" ON)
		mark_as_advanced(SV_USE_SYSTEM_${_pkg})
	else()
		option(SV_USE_SYSTEM_${_pkg} "Use system ${_pkg}" OFF)
	endif()

	mark_as_superbuild(SV_USE_SYSTEM_${_pkg})
	#message("${_pkg}: ${simvascular_external_SVEXTERN_CONFIG}")

	# Hack to make sure ITK does not change location of vtk and gdcm
	if("${_pkg}" STREQUAL "ITK")
	  if(VTK_DIR)
	    set(VTK_TMP_DIR ${VTK_DIR})
	  endif()
	  if(GDCM_DIR)
	    set(GDCM_TMP_DIR ${GDCM_DIR})
	  endif()
	endif()
	if((NOT SV_SUPERBUILD AND simvascular_external_SVEXTERN_CONFIG) OR 
		(simvascular_external_SVEXTERN_CONFIG AND SV_USE_SYSTEM_${_pkg}))

	  simvascular_find_package(${_pkg} ${EXTRA_ARGS} 
		  PATHS ${CMAKE_CURRENT_SOURCE_DIR}/CMake 
		  NO_CMAKE_MODULE_PATH
		  NO_DEFAULT_PATH)
	elseif(NOT SV_SUPERBUILD)
		#message(" ${_pkg} NOT SV_SUPERBUILD AND NOT simvascular_external_SVEXTERN_CONFIG")
		simvascular_find_package(${_pkg} ${EXTRA_ARGS})
	elseif(SV_USE_SYSTEM_${_pkg})
		simvascular_find_package(${_pkg} ${EXTRA_ARGS})
	endif()

	# Hack to make sure ITK does not change location of vtk and gdcm
	if("${_pkg}" STREQUAL "ITK")
	  if(VTK_DIR)
	    set(VTK_DIR ${VTK_TMP_DIR})
	  endif()
	  if(GDCM_DIR)
	    set(GDCM_DIR ${GDCM_TMP_DIR})
	  endif()
	endif()

	if(simvascular_external_DOWNLOADABLE)
		set(SV_DEPENDS ${SV_DEPENDS} ${_pkg})
		list( REMOVE_DUPLICATES SV_DEPENDS )
	endif()

	if(SV_USE_${_pkg})
		set(USE_${_pkg} ON)
	endif()

	if(simvascular_external_SHARED_LIB)
		set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} ${_pkg})
	endif()

	if(${_pkg}_FOUND)
	        message(STATUS "PKG ${_pkg} found!")
		if( ${_pkg}_INCLUDE_DIR )
			dev_message("Including dir: ${${_pkg}_INCLUDE_DIR}")
			# This get many of them
			include_directories(${${_pkg}_INCLUDE_DIR})
		endif()
		if(SV_INSTALL_EXTERNALS)
			if(simvascular_external_ADD_INSTALL)
				getListOfVars("${_pkg}" "LIBRARY" ${_pkg}_VARS_INSTALL)
				# print_vars(${_pkg}_VARS_INSTALL)
				foreach(lib_install ${${_pkg}_VARS_INSTALL})
					list(APPEND ${_pkg}_LIBRARY_INSTALL "${${lib_install}}")
				endforeach()
				#list(REMOVE_DUPLICATES ${_pkg}_LIBRARY_INSTALL)
				#message(STATUS "${_pkg}_LIBRARY_INSTALL: ${${_pkg}_LIBRARY_INSTALL}")
				#install(FILES "${${_pkg}_LIBRARY_INSTALL}" DESTINATION ${SV_INSTALL_EXTERNAL_LIBRARY_DIR})
			endif()
		endif()
	endif()
	unset(simvascular_external_SVEXTERN_CONFIG)
	unset(simvascular_external_ADD_INSTALL)
	if(SV_DEVELOPER_OUTPUT)
		message(STATUS "Finished Configuring ${_pkg}")
		message(STATUS "")
	endif()
endmacro()
#-----------------------------------------------------------------------------
# unset_simvascular_external
#
macro(unset_simvascular_external _pkg)
	string(TOLOWER "${_pkg}" _lower)

	set(options OPTIONAL VERSION_EXACT DOWNLOADABLE SVEXTERN_DEFAULT SVEXTERN_CONFIG)
	set(oneValueArgs VERSION)
	set(multiValueArgs PATHS HINTS)

	CMAKE_PARSE_ARGUMENTS("simvascular_external" 
		"${options}"
		"${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

	unset(SV_USE_SYSTEM_${_pkg})
	list(REMOVE_ITEM SV_DEPENDS ${_pkg})
endmacro()

#-----------------------------------------------------------------------------
# simvascular_third_party
#
macro(simvascular_third_party _pkg)
	string(TOLOWER "${_pkg}" _lower)
	string(TOUPPER "${_pkg}" _upper)

	set(options OPTIONAL VERSION_EXACT 
		DOWNLOADABLE SYSTEM_DEFAULT 
		SVEXTERN_CONFIG ADD_INSTALL
		)
	set(oneValueArgs VERSION)
	set(multiValueArgs PATHS HINTS COMPONENTS)
	
	CMAKE_PARSE_ARGUMENTS("simvascular_third_party" 
		"${options}"
		"${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
	set(${_upper}_SUBDIR ThirdParty/${_pkg})
	if(simvascular_third_party_SYSTEM_DEFAULT)
		option(SV_USE_SYSTEM_${_upper} "Use system ${_pkg}" ON)
	else()
		option(SV_USE_SYSTEM_${_upper} "Use system ${_pkg}" OFF)
	endif()

	mark_as_advanced(SV_USE_SYSTEM_${_upper})
	mark_as_superbuild(SV_USE_SYSTEM_${_upper})

	configure_file(${SV_SOURCE_DIR}/${${_upper}_SUBDIR}/simvascular_${_lower}.h.in 
		${SV_BINARY_DIR}/${${_upper}_SUBDIR}/simvascular_${_lower}.h)
	include_directories(BEFORE ${SV_BINARY_DIR}/${${_upper}_SUBDIR} ${SV_SOURCE_DIR}/${${_upper}_SUBDIR})
	if(SV_USE_SYSTEM_${_upper})
		set(${_upper}_LIBRARIES)
		set(${_upper}_LIBRARY)
	else()
		if(NOT SV_SUPERBUILD)
			set(${_upper}_LIBRARY_NAME lib_simvascular_${_lower})
			add_subdirectory(${${_upper}_SUBDIR}/simvascular_${_lower})
		endif()
	endif()
endmacro()
#-----------------------------------------------------------------------------
# print_vars - THis is a simple marco to print out a list of variables
# with their names and value, used mostly for debugging
#
macro(print_vars _VARLIST)
	foreach(var ${${_VARLIST}})
		message(STATUS "${var}: ${${var}}")
	endforeach()
	message(STATUS "")
endmacro()

macro(dev_print_vars _VARLIST)
	foreach(var ${${_VARLIST}})
		dev_message("${var}: ${${var}}")
	endforeach()
	message(STATUS "")
endmacro()


#-----------------------------------------------------------------------------
# listvars2vars - THis is a simple marco to print out a list of variables
# with their names and value, used mostly for debugging
#
macro(listvars2vals _VARLIST _VALS)
	foreach(var ${${_VARLIST}})
		set(_vals ${_vals} ${${var}})
	endforeach()
	list(REMOVE_DUPLICATES _vals)
	set(${_VALS} ${_vals})
	unset(_vals)
endmacro()

#-----------------------------------------------------------------------------
# create_GUID -
#
macro(create_GUID GUID)
	file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/temp "")
	file(TIMESTAMP ${CMAKE_CURRENT_BINARY_DIR}/temp TIME %S%M%H%j%y UTC)
	file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/temp "${TIME}")
	file(MD5 ${CMAKE_CURRENT_BINARY_DIR}/temp TIME)
	string(SUBSTRING ${TIME} 0 6 ${GUID})
	file(REMOVE ${CMAKE_CURRENT_BINARY_DIR}/temp)
endmacro()

#-----------------------------------------------------------------------------
# getListOfVars -
#
function (getListOfVars _prefix _suffix _varResult)
	get_cmake_property(_vars VARIABLES)
	string (REGEX MATCHALL "(^|;)${_prefix}[A-Za-z0-9_]*${_suffix}" _matchedVars "${_vars}")
	set (${_varResult} ${_matchedVars} PARENT_SCOPE)
endfunction()

function (getListOfVars_concat _prefix _suffix _varResult)
	get_cmake_property(_vars VARIABLES)
	string (REGEX MATCHALL "(^|;)${_prefix}[A-Za-z0-9_]*${_suffix}" _matchedVars "${_vars}")
	set (${_varResult} ${${_varResult}} ${_matchedVars} PARENT_SCOPE)
endfunction()

function (getListOfVarsPrefix _prefix _varResult)
	get_cmake_property(_vars VARIABLES)
	string (REGEX MATCHALL "(^|;)${_prefix}[A-Za-z0-9_]*" _matchedVars "${_vars}")
	set (${_varResult} ${_matchedVars} PARENT_SCOPE)
endfunction()

function(getListofVarsCat _varResult)
	set(options ) 
	set(oneValueArgs)
	set(multiValueArgs SUFFIXES PREFIXES)
	CMAKE_PARSE_ARGUMENTS("" 
		"${options}"
		"${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
	set(_ACCUM_VARLIST)
	foreach(pre ${_PREFIXES})
		foreach(suf ${_SUFFIXES})
			getListOfVars("${pre}" "${suf}" _VARLIST)
			set(_ACCUM_VARLIST ${_ACCUM_VARLIST} ${_VARLIST})
		endforeach()
	endforeach()
	set(_varResult ${_ACCUM_VARLIST} PARENT_SCOPE)
endfunction()

#-----------------------------------------------------------------------------
# check_library_exists_concat -
#
MACRO (check_library_exists_concat LIBRARY SYMBOL VARIABLE)
	check_library_exists ("${LIBRARY};${LINK_LIBS}" ${SYMBOL} "" ${VARIABLE})
	IF (${VARIABLE})
		SET (LINK_LIBS ${LINK_LIBS} ${LIBRARY})
	ENDIF (${VARIABLE})
ENDMACRO ()
#-----------------------------------------------------------------------------
# simvascular_add_executable -
#
macro(simvascular_add_executable TARGET_NAME)
	set(options NO_SCRIPT) 
	set(oneValueArgs DEV_SCRIPT_NAME INSTALL_SCRIPT_NAME COMPONENT INSTALL_DESTINATION)
	set(multiValueArgs SRCS)
	
	unset(simvascular_add_executable_INSTALL_SCRIPT_NAME)
	unset(simvascular_add_executable_DEV_SCRIPT_NAME)
	unset(simvascular_add_executable_NO_SCRIPT)

	CMAKE_PARSE_ARGUMENTS("simvascular_add_executable" 
		"${options}"
		"${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
	add_executable(${TARGET_NAME} ${simvascular_add_executable_SRCS})

	if(simvascular_add_executable_NO_SCRIPT)
		if(	simvascular_add_executable_DEV_SCRIPT_NAME OR simvascular_add_executable_INSTALL_SCRIPT_NAME )
			message(ERROR "Cannot specify no script and specify script names!")
		endif()
		set(${TARGET_NAME}_EXECUTABLE_NAME ${TARGET_NAME} CACHE INTERNAL "" FORCE)
	endif()
	if(NOT simvascular_add_executable_NO_SCRIPT)
		IF(simvascular_add_executable_DEV_SCRIPT_NAME)
			set(SV_SCRIPT_TARGETS_WORK ${SV_SCRIPT_TARGETS})
			list(APPEND SV_SCRIPT_TARGETS_WORK "${TARGET_NAME}")
			list(REMOVE_DUPLICATES SV_SCRIPT_TARGETS_WORK)
			set(SV_SCRIPT_TARGETS ${SV_SCRIPT_TARGETS_WORK} CACHE INTERNAL "" FORCE)
			set(${TARGET_NAME}_DEVELOPER_SCRIPT_NAME ${simvascular_add_executable_DEV_SCRIPT_NAME} CACHE INTERNAL "" FORCE)
			set(${TARGET_NAME}_EXECUTABLE_NAME ${${TARGET_NAME}_DEVELOPER_SCRIPT_NAME} CACHE INTERNAL "" FORCE)
		endif()
		if(simvascular_add_executable_INSTALL_SCRIPT_NAME)
			set(${TARGET_NAME}_INSTALL_SCRIPT_NAME ${simvascular_add_executable_INSTALL_SCRIPT_NAME} CACHE INTERNAL "" FORCE)
		endif()

	endif()
	# CHANGE FOR EXECUTABLE RENAME REMOVE (re enable if statement)
	if(simvascular_add_executable_INSTALL_DESTINATION)
		if(simvascular_add_executable_COMPONENT)
			set(_COMPARGS "COMPONENT ${simvascular_add_executable_COMPONENT}")
		endif()
		install(TARGETS ${TARGET_NAME}
			RUNTIME DESTINATION ${simvascular_add_executable_INSTALL_DESTINATION}
			${_COMPARGS})
	endif()

endmacro()

function(sv_list_match resultVar str)
  set(result)
  foreach(ITR ${ARGN})
  dev_message("${str} ITR: ${ITR}")
    if(ITR MATCHES "${str}")
      list(APPEND result ${ITR})
    endif()
  endforeach()
  set(${resultVar} ${result} PARENT_SCOPE)
endfunction()

function(simvascular_dir_switch DIR_ONE DIR_TWO)
  set(TMP_DIR ${${DIR_ONE}})
  set(${DIR_ONE} ${${DIR_TWO}} PARENT_SCOPE)
  set(${DIR_TWO} ${TMP_DIR} PARENT_SCOPE)
endfunction()

macro(simvascular_find_package pkg)
  if(${pkg}_CONFIG_DIR)
    simvascular_dir_switch(${pkg}_DIR ${pkg}_CONFIG_DIR)
  endif()

  find_package(${pkg} ${ARGN})

  if(${pkg}_CONFIG_DIR)
    simvascular_dir_switch(${pkg}_DIR ${pkg}_CONFIG_DIR)
  endif()
endmacro()

macro(simvascular_find_config_file pkg)
if(NOT "${${pkg}_DIR}" STREQUAL "")
  file(GLOB_RECURSE ${pkg}_CONFIGS ${${pkg}_DIR} ${${pkg}_DIR}/${pkg}Config.cmake*)
  if(${pkg}_CONFIGS)
    list(LENGTH ${pkg}_CONFIGS CONFIG_LIST_LENGTH)
    math(EXPR LIST_INDEX ${CONFIG_LIST_LENGTH}-1)
    list(GET ${pkg}_CONFIGS ${LIST_INDEX} ${pkg}_CONFIG_FILE)
    get_filename_component(${pkg}_CONFIG_DIR ${${pkg}_CONFIG_FILE} DIRECTORY)
    mark_as_superbuild(${pkg}_CONFIG_DIR}:PATH)
    endif()
  endif()
endmacro()


