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

#-----------------------------------------------------------------------------
# print_vars - This is a simple marco to print out a list of variables
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
# dev_message - This is macro to print a developer message for a string
#
macro(dev_message string)
	if(SV_DEVELOPER_OUTPUT)
		message("DEV: ${string}")
	endif()
endmacro()

#-----------------------------------------------------------------------------
# simvascular_glob_dirs - searches a directory for a list of files
#
macro(simvascular_glob_dirs _newlist )
	set(${_newlist})
	foreach(value ${ARGN})
		if(IS_DIRECTORY ${value})
			list(APPEND ${_newlist} ${value})
		endif()
	endforeach()
endmacro()

#-----------------------------------------------------------------------------
# simvascular_combine_files - combines contents of a list of files into one
# file
#
macro(simvascular_combine_files output_file)
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

  # Function options
  set(arg_options NO_MODULE REQUIRED NO_DEFAULT_PATH)
  # Multiple value args
  set(arg_single VERSION SHARED_LIB)
  # Multiple value args
  set(arg_multiple PATHS HINTS COMPONENTS)

  cmake_parse_arguments("simvascular_external"
   "${arg_options}"
   "${arg_single}" "${arg_multiple}" ${ARGN} )

  # Components
  set(EXTRA_ARGS)
  if(simvascular_external_COMPONENTS)
    set(EXTRA_ARGS COMPONENTS ${simvascular_external_COMPONENTS})
  endif()

  # No modules
  if(simvascular_external_NO_MODULE)
  	set(EXTRA_ARGS ${EXTRA_ARGS} NO_MODULE)
  endif()

  # Required
  if(simvascular_external_REQUIRED)
    set(EXTRA_ARGS ${EXTRA_ARGS} REQUIRED)
  endif()

  # No default path
  if(simvascular_external_NO_DEFAULT_PATH)
    set(EXTRA_ARGS ${EXTRA_ARGS} NO_DEFAULT_PATH)
  endif()

  if(simvascular_external_VERSION)
    set(EXTRA_ARGS ${simvascular_external_VERSION} ${EXTRA_ARGS})
  endif()

  # Default PATHS
  unset(ARG_STRING)
  set(_paths "${simvascular_external_PATHS}")
  if(NOT simvascular_external_PATHS)
    set(_paths "${CMAKE_MODULE_PATH}")
  endif()

  #message(STATUS "Search paths for ${_pkg}Config.cmake: ${_paths}")

  # Find Package
  find_package(${_pkg} ${EXTRA_ARGS})

  # Add to shared libs
  if(simvascular_external_SHARED_LIB)
    set(SV_INSTALL_EXTERNALS ON)
    set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} ${_pkg})
  endif()

  # Include include directories
  if(${_pkg}_FOUND)
    message(STATUS "PKG ${_pkg} found!")
    if( ${_pkg}_INCLUDE_DIR )
    	dev_message("Including dir: ${${_pkg}_INCLUDE_DIR}")
    	# This get many of them
    	include_directories(${${_pkg}_INCLUDE_DIR})
    endif()
    if( ${_pkg}_INCLUDE_DIRS )
      dev_message("Including dir: ${${_pkg}_INCLUDE_DIRS}")
    	# This get many of them
      include_directories(${${_pkg}_INCLUDE_DIRS})
    endif()
  endif()

  # Developer help
  if(SV_DEVELOPER_OUTPUT)
    message(STATUS "Finished Configuring ${_pkg}")
    message(STATUS "")
  endif()
endmacro()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# simvascular_third_party
#
macro(simvascular_third_party _pkg)
	string(TOLOWER "${_pkg}" _lower)
	string(TOUPPER "${_pkg}" _upper)

	set(options OPTIONAL VERSION_EXACT
		DOWNLOADABLE SYSTEM_DEFAULT
		SVEXTERN_CONFIG ADD_INSTALL
    SOLVER_DEPEND
		)
	set(oneValueArgs VERSION)
	set(multiValueArgs PATHS HINTS COMPONENTS)

	CMAKE_PARSE_ARGUMENTS("simvascular_third_party"
		"${options}"
		"${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
        if(simvascular_third_party_SOLVER_DEPEND)
          set(${_upper}_SUBDIR svSolver/Code/ThirdParty/${_pkg})
        else()
	  set(${_upper}_SUBDIR ThirdParty/${_pkg})
        endif()
	if(simvascular_third_party_SYSTEM_DEFAULT)
		option(SV_USE_SYSTEM_${_upper} "Use system ${_pkg}" ON)
	else()
		option(SV_USE_SYSTEM_${_upper} "Use system ${_pkg}" OFF)
	endif()

	mark_as_advanced(SV_USE_SYSTEM_${_upper})

	configure_file(${SV_SOURCE_DIR}/${${_upper}_SUBDIR}/simvascular_${_lower}.h.in
		${SV_BINARY_DIR}/${${_upper}_SUBDIR}/simvascular_${_lower}.h)
	include_directories(BEFORE ${SV_BINARY_DIR}/${${_upper}_SUBDIR} ${SV_SOURCE_DIR}/${${_upper}_SUBDIR})
	if(SV_USE_SYSTEM_${_upper})
		set(${_upper}_LIBRARIES)
		set(${_upper}_LIBRARY)
	else()
    set(${_upper}_LIBRARY_NAME _simvascular_thirdparty_${_lower})
    add_subdirectory(${${_upper}_SUBDIR}/simvascular_${_lower})
	endif()
endmacro()
#-----------------------------------------------------------------------------

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
macro(check_library_exists_concat LIBRARY SYMBOL VARIABLE)
	check_library_exists ("${LIBRARY};${LINK_LIBS}" ${SYMBOL} "" ${VARIABLE})
	if(${VARIABLE})
		set(LINK_LIBS ${LINK_LIBS} ${LIBRARY})
	endif(${VARIABLE})
endmacro()
#-----------------------------------------------------------------------------

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

	cmake_parse_arguments("simvascular_add_executable"
		"${options}"
		"${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  set(WINDOWS_ICON_RESOURCE_FILE "")
  if(WIN32)
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/icons/${TARGET_NAME}.rc")
      set(WINDOWS_ICON_RESOURCE_FILE "${CMAKE_CURRENT_SOURCE_DIR}/icons/${TARGET_NAME}.rc")
    endif()
  endif()

  set(_app_compile_flags )
  if(WIN32)
    set(_app_compile_flags "${_app_compile_flags} -DPOCO_NO_UNWINDOWS -DWIN32_LEAN_AND_MEAN")
  endif()

  if(WIN32)
    add_executable(${TARGET_NAME} WIN32 ${simvascular_add_executable_SRCS} ${WINDOWS_ICON_RESOURCE_FILE})
  else()
    add_executable(${TARGET_NAME} ${simvascular_add_executable_SRCS} ${WINDOWS_ICON_RESOURCE_FILE})
  endif()

  set_target_properties(${TARGET_NAME} PROPERTIES
    COMPILE_FLAGS "${_app_compile_flags}")

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
    if(APPLE)
      set_target_properties(${TARGET_NAME} PROPERTIES MACOSX_BUNDLE_NAME "${TARGET_NAME}")
      set(icon_name "icon.icns")
      set(icon_full_path "${CMAKE_CURRENT_SOURCE_DIR}/icons/${icon_name}")
      if(EXISTS "${icon_full_path}")
        set_target_properties(${TARGET_NAME} PROPERTIES MACOSX_BUNDLE_ICON_FILE "${icon_name}")
        install(TARGETS ${TARGET_NAME}
          RUNTIME DESTINATION "${simvascular_add_executable_INSTALL_DESTINATION}"
          ${_COMPARGS})
      endif()
    else()
      if(simvascular_add_executable_COMPONENT)
        set(_COMPARGS "COMPONENT ${simvascular_add_executable_COMPONENT}")
      endif()
      install(TARGETS ${TARGET_NAME}
        RUNTIME DESTINATION ${simvascular_add_executable_INSTALL_DESTINATION}
        ${_COMPARGS})
    endif()
	endif()
endmacro()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
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
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
macro(simvascular_get_external_path_from_include_dir pkg)
  if(NOT ${pkg}_DIR)
    message(FATAL_ERROR "${pkg}_DIR is not set and must be set if using system")
  endif()
  if(${pkg}_INCLUDE_DIRS)
    list(GET ${pkg}_INCLUDE_DIRS 0 TMP_DIR)
    get_filename_component(TMP_DIR ${TMP_DIR} DIRECTORY)
    get_filename_component(TMP_DIR ${TMP_DIR} DIRECTORY)
  endif()
  if(${pkg}_INCLUDE_DIR)
    list(GET ${pkg}_INCLUDE_DIR 0 TMP_DIR)
    get_filename_component(TMP_DIR ${TMP_DIR} DIRECTORY)
    get_filename_component(TMP_DIR ${TMP_DIR} DIRECTORY)
  endif()
  if(NOT TMP_DIR OR NOT EXISTS ${TMP_DIR})
    message("${pkg}_INCLUDE_DIR is not set")
  else()
    set(SV_${pkg}_DIR ${TMP_DIR})
  endif()
endmacro()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Separate string by periods to get portions before and after first period
macro(simvascular_get_major_minor_version version major_version minor_version)
  string(REPLACE "." ";" version_list ${version})
  list(GET version_list 0 ${major_version})
  list(GET version_list 1 ${minor_version})
endmacro()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#!
#! See http://www.cmake.org/Wiki/CMakeMacroParseArguments
#!
#! \ingroup CMakeUtilities
macro(simvascular_parse_arguments prefix arg_names option_names)
  set(DEFAULT_ARGS)
  foreach(arg_name ${arg_names})
    set(${prefix}_${arg_name})
  endforeach()
  foreach(option ${option_names})
    set(${prefix}_${option} FALSE)
  endforeach()

  set(current_arg_name DEFAULT_ARGS)
  set(current_arg_list)
  foreach(arg ${ARGN})
    set(larg_names ${arg_names})
    list(FIND larg_names "${arg}" is_arg_name)
    IF (is_arg_name GREATER -1)
      set(${prefix}_${current_arg_name} ${current_arg_list})
      set(current_arg_name ${arg})
      set(current_arg_list)
    ELSE (is_arg_name GREATER -1)
      set(loption_names ${option_names})
      list(FIND loption_names "${arg}" is_option)
      IF (is_option GREATER -1)
        set(${prefix}_${arg} TRUE)
      ELSE (is_option GREATER -1)
        set(current_arg_list ${current_arg_list} ${arg})
      ENDIF (is_option GREATER -1)
    ENDIF (is_arg_name GREATER -1)
  endforeach()
  set(${prefix}_${current_arg_name} ${current_arg_list})
endmacro()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
function(simvascular_generate_plugin_manifest QRC_SRCS)

  simvascular_parse_arguments(MY
    "ACTIVATIONPOLICY;CATEGORY;CONTACT_ADDRESS;COPYRIGHT;DESCRIPTION;DOC_URL;ICON;LICENSE;NAME;REQUIRE_PLUGIN;SYMBOLIC_NAME;VENDOR;VERSION;CUSTOM_HEADERS"
    ""
    ${ARGN}
    )

  # Sanity checks
  if(NOT DEFINED MY_SYMBOLIC_NAME)
    message(FATAL_ERROR "SYMBOLIC_NAME is mandatory")
  endif()

  set(_manifest_content "Plugin-SymbolicName: ${MY_SYMBOLIC_NAME}")

  if(DEFINED MY_ACTIVATIONPOLICY)
    string(TOLOWER "${MY_ACTIVATIONPOLICY}" _activation_policy)
    if(_activation_policy STREQUAL "eager")
      set(_manifest_content "${_manifest_content}\nPlugin-ActivationPolicy: eager")
    else()
      message(FATAL_ERROR "ACTIVATIONPOLICY is set to '${MY_ACTIVATIONPOLICY}', which is not supported")
    endif()
  endif()

  if(DEFINED MY_CATEGORY)
    set(_manifest_content "${_manifest_content}\nPlugin-Category: ${MY_CATEGORY}")
  endif()

  if(DEFINED MY_CONTACT_ADDRESS)
    set(_manifest_content "${_manifest_content}\nPlugin-ContactAddress: ${MY_CONTACT_ADDRESS}")
  endif()

  if(DEFINED MY_COPYRIGHT)
    set(_manifest_content "${_manifest_content}\nPlugin-Copyright: ${MY_COPYRIGHT}")
  endif()

  if(DEFINED MY_DESCRIPTION)
    set(_manifest_content "${_manifest_content}\nPlugin-Description: ${MY_DESCRIPTION}")
  endif()

  if(DEFINED MY_DOC_URL)
    set(_manifest_content "${_manifest_content}\nPlugin-DocURL: ${MY_DOC_URL}")
  endif()

  if(DEFINED MY_ICON)
    set(_manifest_content "${_manifest_content}\nPlugin-Icon: ${MY_ICON}")
  endif()

  if(DEFINED MY_LICENSE)
    set(_manifest_content "${_manifest_content}\nPlugin-License: ${MY_LICENSE}")
  endif()

  if(DEFINED MY_NAME)
    set(_manifest_content "${_manifest_content}\nPlugin-Name: ${MY_NAME}")
  endif()

  if(DEFINED MY_REQUIRE_PLUGIN)
    string(REPLACE ";" "," require_plugin "${MY_REQUIRE_PLUGIN}")
    set(_manifest_content "${_manifest_content}\nRequire-Plugin: ${require_plugin}")
  endif()

  if(DEFINED MY_VENDOR)
    set(_manifest_content "${_manifest_content}\nPlugin-Vendor: ${MY_VENDOR}")
  endif()

  if(DEFINED MY_VERSION)
    set(_manifest_content "${_manifest_content}\nPlugin-Version: ${MY_VERSION}")
  endif()

  if(DEFINED MY_CUSTOM_HEADERS)
    set(_manifest_content "${_manifest_content}\n")
    foreach(_custom_header ${MY_CUSTOM_HEADERS})
      set(_manifest_content "${_manifest_content}\n${_custom_header}: ${${_custom_header}}")
    endforeach()
  endif()

  set(_manifest_filename "MANIFEST.MF")
  set(_manifest_filepath "${CMAKE_CURRENT_BINARY_DIR}/${_manifest_filename}")
  string(REPLACE "." "_" _symbolic_name ${MY_SYMBOLIC_NAME})
  set(_manifest_qrc_filepath "${CMAKE_CURRENT_BINARY_DIR}/${_symbolic_name}_manifest.qrc")

  set(_manifest_qrc_content
"<!DOCTYPE RCC><RCC version=\"1.0\">
<qresource prefix=\"/${MY_SYMBOLIC_NAME}/META-INF\">
 <file>${_manifest_filename}</file>
</qresource>
</RCC>
")

configure_file("${SV_SOURCE_DIR}/CMake/MANIFEST.MF.in" "${_manifest_filepath}" @ONLY)
  configure_file("${SV_SOURCE_DIR}/CMake/plugin_manifest.qrc.in" "${_manifest_qrc_filepath}" @ONLY)

  if (CTK_QT_VERSION VERSION_GREATER "4")
    QT5_ADD_RESOURCES(_qrc_src ${_manifest_qrc_filepath})
  else()
    QT4_ADD_RESOURCES(_qrc_src ${_manifest_qrc_filepath})
  endif()

  set(${QRC_SRCS} ${${QRC_SRCS}} ${_qrc_src} PARENT_SCOPE)

endfunction()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
function(simvascular_get_target_libraries varname)

  set(expanded_target_library_list)

  set(TARGET_DIRECTORY ${ARGV1})
  set(_target_name )
  if("${TARGET_DIRECTORY}" STREQUAL "")
    set(TARGET_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
    set(_target_name ${PROJECT_NAME})
  endif()

  set(filepath ${TARGET_DIRECTORY}/target_libraries.cmake)
  set(manifestpath ${TARGET_DIRECTORY}/manifest_headers.cmake)

  # Check if "target_libraries.cmake" or "manifest_headers.cmake" file exists
  if(NOT EXISTS ${filepath} AND NOT EXISTS ${manifestpath})
    message(FATAL_ERROR "${filepath} or ${manifestpath} doesn't exists !")
  endif()

  # Make sure the variable is cleared
  set(target_libraries )
  set(Require-Plugin )

  if(EXISTS ${filepath})
    # Let's make sure target_libraries contains only strings
    file(STRINGS "${filepath}" stringtocheck) # read content of 'filepath' into 'stringtocheck'
    string(REGEX MATCHALL "[^\\#]\\$\\{.*\\}" incorrect_elements ${stringtocheck})
    foreach(incorrect_element ${incorrect_elements})
      string(REGEX REPLACE "\\$|\\{|\\}" "" correct_element ${incorrect_element})
      message(FATAL_ERROR "In ${filepath}, ${incorrect_element} should be replaced by ${correct_element}")
    endforeach()

    include(${filepath})

    if(_target_name)
      list(APPEND target_libraries "${${_target_name}_OPTIONAL_DEPENDENCIES}")
    endif()

    # Loop over all target library, if it does *NOT* start with "CTK",
    # let's resolve the variable to access its content
    foreach(target_library ${target_libraries})
      if(${target_library} MATCHES "^CTK[a-zA-Z0-9]+$" OR
         ${target_library} MATCHES "^org_commontk_[a-zA-Z0-9_]+$")
        list(APPEND expanded_target_library_list ${target_library})
      else()
        list(APPEND expanded_target_library_list "${${target_library}}")
      endif()
    endforeach()
  endif()

  if(EXISTS ${manifestpath})
    # Let's make sure Require-Plugins contains only strings
    file(STRINGS "${manifestpath}" stringtocheck) # read content of 'manifestpath' into 'stringtocheck'
    string(REGEX MATCHALL "[^\\#]\\$\\{.*\\}" incorrect_elements ${stringtocheck})
    foreach(incorrect_element ${incorrect_elements})
      string(REGEX REPLACE "\\$|\\{|\\}" "" correct_element ${incorrect_element})
      message(FATAL_ERROR "In ${manifestpath}, ${incorrect_element} should be replaced by ${correct_element}")
    endforeach()

    include(${manifestpath})

    # Loop over all plugin dependencies,
    foreach(plugin_symbolicname ${Require-Plugin})
      string(REPLACE "." "_" plugin_library ${plugin_symbolicname})
      list(APPEND expanded_target_library_list ${plugin_library})
    endforeach()
  endif()

  # Pass the list of target libraries to the caller
  set(${varname} ${expanded_target_library_list} PARENT_SCOPE)

endfunction()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#! \ingroup CMakeUtilities
macro(simvascular_generate_plugin_resource_file QRC_SRCS)

  simvascular_parse_arguments(MY
    "NAME;PREFIX;RESOURCES;BINARY_RESOURCES"
    ""
    ${ARGN}
    )

  set(_qrc_filepath "${CMAKE_CURRENT_BINARY_DIR}/${MY_NAME}")

  set(_qrc_content
"<!DOCTYPE RCC><RCC version=\"1.0\">
<qresource prefix=\"/${MY_PREFIX}\">
")

  if(MY_RESOURCES)
    foreach(_resource_file ${MY_RESOURCES})
      configure_file("${CMAKE_CURRENT_SOURCE_DIR}/${_resource_file}" "${CMAKE_CURRENT_BINARY_DIR}/${_resource_file}" COPYONLY)
      set(_qrc_content "${_qrc_content}<file>${_resource_file}</file>
")
    endforeach()
  endif()

  if(MY_BINARY_RESOURCES)
    foreach(_resource_file ${MY_BINARY_RESOURCES})
      set(_qrc_content "${_qrc_content}<file>${_resource_file}</file>
")
    endforeach()
  endif()

  set(_qrc_content "${_qrc_content}</qresource>
</RCC>
")
configure_file("${SV_SOURCE_DIR}/CMake/plugin_resources_cached.qrc.in" "${_qrc_filepath}" @ONLY)

  qt5_add_resources(${QRC_SRCS} ${_qrc_filepath})

endmacro()

function(simvascular_create_plugin)
  # single value arguments
  set(arg_single
    EXPORT_DIRECTIVE # (required) TODO: could be generated via CMake as it is done for MITK modules already
  )

  # multiple value arguments
  set(arg_multiple
    MODULE_DEPENDS            # (optional)
    PACKAGE_DEPENDS
  )

  cmake_parse_arguments(_PLUGIN "${arg_options}" "${arg_single}" "${arg_multiple}" ${ARGN})

  # Print a warning if the project name does not match the directory name
  get_filename_component(_dir_name ${CMAKE_CURRENT_SOURCE_DIR} NAME)
  string(REPLACE "." "_" _dir_name_with_ ${_dir_name})
  if(NOT _dir_name_with_ STREQUAL ${PROJECT_NAME})
    message(WARNING "Discouraged mismatch of plug-in project name [${PROJECT_NAME}] and top-level directory name [${CMAKE_CURRENT_SOURCE_DIR}].")
  endif()
  set(lib_name ${PROJECT_NAME})

  include(${CMAKE_CURRENT_SOURCE_DIR}/files.cmake)
  usFunctionGenerateModuleInit(CPP_FILES)

  #------------------------------------QT-------------------------------------
  qt5_wrap_ui(UISrcs ${UI_FILES})
  qt5_add_resources(QRCSrcs ${QRC_FILES})
  set(MOC_OPTIONS "-DBOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION -DBOOST_TT_HAS_OPERATOR_HPP_INCLUDED")
  foreach(moc_src ${MOC_H_FILES})
    qt5_wrap_cpp(MOCSrcs ${moc_src} OPTIONS -f${moc_src} ${MOC_OPTIONS} TARGET ${lib_name})
  endforeach()
  #------------------------------------QT-------------------------------------

  #---------------------------------MANIFEST----------------------------------
  # If a file named manifest_headers.cmake exists, read it
  set(CTK_QT_VERSION 5.4)
  set(manifest_headers_dep )
  if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/manifest_headers.cmake")
    include(${CMAKE_CURRENT_SOURCE_DIR}/manifest_headers.cmake)
    set(manifest_headers_dep "${CMAKE_CURRENT_SOURCE_DIR}/manifest_headers.cmake")
  endif()

  string(REPLACE "_" "." Plugin-SymbolicName ${lib_name})

  # Add the generated manifest qrc file
  set(manifest_qrc_src )
  simvascular_generate_plugin_manifest(manifest_qrc_src
    ACTIVATIONPOLICY ${Plugin-ActivationPolicy}
    CATEGORY ${Plugin-Category}
    CONTACT_ADDRESS ${Plugin-ContactAddress}
    COPYRIGHT ${Plugin-Copyright}
    DESCRIPTION ${Plugin-Description}
    DOC_URL ${Plugin-DocURL}
    ICON ${Plugin-Icon}
    LICENSE ${Plugin-License}
    NAME ${Plugin-Name}
    REQUIRE_PLUGIN ${Require-Plugin}
    SYMBOLIC_NAME ${Plugin-SymbolicName}
    VENDOR ${Plugin-Vendor}
    VERSION ${Plugin-Version}
    CUSTOM_HEADERS ${Custom-Headers}
    )

  if(manifest_headers_dep)
    set_property(SOURCE ${manifest_qrc_src} APPEND
                   PROPERTY OBJECT_DEPENDS ${manifest_headers_dep})
  endif()
  list(APPEND QRCSrcs ${manifest_qrc_src})
  #---------------------------------MANIFEST----------------------------------

  #------------------------------PLUGIN RESOURCE------------------------------
  simvascular_generate_plugin_resource_file(resource_qrc_src
  NAME ${lib_name}_cached.qrc
  PREFIX ${Plugin-SymbolicName}
  RESOURCES ${CACHED_RESOURCE_FILES})
  list(APPEND QRCSrcs ${resource_qrc_src})
  #------------------------------PLUGIN RESOURCE------------------------------

  #------------------------------CREATE PLUGIN--------------------------------
  add_library(${lib_name} SHARED ${H_FILES} ${CPP_FILES} ${UISrcs} ${QRCSrcs} ${MOCSrcs})

  set_property(TARGET ${lib_name} PROPERTY US_MODULE_NAME ${lib_name})
  set_property(TARGET ${lib_name} APPEND PROPERTY COMPILE_DEFINITIONS US_MODULE_NAME=${lib_name})

  target_link_libraries(${lib_name} PRIVATE ${MITK_LIBRARIES} ${MITK_PLUGIN_LIBRARIES} ${CTK_LIBRARIES} ${ITK_LIBRARIES} ${QT_LIBRARIES})
  #------------------------------CREATE PLUGIN--------------------------------

  foreach(depender ${_PLUGIN_MODULE_DEPENDS})
    if(NOT "${depender}" STREQUAL "MitkQtWidgets")
      target_link_libraries(${lib_name} PRIVATE ${depender})
    endif()
  endforeach()

  #---------------------------------MITK DEPENDS-----------------------------
  #simvascular_get_target_libraries(MITK_MODULE_DEPENDS "")
  #message(${MITK_MODULE_DEPENDS})
  #target_link_libraries(${lib_name} PRIVATE ${MITK_MODULE_DEPENDS})
  #---------------------------------MITK DEPENDS-----------------------------

  #--------------------------------EXPORT HEADER-----------------------------
  set_target_properties(${lib_name}
      PROPERTIES
      COMPILE_FLAGS "-DQT_PLUGIN"
      LIBRARY_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/plugins
      RUNTIME_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/plugins
  )

  set(MODULE_EXPORT_DEFINE ${_PLUGIN_EXPORT_DIRECTIVE})

  set(_export_macro_names
    EXPORT_MACRO_NAME ${MODULE_EXPORT_DEFINE}
    NO_EXPORT_MACRO_NAME ${_PLUGIN_EXPORT_DIRECTIVE}_NO_EXPORT
    DEPRECATED_MACRO_NAME ${_PLUGIN_EXPORT_DIRECTIVE}_DEPRECATED
    NO_DEPRECATED_MACRO_NAME ${_PLUGIN_EXPORT_DIRECTIVE}_NO_DEPRECATED
  )
  generate_export_header(${lib_name}
    ${_export_macro_names}
    EXPORT_FILE_NAME ${lib_name}_Export.h
  )

  target_include_directories(${lib_name} PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
  #--------------------------------EXPORT HEADER-----------------------------

  if(MSVC)
      foreach( OUTPUTCONFIG ${CMAKE_CONFIGURATION_TYPES} )
          string( TOUPPER ${OUTPUTCONFIG} OUTPUTCONFIG )
          set_target_properties(${lib_name}
              PROPERTIES
              LIBRARY_OUTPUT_DIRECTORY_${OUTPUTCONFIG} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/plugins
              RUNTIME_OUTPUT_DIRECTORY_${OUTPUTCONFIG} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/plugins
          )
      endforeach( OUTPUTCONFIG CMAKE_CONFIGURATION_TYPES )
  endif()

endfunction()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
function(simvascular_create_module)

  set(arg_single
    TARGET
    EXPORT_DIRECTIVE
    SHARED_LIB
  )

  # multiple value arguments
  set(arg_multiple
    LIBRARY_DEPENDS            # (optional)
    PACKAGE_DEPENDS
  )

  cmake_parse_arguments(_MODULE "${arg_options}" "${arg_single}" "${arg_multiple}" ${ARGN})

  if(NOT _MODULE_TARGET)
    message(FATAL_ERROR "Must provide target for module creation")
  endif()
  if(NOT _MODULE_EXPORT_DIRECTIVE)
    message(FATAL_ERROR "Must provide name for header export")
  endif()
  set(MODULE_LIB_TYPE "SHARED")
  if(DEFINED _MODULE_SHARED_LIB AND NOT _MODULE_SHARED_LIB)
    set(MODULE_LIB_TYPE "STATIC")
  endif()

  include(${CMAKE_CURRENT_SOURCE_DIR}/files.cmake)
  qt5_wrap_ui(UISrcs ${UI_FILES})
  qt5_add_resources(QRCSrcs ${QRC_FILES})
  qt5_wrap_cpp(MOCSrcs ${MOC_H_FILES})

  usFunctionGenerateModuleInit(CPP_FILES)
  if(RESOURCE_FILES)
    usFunctionGetResourceSource(TARGET ${_MODULE_TARGET} OUT CPP_FILES)
  endif()

  add_library(${_MODULE_TARGET} ${MODULE_LIB_TYPE} ${H_FILES} ${CPP_FILES} ${UISrcs} ${QRCSrcs} ${MOCSrcs})

  set_property(TARGET ${_MODULE_TARGET} PROPERTY US_MODULE_NAME ${_MODULE_EXPORT_DIRECTIVE})
  set_property(TARGET ${_MODULE_TARGET} APPEND PROPERTY COMPILE_DEFINITIONS US_MODULE_NAME=${_MODULE_EXPORT_DIRECTIVE})

  target_link_libraries(${_MODULE_TARGET} PRIVATE
    ${_MODULE_LIBRARY_DEPENDS}
    ${_MODULE_PACKAGE_DEPENDS})

  if(RESOURCE_FILES)
      set(res_dir resource)
      usFunctionAddResources(TARGET ${_MODULE_TARGET}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${res_dir}
        FILES ${RESOURCE_FILES})
      usFunctionEmbedResources(TARGET ${_MODULE_TARGET})
  endif()


  string(TOUPPER ${_MODULE_EXPORT_DIRECTIVE} MODULE_NAME)
  set(MODULE_EXPORT_DEFINE ${MODULE_NAME}_EXPORT)

  set(_export_macro_names
    EXPORT_MACRO_NAME ${MODULE_EXPORT_DEFINE}
    NO_EXPORT_MACRO_NAME ${MODULE_NAME}_NO_EXPORT
    DEPRECATED_MACRO_NAME ${MODULE_NAME}_DEPRECATED
    NO_DEPRECATED_MACRO_NAME ${MODULE_NAME}_NO_DEPRECATED
    )
  generate_export_header(${_MODULE_TARGET}
    ${_export_macro_names}
    EXPORT_FILE_NAME ${_MODULE_EXPORT_DIRECTIVE}Exports.h
    )

  target_include_directories(${_MODULE_TARGET} PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

endfunction()
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#! \ingroup CMakeUtilities
function(simvascular_extract_option_name_and_value my_opt var_opt_name var_opt_value)

 # Make sure option is correctly formated
  if(NOT "${my_opt}" MATCHES "^[- :/A-Za-z0-9._]+:(ON|OFF)$")
    message(FATAL_ERROR "Option ${my_opt} is incorrect. Options should be specified using the following format OPT1:[ON|OFF]. For example OPT1:OFF or OPT2:ON")
  endif()

  # Extract option name and option default value
  string(REGEX REPLACE ":(ON|OFF)$" "\\\\;\\1" my_opt_list ${my_opt})
  set(my_opt_list ${my_opt_list})
  list(GET my_opt_list 0 opt_name)
  list(GET my_opt_list 1 opt_value)

  set(${var_opt_name} ${opt_name} PARENT_SCOPE)
  set(${var_opt_value} ${opt_value} PARENT_SCOPE)
endfunction()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#!
#!
#! \brief Create a provisioning file
#!
#! \param FILE <provisioning-file> (required) An absolute filename for
#!                                 the new provisioning file.
#! \param INCLUDE <file-list> (optional) A list of additional provisioning files
#!                            which should be included.
#! \param EXCLUDE_PLUGINS <plugin-list> (optional) A list of plug-in symbolic names which should be excluded
#!                                      from the provisioning entries.
#! \param NO_INSTALL (option) Suppress the creation of an additional provisioning file suitable for packaging.
#!
#! This function creates a provisioning file which can be used to provision a BlueBerry
#! application. The syntax of entries in the file is
#! \code
#! (READ|INSTALL|START) <file-url>
#! \endcode
#! READ includes the file at <file-url> and interprets it as a provisioning file, INSTALL installs <file-url>,
#! and START installs and starts <file-url> as a plug-in in the framework.
#!
#! <p>
#! For example the following provisioning file instructs the BlueBerry framework to read the entries in
#! a file called SomeApp.provisioning and subsequently INSTALL and START the plug-in com.mycompany.plugin
#! \code
#! READ file:///opt/etc/SomeApp.provisioning
#! START file:///opt/mycompany/plugins/libcom_mycompany_plugin.so
#! \endcode
#!
#! <p>
#! An example invocation of this macro may look like:
#! \code
#! set(_my_prov_file "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/MyApp.provisioning")
#! set(_my_plugins
#!   com.mycompany.plugin
#!   org.mitk.gui.qt.extapplication
#! )
#! FunctionCreateProvisioningFile(FILE ${_my_prov_file} PLUGINS ${_my_plugins})
#! \endcode
#!
#! \note This function will automatically create entries for all plug-in
#! dependencies of the specified plug-ins.
#!
function(simvascular_create_provisioning_file)

  cmake_parse_arguments(_PROV "NO_INSTALL" "FILE" "INCLUDE;PLUGINS;EXCLUDE_PLUGINS" ${ARGN})

  if(NOT _PROV_FILE)
    message(SEND_ERROR "FILE argument must not be empty")
    return()
  endif()

  set(out_var )
  set(out_var_install )
  if(WIN32)
    set(file_url "file:///")
  else()
    set(file_url "file://")
  endif()

  # Include other provisioning files
  foreach(incl ${_PROV_INCLUDE})
    get_filename_component(incl_filename "${incl}" NAME)
    set(out_var "${out_var}READ ${file_url}${incl}\n")
    set(out_var_install "${out_var_install}READ ${file_url}@EXECUTABLE_DIR/${incl_filename}\n")
  endforeach()

  if(_PROV_INCLUDE)
    set(out_var "${out_var}\n")
    set(out_var_install "${out_var_install}\n")
  endif()

  # List all plugins
  # Temporary list of mitk libraries. In the future, will use installed cmake files
  #-------------------------------TMP-------------------------------------------
  set(MITK_PLUGIN_LIBRARIES org_commontk_configadmin;org_commontk_eventadmin;org_blueberry_core_runtime;org_blueberry_core_expressions;org_blueberry_core_commands;org_blueberry_ui_qt;org_blueberry_ui_qt_help;org_blueberry_ui_qt_log;org_mitk_core_services;org_mitk_gui_common;org_mitk_planarfigure;org_mitk_core_ext;org_mitk_gui_qt_application;org_mitk_gui_qt_ext;org_mitk_gui_qt_extapplication;org_mitk_gui_qt_common;org_mitk_gui_qt_stdmultiwidgeteditor;org_mitk_gui_qt_common_legacy;org_mitk_gui_qt_datamanager;org_mitk_gui_qt_properties;org_mitk_gui_qt_basicimageprocessing;org_mitk_gui_qt_dicom;org_mitk_gui_qt_geometrytools;org_mitk_gui_qt_imagecropper;org_mitk_gui_qt_imagenavigator;org_mitk_gui_qt_measurementtoolbox;org_mitk_gui_qt_python;org_mitk_gui_qt_registration;org_mitk_gui_qt_segmentation;org_mitk_gui_qt_volumevisualization)

  set_property(GLOBAL APPEND PROPERTY CTK_PLUGIN_LIBRARIES_VARS MITK_PLUGIN_LIBRARIES)
  set(MITK_PLUGIN_LIBRARIES_set 1)

  set(_plugin_list )
  foreach(_plugin ${MITK_PLUGIN_LIBRARIES})
    string(REPLACE "." "_" _plugin_target ${_plugin})
    set(plugin_url "${file_url}${MITK_PLUGIN_LIBRARY_DIR}/lib${_plugin_target}${CMAKE_SHARED_LIBRARY_SUFFIX}")
    set(plugin_url_install "${file_url}@EXECUTABLE_DIR/../${SV_INSTALL_MITK_LIBRARY_DIR}/plugins/lib${_plugin_target}${CMAKE_SHARED_LIBRARY_SUFFIX}")
    set(out_var "${out_var}START ${plugin_url}\n")
    set(out_var_install "${out_var_install}START ${plugin_url_install}\n")
  endforeach()

  if(_PROV_PLUGINS)
    foreach(_plugin ${_PROV_PLUGINS})
      string(REPLACE "." "_" _plugin_target ${_plugin})
      list(APPEND _plugin_list ${_plugin_target})
    endforeach()
  endif()

  set(_exclude_targets )
  if(_PROV_EXCLUDE_PLUGINS)
    # Convert the plug-in symbolic names to valid target names
    foreach(_exclude_entry ${_PROV_EXCLUDE_PLUGINS})
      string(REPLACE "." "_" _exclude_target ${_exclude_entry})
      list(APPEND _exclude_targets ${_exclude_target})
    endforeach()
    list(REMOVE_ITEM _plugin_list ${_exclude_targets})
  endif()
  # Go through the list of plug-ins
  foreach(plugin ${_plugin_list})
    set(_plugin_target)
    if(TARGET ${plugin})
      # The entry already is a valid target (either imported or declared in the current project)
      set(_plugin_target ${plugin})
    elseif(${plugin} MATCHES "^[- :/A-Za-z0-9._]+:(ON|OFF)$")
      # Check if the entry if of the form "Some/Dir/org.my.plugin:OPTION"
      simvascular_extract_option_name_and_value(${plugin} plugin_name_with_dirs plugin_value)
      string(REPLACE "/" ";" _tokens ${plugin_name_with_dirs})
      list(GET _tokens -1 plugin_name)
      string(REPLACE "." "_" _plugin_target_name ${plugin_name})
      if(TARGET ${_plugin_target_name})
        # Check if the extracted last directory entry is a valid target
        set(_plugin_target ${_plugin_target_name})
      endif()
    endif()

    if(_plugin_target)
      # We got a valid target, either imported or from this project.
      set(_plugin_location)
      get_target_property(_is_imported ${_plugin_target} IMPORTED)
      if(_is_imported)
        get_target_property(_plugin_location ${_plugin_target} IMPORTED_LOCATION)
        if(NOT _plugin_location)
          get_target_property(_plugin_configs ${_plugin_target} IMPORTED_CONFIGURATIONS)
          foreach(_plugin_config ${_plugin_configs})
            get_target_property(_plugin_location ${_plugin_target} IMPORTED_LOCATION_${_plugin_config})
            if(_plugin_location)
              if(CMAKE_CONFIGURATION_TYPES)
                # Strip the last directory and filename
                string(REGEX REPLACE "(.*)/[^/]*/[^/]*$" "\\1" _plugin_location "${_plugin_location}")
              else()
                # Just strip the filename
                get_filename_component(_plugin_location "${_plugin_location}" DIRECTORY)
              endif()
              break()
            endif()
          endforeach()
        endif()
      else()
        if(WIN32)
          get_target_property(_plugin_location ${_plugin_target} RUNTIME_OUTPUT_DIRECTORY)
        else()
          get_target_property(_plugin_location ${_plugin_target} LIBRARY_OUTPUT_DIRECTORY)
        endif()
      endif()

      set(plugin_url "${file_url}${_plugin_location}/lib${_plugin_target}${CMAKE_SHARED_LIBRARY_SUFFIX}")
      set(plugin_url_install "${file_url}@EXECUTABLE_DIR/../Lib/plugins/lib${_plugin_target}${CMAKE_SHARED_LIBRARY_SUFFIX}")

      set(out_var "${out_var}START ${plugin_url}\n")
      set(out_var_install "${out_var_install}START ${plugin_url_install}\n")
    else()
      #message(WARNING "Ignoring unknown plug-in target \"${plugin}\" for provisioning.")
    endif()

  endforeach()

  file(WRITE ${_PROV_FILE} "${out_var}")

  if(NOT _PROV_NO_INSTALL)
    file(WRITE ${_PROV_FILE}.install "${out_var_install}")
  endif()

endfunction()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
macro(simvascular_get_subdirs result curdir)
  file(GLOB children RELATIVE ${curdir} ${curdir}/*)
  set(dirlist "")
  foreach(child ${children})
    if(IS_DIRECTORY ${curdir}/${child})
      list(APPEND dirlist ${child})
    endif()
  endforeach()
  set(${result} ${dirlist})
endmacro()

function(simvascular_install_external project_name)

  if(${CMAKE_PROJECT_NAME}_ENABLE_DISTRIBUTION)
    set(LIB_DESTINATION "${SV_EXTERNALS_INSTALL_PREFIX}")
  else()
    set(LIB_DESTINATION "${SV_EXTERNALS_${proj}_INSTALL_PREFIX}")
  endif()
  if(EXISTS ${SV_${proj}_DIR})
    if(EXISTS ${SV_${proj}_DIR}/lib)
      install(DIRECTORY ${SV_${proj}_DIR}/lib DESTINATION ${LIB_DESTINATION})
    endif()
    if(EXISTS ${SV_${proj}_DIR}/bin)
      install(DIRECTORY ${SV_${proj}_DIR}/bin DESTINATION ${LIB_DESTINATION})
    endif()
  endif()

endfunction()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# simvascular_add_new_external
macro(simvascular_add_new_external proj version use shared dirname)
  option(SV_USE_${proj} "Enable ${proj} Plugin" ${use})
  option(SV_USE_${proj}_SHARED "Build ${proj} libraries as shared libs" ${shared})

  set(${proj}_VERSION "${version}" CACHE TYPE STRING)
  simvascular_get_major_minor_version(${${proj}_VERSION} ${proj}_MAJOR_VERSION ${proj}_MINOR_VERSION)
  set(SV_EXT_${proj}_SRC_DIR ${SV_EXTERNALS_SRC_DIR}/${dirname}-${${proj}_VERSION})
  set(SV_EXT_${proj}_BIN_DIR ${SV_EXTERNALS_BIN_DIR}/${dirname}-${${proj}_VERSION})
  set(SV_EXT_${proj}_BLD_DIR ${SV_EXTERNALS_BLD_DIR}/${dirname}-${${proj}_VERSION})
  set(SV_EXT_${proj}_PFX_DIR ${SV_EXTERNALS_PFX_DIR}/${dirname}-${${proj}_VERSION})

  # Install rules
  set(SV_EXTERNALS_${proj}_INSTALL_PREFIX ${SV_EXTERNALS_INSTALL_PREFIX}/${dirname}-${${proj}_VERSION})
  if(${CMAKE_PROJECT_NAME}_ENABLE_DISTRIBUTION)
    set(LIB_DESTINATION "${SV_EXTERNALS_INSTALL_PREFIX}")
  else()
    set(LIB_DESTINATION "${SV_EXTERNALS_${proj}_INSTALL_PREFIX}")
  endif()

  if(NOT SV_INSTALL_${proj}_RUNTIME_DIR)
    set(SV_INSTALL_${proj}_RUNTIME_DIR ${LIB_DESTINATION}/bin)
  endif()

  if(NOT SV_INSTALL_${proj}_LIBRARY_DIR)
    set(SV_INSTALL_${proj}_LIBRARY_DIR ${LIB_DESTINATION}/lib)
  endif()

  if(NOT SV_INSTALL_${proj}_ARCHIVE_DIR)
    set(SV_INSTALL_${proj}_ARCHIVE_DIR ${LIB_DESTINATION}/lib)
  endif()

  if(NOT SV_INSTALL_${proj}_INCLUDE_DIR)
    set(SV_INSTALL_${proj}_INCLUDE_DIR ${LIB_DESTINATION}/include)
  endif()

  if(SV_USE_${proj})
    list(APPEND SV_EXTERNALS_LIST ${proj})
    set(SV_${proj}_DIR ${SV_EXT_${proj}_BIN_DIR})
    if(NOT ${proj}_DIR)
      set(${proj}_DIR "" CACHE PATH "For external projects with a Config.cmake file, path to that file; for externals without a Config.cmake, the path to the toplevel bin directory")
    else()
      set(${proj}_DIR "${${proj}_DIR}" CACHE PATH "For external projects with a Config.cmake file, path to that file; for externals without a Config.cmake, the path to the toplevel bin directory")
    endif()
    if("${proj}" STREQUAL "MITK")
      if(SV_USE_MITK_CONFIG)
        set(SV_${proj}_DIR ${SV_EXT_${proj}_BLD_DIR}/MITK-build)
      endif()
    endif()
  endif()
  if(SV_EXTERNALS_LIST)
    list(REMOVE_DUPLICATES SV_EXTERNALS_LIST)
  endif()
endmacro()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# System Macros
macro(env_variable_to_value_variable value_variable variable)
	if(WIN32 AND NOT UNIX)
		set(${value_variable} "%${variable}%")
	endif()
	if(UNIX)
		set(${value_variable} "$${variable}")
	endif()
endmacro()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
function(append_env_string evn_var value output_variable)
	env_variable_to_value_variable(ENV_VALUE ${evn_var})
	set(${output_variable} "${ENV_SET_COMMAND} ${evn_var}=${ENV_VALUE}${ENV_SEPERATOR}${value}" PARENT_SCOPE)
endfunction()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
function(set_env_string evn_var value output_variable)
	set(${output_variable} "${ENV_SET_COMMAND} ${evn_var}=${value}\n" PARENT_SCOPE)
endfunction()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
macro(set_env_string_concat evn_var value output_variable)
	set_env_string(${evn_var} ${value} _tmp)
	set(${output_variable} "${${output_variable}}${_tmp}")
endmacro()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
macro(append_env_string_concat evn_var value output_variable)
	append_env_string(${evn_var} ${value} _tmp)
	set(${output_variable} "${${output_variable}}${_tmp}\n")
endmacro()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
macro(simvascular_property_list_find_and_replace TARGET PROPERTY VALUE NEWVALUE)
  get_target_property(_LIST_VAR ${TARGET} ${PROPERTY})
  if("${_LIST_VAR}" STREQUAL "_LIST_VAR-NOTFOUND")
    dev_message("Property list find and replace, property ${PROPERTY} not found")
  else()
    simvascular_list_find_and_replace(_LIST_VAR "${VALUE}" ${NEWVALUE})
    set_target_properties(${TARGET} PROPERTIES ${PROPERTY} "${_LIST_VAR}")
  endif()
endmacro()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
macro(simvascular_list_find_and_replace LIST VALUE NEWVALUE)
  set(_COUNT "0")
  foreach(ITEM ${${LIST}})
    string(REGEX MATCH "${VALUE}" _FOUND ${ITEM})
    if(_FOUND)
      simvascular_list_replace(${LIST} ${_COUNT} ${NEWVALUE})
    endif()
    math(EXPR _COUNT "${_COUNT}+1")
  endforeach()
endmacro()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
macro(simvascular_list_replace LIST INDEX NEWVALUE)
  list(INSERT ${LIST} ${INDEX} ${NEWVALUE})
  math(EXPR __INDEX "${INDEX} + 1")
  list (REMOVE_AT ${LIST} ${__INDEX})
endmacro()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
function(print_target_properties tgt)
  if(NOT TARGET ${tgt})
    message("There is no target named '${tgt}'")
      return()
    endif()

    execute_process(COMMAND cmake --help-property-list OUTPUT_VARIABLE CMAKE_PROPERTY_LIST)
    # Convert command output into a CMake list
    STRING(REGEX REPLACE ";" "\\\\;" CMAKE_PROPERTY_LIST "${CMAKE_PROPERTY_LIST}")
    STRING(REGEX REPLACE "\n" ";" CMAKE_PROPERTY_LIST "${CMAKE_PROPERTY_LIST}")

    foreach (prop ${CMAKE_PROPERTY_LIST})
      string(REPLACE "<CONFIG>" "${CMAKE_BUILD_TYPE}" prop ${prop})
      # message ("Checking ${prop}")
      get_property(propval TARGET ${tgt} PROPERTY ${prop} SET)
      if (propval)
        get_target_property(propval ${tgt} ${prop})
        message ("${tgt} ${prop} = ${propval}")
      endif()
   endforeach(prop)
endfunction(print_target_properties)
#-----------------------------------------------------------------------------
