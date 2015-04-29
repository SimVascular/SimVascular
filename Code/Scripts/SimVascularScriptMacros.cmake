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

if(WIN32)
	set(ENV_SET_COMMAND "set")
	set(ENV_PATH_VARIABLE "PATH")
	set(ENV_LIBRARY_PATH_VARIABLE "PATH")
	set(ENV_SEPERATOR ";")
	set(DIR_SEPERATOR "\\")
	set(COMMENT_CHAR "rem")
endif()

if(UNIX)
	set(ENV_SET_COMMAND "export")
	set(ENV_PATH_VARIABLE "PATH")
	set(ENV_SEPERATOR ":")
	set(DIR_SEPERATOR "/")
	set(COMMENT_CHAR "#")
	if(APPLE)
		set(ENV_LIBRARY_PATH_VARIABLE "DYLD_LIBRARY_PATH")
		set(DYLD "DYLD")
	endif()
	if(UNIX AND NOT APPLE)
		set(ENV_LIBRARY_PATH_VARIABLE "LD_LIBRARY_PATH")
		set(DYLD "LD")
	endif()
endif()

#-----
# Script Macros
#
macro(simvascular_configure_script)
	set(options INSTALL) 
	set(oneValueArgs TARGET OUTPUT_NAME DESTINATION)
	set(multiValueArgs FILES)

	CMAKE_PARSE_ARGUMENTS(""
		"${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )	
	message(STATUS "Generating ${_TARGET} scripts. Dev name: ${${_TARGET}_DEVELOPER_SCRIPT_NAME}, install name:  ${${_TARGET}_INSTALL_SCRIPT_NAME}")
	foreach(MODE DEVELOPER INSTALL)

		string(TOLOWER ${MODE} mode)
		dev_message("mode: ${MODE} ${mode}")
		if(MODE MATCHES "^INSTALL$")
			set(RELEASE_MODE 1)
		else()
			set(RELEASE_MODE 0)
		endif()
		
		set(sc ";")
		set(ACCUM_SCRIPT_STRING)
		set(TEMP_SCRIPT)
		string(CONFIGURE "${SCRIPT_FILES}" ${MODE}_FILES @ONLY)
		dev_message("${SCRIPT_FILES} \n ${${MODE}_FILES}")
		foreach(file ${${MODE}_FILES})
			dev_message("    Processing file: ${file}")
			file(READ ${file} TEMP_SCRIPT)
			STRING(REGEX REPLACE ";" "\@sc\@" TEMP_SCRIPT "${TEMP_SCRIPT}")
			set(ACCUM_SCRIPT_STRING "${ACCUM_SCRIPT_STRING}${TEMP_SCRIPT}\n")
		endforeach()
		string(CONFIGURE ${ACCUM_SCRIPT_STRING} SCRIPT_STRING @ONLY)

		FILE(WRITE "${TEMP_DIR}/${mode}/${${_TARGET}_${MODE}_SCRIPT_NAME}${WIN_BAT}"
			"${SCRIPT_STRING}")

	endforeach()
	
	file(REMOVE "${SIMVASCULAR_DEVELOPER_SCRIPT_DIR}/${${_TARGET}_DEVELOPER_SCRIPT_NAME}${WIN_BAT}")
	file(COPY "${TEMP_DIR}/developer/${${_TARGET}_DEVELOPER_SCRIPT_NAME}${WIN_BAT}" 
		DESTINATION ${SIMVASCULAR_DEVELOPER_SCRIPT_DIR}
		FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ
		GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

	#add_custom_target(CopyScriptDev-${_TARGET} ALL
	#	COMMAND ${CMAKE_COMMAND} -E rename
	#	${SIMVASCULAR_DEVELOPER_SCRIPT_DIR}/${${_TARGET}_DEVELOPER_SCRIPT_NAME}${WIN_BAT}-config ${SIMVASCULAR_DEVELOPER_SCRIPT_DIR}/${${_TARGET}_DEVELOPER_SCRIPT_NAME}${WIN_BAT})

	#add_custom_target(CopyScriptInstall-${_TARGET} ALL
	#	COMMAND ${CMAKE_COMMAND} -E rename
	#	${TEMP_DIR}/install/${${_TARGET}_INSTALL_SCRIPT_NAME}${WIN_BAT}-config ${TEMP_DIR}/install/${${_TARGET}_INSTALL_SCRIPT_NAME}${WIN_BAT})

	if(_INSTALL)
		install(FILES ${TEMP_DIR}/install/${${_TARGET}_INSTALL_SCRIPT_NAME}${WIN_BAT} 
			DESTINATION ${SIMVASCULAR_INSTALL_SCRIPT_DIR}
			PERMISSIONS 
			OWNER_READ OWNER_WRITE OWNER_EXECUTE 
			GROUP_READ GROUP_EXECUTE 
			WORLD_READ WORLD_EXECUTE
			)
	endif()

endmacro()


macro(env_variable_to_value_variable value_variable variable)
	if(WIN32 AND NOT UNIX)
		set(${value_variable} "%${variable}%")
	endif()
	if(UNIX)
		set(${value_variable} "$${variable}")
	endif()
endmacro()

function(append_env_string evn_var value output_variable)
	env_variable_to_value_variable(ENV_VALUE ${evn_var})
	set(${output_variable} "${ENV_SET_COMMAND} ${evn_var}=${ENV_VALUE}${ENV_SEPERATOR}${value}" PARENT_SCOPE)
endfunction()

function(set_env_string evn_var value output_variable)
	set(${output_variable} "${ENV_SET_COMMAND} ${evn_var}=${value}\n" PARENT_SCOPE)
endfunction()

macro(set_env_string_concat evn_var value output_variable)
	set_env_string(${evn_var} ${value} _tmp)
	set(${output_variable} "${${output_variable}}${_tmp}")
endmacro()

macro(append_env_string_concat evn_var value output_variable)
	append_env_string(${evn_var} ${value} _tmp)
	set(${output_variable} "${${output_variable}}${_tmp}\n")
endmacro()

macro(comment_concat msg output_variable)
	set(${output_variable} "${${output_variable}}${COMMENT_CHAR} ${msg}\n")
endmacro()