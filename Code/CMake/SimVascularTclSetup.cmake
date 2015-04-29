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

macro(tcl_cmd)
	set(options MESSAGE DEV_MESSAGE)
	set(oneValueArgs OUTPUT_VARIABLE)
	set(multiValueArgs FILES CODE)
	CMAKE_PARSE_ARGUMENTS("" 
		"${options}"
		"${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
	if(_CODE)
		file(WRITE ${TEMP_DIR}/tmp.tcl "${_CODE}")
	endif()
	if(NOT TCL_TCLSH)
		message(AUTHOR_WARNING "No tclsh command specicied. Command not run")
	endif()
	if(TCL_TCLSH)
		exec_program(${TCL_TCLSH} 
			ARGS ${TEMP_DIR}/tmp.tcl
			OUTPUT_VARIABLE ${_OUTPUT_VARIABLE}
			)
		file(REMOVE ${TEMP_DIR}/tmp.tcl)
	endif()
	if(_CODE)
		file(REMOVE ${TEMP_DIR}/tmp.tcl)
	endif()
endmacro()

if(NOT SimVascular_SUPERBUILD)
	set(TCL_CONFIG_FILES)
	tcl_cmd(CODE "puts \"[clock seconds]\""
		OUTPUT_VARIABLE SIMVASCULAR_TIMESTAMP)
	
	set(SimVascular_SOURCE_TCL_DIR ${SimVascular_SOURCE_HOME}/Tcl)
	set(SimVascular_BINARY_TCL_DIR ${SimVascular_BINARY_HOME}/Tcl)
	set(SimVascular_TCL ${SimVascular_BINARY_TCL_DIR})
	add_custom_target(copy-tcl ALL)
	add_custom_command(TARGET copy-tcl POST_BUILD
		COMMAND ${CMAKE_COMMAND} -E remove_directory ${SimVascular_BINARY_TCL_DIR}
		COMMAND ${CMAKE_COMMAND} -E make_directory ${SimVascular_BINARY_TCL_DIR}
		COMMAND ${CMAKE_COMMAND} -E copy_directory ${SimVascular_SOURCE_TCL_DIR} ${SimVascular_BINARY_TCL_DIR}
		COMMENT "Copying Tcl Directory..."
		)
	

	set(TCL_STARTUP_CONFIG_FILE "${TEMP_DIR}/startup_configure.tcl")
	set(TCL_SPLASH_CONFIG_FILE "${TEMP_DIR}/splash_configure.tcl")
	set(TCL_EXTERNAL_CONFIG_FILE "${TEMP_DIR}/externals_configure.tcl")

	include(SimVascularTclConfigure)

	set(TCL_CONFIG_FILES 
		${TCL_SPLASH_CONFIG_FILE} ${TCL_STARTUP_CONFIG_FILE} ${TCL_EXTERNAL_CONFIG_FILE})

	foreach(tcl_file ${TCL_CONFIG_FILES})
		dev_message("Configuring ${tcl_file}")
		add_custom_command(TARGET copy-tcl POST_BUILD
			COMMAND ${CMAKE_COMMAND} -E copy ${tcl_file} ${SimVascular_BINARY_TCL_DIR}
			COMMENT "Copying ${tcl_file}..."
			)
		add_dependencies(copy-tcl ${tcl_file})
	endforeach()
	
	#-----------------------------------------------------------------------------
	# Install Steps
	#-----------------------------------------------------------------------------
	
	include(PrepareTcl)

	install(DIRECTORY ${TEMP_DIR}/Tcl DESTINATION ${SIMVASCULAR_INSTALL_SCRIPT_DIR})
	install(FILES ${TCL_CONFIG_FILES}
		DESTINATION ${SIMVASCULAR_INSTALL_TCL_CODE_DIR}
		)
endif()