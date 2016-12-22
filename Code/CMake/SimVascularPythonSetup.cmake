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

macro(py_cmd)
	set(options MESSAGE DEV_MESSAGE)
	set(oneValueArgs OUTPUT_VARIABLE)
	set(multiValueArgs FILES CODE)
	CMAKE_PARSE_ARGUMENTS(""
		"${options}"
		"${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
	if(_CODE)
		file(WRITE ${TEMP_DIR}/tmp.py "${_CODE}")
	endif()
	if(_CODE)
		file(REMOVE ${TEMP_DIR}/tmp.py)
	endif()
endmacro()

set(SV_NO_RENDERER 0)

py_cmd(CODE "puts \"[clock seconds]\""
        OUTPUT_VARIABLE SV_TIMESTAMP)

set(SV_SOURCE_PYTHON_DIR ${SV_SOURCE_HOME}/Python)
set(SV_BINARY_PYTHON_DIR ${SV_BINARY_HOME}/Python)
set(SV_PYTHON ${SV_BINARY_PYTHON_DIR})
add_custom_target(copy-py ALL)
add_custom_command(TARGET copy-py POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E remove_directory ${SV_BINARY_PYTHON_DIR}
  COMMAND ${CMAKE_COMMAND} -E make_directory ${SV_BINARY_PYTHON_DIR}
  COMMAND ${CMAKE_COMMAND} -E copy_directory ${SV_SOURCE_PYTHON_DIR} ${SV_BINARY_PYTHON_DIR}
  COMMENT "Copying Python Directory..."
  )

#include(SimVascularPythonConfigure)

#-----------------------------------------------------------------------------
# Install Steps
#-----------------------------------------------------------------------------

include(PreparePython)

install(DIRECTORY ${TEMP_DIR}/Python DESTINATION ${SV_INSTALL_SCRIPT_DIR})
