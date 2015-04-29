##=========================================================================
 #
 # Copyright (c) 2014 The Regents of the University of California.
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
 #
 #=========================================================================#

set(SIMVASCULAR_INCLUDE_DIRS_WORK ${SIMVASCULAR_INCLUDE_DIRS})
set(INDENTS "${INDENTS}")

#-----------------------------------------------------------------------------
# ITK Setup
if(Plugin_ITK)
	set(SIMVASCULAR_PLUGIN_LIBRARY_DIRS ${SIMVASCULAR_PLUGIN_LIBRARY_DIRS} ITK)
endif()
#-----------------------------------------------------------------------------
# TETGEN Setup
if(Plugin_TETGEN)
	set(USE_TETGEN ON)
	if(TETGEN_VERSION MATCHES "1.5.1")
		set(TETGEN151 ON)
	elseif(TETGEN_VERSION MATCHES "1.5.0")
		set(TETGEN150 ON)
	elseif(TETGEN_VERSION MATCHES "1.4.3")
		set(TETGEN143 ON)
	else()
		message(FATAL_ERROR "Unknown Tetgen versions, please specify!")
	endif()
	set(SIMVASCULAR_PLUGIN_LIBRARY_DIRS ${SIMVASCULAR_PLUGIN_LIBRARY_DIRS} TetGenMeshObject)
endif()

if(Plugin_TET_ADAPTOR)
	set(SIMVASCULAR_PLUGIN_EXECUTABLE_DIRS ${SIMVASCULAR_PLUGIN_EXECUTABLE_DIRS} TetAdapt)
endif()

#-----------------------------------------------------------------------------
# VMTK
if(Plugin_VMTK)
	set(USE_VMTK ON)
	set(SIMVASCULAR_PLUGIN_LIBRARY_DIRS ${SIMVASCULAR_PLUGIN_LIBRARY_DIRS} VMTK)
endif()


if(SimVascular_SUPERBUILD)
	RETURN()
	###### RETURN_SUPERBUILD ######
endif()

#-----------------------------------------------------------------------------
# Add the Plugin directories
if(SIMVASCULAR_PLUGIN_LIBRARY_DIRS)
	foreach(dir ${SIMVASCULAR_PLUGIN_LIBRARY_DIRS})
		include_directories("${dir}")
		set(SIMVASCULAR_INCLUDE_DIRS_WORK ${SIMVASCULAR_INCLUDE_DIRS_WORK} "Plugins/${dir}")
	endforeach()

	message(STATUS "${INDENTS}Plugin Libraries")
	foreach(dir ${SIMVASCULAR_PLUGIN_LIBRARY_DIRS})
		message(STATUS "${INDENTS}    ${dir}")
		add_subdirectory("${dir}")
	endforeach()
endif()

if(SIMVASCULAR_PLUGIN_EXECUTABLE_DIRS)
	message(STATUS "${INDENTS}Plugin Executables")
	foreach(dir ${SIMVASCULAR_PLUGIN_EXECUTABLE_DIRS})
		message(STATUS "	${dir}")
		add_subdirectory("${dir}")
	endforeach()
endif()
if(SIMVASCULAR_INCLUDE_DIRS_WORK)
	list(REMOVE_DUPLICATES SIMVASCULAR_INCLUDE_DIRS_WORK)
endif()
set(SIMVASCULAR_INCLUDE_DIRS ${SIMVASCULAR_INCLUDE_DIRS_WORK} CACHE INTERNAL "" FORCE)
