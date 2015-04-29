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

if(Licensed_ADAPTOR)
	set(SIMVASCULAR_LICENSED_EXECUTABLE_DIRS ${SIMVASCULAR_LICENSED_EXECUTABLE_DIRS} Adaptor)
endif()

if(Licensed_MESHSIM OR Licensed_MESHSIM_DISCRETE_SOLIDMODEL)
	set(SIMVASCULAR_LICENSED_LIBRARY_DIRS ${SIMVASCULAR_LICENSED_LIBRARY_DIRS} MeshSimMeshObject)
endif()

if(NOT SimVascular_EXCLUDE_SOLID_MODEL)
	if(Licensed_PARASOLID)
		set(SIMVASCULAR_LICENSED_LIBRARY_DIRS ${SIMVASCULAR_LICENSED_LIBRARY_DIRS} ParasolidSolidModel)
	endif()

	if(Licensed_MESHSIM_DISCRETE_SOLIDMODEL)
		set(SIMVASCULAR_LICENSED_LIBRARY_DIRS ${SIMVASCULAR_LICENSED_LIBRARY_DIRS} MeshSimDiscreteSolidModel)
	endif()
endif()
if(SimVascular_SUPERBUILD)
	RETURN()
	###### RETURN_SUPERBUILD ######
endif()

#-----------------------------------------------------------------------------
# Add the Core directories
if(SIMVASCULAR_LICENSED_LIBRARY_DIRS)
	foreach(dir ${SIMVASCULAR_LICENSED_LIBRARY_DIRS})
		include_directories("${dir}")
		set(SIMVASCULAR_INCLUDE_DIRS_WORK ${SIMVASCULAR_INCLUDE_DIRS_WORK} "Licensed/${dir}")
	endforeach()

	message(STATUS "${INDENTS}Licensed Libraries:")
	foreach(dir ${SIMVASCULAR_LICENSED_LIBRARY_DIRS})
		message(STATUS "${INDENTS}	${dir}")
		add_subdirectory("${dir}")
	endforeach()

endif()
if(SIMVASCULAR_LICENSED_EXECUTABLE_DIRS)
	message(STATUS "Licensed Executables")
	foreach(dir ${SIMVASCULAR_LICENSED_EXECUTABLE_DIRS})
		message(STATUS "${INDENTS}	${dir}")
		add_subdirectory("${dir}")
	endforeach()
endif()

if(SIMVASCULAR_INCLUDE_DIRS_WORK)
	list(REMOVE_DUPLICATES SIMVASCULAR_INCLUDE_DIRS_WORK)
endif()
set(SIMVASCULAR_INCLUDE_DIRS "${SIMVASCULAR_INCLUDE_DIRS_WORK}" CACHE INTERNAL "" FORCE)
