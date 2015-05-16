##=========================================================================
 #
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
 #
 #=========================================================================#

# Set c++ sources
SET(CXXSRCS cmd.cxx displacements.cxx helpers.cxx supre-cmds.cxx supre.cxx
	presolver-vtk-cmds.cxx)

# Add c++ sources for variable wall module
if(ThreeDSolver_USE_VARWALL)
	SET(CXXSRCS ${CXXSRCS} Laplace_Solve.cxx Transient_Laplace_Solve.cxx displacements_var_prop.cxx)
endif()

# Add c sources
set(HDRS cmd.h)
SET(CSRCS directsolve.c)

# Add fortran sources
SET(FSRCS stannspcg.f)

# Add fortran sources for variable wall module
if(ThreeDSolver_USE_VARWALL)
	SET(FSRCS ${FSRCS} gmresfortran.f90)
endif()

# Visual Studio Linker Flags



ADD_LIBRARY(presolver_fortran_lib ${FSRCS})

simvascular_add_executable(${PRESOLVER_EXE}
	SRCS ${CXXSRCS} ${CSRCS}
	DEV_SCRIPT_NAME "mypre" 
	INSTALL_SCRIPT_NAME "svpre"
	INSTALL_COMPONENT ModuleExecutables
	INSTALL_DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR})

TARGET_LINK_LIBRARIES(${PRESOLVER_EXE} presolver_fortran_lib
	${GLOBAL_LIBRARIES} ${INTELRUNTIME_LIBRARIES}
	${METIS_LIBRARY} ${SPARSE_LIBRARY}
	${NSPCG_LIBRARY} ${VTK_LIBRARIES}
	lib_simvascular_solverio)

if(NOT MSVC)
	SET_TARGET_PROPERTIES(presolver_fortran_lib PROPERTIES LINKER_LANGUAGE Fortran)
	SET_TARGET_PROPERTIES(${PRESOLVER_EXE} PROPERTIES LINKER_LANGUAGE Fortran)
endif()
if(SIMVASCULAR_INSTALL_LIBS)
	install(TARGETS presolver_fortran_lib
		RUNTIME DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR} COMPONENT ModuleExecutables
		LIBRARY DESTINATION ${SIMVASCULAR_INSTALL_LIBRARY_DIR} COMPONENT ModuleLibraries
		ARCHIVE DESTINATION ${SIMVASCULAR_INSTALL_ARCHIVE_DIR} COMPONENT ModuleLibraries
		)
endif()
if(SIMVASCULAR_INSTALL_HEADERS)
	install(FILES ${HDRS}
		DESTINATION ${SIMVASCULAR_INSTALL_INCLUDE_DIR}/core COMPONENT CoreHeaders
		)
endif()


