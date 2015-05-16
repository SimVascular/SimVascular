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

# TODO: Remove this! needs to be refactored
include_directories(${SimVascular_SOURCE_DIR}/FlowSolvers/ThreeDSolver/SolverIO/)
include_directories(${FLOWSOLVER_OPTIONS_HEADER_PATH})

SET(CXXSRCS cvAdaptCore.cxx main.cxx
	cvAdaptHelpers.cxx cvAdaptSF.cxx)


SET(FSRCS pythag.f tred2EISPACK.f tql2.f lubksb.f ludcmp.f)
add_library(lib_adaptor ${FSRCS})
target_link_libraries(lib_adaptor ${MPI_EXTRA_LIBRARY} ${MPI_Fortran_LIBRARIES})

simvascular_add_executable(${ADAPTOR_EXE} 
	SRCS ${CXXSRCS} 
	DEV_SCRIPT_NAME "myadaptor" 
	INSTALL_SCRIPT_NAME "svadapt"
	INSTALL_COMPONENT LicensedExecutables
	INSTALL_DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR})


target_link_libraries(${ADAPTOR_EXE} lib_adaptor 
	${GLOBAL_LIBRARIES}
	${PARASOLID_LIBRARIES} ${MESHSIM_LIBRARIES}
	${ZLIB_LIBRARY} ${VTK_LIBRARIES}
	${TCL_LIBRARY} ${TK_LIBRARY}
	${INTELRUNTIME_LIBRARIES}
	lib_simvascular_solid			
	lib_simvascular_sysgeom	
	lib_simvascular_solverio 
	lib_simvascular_globals 
	lib_simvascular_utils
	)

if(Licensed_PARASOLID)
	target_link_libraries(${ADAPTOR_EXE} lib_simvascular_parasolid)
endif()


