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
 
set(lib lib_simvascular_tet_adaptor)
LIST(APPEND CORELIBS ${lib})

include_directories(${SimVascular_SOURCE_DIR}/FlowSolvers/ThreeDSolver/SolverIO/)
include_directories(${FLOWSOLVER_OPTIONS_HEADER_PATH})
include_directories(${TETGEN_INCLUDE_DIR})

set(HDRS cvTetGenAdapt.h cv_tetgen_adapt_init.h)
SET(CXXSRCS cvTetGenAdapt.cxx cv_tetgen_adapt_init.cxx)

add_library(${lib} ${CXXSRCS})

target_link_libraries(${lib}
			${GLOBAL_LIBRARIES}
			${ZLIB_LIBRARY} ${VTK_LIBRARIES}
			${TCL_LIBRARY} ${TK_LIBRARY}
			${INTELRUNTIME_LIBRARIES}
			lib_simvascular_adaptor
			lib_simvascular_mesh
			lib_simvascular_solid			
			lib_simvascular_sysgeom	
			lib_simvascular_solverio
			lib_simvascular_globals 
			lib_simvascular_utils
			lib_simvascular_tetgen_mesh
)


if(SIMVASCULAR_INSTALL_LIBS)
  install(TARGETS ${lib}
    RUNTIME DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR} COMPONENT PluginExecutables
    LIBRARY DESTINATION ${SIMVASCULAR_INSTALL_LIBRARY_DIR} COMPONENT PluginLibraries
    ARCHIVE DESTINATION ${SIMVASCULAR_INSTALL_ARCHIVE_DIR} COMPONENT PluginLibraries
    )
endif()
if(SIMVASCULAR_INSTALL_HEADERS)
  install(FILES ${HDRS}
    DESTINATION ${SIMVASCULAR_INSTALL_INCLUDE_DIR}/plugins/tetadapt COMPONENT PluginHeaders
    )
endif()
