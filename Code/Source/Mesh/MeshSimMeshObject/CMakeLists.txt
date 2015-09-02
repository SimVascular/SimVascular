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
SET(lib lib_simvascular_meshsim_mesh) 
SET(CXXSRCS cvMeshSimMeshObject.cxx
          cvMeshSimMeshSystem.cxx stats.cxx scorecXYZ.cxx)

add_library(${lib} ${CXXSRCS} cv_meshsim_mesh_init.cxx)

if(Licensed_ADAPTOR)
    include_directories(${SimVascular_SOURCE_DIR}/FlowSolvers/ThreeDSolver/SolverIO/)
endif()

target_link_libraries(${lib}
			${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
			${PARASOLID_LIBRARIES} ${MESHSIM_LIBRARIES}
			${ZLIB_LIBRARY} ${GLOBAL_LIBRARIES}
			lib_simvascular_globals lib_simvascular_mesh 
			lib_simvascular_utils lib_simvascular_sysgeom
			lib_simvascular_repository lib_simvascular_solid
			)
		#MAKE SURE LIBS ARE SPLIT UP WITH COMPILE FLAGS ADPTOR
if(Licensed_PARASOLID)
    target_link_libraries(${lib} lib_simvascular_parasolid)
endif()

if(Licensed_ADAPTOR)
    target_link_libraries(${lib} lib_simvascular_meshsim_adaptor
			          lib_simvascular_solverio)
endif()


