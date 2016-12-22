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
set(lib ${SV_LIB_MESHSIM_MESH_NAME})
set(CXXSRCS cvMeshSimMeshObject.cxx
          cvMeshSimMeshSystem.cxx stats.cxx scorecXYZ.cxx)
set(HDRS cvMeshSimMeshObject.h
  cvMeshSimMeshSystem.h)

if(SV_USE_MESHSIM_SHARED_LIBRARIES)
  add_library(${lib} SHARED ${CXXSRCS} cv_meshsim_mesh_init.cxx)
else()
  add_library(${lib} STATIC ${CXXSRCS} cv_meshsim_mesh_init.cxx)
endif()

if(SV_USE_ADAPTOR)
  include_directories(${SV_SOURCE_DIR}/FlowSolvers/ThreeDSolver/SolverIO/)
endif()

target_link_libraries(${lib}
	${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
	${PARASOLID_LIBRARIES}
	${MESHSIM_LIBRARIES}
	${SV_LIB_MESH_NAME}
	${SV_LIB_GLOBALS_NAME}
	${SV_LIB_UTILS_NAME})

if(SV_USE_MESHSIM_DISCRETE_SHARED_LIBRARIES)
	target_link_libraries(${lib} ${SV_LIB_MESHSIM_DISCRETE_SOLID_NAME})
endif()
if(SV_USE_PARASOLID)
  target_link_libraries(${lib} ${SV_LIB_PARASOLID_SOLID_NAME})
endif()

if(SV_USE_ADAPTOR)
  target_link_libraries(${lib} ${SV_LIB_MESHSIM_ADAPTOR_NAME})
endif()

if(SV_INSTALL_LIBS)
	install(TARGETS ${lib}
		RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT PluginExecutables
		LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT PluginLibraries
		ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT PluginLibraries
		)
endif()
if(SV_INSTALL_HEADERS)
	install(FILES ${HDRS}
		DESTINATION ${SV_INSTALL_INCLUDE_DIR}/core COMPONENT PluginHeaders
		)
endif()

