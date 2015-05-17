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
 
#-----------------------------------------------------------------------------
# Plugin - TetGen Mesh
#-----------------------------------------------------------------------------
set(lib lib_simvascular_tetgen_mesh)
LIST(APPEND PLUGINLIBS ${lib})
include_directories(${TETGEN_INCLUDE_DIR})

SET(CXXSRCS cvTetGenMeshObject.cxx cvTetGenMeshSystem.cxx cv_tetgenmesh_utils.cxx)
SET(HDRS cvTetGenMeshObject.h cvTetGenMeshSystem.h cv_tetgenmesh_utils.h)

add_library(${lib} ${CXXSRCS} cv_tetgen_mesh_init.cxx)

if(Plugin_VMTK)
	SET(OPTLIBS lib_simvascular_vmtk)
endif()

target_link_libraries(${lib} 
   ${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY} 
   ${OPTLIBS}	
   ${PARASOLID_LIBRARIES} 
   ${GLOBAL_LIBRARIES}
   ${TETGEN_LIBRARY}
   lib_simvascular_globals lib_simvascular_utils
   lib_simvascular_polydatasolid
   )

set_target_properties(${lib} PROPERTIES "COMPILE_DEFINITIONS" TETLIBRARY)

if(SIMVASCULAR_INSTALL_LIBS)
	install(TARGETS ${lib}
		RUNTIME DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR} COMPONENT PluginExecutables
		LIBRARY DESTINATION ${SIMVASCULAR_INSTALL_LIBRARY_DIR} COMPONENT PluginLibraries
		ARCHIVE DESTINATION ${SIMVASCULAR_INSTALL_ARCHIVE_DIR} COMPONENT PluginLibraries
		)
endif()
if(SIMVASCULAR_INSTALL_HEADERS)
	install(FILES ${HDRS}
		DESTINATION ${SIMVASCULAR_INSTALL_INCLUDE_DIR}/plugins/vmtk COMPONENT PluginHeaders
		)
endif()
