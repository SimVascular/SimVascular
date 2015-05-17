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
# PolyData Solid
#-----------------------------------------------------------------------------

set(lib lib_simvascular_polydatasolid)
LIST(APPEND CORELIBS ${lib})

SET(CXXSRCS cvPolyDataSolid.cxx cv_polydatasolid_utils.cxx 
            vtkGetBoundaryFaces.cxx
            )
SET(HDRS cvPolyDataSolid.h cv_polydatasolid_init.h 
         cv_polydatasolid_utils.h vtkGetBoundaryFaces.h
            ) 

add_library(${lib} ${CXXSRCS} cv_polydatasolid_init.cxx)

target_link_libraries(${lib} 
			${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY} 								
			${PARASOLID_LIBRARIES} 
			${GLOBAL_LIBRARIES}
			lib_simvascular_globals lib_simvascular_utils lib_simvascular_sysgeom
            )
if(Plugin_VMTK)
    target_link_libraries(${lib} lib_simvascular_vmtk)
endif()


if(SIMVASCULAR_INSTALL_LIBS)
	install(TARGETS ${lib}
	    RUNTIME DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR} COMPONENT CoreExecutables
		LIBRARY DESTINATION ${SIMVASCULAR_INSTALL_LIBRARY_DIR} COMPONENT CoreLibraries
		ARCHIVE DESTINATION ${SIMVASCULAR_INSTALL_ARCHIVE_DIR} COMPONENT CoreLibraries
		)
endif()
if(SIMVASCULAR_INSTALL_HEADERS)
	install(FILES ${HDRS}
		DESTINATION ${SIMVASCULAR_INSTALL_INCLUDE_DIR}/core COMPONENT CoreHeaders
		)
endif()

