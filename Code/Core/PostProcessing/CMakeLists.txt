#-----------------------------------------------------------------------------
# PostProcessing
#-----------------------------------------------------------------------------

set(lib lib_simvascular_post)
LIST(APPEND CORELIBS ${lib})

SET(CXXSRCS cvConvertVisFiles.cxx cvCalculateWallShearStress.cxx
            cvCalculateTKE.cxx
            )
SET(HDRS    cvConvertVisFiles.h cvCalculateWallShearStress.h
            cvCalculateTKE.h
            )

add_library(${lib} ${CXXSRCS})

target_link_libraries(lib_simvascular_post
			${VTK_LIBRARIES} ${TCL_LIBRARY} 
			${TK_LIBRARY} 
			${GLOBAL_LIBRARIES}
			${ZLIB_LIBRARY}
			lib_simvascular_globals)

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