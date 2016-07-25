#-----------------------------------------------------------------------------
# PostProcessing
#-----------------------------------------------------------------------------

set(lib lib_simvascular_post)
LIST(APPEND CORELIBS ${lib})

SET(CXXSRCS cvConvertVisFiles.cxx cvCalculateWallShearStress.cxx
  cvCalculateTKE.cxx cv_post_init.cxx
            )
SET(HDRS    cvConvertVisFiles.h cvCalculateWallShearStress.h
  cvCalculateTKE.h cv_post_init.h
            )

add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS})

target_link_libraries(${lib}
			${VTK_LIBRARIES} ${TCL_LIBRARY}
			${TK_LIBRARY}
			${GLOBAL_LIBRARIES}
			${ZLIB_LIBRARY}
			lib_simvascular_globals lib_simvascular_repository)

if(SV_INSTALL_LIBS)
	install(TARGETS ${lib}
	    RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT CoreExecutables
		LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT CoreLibraries
		ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT CoreLibraries
		)
endif()
if(SV_INSTALL_HEADERS)
	install(FILES ${HDRS}
		DESTINATION ${SV_INSTALL_INCLUDE_DIR}/core COMPONENT CoreHeaders
		)
endif()
