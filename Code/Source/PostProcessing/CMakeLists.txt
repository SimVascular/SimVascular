#-----------------------------------------------------------------------------
# PostProcessing
#-----------------------------------------------------------------------------

set(lib lib_simvascular_post)
set(lib2 lib_simvascular_gdscpost)
LIST(APPEND CORELIBS ${lib})

SET(CXXSRCS cvConvertVisFiles.cxx cvCalculateWallShearStress.cxx
  cvCalculateTKE.cxx
            )
SET(HDRS    cvConvertVisFiles.h cvCalculateWallShearStress.h
  cvCalculateTKE.h
            )

add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS})
add_library(${lib2} ${SV_LIBRARY_TYPE} cv_post_init.cxx)

target_link_libraries(${lib}
			${VTK_LIBRARIES} ${TCL_LIBRARY}
			${TK_LIBRARY}
			${GLOBAL_LIBRARIES}
			${ZLIB_LIBRARY}
			lib_simvascular_globals lib_simvascular_repository)
target_link_libraries(${lib2}
			${VTK_LIBRARIES} ${TCL_LIBRARY}
			${TK_LIBRARY}
			${GLOBAL_LIBRARIES}
			${ZLIB_LIBRARY}
			${lib}
			lib_simvascular_globals lib_simvascular_repository)

if(SV_INSTALL_LIBS)
	install(TARGETS ${lib} ${lib2}
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
