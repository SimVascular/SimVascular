#-----------------------------------------------------------------------------
# PostProcessing
#-----------------------------------------------------------------------------

set(lib ${SV_LIB_POST_NAME})
list(APPEND CORELIBS ${lib})

set(CXXSRCS cvConvertVisFiles.cxx cvCalculateWallShearStress.cxx cvCalculateTKE.cxx)
set(HDRS cvConvertVisFiles.h cvCalculateWallShearStress.h cvCalculateTKE.h)

add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS} cv_post_init.cxx)

target_link_libraries(${lib}
	${VTK_LIBRARIES} ${TCL_LIBRARY}
	${TK_LIBRARY}
	${ZLIB_LIBRARY}
	${SV_LIB_GLOBALS_NAME} ${SV_LIB_REPOSITORY_NAME})

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
