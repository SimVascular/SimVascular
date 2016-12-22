#-----------------------------------------------------------------------------
# Image
#-----------------------------------------------------------------------------

set(lib ${SV_LIB_GLOBALS_NAME})
LIST(APPEND CORELIBS ${lib})

include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})

set(CXXSRCS cv_globals.cxx)
set(HDRS cv_globals.h)

add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS})

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
