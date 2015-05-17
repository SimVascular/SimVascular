#-----------------------------------------------------------------------------
# Image 
#-----------------------------------------------------------------------------

set(lib lib_simvascular_globals)
LIST(APPEND CORELIBS ${lib})

include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})

set(CXXSRCS cv_globals.cxx)
set(HDRS cv_globals.h)

add_library(${lib} ${CXXSRCS})

target_link_libraries(${lib} 
   ${GLOBAL_LIBRARIES})

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