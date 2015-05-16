#-----------------------------------------------------------------------------
# Repository
#-----------------------------------------------------------------------------

set(lib lib_simvascular_repository)
LIST(APPEND CORELIBS ${lib})

include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})

SET(CXXSRCS cvRepository.cxx cvRepositoryData.cxx 
    cvDataObject.cxx cvPolyData.cxx 
    cvStrPts.cxx cvUnstructuredGrid.cxx
    cv_repos_init.cxx cvDataSet.cxx
    )
SET(HDRS cvRepository.h cvRepositoryData.h
    cvDataObject.h cvPolyData.h
    cvStrPts.h cvUnstructuredGrid.h
    cv_repos_init.h cvDataSet.h
    )

add_library(${lib} ${CXXSRCS})

target_link_libraries(${lib} 
   ${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY} 
   ${GLOBAL_LIBRARIES}
   lib_simvascular_globals lib_simvascular_utils
   )

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
