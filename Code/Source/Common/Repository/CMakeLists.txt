#-----------------------------------------------------------------------------
# Repository
#-----------------------------------------------------------------------------

set(lib lib_simvascular_repository)
LIST(APPEND CORELIBS ${lib})

include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})
if(USE_TCLPYTHON)
	include_directories(${PYTHON_INCLUDE_PATH}
		    ${PYTHON_INCLUDE_DIR}
		    ${PYTHON_INCLUDE_DIRS})
endif()

SET(CXXSRCS cvRepository.cxx cvRepositoryData.cxx
    cvDataObject.cxx cvPolyData.cxx
    cvStrPts.cxx cvUnstructuredGrid.cxx
    cv_repos_init.cxx cvDataSet.cxx
    )
#if(USE_TCLPYTHON)
#    set(CXXSRCS ${CXXSRCS} pythonModules.cxx)
#endif()

SET(HDRS cvRepository.h cvRepositoryData.h
    cvDataObject.h cvPolyData.h
    cvStrPts.h cvUnstructuredGrid.h
    cv_repos_init.h cvDataSet.h
    )
#if(USE_TCLPYTHON)
#    set(HDRS ${HDRS} pythonModules.h)
#endif()

add_library(${lib} ${CXXSRCS})

target_link_libraries(${lib}
   ${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
   ${GLOBAL_LIBRARIES}
   lib_simvascular_globals lib_simvascular_utils
   )
if(USE_TCLPYTHON)
   target_link_libraries(${lib} ${PYTHON_LIBRARIES})
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
