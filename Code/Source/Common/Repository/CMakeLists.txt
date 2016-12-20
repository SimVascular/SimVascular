#-----------------------------------------------------------------------------
# Repository
#-----------------------------------------------------------------------------

set(lib ${SV_LIB_REPOSITORY_NAME})
LIST(APPEND CORELIBS ${lib})

include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})

SET(CXXSRCS cvRepository.cxx cvRepositoryData.cxx
  cvDataObject.cxx cvPolyData.cxx
  cvStrPts.cxx cvUnstructuredGrid.cxx
  cvDataSet.cxx
  )

SET(HDRS cvRepository.h cvRepositoryData.h
  cvDataObject.h cvPolyData.h
  cvStrPts.h cvUnstructuredGrid.h
  cvDataSet.h
  )

add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS} cv_repos_init.cxx)

target_link_libraries(${lib}
  ${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
  ${SV_LIB_GLOBALS_NAME} ${SV_LIB_UTILS_NAME}
  )

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
