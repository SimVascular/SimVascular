#-----------------------------------------------------------------------------
# Repository
#-----------------------------------------------------------------------------

set(lib ${SV_LIB_REPOSITORY_NAME})
set(export_directive repository)
set(export_file_name svRepositoryExports.h)

LIST(APPEND CORELIBS ${lib})
if(SV_USE_PYTHON)
	include_directories(${PYTHON_INCLUDE_PATH}
	  ${PYTHON_INCLUDE_DIR}
	  ${PYTHON_INCLUDE_DIRS})
endif()

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

add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS} cv_repos_init_py.cxx cv_repos_init.cxx)

# Set up for exports
string(TOUPPER ${export_directive} EXPORT_NAME)
set(_export_macro_names
  EXPORT_MACRO_NAME SV_EXPORT_${EXPORT_NAME}
  NO_EXPORT_MACRO_NAME SV_EXPORT_${EXPORT_NAME}_NO_EXPORT
  DEPRECATED_MACRO_NAME SV_EXPORT_${EXPORT_NAME}_DEPRECATED
  NO_DEPRECATED_MACRO_NAME SV_EXPORT_${EXPORT_NAME}_NO_DEPRECATED
  )

# Generate export headers
generate_export_header(${lib}
  ${_export_macro_names}
  EXPORT_FILE_NAME ${export_file_name})


target_link_libraries(${lib}
  ${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
  ${SV_LIB_GLOBALS_NAME} ${SV_LIB_UTILS_NAME}
  )
if(SV_USE_PYTHON)
  target_link_libraries(${lib} ${PYTHON_LIBRARY})
endif()

if(SV_INSTALL_LIBS)
	install(TARGETS ${lib}
	  RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT CoreExecutables
		LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT CoreLibraries
		ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT CoreLibraries
		)
endif()
if(SV_INSTALL_HEADERS)
  install(FILES ${HDRS} ${CMAKE_CURRENT_BINARY_DIR}/${export_file_name}
		DESTINATION ${SV_INSTALL_INCLUDE_DIR}/core COMPONENT CoreHeaders
		)
endif()

PYTHON_ADD_MODULE(pyRepository cv_repos_init_py.cxx)
target_link_libraries(pyRepository ${lib} ${PYTHON_LIBRARY})