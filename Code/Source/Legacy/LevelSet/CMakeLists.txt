#-----------------------------------------------------------------------------
# Legacy Level Sets
#-----------------------------------------------------------------------------

set(lib ${SV_LIB_LSET_NAME})
set(export_directive lset)
set(export_file_name svLSetExports.h)

list(APPEND CORELIBS ${lib})

set(CXXSRCS cvLevelSet.cxx cvLevelSetNode.cxx
  cvLevelSetStructuredGrid.cxx cvLevelSetDenseGrid.cxx
  cvLevelSetSparseGrid.cxx cvLevelSetVelocity.cxx
  cvLevelSetVelocityKGI.cxx cvLevelSetVelocityConstant.cxx
  cvLevelSetVelocityThreshold.cxx cvLevelSetVelocityPotential.cxx
  cvLevelSetVelocityExponentialDecay.cxx
  cvLevelSetVelocitySmooth.cxx cvLevelSetVelocityImage.cxx
  )
set(HDRS cvLevelSet.h cvLevelSetNode.h
  cvLevelSetStructuredGrid.h cvLevelSetDenseGrid.h
  cvLevelSetSparseGrid.h cvLevelSetVelocity.h
  cvLevelSetVelocityKGI.h cvLevelSetVelocityConstant.h
  cvLevelSetVelocityThreshold.h cvLevelSetVelocityPotential.h
  cvLevelSetVelocityExponentialDecay.h cvLevelSetVelocitySmooth.h
  cvLevelSetVelocityImage.h
  )

add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS} cv_LsetCore_init.cxx cv_LsetV_init.cxx cv_Lset_init.cxx)

target_link_libraries(${lib}
  ${ITK_LIBRARIES} ${VTK_LIBRARIES}
  ${TCL_LIBRARY} ${TK_LIBRARY}
  ${SV_LIB_GLOBALS_NAME} ${SV_LIB_IMAGE_NAME})

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
