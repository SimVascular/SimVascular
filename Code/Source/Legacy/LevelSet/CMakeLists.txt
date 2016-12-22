#-----------------------------------------------------------------------------
# Legacy Level Sets
#-----------------------------------------------------------------------------

set(lib ${SV_LIB_LSET_NAME})

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
