#-----------------------------------------------------------------------------
# Legacy Level Sets
#-----------------------------------------------------------------------------

set(lib lib_simvascular_lset)
LIST(APPEND CORELIBS ${lib})

SET(CXXSRCS cvLevelSet.cxx cvLevelSetNode.cxx 
    cvLevelSetStructuredGrid.cxx cvLevelSetDenseGrid.cxx
    cvLevelSetSparseGrid.cxx cvLevelSetVelocity.cxx
    cvLevelSetVelocityKGI.cxx cvLevelSetVelocityConstant.cxx
    cvLevelSetVelocityThreshold.cxx cvLevelSetVelocityPotential.cxx
    cvLevelSetVelocityExponentialDecay.cxx
    cvLevelSetVelocitySmooth.cxx cvLevelSetVelocityImage.cxx 
    )
SET(HDRS cvLevelSet.h cvLevelSetNode.h
    cvLevelSetStructuredGrid.h cvLevelSetDenseGrid.h 
    cvLevelSetSparseGrid.h cvLevelSetVelocity.h 
    cvLevelSetVelocityKGI.h cvLevelSetVelocityConstant.h
    cvLevelSetVelocityThreshold.h cvLevelSetVelocityPotential.h
    cvLevelSetVelocityExponentialDecay.h cvLevelSetVelocitySmooth.h 
    cvLevelSetVelocityImage.h
    )

add_library(${lib} ${CXXSRCS} cv_LsetCore_init.cxx cv_LsetV_init.cxx)

target_link_libraries(${lib} 
   ${ITK_LIBRARIES} ${VTK_LIBRARIES} 
   ${TCL_LIBRARY} ${TK_LIBRARY} 
   ${GLOBAL_LIBRARIES}
   lib_simvascular_globals lib_simvascular_image)

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
