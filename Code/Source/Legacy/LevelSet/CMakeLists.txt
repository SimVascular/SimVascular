#-----------------------------------------------------------------------------
# Legacy Level Sets
#-----------------------------------------------------------------------------

set(lib lib_simvascular_lset)
set(lib2 lib_simvascular_lsetcore)
set(lib3 lib_simvascular_lsetv)
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

add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS} cv_LsetCore_init.cxx cv_LsetV_init.cxx)
add_library(${lib2} ${SV_LIBRARY_TYPE} cv_LsetCore_init.cxx)
add_library(${lib3} ${SV_LIBRARY_TYPE} cv_LsetV_init.cxx)

target_link_libraries(${lib}
   ${ITK_LIBRARIES} ${VTK_LIBRARIES}
   ${TCL_LIBRARY} ${TK_LIBRARY}
   ${GLOBAL_LIBRARIES}
   lib_simvascular_globals lib_simvascular_image)
target_link_libraries(${lib2}
   ${ITK_LIBRARIES} ${VTK_LIBRARIES}
   ${TCL_LIBRARY} ${TK_LIBRARY}
   ${GLOBAL_LIBRARIES}
   ${lib}
   lib_simvascular_globals lib_simvascular_image)
target_link_libraries(${lib3}
   ${ITK_LIBRARIES} ${VTK_LIBRARIES}
   ${TCL_LIBRARY} ${TK_LIBRARY}
   ${GLOBAL_LIBRARIES}
   ${lib}
   lib_simvascular_globals lib_simvascular_image)

if(SV_INSTALL_LIBS)
	install(TARGETS ${lib} ${lib2} ${lib3}
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
