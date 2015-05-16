#-----------------------------------------------------------------------------
# Image 
#-----------------------------------------------------------------------------

set(lib lib_simvascular_image)
LIST(APPEND CORELIBS ${lib})

include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})

SET(CXXSRCS cv_image.cxx cv_read_header.cxx 
    cv_decode.cxx cv_calc_correction_eqn.cxx
    cv_img_threshold.cxx cvDistanceMap.cxx
    cv_mask_image_in_place.cxx
    )
SET(HDRS cv_image.h cv_img_offsets.h 
    cv_decode.h cv_calc_correction_eqn.h
    cv_img_threshold.h cvDistanceMap.h 
    cv_mask_image_in_place.h
    )

add_library(${lib} ${CXXSRCS})

target_link_libraries(${lib}
   ${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
   ${GLOBAL_LIBRARIES}
   lib_simvascular_globals)

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