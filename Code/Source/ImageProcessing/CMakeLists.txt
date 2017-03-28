#-----------------------------------------------------------------------------
# Image
#-----------------------------------------------------------------------------

set(lib ${SV_LIB_IMAGE_NAME})
set(export_directive image)
set(export_file_name svImageExports.h)

list(APPEND CORELIBS ${lib})

include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})

set(CXXSRCS cv_image.cxx cv_read_header.cxx
  cv_decode.cxx cv_calc_correction_eqn.cxx
  cv_img_threshold.cxx cvDistanceMap.cxx
  cv_mask_image_in_place.cxx
  )
set(HDRS cv_image.h cv_img_offsets.h
  cv_decode.h cv_calc_correction_eqn.h
  cv_img_threshold.h cvDistanceMap.h
  cv_mask_image_in_place.h
  )

add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS} cv_image_init.cxx)

target_link_libraries(${lib}
  ${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
  ${SV_LIB_GLOBALS_NAME} ${SV_LIB_GEOM_NAME})

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
