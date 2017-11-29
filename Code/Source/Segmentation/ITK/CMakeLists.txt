
#-----------------------------------------------------------------------------
# Plugin - ITK Level Sets
#-----------------------------------------------------------------------------
set(lib ${SV_LIB_ITK_LSET_NAME})
set(export_directive segitk)
set(export_file_name svSegITKExports.h)

list(APPEND PLUGINLIBS ${lib})

add_subdirectory(Include)

# Contains Pure ITK Code, Mostly Level Set Code
add_subdirectory(ITKCode)

set(CXXSRCS  cvITKUtils.cxx cvITKLevelSet.cxx)
set(HDRS cvITKLevelSet.h cvITKLevelSetBase.h)
add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS} cv_ITKUtils_init.cxx cv_ITKLset2d_init.cxx cv_ITKLset3d_init.cxx cv_ITKLset_init.cxx)

target_link_libraries(${lib}
 ${SV_LIB_REPOSITORY_NAME}
 ${ITK_LIBRARIES} ${VTK_LIBRARIES})

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
