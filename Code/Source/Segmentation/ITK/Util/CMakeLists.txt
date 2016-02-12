#-----------------------------------------------------------------------------
# Plugin - ITK Utils
#-----------------------------------------------------------------------------
set(lib lib_simvascular_itkutils)
LIST(APPEND PLUGINLIBS ${lib})

SET(CXXSRCS cvITKUtils.cxx)
SET(HDRS cvITKUtils.cxx)
add_library(${lib} ${CXXSRCS} cv_ITKUtils_init.cxx)
    
target_link_libraries(${lib} ${ITK_LIBRARIES} ${VTK_LIBRARIES})
if(SV_INSTALL_LIBS)
install(TARGETS ${lib}
    RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT PluginExecutables
    LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT PluginLibraries
    ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT PluginLibraries
    )
endif()
if(SV_INSTALL_HEADERS)
install(FILES ${HDRS}
    DESTINATION ${SV_INSTALL_INCLUDE_DIR}/plugins/itk/util COMPONENT PluginHeaders
    )
endif()
