#-----------------------------------------------------------------------------
# Plugin - ITK Utils
#-----------------------------------------------------------------------------
set(lib lib_simvascular_itkutils)
LIST(APPEND PLUGINLIBS ${lib})

SET(CXXSRCS cvITKUtils.cxx)
SET(HDRS cvITKUtils.cxx)
add_library(${lib} ${CXXSRCS} cv_ITKUtils_init.cxx)
    
target_link_libraries(${lib} ${ITK_LIBRARIES} ${VTK_LIBRARIES})
if(SIMVASCULAR_INSTALL_LIBS)
install(TARGETS ${lib}
    RUNTIME DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR} COMPONENT PluginExecutables
    LIBRARY DESTINATION ${SIMVASCULAR_INSTALL_LIBRARY_DIR} COMPONENT PluginLibraries
    ARCHIVE DESTINATION ${SIMVASCULAR_INSTALL_ARCHIVE_DIR} COMPONENT PluginLibraries
    )
endif()
if(SIMVASCULAR_INSTALL_HEADERS)
install(FILES ${HDRS}
    DESTINATION ${SIMVASCULAR_INSTALL_INCLUDE_DIR}/plugins/itk/util COMPONENT PluginHeaders
    )
endif()