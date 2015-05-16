
#-----------------------------------------------------------------------------
# Plugin - ITK Level Sets
#-----------------------------------------------------------------------------
set(lib lib_simvascular_cvitk)
LIST(APPEND PLUGINLIBS ${lib})

include_directories(./Include)
add_subdirectory(Include)

# Contains Various ITK, VTK and SV code needed for ITK Level Sets
include_directories(./Util)
add_subdirectory(Util)

# Contains Pure ITK Code, Mostly Level Set Code
include_directories(./ITKCode)
add_subdirectory(ITKCode)

SET(CXXSRCS cvITKLevelSet.cxx)
SET(HDRS cvITKLevelSet.h cvITKLevelSetBase.h)
add_library(${lib} ${CXXSRCS} cv_ITKLset2d_init.cxx cv_ITKLset3d_init.cxx)

TARGET_LINK_LIBRARIES(${lib}
 lib_simvascular_itkutils
 ${ITK_LIBRARIES} ${VTK_LIBRARIES} 
 ${GLOBAL_LIBRARIES})
if(SIMVASCULAR_INSTALL_LIBS)
    install(TARGETS ${lib}
        RUNTIME DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR} COMPONENT PluginExecutables
        LIBRARY DESTINATION ${SIMVASCULAR_INSTALL_LIBRARY_DIR} COMPONENT PluginLibraries
        ARCHIVE DESTINATION ${SIMVASCULAR_INSTALL_ARCHIVE_DIR} COMPONENT PluginLibraries
        )
endif()
if(SIMVASCULAR_INSTALL_HEADERS)
    install(FILES ${HDRS}
        DESTINATION ${SIMVASCULAR_INSTALL_INCLUDE_DIR}/plugins/itk/ COMPONENT PluginHeaders
        )
endif()



