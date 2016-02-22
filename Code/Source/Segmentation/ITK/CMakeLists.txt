
#-----------------------------------------------------------------------------
# Plugin - ITK Level Sets
#-----------------------------------------------------------------------------
set(lib lib_simvascular_cvitk2d)
set(lib2 lib_simvascular_cvitk3d)
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
add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS} cv_ITKLset2d_init.cxx)
add_library(${lib2} ${SV_LIBRARY_TYPE} cv_ITKLset3d_init.cxx)

TARGET_LINK_LIBRARIES(${lib}
 lib_simvascular_itkutils
 lib_simvascular_repository
 ${ITK_LIBRARIES} ${VTK_LIBRARIES}
 ${GLOBAL_LIBRARIES})
TARGET_LINK_LIBRARIES(${lib2}
 lib_simvascular_itkutils
 lib_simvascular_repository
 ${ITK_LIBRARIES} ${VTK_LIBRARIES}
 ${GLOBAL_LIBRARIES}
 ${lib})
if(SV_INSTALL_LIBS)
    install(TARGETS ${lib} ${lib2}
        RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT PluginExecutables
        LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT PluginLibraries
        ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT PluginLibraries
        )
endif()
if(SV_INSTALL_HEADERS)
    install(FILES ${HDRS}
        DESTINATION ${SV_INSTALL_INCLUDE_DIR}/plugins/itk/ COMPONENT PluginHeaders
        )
endif()



