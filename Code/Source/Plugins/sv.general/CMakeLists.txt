
# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(lib ${SV_LIB_QTGUI_PLUGIN_GENERAL_NAME})

set(HDRS
    svRedo.h
    svUndo.h
    svOpenFile.h
    svSaveScene.h
    svCloseAll.h
    svExit.h
    svGeneralInit.h
    svProjectCreate.h
    svProjectOpen.h
    svProjectSaveAll.h
    svProjectPluginActivator.h
)

set(CXXSRCS
    svRedo.cxx
    svUndo.cxx
    svOpenFile.cxx
    svSaveScene.cxx
    svCloseAll.cxx
    svExit.cxx
    svGeneralInit.cxx
    svProjectCreate.cxx
    svProjectOpen.cxx
    svProjectSaveAll.cxx
    svProjectPluginActivator.cxx
)

set(MOC_HDRS ${HDRS})

set(UI_FILES
    svProjectCreate.ui
)

set(QRC_FILES
    resources/svgeneral.qrc
)

qt5_wrap_ui(UISrcs ${UI_FILES})
qt5_add_resources(QRCSrcs ${QRC_FILES})
qt5_wrap_cpp(MOCSrcs ${MOC_HDRS})

add_library(${lib} SHARED ${HDRS} ${CXXSRCS} ${UISrcs} ${QRCSrcs}  ${MOCSrcs})
target_link_libraries(${lib} ${MITK_LIBRARIES} ${ITK_LIBRARIES} ${SV_LIB_QTGUI_MODULE_APPBASE_NAME} ${SV_LIB_QTGUI_MODULE_PROJECTMANAGEMENT_NAME})
set_target_properties(${lib}
    PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/plugins
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/plugins
)

if(MSVC)
    foreach( OUTPUTCONFIG ${CMAKE_CONFIGURATION_TYPES} )
        string( TOUPPER ${OUTPUTCONFIG} OUTPUTCONFIG )
        set_target_properties(${lib}
            PROPERTIES
            LIBRARY_OUTPUT_DIRECTORY_${OUTPUTCONFIG} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/plugins
            RUNTIME_OUTPUT_DIRECTORY_${OUTPUTCONFIG} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/plugins
        )
    endforeach( OUTPUTCONFIG CMAKE_CONFIGURATION_TYPES )
endif()

install(TARGETS ${lib}
        RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR}/plugins COMPONENT CoreExecutables
        LIBRARY DESTINATION ${SV_INSTALL_RUNTIME_DIR}/plugins COMPONENT CoreLibraries
        ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT CoreLibraries
        )

install(FILES ${HDRS}
        DESTINATION ${SV_INSTALL_INCLUDE_DIR}/core COMPONENT CoreHeaders
        )
