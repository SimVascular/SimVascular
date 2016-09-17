
# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(lib ${SV_LIB_QTGUI_PLUGIN_TEST_NAME})
file(GLOB HDRS *.h)
file(GLOB CXXSRCS *.cxx)
file(GLOB UI_FILES *.ui)
file(GLOB QRC_FILES *.qrc)
set(MOC_HDRS svTest.h svTestPluginActivator.h)

qt5_wrap_ui(UISrcs ${UI_FILES})
qt5_add_resources(QRCSrcs ${QRC_FILES})
qt5_wrap_cpp(MOCSrcs ${MOC_HDRS})

add_library(${lib} SHARED ${HDRS} ${CXXSRCS} ${UISrcs} ${QRCSrcs} ${MOCSrcs})
target_link_libraries(${lib} ${MITK_LIBRARIES} ${SV_LIB_QTGUI_MODULE_APPBASE_NAME})
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

install(FILES ${HDRS} ${MOC_HDRS}
        DESTINATION ${SV_INSTALL_INCLUDE_DIR}/core COMPONENT CoreHeaders
        )
