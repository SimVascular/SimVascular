
# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(lib ${SV_LIB_QTGUI_PLUGIN_MITKSEGMENTATION_NAME})

set(HDRS
    QmitkDataSelectionWidget.h
    QmitkAutocrop.h
    QmitkCreatePolygonModel.h
    QmitkSegmentationView.h
    MitkSegmentationPluginActivator.h
)

set(CXXSRCS
    QmitkDataSelectionWidget.cxx
    QmitkAutocrop.cxx
    QmitkCreatePolygonModel.cxx
    QmitkSegmentationView.cxx
    MitkSegmentationPluginActivator.cxx
)

set(MOC_HDRS ${HDRS})

set(UI_FILES
    QmitkDataSelectionWidgetControls.ui
    QmitkSegmentationControls.ui
)

set(QRC_FILES
    resources/segmentation.qrc
)

qt5_wrap_ui(UISrcs ${UI_FILES})
qt5_add_resources(QRCSrcs ${QRC_FILES})
qt5_wrap_cpp(MOCSrcs ${MOC_HDRS})

add_library(${lib} SHARED ${HDRS} ${CXXSRCS} ${UISrcs} ${QRCSrcs} ${MOCSrcs})
target_link_libraries(${lib} ${MITK_LIBRARIES} ${ITK_LIBRARIES} ${SV_LIB_GLOBALS_NAME} ${SV_LIB_UTILS_NAME} ${SV_LIB_GEOM_NAME} ${SV_LIB_QTGUI_MODULE_APPBASE_NAME})
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
