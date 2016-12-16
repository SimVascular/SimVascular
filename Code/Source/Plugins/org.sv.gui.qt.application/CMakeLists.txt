project(org_sv_gui_qt_application)

set(lib org_sv_gui_qt_application)

if(SV_USE_MITK_CONFIG)
  mitk_create_plugin(
    EXPORT_DIRECTIVE SV_QT_APPLICATION
    EXPORTED_INCLUDE_SUFFIXES src
    MODULE_DEPENDS MitkQtWidgets ${SV_LIB_MODULE_PROJECTMANAGEMENT_NAME}
  )
else()
  simvascular_create_plugin(
    EXPORT_DIRECTIVE SV_QT_APPLICATION
    MODULE_DEPENDS MitkQtWidgets ${SV_LIB_MODULE_PROJECTMANAGEMENT_NAME}
  )
endif()

target_include_directories(${lib} PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/../org.sv.gui.qt.projectmanager")

if(SV_INSTALL_LIBS)
    install(TARGETS ${lib}
        RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR}/plugins COMPONENT CoreExecutables
        LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR}/plugins COMPONENT CoreLibraries
        ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR}/plugins COMPONENT CoreLibraries
    )
endif()

