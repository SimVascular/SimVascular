project(org_sv_gui_qt_projectmanager)

set(lib org_sv_gui_qt_projectmanager)

if(SV_USE_MITK_CONFIG)
  mitk_create_plugin(
    EXPORT_DIRECTIVE SV_QT_PROJECTMANAGER
    EXPORTED_INCLUDE_SUFFIXES src
    MODULE_DEPENDS MitkQtWidgets ${SV_LIB_MODULE_PROJECTMANAGEMENT_NAME}
  )
else()
  simvascular_create_plugin(
    EXPORT_DIRECTIVE SV_QT_PROJECTMANAGER
    MODULE_DEPENDS MitkQtWidgets ${SV_LIB_MODULE_PROJECTMANAGEMENT_NAME}
  )
endif()

if(SV_INSTALL_LIBS)
    install(TARGETS ${lib}
        RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR}/plugins COMPONENT CoreExecutables
        LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR}/plugins COMPONENT CoreLibraries
        ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR}/plugins COMPONENT CoreLibraries
    )
endif()

