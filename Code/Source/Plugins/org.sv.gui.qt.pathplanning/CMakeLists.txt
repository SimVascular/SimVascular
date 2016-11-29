project(org_sv_gui_qt_pathplanning)

set(lib org_sv_gui_qt_pathplanning)

if(SV_USE_MITK_CONFIG)
  mitk_create_plugin(
    EXPORT_DIRECTIVE SV_QT_PATHPLANNING
    EXPORTED_INCLUDE_SUFFIXES src
    MODULE_DEPENDS MitkQtWidgets svPath svSegmentation svQtWidgets
  )
else()
  SimVascularCreatePlugin(
    EXPORT_DIRECTIVE SV_QT_PATHPLANNING
    MODULE_DEPENDS MitkQtWidgets svPath svSegmentation svQtWidgets
  )
endif()
if(SV_INSTALL_LIBS)
    install(TARGETS ${lib}
        RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT CoreExecutables
        LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT CoreLibraries
        ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT CoreLibraries
    )
endif()
