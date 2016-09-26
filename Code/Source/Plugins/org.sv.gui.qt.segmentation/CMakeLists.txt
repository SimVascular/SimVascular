project(org_sv_gui_qt_segmentation)

set(lib org_sv_gui_qt_segmentation)

mitk_create_plugin(
  EXPORT_DIRECTIVE SV_QT_SEGMENTATION
  EXPORTED_INCLUDE_SUFFIXES src
  MODULE_DEPENDS MitkQtWidgets svPath svSegmentation svModel svQtWidgets
)

if(SV_INSTALL_LIBS)
    install(TARGETS ${lib}
        RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT CoreExecutables
        LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT CoreLibraries
        ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT CoreLibraries
    )
endif()
