project(org_sv_gui_qt_modeling)

set(lib org_sv_gui_qt_modeling)

if(SV_USE_MITK_CONFIG)
  mitk_create_plugin(
    EXPORT_DIRECTIVE SV_QT_MODELING
    EXPORTED_INCLUDE_SUFFIXES src
    MODULE_DEPENDS org_sv_gui_qt_segmentation org_sv_gui_qt_datamanager MitkQtWidgets ${SV_LIB_MODULE_PATH_NAME} ${SV_LIB_MODULE_SEGMENTATION_NAME} ${SV_LIB_MODULE_MODEL_NAME} ${SV_LIB_MODULE_MESH_NAME} ${SV_LIB_MODULE_PROJECTMANAGEMENT_NAME}
  )
else()
  simvascular_create_plugin(
    EXPORT_DIRECTIVE SV_QT_MODELING
    MODULE_DEPENDS MitkQtWidgets org_sv_gui_qt_segmentation org_sv_gui_qt_datamanager ${VTK_LIBRARIES} ${SV_LIB_MODULE_PATH_NAME} ${SV_LIB_MODULE_SEGMENTATION_NAME} ${SV_LIB_MODULE_MODEL_NAME} ${SV_LIB_MODULE_MESH_NAME} ${SV_LIB_MODULE_PROJECTMANAGEMENT_NAME}
  )
endif()

if(SV_INSTALL_LIBS)
    install(TARGETS ${lib}
        RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR}/plugins COMPONENT CoreExecutables
        LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR}/plugins COMPONENT CoreLibraries
        ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR}/plugins COMPONENT CoreLibraries
    )
endif()
