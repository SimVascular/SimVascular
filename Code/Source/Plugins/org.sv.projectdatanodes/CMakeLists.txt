project(org_sv_projectdatanodes)

set(lib org_sv_projectdatanodes)

if(SV_USE_MITK_CONFIG)
  mitk_create_plugin(
    EXPORT_DIRECTIVE SV_PROJECTDATANODES
    EXPORTED_INCLUDE_SUFFIXES src
    MODULE_DEPENDS svQtWidgets svProjectManagement svPath svSegmentation svModel svMesh svSimulation
  )
else()
  SimVascularCreatePlugin(
    EXPORT_DIRECTIVE SV_QT_PROJECTMANAGER
    MODULE_DEPENDS svQtWidgets svProjectManagement svPath svSegmentation svModel svMesh svSimulation
  )
endif()

if(SV_INSTALL_LIBS)
    install(TARGETS ${lib}
        RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT CoreExecutables
        LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT CoreLibraries
        ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT CoreLibraries
    )
endif()

