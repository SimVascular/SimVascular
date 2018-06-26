project(org_sv_gui_qt_svfsi)

set(lib org_sv_gui_qt_svfsi)

set(SV_LIB_MODULE_SVFSI_NAME "_simvascular_module_svfsi")

if(SV_USE_MITK_CONFIG)
  mitk_create_plugin(
    EXPORT_DIRECTIVE SV_QT_SVFSI
    EXPORTED_INCLUDE_SUFFIXES src
    MODULE_DEPENDS org_sv_gui_qt_datamanager ${MITK_LIBRARIES} ${ITK_LIBRARIES} ${QT_LIBRARIES} ${SV_LIB_MODULE_SVFSI_NAME} ${SV_LIB_MODULE_PROJECTMANAGEMENT_NAME}
  )
else()
  simvascular_create_plugin(
    EXPORT_DIRECTIVE SV_QT_SVFSI
    MODULE_DEPENDS org_sv_gui_qt_datamanager ${MITK_LIBRARIES} ${ITK_LIBRARIES} ${QT_LIBRARIES} ${SV_LIB_MODULE_SVFSI_NAME} ${SV_LIB_MODULE_PROJECTMANAGEMENT_NAME}
  )
endif()
if(SV_INSTALL_LIBS)
    install(TARGETS ${lib}
        RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR}/plugins COMPONENT CoreExecutables
        LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR}/plugins COMPONENT CoreLibraries
        ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR}/plugins COMPONENT CoreLibraries
    )
endif()

#-----------------------------------------------------------------------------
# Include and link to SimVascular
target_link_libraries(org_sv_gui_qt_svfsi PRIVATE ${SimVascular_LIBRARIES})
#-----------------------------------------------------------------------------
foreach(so ${SimVascular_LIBRARIES})
  message(WARNING ${so})
  target_link_libraries(${lib} PRIVATE ${so})
endforeach()
