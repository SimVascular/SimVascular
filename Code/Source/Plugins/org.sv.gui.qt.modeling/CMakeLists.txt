project(org_sv_gui_qt_modeling)

set(lib org_sv_gui_qt_modeling)

if(SV_USE_MITK_CONFIG)
mitk_create_plugin(
  EXPORT_DIRECTIVE SV_QT_MODELING
  EXPORTED_INCLUDE_SUFFIXES src
  MODULE_DEPENDS MitkQtWidgets svPath svSegmentation svModel svMesh
)
else()
  #---------------------------------TRYING OLD----------------------------------
  include(${CMAKE_CURRENT_SOURCE_DIR}/files.cmake)
  usFunctionGenerateModuleInit(CPP_FILES)

  if(RESOURCE_FILES)
    usFunctionGetResourceSource(TARGET ${lib} OUT CPP_FILES)
  endif()

  qt5_wrap_ui(UISrcs ${UI_FILES})
  qt5_add_resources(QRCSrcs ${QRC_FILES})
  qt5_wrap_cpp(MOCSrcs ${MOC_H_FILES})
    #---------------------------------MANIFESTTTTT----------------------------------
    # If a file named manifest_headers.cmake exists, read it
    set(CTK_QT_VERSION 5.4)
    set(manifest_headers_dep )
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/manifest_headers.cmake")
      include(${CMAKE_CURRENT_SOURCE_DIR}/manifest_headers.cmake)
      set(manifest_headers_dep "${CMAKE_CURRENT_SOURCE_DIR}/manifest_headers.cmake")
    endif()

    string(REPLACE "_" "." Plugin-SymbolicName ${lib})

    # Add the generated manifest qrc file
    set(manifest_qrc_src )
    svFunctionGeneratePluginManifest(manifest_qrc_src
      ACTIVATIONPOLICY ${Plugin-ActivationPolicy}
      CATEGORY ${Plugin-Category}
      CONTACT_ADDRESS ${Plugin-ContactAddress}
      COPYRIGHT ${Plugin-Copyright}
      DESCRIPTION ${Plugin-Description}
      DOC_URL ${Plugin-DocURL}
      ICON ${Plugin-Icon}
      LICENSE ${Plugin-License}
      NAME ${Plugin-Name}
      REQUIRE_PLUGIN ${Require-Plugin}
      SYMBOLIC_NAME ${Plugin-SymbolicName}
      VENDOR ${Plugin-Vendor}
      VERSION ${Plugin-Version}
      CUSTOM_HEADERS ${Custom-Headers}
      )

    if(manifest_headers_dep)
      set_property(SOURCE ${manifest_qrc_src} APPEND
                     PROPERTY OBJECT_DEPENDS ${manifest_headers_dep})
    endif()
    list(APPEND QRCSrcs ${manifest_qrc_src})
    #---------------------------------MANIFESTTTTT----------------------------------

  add_library(${lib} SHARED ${H_FILES} ${CPP_FILES} ${UISrcs} ${QRCSrcs} ${MOCSrcs})

  set(PLUGIN_NAME SV_QT_MODELING)
  set_property(TARGET ${lib} PROPERTY US_MODULE_NAME ${PLUGIN_NAME})
  set_property(TARGET ${lib} APPEND PROPERTY COMPILE_DEFINITIONS US_MODULE_NAME=${PLUGIN_NAME})

  target_link_libraries(${lib} ${MITK_LIBRARIES} ${CTK_LIBRARIES}  ${ITK_LIBRARIES} svPath svSegmentation svModel svMesh)
  set_target_properties(${lib}
      PROPERTIES
      LIBRARY_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/plugins
      RUNTIME_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/plugins
  )

  set(MODULE_EXPORT_DEFINE ${PLUGIN_NAME})

  set(_export_macro_names
    EXPORT_MACRO_NAME ${MODULE_EXPORT_DEFINE}
    NO_EXPORT_MACRO_NAME ${PLUGIN_NAME}_NO_EXPORT
    DEPRECATED_MACRO_NAME ${PLUGIN_NAME}_DEPRECATED
    NO_DEPRECATED_MACRO_NAME ${PLUGIN_NAME}_NO_DEPRECATED
  )
  generate_export_header(${lib}
    ${_export_macro_names}
    EXPORT_FILE_NAME ${lib}_Export.h
  )

  target_include_directories(${lib} PUBLIC ${CMAKE_CURRENT_BINARY_DIR})


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


  #---------------------------------TRYING OLD----------------------------------

endif()
if(SV_INSTALL_LIBS)
    install(TARGETS ${lib}
        RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT CoreExecutables
        LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT CoreLibraries
        ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT CoreLibraries
    )
endif()
