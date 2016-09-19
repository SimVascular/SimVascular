set(lib svCommon)

MITK_CREATE_MODULE( ${lib}
  DEPENDS MitkCore
  PACKAGE_DEPENDS
    PRIVATE tinyxml
  ADDITIONAL_LIBS
    ${SV_LIB_UTILS_NAME}
#  WARNINGS_AS_ERRORS
)

if(SV_INSTALL_LIBS)
        install(TARGETS ${lib}
            RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT CoreExecutables
                LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT CoreLibraries
                ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT CoreLibraries
                )
endif()
if(SV_INSTALL_HEADERS)
        install(FILES ${HDRS}
                DESTINATION ${SV_INSTALL_INCLUDE_DIR}/core COMPONENT CoreHeaders
                )
endif()


