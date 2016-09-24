set(lib svProjectManagement)

MITK_CREATE_MODULE( ${lib}
    FORCE_SHARED
    DEPENDS PUBLIC MitkCore svPath svSegmentation svModel
    PACKAGE_DEPENDS PUBLIC Qt5|Core+Xml
)

if(SV_INSTALL_LIBS)
    install(TARGETS ${lib}
        RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT CoreExecutables
        LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT CoreLibraries
        ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT CoreLibraries
    )
endif()

if(SV_INSTALL_HEADERS)
    set(MODULE_FILES_CMAKE files.cmake)
    if(NOT IS_ABSOLUTE ${MODULE_FILES_CMAKE})
        set(MODULE_FILES_CMAKE ${CMAKE_CURRENT_SOURCE_DIR}/${MODULE_FILES_CMAKE})
    endif()
    if (EXISTS ${MODULE_FILES_CMAKE})
        include(${MODULE_FILES_CMAKE})
    endif()
    install(FILES ${H_FILES}
            DESTINATION ${SV_INSTALL_INCLUDE_DIR}/core COMPONENT CoreHeaders
    )
endif()

