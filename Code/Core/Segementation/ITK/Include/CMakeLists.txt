SET(HDRS ConnectVTKITK.h cvITKMacros.h 
    cvMacros.h cvTCLMacros.h 
    cvVTKMacros.h ImgInfo.h
    )
if(SIMVASCULAR_INSTALL_HEADERS)
    install(FILES ${HDRS}
        DESTINATION ${SIMVASCULAR_INSTALL_INCLUDE_DIR}/plugins/itk/include COMPONENT PluginHeaders
        )
endif()


