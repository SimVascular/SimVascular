set(HDRS ConnectVTKITK.h cvITKMacros.h
  cvMacros.h cvTCLMacros.h
  cvVTKMacros.h ImgInfo.h
  )
if(SV_INSTALL_HEADERS)
  install(FILES ${HDRS}
    DESTINATION ${SV_INSTALL_INCLUDE_DIR}/plugins/itk/include COMPONENT PluginHeaders
    )
endif()


