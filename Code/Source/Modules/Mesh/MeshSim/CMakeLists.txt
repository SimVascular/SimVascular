set(lib ${SV_LIB_MODULE_MESHSIM_NAME})

#-----------------------------------------------------------------------------
# Extra depends
set(EXTRA_DEPENDS "")
if(SV_USE_MESHSIM_ADAPTOR)
  set(EXTRA_DEPENDS ${EXTRA_DEPENDS} 
    ${SV_LIB_MESHSIM_ADAPTOR_NAME})
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Create module
simvascular_create_module(
  TARGET ${lib}
  EXPORT_DIRECTIVE svMeshSim
  SHARED_LIB ${SV_USE_MODULES_SHARED_LIBRARIES}
  LIBRARY_DEPENDS
  ${QT_LIBRARIES}
  ${VTK_LIBRARIES}
  ${ITK_LIBRARIES}
  ${MITK_LIBRARIES}
  ${SV_LIB_MODULE_MODEL_NAME}
  ${SV_LIB_MODULE_MODEL_PARASOLID_NAME}
  ${SV_LIB_MODULE_MESH_NAME}
  ${SV_LIB_MESH_NAME}
  ${SV_LIB_MESHSIM_MESH_NAME}
  ${SV_LIB_SOLID_NAME}
  ${SV_LIB_PARASOLID_SOLID_NAME}
)
#-----------------------------------------------------------------------------
if(NOT SV_DISTRIBUTE_COMPONENTS STREQUAL OpenSource)
  if(SV_INSTALL_LIBS)
    install(TARGETS ${lib}
      RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT LicensedExecutables
      LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT LicensedLibraries
      ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT LicensedLibraries
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
      DESTINATION ${SV_INSTALL_INCLUDE_DIR}/core COMPONENT LicensedHeaders
    )
  endif()
endif()

