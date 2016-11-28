set(lib svMesh)

#svFunctionCreateModule( ${lib}
#    FORCE_SHARED
#    DEPENDS PUBLIC svModel
#    ADDITIONAL_LIBS
#        ${SV_LIB_SOLID_NAME}
#        ${SV_LIB_POLYDATA_SOLID_NAME}
#        ${SV_LIB_THIRDPARTY_VMTK_NAME}
#        ${SV_LIB_MESH_NAME}
#        ${SV_LIB_TETGEN_MESH_NAME}
##        ${SV_LIB_TETGEN_ADAPTOR_NAME}
#)

#---------------------------------TRYING OLD----------------------------------
include(${CMAKE_CURRENT_SOURCE_DIR}/files.cmake)
usFunctionGenerateModuleInit(CPP_FILES)

if(RESOURCE_FILES)
  usFunctionGetResourceSource(TARGET ${lib} OUT CPP_FILES)
endif()

if(SV_USE_MODULES_SHARED_LIBRARIES)
  add_library(${lib} SHARED ${H_FILES} ${CPP_FILES})
else()
  add_library(${lib} STATIC ${H_FILES} ${CPP_FILES})
endif()

set(module_name svMesh)
set_property(TARGET ${lib} PROPERTY US_MODULE_NAME ${module_name})
set_property(TARGET ${lib} APPEND PROPERTY COMPILE_DEFINITIONS US_MODULE_NAME=${module_name})

target_link_libraries(${lib} ${VTK_LIBRARIES}
                             svModel
                             ${SV_LIB_SOLID_NAME}
                             ${SV_LIB_POLYDATA_SOLID_NAME}
                             ${SV_LIB_THIRDPARTY_VMTK_NAME}
                             ${SV_LIB_MESH_NAME}
                             ${SV_LIB_TETGEN_MESH_NAME}
                             ${SV_LIB_TETGEN_ADAPTOR_NAME})

if(RESOURCE_FILES)
    set(res_dir resource)
    usFunctionAddResources(TARGET ${lib}
                           WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${res_dir}
                           FILES ${RESOURCE_FILES})
    usFunctionEmbedResources(TARGET ${lib})
endif()

string(TOUPPER ${module_name} MODULE_NAME)
set(MODULE_EXPORT_DEFINE ${MODULE_NAME}_EXPORT)

set(_export_macro_names
  EXPORT_MACRO_NAME ${MODULE_EXPORT_DEFINE}
  NO_EXPORT_MACRO_NAME ${MODULE_NAME}_NO_EXPORT
  DEPRECATED_MACRO_NAME ${MODULE_NAME}_DEPRECATED
  NO_DEPRECATED_MACRO_NAME ${MODULE_NAME}_NO_DEPRECATED
)
generate_export_header(${lib}
  ${_export_macro_names}
  EXPORT_FILE_NAME ${module_name}Exports.h
)

target_include_directories(${lib} PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
#---------------------------------TRYING OLD----------------------------------

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

