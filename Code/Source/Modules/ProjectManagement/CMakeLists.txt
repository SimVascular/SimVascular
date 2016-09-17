
# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(lib ${SV_LIB_QTGUI_MODULE_PROJECTMANAGEMENT_NAME})

set(HDRS
    svDataFolder.h
    svImageFolder.h
    svPathFolder.h
    svSegmentationFolder.h
    svModelFolder.h
    svMeshFolder.h
    svSimulationFolder.h
    svProjectManager.h
)

set(CXXSRCS
    svDataFolder.cxx
    svProjectManager.cxx
)

set(RESOURCE_FILES
)

#find_package(CppMicroServices QUIET NO_MODULE REQUIRED)
usFunctionGenerateModuleInit(CXXSRCS)

if(RESOURCE_FILES)
    usFunctionGetResourceSource(TARGET ${lib} OUT CXXSRCS)
endif()

if(SV_USE_MODULES_SHARED_LIBRARIES)
  add_library(${lib} SHARED ${HDRS} ${CXXSRCS} ${UISrcs} ${QRCSrcs})
else()
  add_library(${lib} STATIC ${HDRS} ${CXXSRCS} ${UISrcs} ${QRCSrcs})
endif()

set(module_name "svProjectManagement")
set_property(TARGET ${lib} PROPERTY US_MODULE_NAME ${module_name})
set_property(TARGET ${lib} APPEND PROPERTY COMPILE_DEFINITIONS US_MODULE_NAME=${module_name})

target_link_libraries(${lib} ${MITK_LIBRARIES}
                             ${QT_LIBRARIES}
                             ${ITK_LIBRARIES})

if(RESOURCE_FILES)
    set(res_dir resources)
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
