
# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)
# Instruct CMake to run moc automatically when needed.
set(CMAKE_AUTOMOC ON)

set(lib ${SV_LIB_QTGUI_MODULE_PATH_NAME})

set(HDRS
    svPathElement.h
    svPath.h
    svPathOperation.h
    svPathDataInteractor.h
    svPathVtkMapper2D.h
    svPathVtkMapper3D.h
    svPathIO.h
    svPathLegacyIO.h
    svPathObjectFactory.h
)

set(CXXSRCS
    svPathElement.cxx
    svPath.cxx
    svPathOperation.cxx
    svPathDataInteractor.cxx
    svPathVtkMapper2D.cxx
    svPathVtkMapper3D.cxx
    svPathIO.cxx
    svPathLegacyIO.cxx
    svPathObjectFactory.cxx
)

set(RESOURCE_FILES
    Interactions/svPath.xml
    Interactions/svPathConfig.xml
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

set(module_name svPath)
set_property(TARGET ${lib} PROPERTY US_MODULE_NAME ${module_name})
set_property(TARGET ${lib} APPEND PROPERTY COMPILE_DEFINITIONS US_MODULE_NAME=${module_name})

target_link_libraries(${lib} ${MITK_LIBRARIES}
                             ${ITK_LIBRARIES}
                             ${SV_LIB_GLOBALS_NAME}
                             ${SV_LIB_UTILS_NAME}
                             ${SV_LIB_GEOM_NAME}
                             ${SV_LIB_QTGUI_MODULE_COMMON_NAME})

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
