
# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(lib ${SV_LIB_QTGUI_MODULE_MODEL_NAME})

set(HDRS
    svModelElement.h
    svModelOperation.h
    svModel.h
    svModelUtils.h
    svModelIO.h
    svModelLegacyIO.h
    svModelObjectFactory.h
    svSurface.h
)

set(CXXSRCS
    svModelElement.cxx
    svModelOperation.cxx
    svModel.cxx
    svModelUtils.cxx
    svModelIO.cxx
    svModelLegacyIO.cxx
    svModelObjectFactory.cxx
    svSurface.cxx
)

set(RESOURCE_FILES
)

usFunctionGenerateModuleInit(CXXSRCS)

if(RESOURCE_FILES)
    usFunctionGetResourceSource(TARGET ${lib} OUT CXXSRCS)
endif()

if(SV_USE_MODULES_SHARED_LIBRARIES)
  add_library(${lib} SHARED ${HDRS} ${CXXSRCS})
else()
  add_library(${lib} STATIC ${HDRS} ${CXXSRCS})
endif()

set(module_name "svModel")

set_property(TARGET ${lib} PROPERTY US_MODULE_NAME ${module_name})
set_property(TARGET ${lib} APPEND PROPERTY COMPILE_DEFINITIONS US_MODULE_NAME=${module_name})

target_link_libraries(${lib} ${SV_LIB_SOLID_NAME} ${SV_LIB_POLYDATA_SOLID_NAME} ${SV_LIB_QTGUI_MODULE_SEGMENTATION_NAME})

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
