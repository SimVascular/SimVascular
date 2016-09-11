
# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(lib ${SV_LIB_QTGUI_MODULE_COMMON_NAME})

set(DEPENDENT_EXTERNAL_LIBRARIES
    MitkCore
    ${VTK_LIBRARIES}
    tinyxml
)

set(DEPENDENT_SV_LIBRARIES
    _simvascular_utils
)

set(HDRS
    svMath3.h
    svVtkParametricSpline.h
    svSpline.h
    svXmlIOUtil.h
)

set(CXXSRCS
    svMath3.cxx
    svVtkParametricSpline.cxx
    svSpline.cxx
    svXmlIOUtil.cxx
)

if(SV_USE_MODULES_SHARED_LIBRARIES)
  add_library(${lib} SHARED ${HDRS} ${CXXSRCS})
else()
  add_library(${lib} STATIC ${HDRS} ${CXXSRCS})
endif()

target_link_libraries(${lib} ${DEPENDENT_EXTERNAL_LIBRARIES} ${DEPENDENT_SV_LIBRARIES})

set(module_name "svCommon")
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
