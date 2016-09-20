
# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(lib ${SV_LIB_QTGUI_MODULE_QTWIDGETS_NAME})

set(HDRS
    svResliceSlider.h
)

set(CXXSRCS
    svResliceSlider.cxx
)

set(MOC_HDRS ${HDRS})

set(UI_FILES
)

set(QRC_FILES
)

set(RESOURCE_FILES

)

qt5_wrap_ui(UISrcs ${UI_FILES})
qt5_add_resources(QRCSrcs ${QRC_FILES})
qt5_wrap_cpp(MOCSrcs ${MOC_HDRS})

if(SV_USE_MODULES_SHARED_LIBRARIES)
  add_library(${lib} SHARED ${HDRS} ${CXXSRCS} ${UISrcs} ${QRCSrcs} ${MOCSrcs})
else()
  add_library(${lib} STATIC ${HDRS} ${CXXSRCS} ${UISrcs} ${QRCSrcs} ${MOCSrcs})
endif()
target_link_libraries(${lib} ${MITK_LIBRARIES} ${SV_LIB_QTGUI_MODULE_PATH_NAME} ${SV_LIB_QTGUI_MODULE_SEGMENTATION_NAME})

set(module_name "svQtWidgets")
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
