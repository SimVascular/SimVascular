
# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(lib ${SV_LIB_QTGUI_MODULE_SEGMENTATION_NAME})

set(HDRS
    svSegmentationUtils.h
    svContour.h
    svContourCircle.h
    svContourEllipse.h
    svContourPolygon.h
    svContourTensionPolygon.h
    svContourSplinePolygon.h
    svContourOperation.h
    svContourModel.h
    svContourModelVtkMapper2D.h
    svContourModelThresholdInteractor.h
    svContourGroup.h
    svContourGroupVtkMapper2D.h
    svContourGroupVtkMapper3D.h
    svContourGroupDataInteractor.h
    svContourGroupIO.h
    svSegmentationLegacyIO.h
    svSegmentationObjectFactory.h

)

set(CXXSRCS
    svSegmentationUtils.cxx
    svContour.cxx
    svContourCircle.cxx
    svContourEllipse.cxx
    svContourPolygon.cxx
    svContourTensionPolygon.cxx
    svContourSplinePolygon.cxx
    svContourOperation.cxx
    svContourModel.cxx
    svContourModelVtkMapper2D.cxx
    svContourModelThresholdInteractor.cxx
    svContourGroup.cxx
    svContourGroupVtkMapper2D.cxx
    svContourGroupVtkMapper3D.cxx
    svContourGroupDataInteractor.cxx
    svContourGroupIO.cxx
    svSegmentationLegacyIO.cxx
    svSegmentationObjectFactory.cxx
)

set(RESOURCE_FILES
    Interactions/svContourGroupInteraction.xml
    Interactions/svContourModelThresholdInteraction.xml
    Interactions/svSegmentationConfig.xml
)

#find_package(CppMicroServices QUIET NO_MODULE REQUIRED)
usFunctionGenerateModuleInit(CXXSRCS)

if(RESOURCE_FILES)
    usFunctionGetResourceSource(TARGET ${lib} OUT CXXSRCS)
endif()

#include_directories("${SV_SOURCE_DIR}/Source/Segmentation/ITK/Include")
if(SV_USE_MODULES_SHARED_LIBRARIES)
  add_library(${lib} SHARED ${HDRS} ${CXXSRCS} )
else()
  add_library(${lib} STATIC ${HDRS} ${CXXSRCS} )
endif()

set(module_name svSegmentation)
set_property(TARGET ${lib} PROPERTY US_MODULE_NAME ${module_name})
set_property(TARGET ${lib} APPEND PROPERTY COMPILE_DEFINITIONS US_MODULE_NAME=${module_name})

target_link_libraries(${lib} ${MITK_LIBRARIES}
                             ${VTK_LIBRARIES}
                             ${ITK_LIBRARIES}
                             ${SV_LIB_GLOBALS_NAME}
                             ${SV_LIB_UTILS_NAME}
                             ${SV_LIB_GEOM_NAME}
                             ${SV_LIB_REPOSITORY_NAME}
                             ${SV_LIB_ITK_LSET_NAME}
                             ${SV_LIB_QTGUI_MODULE_COMMON_NAME}
                             ${SV_LIB_QTGUI_MODULE_PATH_NAME})

if(RESOURCE_FILES)
    set(res_dir resources)
    usFunctionAddResources(TARGET ${lib}
                           WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${res_dir}
                           FILES ${RESOURCE_FILES})
    usFunctionEmbedResources(TARGET ${lib})
endif()

string(TOUPPER "${module_name}" MODULE_NAME)
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
