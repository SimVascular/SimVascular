# need this since opencascade doesn't have freetype
# path explicitly defined
if(WIN32)
get_filename_component(TMP_DIR_FREETYPE_LIBRARY ${FREETYPE_LIBRARY} DIRECTORY)
link_directories(${TMP_DIR_FREETYPE_LIBRARY})
endif()

set(APP_NAME ${SV_EXE})
set(APP_SHOW_CONSOLE ON)

SET(CXXSRCS main.cxx SimVascular_Init.cxx)

#if(SV_USE_QT_GUI)
#  SET(MOC_HDRS svMainWindow.h)
#  SET(QRC_FILES
#      resources/sv.qrc
#  )
#  qt5_add_resources(QRCSRCS ${QRC_FILES})
#  qt5_wrap_cpp(MOCSRCS ${MOC_HDRS})

#  set(CXXSRCS ${CXXSRCS} svMainWindow.cxx ${QRCSRCS} ${MOCSRCS})
#endif()


UNSET(OPTLIBS)

if(NOT ${USE_SYSTEM_TCL})
	include_directories(${TCL_INCLUDE_PATH})
	include_directories(${TK_INCLUDE_PATH})
endif()

if(SV_USE_TETGEN)
	SET(OPTLIBS ${OPTLIBS} ${SV_LIB_TETGEN_MESH_NAME})
endif()
#
if(SV_USE_MMG)
	SET(OPTLIBS ${OPTLIBS} ${SV_LIB_MMG_MESH_NAME})
endif()

if(SV_USE_MESHSIM)
	if (NOT SV_USE_MESHSIM_SHARED_LIBRARIES)
	  SET(OPTLIBS ${OPTLIBS} ${SV_LIB_MESHSIM_MESH_NAME})
        endif()
endif()

if(SV_USE_MESHSIM_DISCRETE_MODEL)
	if (NOT SV_USE_MESHSIM_DISCRETE_SHARED_LIBRARIES)
	  SET(OPTLIBS ${OPTLIBS} ${SV_LIB_MESHSIM_DISCRETE_SOLID_NAME})
        endif()
endif()

if(SV_USE_PARASOLID)
	if (NOT SV_USE_PARASOLID_SHARED_LIBRARIES)
	  SET(OPTLIBS ${OPTLIBS} ${SV_LIB_PARASOLID_SOLID_NAME})
        endif()
endif()

if(SV_USE_OpenCASCADE)
	SET(OPTLIBS ${OPTLIBS} ${SV_LIB_OpenCASCADE_SOLID_NAME})
endif()

if(SV_USE_ITK)
	SET(OPTLIBS ${OPTLIBS} ${SV_LIB_ITK_LSET_NAME})
  endif()

if(SV_USE_TET_ADAPTOR)
        SET(OPTLIBS ${OPTLIBS} ${SV_LIB_ADAPTOR_NAME})
	SET(OPTLIBS ${OPTLIBS} ${SV_LIB_TETGEN_ADAPTOR_NAME})
endif()

if (SV_USE_MESHSIM_ADAPTOR)
	if(NOT SV_USE_MESHSIM_SHARED_LIBRARIES)
	  SET(OPTLIBS ${OPTLIBS} ${SV_LIB_ADAPTOR_NAME})
	  SET(OPTLIBS ${OPTLIBS} ${SV_LIB_MESHSIM_ADAPTOR_NAME})
        endif()
endif()

if (SV_USE_PYTHON)
	SET(OPTLIBS ${OPTLIBS} ${SV_LIB_PYTHON_INTERP_NAME})
endif()

#if(SV_USE_QT_GUI)
#  SET(OPTLIBS ${OPTLIBS} ${SV_LIB_QTGUI_PLUGIN_IMAGE_NAME}
#                         ${SV_LIB_QTGUI_PLUGIN_MITKSEGMENTATION_NAME}
#                         ${SV_LIB_QTGUI_PLUGIN_GENERAL_NAME}
#                         ${SV_LIB_QTGUI_PLUGIN_MODELING_NAME}
#                         ${SV_LIB_QTGUI_PLUGIN_PATHPLANNING_NAME}
#                         ${SV_LIB_QTGUI_PLUGIN_SEGMENTATION_NAME}
#                         ${SV_LIB_QTGUI_PLUGIN_TEST_NAME}
#                         ${SV_LIB_QTGUI_MODULE_COMMON_NAME}
#                         ${SV_LIB_QTGUI_MODULE_MODEL_NAME}
#                         ${SV_LIB_QTGUI_MODULE_PATH_NAME}
#                         ${SV_LIB_QTGUI_MODULE_PROJECTMANAGEMENT_NAME}
#                         ${SV_LIB_QTGUI_MODULE_APPBASE}
#                         ${SV_LIB_QTGUI_MODULE_QTWIDGETS_NAME}
#                         ${SV_LIB_QTGUI_MODULE_SEGMENTATION_NAME})
#endif()

simvascular_add_executable(${SV_EXE}
	SRCS ${CXXSRCS}
	DEV_SCRIPT_NAME "mysim"
	INSTALL_SCRIPT_NAME "simvascular"
	INSTALL_COMPONENT CoreExecutables
	INSTALL_DESTINATION ${SV_INSTALL_RUNTIME_DIR})
#message("OPTLIBS ${OPTLIBS}")

target_link_libraries(${SV_EXE}
			${SV_LIB_POST_NAME} ${SV_LIB_IMAGE_NAME}
			${SV_LIB_REPOSITORY_NAME} ${SV_LIB_LSET_NAME}
			${SV_LIB_SOLID_NAME} ${SV_LIB_MESH_NAME}
                        ${SV_LIB_GEOM_NAME} ${SV_LIB_UTILS_NAME}
			${SV_LIB_GLOBALS_NAME} ${SV_LIB_POLYDATA_SOLID_NAME})

target_link_libraries(${SV_EXE} ${OPTLIBS}
			${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
			${PARASOLID_LIBRARIES} ${MESHSIM_LIBRARIES}
			${OpenCASCADE_LIBRARIES}
			${ZLIB_LIBRARY} ${ITK_LIBRARIES}
			${INTELRUNTIME_LIBRARIES}
			${GLOBAL_LIBRARIES}
			${MMG_LIBRARY}
			${FREETYPE_LIBRARY}
                        ${PYTHON_LIBRARY}
                        ${MITK_LIBRARIES}
                        ${CTK_LIBRARIES}
                        ${QT_LIBRARIES})

if(WIN32)
  target_link_libraries(${SV_EXE} PRIVATE ${QT_QTMAIN_LIBRARY})
endif()

if(SV_INSTALL_HEADERS)
  install(FILES ${MOC_HDRS}
		DESTINATION ${SV_INSTALL_INCLUDE_DIR}/core COMPONENT CoreHeaders
		)
endif()
