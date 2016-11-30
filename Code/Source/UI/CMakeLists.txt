project(simvascular)

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


set(SV_WORKBENCH_PROVISIONING_FILE "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${SV_EXE}.provisioning" CACHE INTERNAL "svWorkbench provisioning file" FORCE)

if (NOT SV_USE_MITK_CONFIG)
  simvascular_add_executable(${SV_EXE}
          SRCS ${CXXSRCS}
          DEV_SCRIPT_NAME "mysim"
          INSTALL_SCRIPT_NAME "simvascular"
          INSTALL_COMPONENT CoreExecutables
          INSTALL_DESTINATION ${SV_INSTALL_RUNTIME_DIR})
  #message("OPTLIBS ${OPTLIBS}")

  simvascular_create_provisioning_file(
          FILE ${SV_WORKBENCH_PROVISIONING_FILE}
          PLUGINS ${SV_PLUGINS}
          EXCLUDE_PLUGINS ${exclude_plugins})
  install(FILES "${SV_WORKBENCH_PROVISIONING_FILE}.install"
          DESTINATION ${SV_INSTALL_RUNTIME_DIR}
          RENAME "${SV_EXE}.provisioning")

  # Create a .ini file for initial parameters
  if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${SV_EXE}.ini")
    configure_file(${SV_EXE}.ini
      ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${SV_EXE}.ini)
  endif()

else()
  #-------------------------------TMP-------------------------------------------
  set(SV_SCRIPT_TARGETS_WORK ${SV_SCRIPT_TARGETS})
  list(APPEND SV_SCRIPT_TARGETS_WORK "${SV_EXE}")
  list(REMOVE_DUPLICATES SV_SCRIPT_TARGETS_WORK)
  set(SV_SCRIPT_TARGETS ${SV_SCRIPT_TARGETS_WORK} CACHE INTERNAL "" FORCE)

  set(DEV_SCRIPT_NAME "mysim")
  set(${SV_EXE}_DEVELOPER_SCRIPT_NAME ${DEV_SCRIPT_NAME} CACHE INTERNAL "" FORCE)
  set(${SV_EXE}_EXECUTABLE_NAME ${${SV_EXE}_DEVELOPER_SCRIPT_NAME} CACHE INTERNAL "" FORCE)

  set(INSTALL_SCRIPT_NAME "simvascular")
  set(${SV_EXE}_INSTALL_SCRIPT_NAME ${INSTALL_SCRIPT_NAME} CACHE INTERNAL "" FORCE)

  set(_app_options)
  #if(SV_SHOW_CONSOLE_WINDOW)
    list(APPEND _app_options SHOW_CONSOLE)
  #endif()

  set(_exclude_plugins
    org.blueberry.test
    org.blueberry.uitest
    org.mitk.gui.qt.coreapplication
    org.mitk.gui.qt.diffusionimagingapp
    org.mitk.example.gui.customviewer
    org.mitk.example.gui.customviewer.views
    org.mitk.example.gui.selectionservicemitk
    org.mitk.example.gui.selectionservicemitk.views
    org.mitk.example.gui.selectionserviceqt
    org.mitk.example.gui.extensionpointcontribution
    org.mitk.example.gui.extensionpointdefinition
    org.mitk.example.gui.minimalapplication
    org.mitk.example.gui.multipleperspectives
  )

  mitkFunctionCreateBlueBerryApplication(
    NAME ${SV_EXE}
    SOURCES ${CXXSRCS}
    DESCRIPTION "SimVascular Workbench"
    EXCLUDE_PLUGINS ${_exclude_plugins}
    ${_app_options}
  )

  mitk_use_modules(TARGET ${SV_EXE}
    MODULES MitkAppUtil
    PACKAGES Qt5|Widgets
  )

  # Add meta dependencies (e.g. on auto-load modules from depending modules)
  if(TARGET ${CMAKE_PROJECT_NAME}-autoload)
    add_dependencies(${SV_EXE} ${CMAKE_PROJECT_NAME}-autoload)
  endif()

  #Setting application icon for mac os x systems
  set_target_properties(${SV_EXE} PROPERTIES MACOSX_BUNDLE_ICON_FILE "icon.icns")

  if(APPLE)
    install(FILES "icons/icon.icns" DESTINATION "${SV_EXE}.app/Contents/Resources")
  endif(APPLE)

  # Add a build time dependency to legacy BlueBerry bundles.
  if(MITK_MODULES_ENABLED_PLUGINS)
    add_dependencies(${SV_EXE} ${MITK_MODULES_ENABLED_PLUGINS})
  endif()
  #--------------------------------TMP-------------------------------------------

endif()


#-
target_link_libraries(${SV_EXE} PRIVATE
			${SV_LIB_POST_NAME} ${SV_LIB_IMAGE_NAME}
			${SV_LIB_REPOSITORY_NAME} ${SV_LIB_LSET_NAME}
			${SV_LIB_SOLID_NAME} ${SV_LIB_MESH_NAME}
                        ${SV_LIB_GEOM_NAME} ${SV_LIB_UTILS_NAME}
			${SV_LIB_GLOBALS_NAME} ${SV_LIB_POLYDATA_SOLID_NAME})

                      #target_link_libraries(${SV_EXE} PRIVATE "/Users/adamupdegrove/Documents/Software/MITK-11-21-2016/Build/install/lib/libMitkAppUtil.dylib")
                      #target_link_libraries(${SV_EXE} PRIVATE "/Users/adamupdegrove/Documents/Software/MITK-11-21-2016/Build/install/lib/libPocoUtil.dylib")
                      #target_link_libraries(${SV_EXE} PRIVATE "/Users/adamupdegrove/Documents/Software/MITK-11-21-2016/Build/install/lib/libCTKPluginFramework.dylib")
target_link_libraries(${SV_EXE} PRIVATE ${OPTLIBS}
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
