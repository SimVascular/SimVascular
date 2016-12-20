project(simvascular)

set(APP_NAME ${SV_EXE})
set(APP_SHOW_CONSOLE ON)

set(CXXSRCS main.cxx SimVascular_Init.cxx)

unset(OPTLIBS)

if(NOT ${USE_SYSTEM_TCL})
	include_directories(${TCL_INCLUDE_PATH})
	include_directories(${TK_INCLUDE_PATH})
endif()

if(SV_USE_TETGEN)
	set(OPTLIBS ${OPTLIBS} ${SV_LIB_TETGEN_MESH_NAME})
endif()
#
if(SV_USE_MMG)
	set(OPTLIBS ${OPTLIBS} ${SV_LIB_MMG_MESH_NAME})
endif()

if(SV_USE_MESHSIM)
	if (NOT SV_USE_MESHSIM_SHARED_LIBRARIES)
	  set(OPTLIBS ${OPTLIBS} ${SV_LIB_MESHSIM_MESH_NAME})
  endif()
endif()

if(SV_USE_MESHSIM_DISCRETE_MODEL)
	if (NOT SV_USE_MESHSIM_DISCRETE_SHARED_LIBRARIES)
	  set(OPTLIBS ${OPTLIBS} ${SV_LIB_MESHSIM_DISCRETE_SOLID_NAME})
  endif()
endif()

if(SV_USE_PARASOLID)
	if (NOT SV_USE_PARASOLID_SHARED_LIBRARIES)
	  set(OPTLIBS ${OPTLIBS} ${SV_LIB_PARASOLID_SOLID_NAME})
  endif()
endif()

if(SV_USE_OpenCASCADE)
	set(OPTLIBS ${OPTLIBS} ${SV_LIB_OpenCASCADE_SOLID_NAME})
  if(SV_USE_FREETYPE)
    link_directories(${FREETYPE_LIBRARY_DIR})
  endif()
endif()

if(SV_USE_ITK)
	set(OPTLIBS ${OPTLIBS} ${SV_LIB_ITK_LSET_NAME})
endif()

if(SV_USE_TETGEN_ADAPTOR)
  set(OPTLIBS ${OPTLIBS} ${SV_LIB_ADAPTOR_NAME})
	set(OPTLIBS ${OPTLIBS} ${SV_LIB_TETGEN_ADAPTOR_NAME})
endif()

if (SV_USE_MESHSIM_ADAPTOR)
	if(NOT SV_USE_MESHSIM_SHARED_LIBRARIES)
	  set(OPTLIBS ${OPTLIBS} ${SV_LIB_ADAPTOR_NAME})
	  set(OPTLIBS ${OPTLIBS} ${SV_LIB_MESHSIM_ADAPTOR_NAME})
  endif()
endif()

if (SV_USE_PYTHON)
	set(OPTLIBS ${OPTLIBS} ${SV_LIB_PYTHON_INTERP_NAME})
endif()


set(SV_WORKBENCH_PROVISIONING_FILE "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${SV_EXE}.provisioning" CACHE INTERNAL "svWorkbench provisioning file" FORCE)

if(NOT SV_USE_MITK_CONFIG)
  # Not using MITK config file, so just use regular simvascular functions
  simvascular_add_executable(${SV_EXE}
    SRCS ${CXXSRCS}
    DEV_SCRIPT_NAME "mysim"
    INSTALL_SCRIPT_NAME "simvascular"
    INSTALL_COMPONENT CoreExecutables
    INSTALL_DESTINATION ${SV_INSTALL_RUNTIME_DIR})

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
  # Using MITK Config files, have to create mysim script
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
  list(APPEND _app_options SHOW_CONSOLE)

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
endif()

#-
target_link_libraries(${SV_EXE} PRIVATE
	${SV_LIB_POST_NAME} ${SV_LIB_IMAGE_NAME}
	${SV_LIB_REPOSITORY_NAME} ${SV_LIB_LSET_NAME}
	${SV_LIB_SOLID_NAME} ${SV_LIB_MESH_NAME}
  ${SV_LIB_GEOM_NAME} ${SV_LIB_UTILS_NAME}
	${SV_LIB_GLOBALS_NAME} ${SV_LIB_POLYDATA_SOLID_NAME})

target_link_libraries(${SV_EXE} PRIVATE ${OPTLIBS}
  ${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
  ${PARASOLID_LIBRARIES} ${MESHSIM_LIBRARIES}
  ${OpenCASCADE_LIBRARIES}
  ${ZLIB_LIBRARY} ${ITK_LIBRARIES}
  ${GDCM_LIBRARIES}
  ${INTELRUNTIME_LIBRARIES}
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
