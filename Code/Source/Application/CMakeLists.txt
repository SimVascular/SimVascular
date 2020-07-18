# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

project(simvascular)

set(APP_NAME ${SV_EXE})
set(APP_SHOW_CONSOLE ON)

set(CXXSRCS main.cxx SimVascular_Init.cxx)

if(SV_USE_TCL)
  LIST(APPEND CXXSRCS svTcl_AppInit.cxx)
endif()

if(SV_USE_WIN32_REGISTRY)
  LIST(APPEND CXXSRCS sv_use_win32_registry.cxx)
endif()

if(SV_USE_PYTHON)
  LIST(APPEND CXXSRCS PythonShell_Init.cxx)
  LIST(APPEND CXXSRCS SimVascular_Init_py.cxx)
endif()

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

set(SV_WORKBENCH_PROVISIONING_FILE "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${SV_EXE}.provisioning" CACHE INTERNAL "svWorkbench provisioning file" FORCE)

if(NOT SV_USE_MITK_CONFIG)
  # Not using MITK config file, so just use regular simvascular functions
  simvascular_add_executable(${SV_EXE}
    SRCS ${CXXSRCS}
    DEV_SCRIPT_NAME "sv"
    INSTALL_SCRIPT_NAME "simvascular"
    INSTALL_COMPONENT CoreExecutables
    INSTALL_DESTINATION ${SV_INSTALL_RUNTIME_DIR})

  simvascular_create_provisioning_file(
    FILE ${SV_WORKBENCH_PROVISIONING_FILE}
    PLUGINS ${SV_PLUGINS}
    EXCLUDE_PLUGINS ${exclude_plugins})

  install(FILES "${SV_WORKBENCH_PROVISIONING_FILE}.install"
    DESTINATION ${SV_INSTALL_RUNTIME_DIR}
    RENAME "${SV_EXE}.provisioning"
    COMPONENT CoreFiles)

  # Create a .ini file for initial parameters
  if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${SV_EXE}.ini")
    configure_file(${SV_EXE}.ini
      ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${SV_EXE}.ini)
  endif()
else()
  # Using MITK Config files, have to create sv script
  set(SV_SCRIPT_TARGETS_WORK ${SV_SCRIPT_TARGETS})
  list(APPEND SV_SCRIPT_TARGETS_WORK "${SV_EXE}")
  list(REMOVE_DUPLICATES SV_SCRIPT_TARGETS_WORK)
  set(SV_SCRIPT_TARGETS ${SV_SCRIPT_TARGETS_WORK} CACHE INTERNAL "" FORCE)

  set(DEV_SCRIPT_NAME "sv")
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
    DESCRIPTION "SimVascular"
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
    install(FILES "icons/icon.icns" DESTINATION "${SV_EXE}.app/Contents/Resources" COMPONENT CoreFiles)
  endif(APPLE)

  # Add a build time dependency to legacy BlueBerry bundles.
  if(MITK_MODULES_ENABLED_PLUGINS)
    add_dependencies(${SV_EXE} ${MITK_MODULES_ENABLED_PLUGINS})
  endif()
endif()

#-
target_link_libraries(${SV_EXE} PRIVATE
	${SV_LIB_POST_NAME}
	${SV_LIB_IMAGE_NAME}
	${SV_LIB_REPOSITORY_NAME}
	${SV_LIB_LSET_NAME}
	${SV_LIB_SOLID_NAME}
	${SV_LIB_MESH_NAME}
	${SV_LIB_GEOM_NAME}
	${SV_LIB_UTILS_NAME}
	${SV_LIB_GLOBALS_NAME}
	${SV_LIB_PYTHON_API_NAME}
	${SV_LIB_POLYDATA_SOLID_NAME}
	${SV_LIB_VTKSVCOMMON_NAME}
	${SV_LIB_VTKSVFILTERS_NAME}
	${SV_LIB_VTKSVGEOMETRY_NAME}
	${SV_LIB_VTKSVBOOLEAN_NAME}
	${SV_LIB_VTKSVNURBS_NAME}
	${SV_LIB_VTKSVPARAMETERIZATION_NAME}
	${SV_LIB_PATH_NAME}
	${SV_LIB_SEGMENTATION_NAME})

if(SV_USE_SV4_GUI)
  target_link_libraries(${SV_EXE} PRIVATE org_sv_pythondatanodes org_sv_gui_qt_application)
endif()

target_link_libraries(${SV_EXE} PRIVATE ${OPTLIBS}
  ${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
  ${ZLIB_LIBRARY} ${ITK_LIBRARIES}
  ${GDCM_LIBRARIES}
  ${INTELRUNTIME_LIBRARIES}
  ${MMG_LIBRARY}
  ${FREETYPE_LIBRARY}
  ${PYTHON_LIBRARY})

if(SV_USE_MITK)
  target_link_libraries(${SV_EXE} PRIVATE ${MITK_LIBRARIES}
           ${MITK_PLUGIN_LIBRARIES} ${CTK_LIBRARIES})
endif()

if(SV_USE_QT)
  target_link_libraries(${SV_EXE} PRIVATE ${QT_LIBRARIES})
endif()

if(WIN32)
  target_link_libraries(${SV_EXE} PRIVATE ${QT_QTMAIN_LIBRARY})
endif()

if(SV_INSTALL_HEADERS)
  install(FILES ${MOC_HDRS}
		DESTINATION ${SV_INSTALL_INCLUDE_DIR}/core COMPONENT CoreHeaders
		)
endif()
