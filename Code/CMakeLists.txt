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

#-----------------------------------------------------------------------------
# This is the main CMakeListst.txt to download or build  SimVascular.
# If you are building SimVascular for the first time, we recommend moving
# back one directory and using the SV_TOP project to automatically download
# the externals of SimVascular for your platform and compatible with
# SimVascular. However, if you already have compatible externals and want
# to build SimVascular with those externals, this is the right place to be.
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
cmake_minimum_required(VERSION 3.10.0)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
Project(SV)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# CMake module path
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/CMake"
	"${CMAKE_MODULE_PATH}")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# CMake Includes
include(CheckLibraryExists)
include(GetPrerequisites)
include(GenerateExportHeader)
include(ExternalProject)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# SimVascular Includes
include(SimVascularMacros)
include(SimVascularSystemSetup)
include(SimVascularFunctionCheckCompilerFlags)
include(SimVascularInstallSetup)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# RPATH handling
# Don't skip the full RPATH for the build tree
SET(CMAKE_SKIP_BUILD_RPATH  FALSE)
# Don't use the install RPATH
SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
# The RPATH that will be used when installing
SET(CMAKE_INSTALL_RPATH "")
# Don't automatically add determined parts of the RPATH which point to
# directories outside the build tree to the install tree
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH FALSE)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Include Initial Options
# NOTE: seems like SimVascularDependentOptions should be read BEFORE
# including SimVascularExternalVersions.  This seems to break things,
# so for now repeating code to ignore mitk when buiding without sv4gui
if(DEFINED SV_USE_SV4_GUI)
  if(NOT SV_USE_SV4_GUI)
    set(SV_USE_MITK "OFF" CACHE BOOL "Force OFF" FORCE)
    set(SV_USE_MITK_SHARED "OFF" CACHE BOOL "Force OFF" FORCE)
  endif()
endif()
include(SimVascularExternalsVersions)
include(SimVascularOptions)
include(SimVascularDependentOptions)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Include SimVascular Internals
include(SimVascularInternals)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Include SimVascular Externals
include(SimVascularExternals)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Include SimVascular Third Party
include(SimVascularThirdParty)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Include SimVascular Licensed
include(SimVascularLicensed)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Include Solver - TEMPORARY
if(SV_USE_THREEDSOLVER)
  include(SimVascularSolver)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Set EXE names for the executables
set(SV_EXE "simvascular")
set(SV_PRESOLVER_EXE "svpre")
set(SV_FLOWSOLVER_EXE "svsolver")
set(SV_POSTSOLVER_EXE "svpost")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# start internal cache variables
set(SV_INCLUDE_DIRS "")
set(SV_CORE_LIBRARY_DIRS "")
set(SV_MODULE_DIRS "")
set(SV_PLUGIN_DIRS "")
set(SV_CORE_EXECUTABLE_DIRS "")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Tcl Setup
include(SimVascularTclSetup)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Doxygen
find_package(Doxygen)
if(DOXYGEN_FOUND)
	file(TO_NATIVE_PATH "${SV_BINARY_DIR}/Doxygen/" SV_DOCS_DIR_WORK)
	set(SV_DOCS_DIR ${SV_DOCS_DIR_WORK} CACHE PATH "Location to place docs")
	configure_file(${SV_SOURCE_DIR}/../Documentation/simvascular.Doxyfile.in
		${SV_BINARY_DIR}/simvascular.Doxyfile @ONLY)
	add_custom_target(doc
		${DOXYGEN_EXECUTABLE} ${SV_BINARY_DIR}/simvascular.Doxyfile
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
		COMMENT "Generating API documentation with Doxygen" VERBATIM
		)
endif(DOXYGEN_FOUND)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Package and Subdirectory Logic
# Now we begin actually doing the logic to figure out which directories to add#
# and finding the libraries that those parts need.
#-----------------------------------------------------------------------------
# Basic Core Libs
set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS}
  "Source/PythonAPI"
  "Source/sv2/Globals"
  "Source/sv/Utils"
  "Source/sv/Repository"
  "Source/sv3/Path"
  "Source/sv3/Common"
  "Source/vtkSV/Common"
  "Source/vtkSV/IO"
  "Source/vtkSV/Modules/Misc"
  "Source/vtkSV/Modules/Geometry"
  "Source/vtkSV/Modules/Boolean"
  "Source/vtkSV/Modules/NURBS"
  "Source/vtkSV/Modules/Parameterization"
  "Source/vtkSV/Modules/Segmentation"
  "Source/sv2/ImageProcessing"
  "Source/sv/Geometry"
  "Source/sv2/Segmentation"
  "Source/sv/Model/SolidModel"
  "Source/sv2/PostProcessing"
  "Source/sv/Mesh/MeshObject"
  "Source/sv/Model/PolyDataSolidModel"
  "Source/sv3/Segmentation")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Optional Core Libs
#-----------------------------------------------------------------------------
# Tetgen
if(SV_USE_TETGEN)
	set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/sv/Mesh/TetGenMeshObject")
endif()
# Meshsim
if(SV_USE_MESHSIM OR SV_USE_MESHSIM_DISCRETE_MODEL)
	set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/sv/Mesh/MeshSimMeshObject")
endif()
if(SV_USE_MESHSIM_DISCRETE_MODEL)
	set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/sv/Model/MeshSimDiscreteSolidModel")
endif()
# Adaptors
if(SV_USE_MESHIM_ADAPTOR OR SV_USE_TETGEN_ADAPTOR)
    set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/sv/Mesh/AdaptObject")
endif()
if(SV_USE_MESHSIM_ADAPTOR)
  set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/sv/Mesh/MeshSimAdapt")
endif()
if(SV_USE_TETGEN_ADAPTOR)
  set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/sv/Mesh/TetGenAdapt")
endif()
# VMTK
if(SV_USE_VMTK)
  set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/sv/Mesh/VMTKUtils")
endif()
# MMG
if(SV_USE_MMG)
	set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/sv/Mesh/MMGMeshUtils")
endif()
# ITK
if(SV_USE_ITK)
	set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/sv3/ITKSegmentation")
endif()
# OpenCASCADE
if(SV_USE_OpenCASCADE)
	set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/sv/Model/OCCTSolidModel")
endif()
# PARASOLID
if(SV_USE_PARASOLID)
	set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Licensed/ParasolidSolidModel")
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Modules
if(SV_USE_SV4_GUI)
  set(SV_MODULE_DIRS ${SV_MODULE_DIRS}
    "Source/sv4gui/Modules/Common"
    "Source/sv4gui/Modules/Path"
    "Source/sv4gui/Modules/Segmentation"
    "Source/sv4gui/Modules/Model/Common"
    "Source/sv4gui/Modules/Mesh/Common"
    "Source/sv4gui/Modules/Simulation"
    "Source/sv4gui/Modules/ROMSimulation"
    "Source/sv4gui/Modules/ProjectManagement"
    "Source/sv4gui/Modules/QtWidgets"
    "Source/sv4gui/Modules/ImageProcessing"
    "Source/sv4gui/Modules/svFSI"
		"Source/sv4gui/Modules/MachineLearning"
     )

  #if(SV_USE_ONEDSOLVER)
   # set(SV_MODULE_DIRS ${SV_MODULE_DIRS} "Source/sv4gui/Modules/ROMSimulation")
  #endif()

  if(SV_USE_OpenCASCADE)
    set(SV_MODULE_DIRS ${SV_MODULE_DIRS} "Source/sv4gui/Modules/Model/OCCT")
  endif()

  if(SV_USE_PARASOLID)
    set(SV_MODULE_DIRS ${SV_MODULE_DIRS} "Source/sv4gui/Modules/Model/Parasolid")
  endif()

  if(SV_USE_MESHSIM)
    set(SV_MODULE_DIRS ${SV_MODULE_DIRS} "Source/sv4gui/Modules/Mesh/MeshSim")
  endif()

endif()

#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Plugins
if(SV_USE_SV4_GUI)
  if(NOT SV_USE_MITK_CONFIG)
      set(SV_PLUGIN_DIRS ${SV_PLUGIN_DIRS}
      "Source/sv4gui/Plugins/org.sv.projectdatanodes"
      "Source/sv4gui/Plugins/org.sv.gui.qt.datamanager"
      "Source/sv4gui/Plugins/org.sv.gui.qt.projectmanager"
      "Source/sv4gui/Plugins/org.sv.gui.qt.pathplanning"
      "Source/sv4gui/Plugins/org.sv.gui.qt.segmentation"
      "Source/sv4gui/Plugins/org.sv.gui.qt.modeling"
      "Source/sv4gui/Plugins/org.sv.gui.qt.meshing"
      "Source/sv4gui/Plugins/org.sv.gui.qt.romsimulation"
      "Source/sv4gui/Plugins/org.sv.gui.qt.simulation"
      "Source/sv4gui/Plugins/org.sv.gui.qt.application"
      "Source/sv4gui/Plugins/org.sv.gui.qt.imageprocessing"
      "Source/sv4gui/Plugins/org.sv.gui.qt.svfsi")
      if(SV_USE_PYTHON)
        LIST(APPEND SV_PLUGIN_DIRS "Source/sv4gui/Plugins/org.sv.pythondatanodes")
      endif()
      if(SV_USE_MITK_SEGMENTATION)
        LIST(APPEND SV_PLUGIN_DIRS "Source/sv4gui/Plugins/org.sv.gui.qt.mitksegmentation")
      endif()
  endif()
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Executables
set(SV_CORE_EXECUTABLE_DIRS ${SV_CORE_EXECUTABLE_DIRS} "Source/Application")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Enable Testing
if(BUILD_TESTING)
	enable_testing()
	add_subdirectory(Testing)
endif()

if(WIN32)
	if(SV_ENABLE_DISTRIBUTION)
		set(SV_USE_WIN32_REGISTRY ON)
	endif()
endif()
if(SV_USE_WIN32_REGISTRY_FORCE_OFF)
	set(SV_USE_WIN32_REGISTRY OFF)
	message(STATUS "Forcing SV_USE_WIN32_REGISTRY off, even in distribution mode")
	message(STATUS "SV_USE_WIN32_REGISTRY: ${SV_USE_WIN32_REGISTRY}")
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Put timestamp
message(STATUS "Install timestamp: ${SV_RELEASE_TIMESTAMP}")
file(WRITE "${TEMP_DIR}/release-date" "${SV_RELEASE_TIMESTAMP}")
install(FILES "${TEMP_DIR}/release-date" DESTINATION ${SV_INSTALL_HOME_DIR} COMPONENT CoreFiles)

if(SV_ENABLE_DISTRIBUTION)
  add_subdirectory(${SV_DISTRIBUTION_DIR} ${SV_BINARY_DISTRIBUTION_DIR})
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Add GLOBAL_DEFINES to definitions
dev_message(" Global Flags: ${GLOBAL_DEFINES}")
add_definitions(${GLOBAL_DEFINES})
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Add Local Includes
set(LOCAL_LIBDIR lib)
set(SV_SCRIPT_TARGETS "" CACHE INTERNAL "" FORCE)
set(INDENTS "")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Include the Core subdirs
# These are handled here because other sub directories depend on them
# set
set(SV_INCLUDE_DIRS ${SV_INCLUDE_DIRS} "Source/Include")
if(SV_USE_THREEDSOLVER)
  set(SV_INCLUDE_DIRS ${SV_INCLUDE_DIRS} "svSolver/Code/FlowSolvers/Include")
endif()

# Include directories that don't have targets
if(SV_USE_SOLVERIO)
  include_directories("${SV_SOURCE_DIR}/ThirdParty/SolverIO/simvascular_solverio")
endif()
if(SV_USE_VMTK)
  include_directories("${SV_SOURCE_DIR}/ThirdParty/vmtk/simvascular_vmtk")
endif()

foreach(dir ${SV_INCLUDE_DIRS})
	add_subdirectory(${dir})
	include_directories(${SV_SOURCE_DIR}/${dir} ${SV_BINARY_DIR}/${dir})
endforeach()

foreach(dir ${SV_CORE_LIBRARY_DIRS})
	include_directories(${SV_SOURCE_DIR}/${dir} ${SV_BINARY_DIR}/${dir})
endforeach()

foreach(dir ${SV_MODULE_DIRS})
	include_directories(${SV_SOURCE_DIR}/${dir} ${SV_BINARY_DIR}/${dir})
endforeach()

foreach(dir ${SV_PLUGIN_DIRS})
        include_directories(${SV_SOURCE_DIR}/${dir} ${SV_SOURCE_DIR}/${dir}/src/internal ${SV_BINARY_DIR}/${dir})
endforeach()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Add directories
# Modules
if(SV_USE_THREEDSOLVER)
  add_subdirectory(svSolver/Code/FlowSolvers)
endif()

# Add the Core Direcotories
message(STATUS "Libraries:")
foreach(dir ${SV_CORE_LIBRARY_DIRS})
  message(STATUS "	${dir}")
  include_directories("${dir}")
  add_subdirectory("${dir}")
endforeach()

# Add the Modules
message(STATUS "Modules:")
foreach(dir ${SV_MODULE_DIRS})
  message(STATUS "	${dir}")
  include_directories("${dir}")
  add_subdirectory("${dir}")
endforeach()

# Add the Plugins
message(STATUS "Plugins:")
foreach(dir ${SV_PLUGIN_DIRS})
  message(STATUS "	${dir}")
  include_directories("${dir}")
  add_subdirectory("${dir}")
  get_filename_component(plugin_dir_name ${dir} NAME)
  string(REPLACE "." "_" plugin_name ${plugin_dir_name})
  list(APPEND SV_PLUGIN_NAMES ${plugin_name})
endforeach()
#-----------------------------------------------------------------------------

#----------------------------------------------------------------------------
# Setup Plugins for provisioning file, must after add_subdirectory for Modules
include(${CMAKE_CURRENT_SOURCE_DIR}/Source/sv4gui/Plugins/PluginList.cmake)
if(SV_USE_MITK_CONFIG)
  if(SV_USE_SV4_GUI)
    macro(GetMyTargetLibraries all_target_libraries varname)
      set(re_ctkplugin "^org_sv_[a-zA-Z0-9_]+$")
      set(_tmp_list)
      list(APPEND _tmp_list ${all_target_libraries})
      ctkMacroListFilter(_tmp_list re_ctkplugin OUTPUT_VARIABLE ${varname})
    endmacro()

    set(sv_app_fullpath "${CMAKE_CURRENT_SOURCE_DIR}/Source/Application^^${PROJECT_NAME}_Apps/simvascular")
    message(STATUS "sv app full path: ${sv_app_fullpath}")

    set(sv_plugins_fullpath "")
    foreach(sv_plugin ${SV_PLUGINS})
      list(APPEND sv_plugins_fullpath "Source/sv4gui/Plugins/${sv_plugin}")
    endforeach()

    message(STATUS "sv_plugins_fullpath: ${sv_plugins_fullpath}")

    ctkMacroSetupPlugins(${sv_plugins_fullpath}
                       APPS ${sv_app_fullpath}
                       BUILD_OPTION_PREFIX ${PROJECT_NAME}_
                       BUILD_ALL ${${PROJECT_NAME}_BUILD_ALL_PLUGINS}
                       COMPACT_OPTIONS)

  endif()
endif()

message(STATUS "Executables")
foreach(dir ${SV_CORE_EXECUTABLE_DIRS})
        message(STATUS "	${dir}")
        add_subdirectory("${dir}")
endforeach()
#-----------------------------------------------------------------------------

# add git submodules
if(SV_GIT_PULL_SUBMODULES)
  #message(STATUS "inside of SV_GIT_PULL_SUBMODULES")
  find_package(Git)
  message (STATUS "Checking if git repository: ${PROJECT_SOURCE_DIR}/../.git")  
  if(GIT_FOUND AND EXISTS "${PROJECT_SOURCE_DIR}/../.git")
  # Update submodules as needed
    message(STATUS "Submodule update")
    execute_process(COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive --remote
                        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                        RESULT_VARIABLE GIT_SUBMOD_RESULT)
    if(NOT GIT_SUBMOD_RESULT EQUAL "0")
            message(WARNING "git submodule update --init failed with ${GIT_SUBMOD_RESULT}, please checkout submodules")
    endif()
  endif()
endif()

#-----------------------------------------------------------------------------
# Configure Exe Scripts, this should be the last subdirectory
if(SV_USE_SV4_GUI)
  include(SimVascularInstallQt)
endif()
include(SimVascularInstallLibs)
add_subdirectory(Scripts)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Create SimVascularConfig.cmake
include(SimVascularConfigSetup)
#-----------------------------------------------------------------------------
