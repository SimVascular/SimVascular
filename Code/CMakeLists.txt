cmake_minimum_required(VERSION 2.8.12)

if (POLICY CMP0040)
	cmake_policy(SET CMP0040 OLD)
endif()
if (POLICY CMP0046)
	cmake_policy(SET CMP0046 OLD)
endif()
if (POLICY CMP0053)
	cmake_policy(SET CMP0053 OLD)
endif()
if (POLICY CMP0054)
	cmake_policy(SET CMP0054 OLD)
endif()

#-----------------------------------------------------------------------------
Project(SV)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Superbuild
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/CMake"
	"${CMAKE_MODULE_PATH}")

#-----------------------------------------------------------------------------
# CMake Includes
include(CheckLibraryExists)
include(GetPrerequisites)
include(GenerateExportHeader)

#-----------------------------------------------------------------------------
# SimVascular Includes
include(SimVascularMacros)
include(SimVascularSystemCheck)
include(SimVascularVersion)
include(SimVascularFunctionCheckCompilerFlags)

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
include(SimVascularOptions)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Include SimVascular Externals
include(SimVascularExternals)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Include SimVascular Internals
include(SimVascularInternals)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Include Dependent Options
include(SimVascularDependentOptions)
include(SimVascularExternals)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Set EXE names for the executables
set(SV_EXE "simvascular")
set(SV_PRESOLVER_EXE "svpre")
set(SV_FLOWSOLVER_EXE "svsolver")
set(SV_POSTSOLVER_EXE "svpost")

#-----------------------------------------------------------------------------
# start internal cache variables
set(SV_INCLUDE_DIRS "")
set(SV_CORE_LIBRARY_DIRS "")
set(SV_MODULE_DIRS "")
set(SV_PLUGIN_DIRS "")
set(SV_CORE_EXECUTABLE_DIRS "")

#-----------------------------------------------------------------------------
# SimVascular Setup
#-----------------------------------------------------------------------------
include(SimVascularInstallSetup)
include(SimVascularHomeSetup)
include(SimVascularTclSetup)
include(SimVascularPythonSetup)

#-----------------------------------------------------------------------------
# Package and Subdirectory Logic
# Now we begin actually doing the logic to figure out which directories to add#
# and finding the libraries that those parts need.
#-----------------------------------------------------------------------------
# Basic Core Libs
set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS}
	"Source/Common/Globals"
	"Source/Common/Utils"
	"Source/Common/Repository"
	"Source/ImageProcessing"
	"Source/Common/Geometry"
	"Source/Legacy/LevelSet"
	"Source/Model/SolidModel"
	"Source/PostProcessing"
	"Source/Mesh/MeshObject"
	"Source/Model/PolyDataSolidModel")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Modules
set(SV_MODULE_DIRS ${SV_MODULE_DIRS}
  "Source/Modules/Common"
  "Source/Modules/Path"
  "Source/Modules/Segmentation"
  "Source/Modules/Model"
  "Source/Modules/Mesh"
  "Source/Modules/Simulation"
  "Source/Modules/ProjectManagement"
  "Source/Modules/QtWidgets")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Plugins
if(NOT SV_USE_MITK_CONFIG)
  set(SV_PLUGIN_DIRS ${SV_PLUGIN_DIRS}
    "Source/Plugins/org.sv.projectdatanodes"
    "Source/Plugins/org.sv.gui.qt.projectmanager"
    "Source/Plugins/org.sv.gui.qt.pathplanning"
    "Source/Plugins/org.sv.gui.qt.segmentation"
    "Source/Plugins/org.sv.gui.qt.modeling"
    "Source/Plugins/org.sv.gui.qt.simulation"
    "Source/Plugins/org.sv.gui.qt.application")
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
set(SV_CORE_EXECUTABLE_DIRS ${SV_CORE_EXECUTABLE_DIRS} "Source/UI")
#-----------------------------------------------------------------------------

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
# We can not begin enabling libraries
# Libraries that we always need...
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# TCL
simvascular_external(TCL DOWNLOADABLE SHARED_LIB)
if(SV_USE_SYSTEM_TCL)
  set(SV_TCL_DIR ${TCL_DIR})
endif()

STRING(REGEX REPLACE
	"^.*libtcl([0-9]\\.*[0-9]).*$" "\\1" TCL_VERSION "${TCL_LIBRARY}")

#-----------------------------------------------------------------------------
# PYTHON
if(SV_USE_PYTHON)
	simvascular_external(PYTHON SYSTEM_DEFAULT SHARED_LIB)

	if(PYTHONLIBS_FOUND)
		if(NOT WIN32)
		  string(REPLACE "." ";" PYTHONLIBS_VERSION_LIST "${PYTHONLIBS_VERSION_STRING}")
		  list(GET PYTHONLIBS_VERSION_LIST 0 PYTHONLIBS_MAJOR_VERSION)
		  list(GET PYTHONLIBS_VERSION_LIST 1 PYTHONLIBS_MINOR_VERSION)
	        else()
		  set (PYTHONLIBS_MAJOR_VERSION 2)
		  set (PYTHONLIBS_MINOR_VERSION 7)
		endif()
	else()
		message(STATUS "")
		message(STATUS "Python Libs NOT FOUND")
		message(STATUS "Make sure you have python installed on your system.")
	endif()
        set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/TclPython")
        set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_PYTHON")
	set(SV_DEPENDS ${SV_DEPENDS} "PYTHON")
endif()

#-----------------------------------------------------------------------------
# VTK
set(SV_DEPENDS ${SV_DEPENDS} "TCL")
if(SV_USE_VTK_SHARED)
	set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} VTK)
	set(SV_INSTALL_EXTERNALS ON)
endif()
simvascular_external(VTK DOWNLOADABLE
	COMPONENTS
        vtkFiltersFlowPaths vtkWrappingTcl
	vtkRenderingTk vtkCommonDataModel vtkCommonCore vtkChartsCore vtkCommonExecutionModel
	vtkFiltersCore vtkIOLegacy)
if(SV_USE_SYSTEM_VTK)
  simvascular_get_external_path_from_include_dir(VTK)
endif()

#-----------------------------------------------------------------------------
# tkcximage (Legacy)
if(WIN32)
	if(SV_USE_TKCXIMAGE)
		find_library(TKCXIMAGE_DLL tkcximage)
		if(TKCXIMAGE_DLL)
			set(TKCXIMAGE_DLL_LIBRARY ${TKCXIMAGE_DLL})
			get_filename_component(TKCXIMAGE_DLL_PATH ${TKCXIMAGE_DLL} DIRECTORY CACHE)
			set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} "TKCXIMAGE")
		endif()
	endif()
endif()

#-----------------------------------------------------------------------------
# ZLIB
if(SV_USE_ZLIB)
	SET(USE_ZLIB ON)
	simvascular_third_party(zlib)
	if(NOT SV_USE_SYSTEM_ZLIB)
          set(ZLIB_LIBRARY ${SV_LIB_THIRDPARTY_ZLIB_NAME})
	else()
		find_package(ZLIB)
	endif()
else()
	unset(ZLIB_LIBRARY CACHE)
endif()

#-----------------------------------------------------------------------------
# Begin Plugin Logic
#-----------------------------------------------------------------------------
# Plugin Mmg
if(SV_USE_MMG)
	simvascular_external(MMG DOWNLOADABLE)
	set(USE_MMG ON)
	set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/Mesh/MMGMeshUtils")
	if(SV_USE_MMG_SHARED)
	  set(SV_INSTALL_EXTERNALS ON)
	  set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} "MMG")
	  set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_MMG_SHARED")
        endif()
      if(SV_USE_SYSTEM_MMG)
        simvascular_get_external_path_from_include_dir(MMG)
      endif()
endif()

#-----------------------------------------------------------------------------
# Plugin Tetgen
if(SV_USE_TETGEN)
	SET(USE_TETGEN ON)
	simvascular_third_party(tetgen)
	if(NOT SV_USE_SYSTEM_TETGEN)
		set(TETGEN_VERSION "1.5.1")
                set(TETGEN_LIBRARY ${SV_LIB_THIRDPARTY_TETGEN_NAME})
	else()
		find_package(TETGEN)
	endif()
	if(TETGEN_VERSION MATCHES "1.5.1")
		set(TETGEN151 ON)
	elseif(TETGEN_VERSION MATCHES "1.5.0")
		set(TETGEN150 ON)
	elseif(TETGEN_VERSION MATCHES "1.4.3")
		set(TETGEN143 ON)
	else()
		message(FATAL_ERROR "Unknown Tetgen versions, please specify!")
	endif()
	set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/Mesh/TetGenMeshObject")
else()
	unset(TETGEN_LIBRARY CACHE)
endif()

if(SV_USE_MESHSIM_ADAPTOR)
    set(SV_USE_SOLVERIO ON)
    set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/Mesh/AdaptObject")
elseif(SV_USE_TET_ADAPTOR)
    set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/Mesh/AdaptObject")
endif()

if(SV_USE_TET_ADAPTOR)
	SET(USE_TET_ADAPTOR ON)
        set(SV_USE_SOLVERIO ON)
	set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/Mesh/TetGenAdapt")
endif()

#-----------------------------------------------------------------------------
# VMTK
if(SV_USE_VMTK)
	set(USE_VMTK ON)
	set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "ThirdParty/vmtk")
endif()

#-----------------------------------------------------------------------------
# SOLVERIO
if(SV_USE_SOLVERIO)
        set(USE_SOLVERIO ON)
        set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "ThirdParty/SolverIO")
endif()

#-----------------------------------------------------------------------------
# GDCM
if(SV_USE_GDCM)
	simvascular_external(GDCM DOWNLOADABLE)
	set(USE_GDCM ON)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_GDCM")
	if(SV_USE_GDCM_SHARED)
	  set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} GDCM)
	  set(SV_INSTALL_EXTERNALS ON)
	endif()
        if(SV_USE_SYSTEM_GDCM)
          simvascular_get_external_path_from_include_dir(GDCM)
        endif()
endif()

#-----------------------------------------------------------------------------
# Plugin LevelSets
if(SV_USE_ITK)
	if(SV_USE_ITK_SHARED)
		set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} ITK)
		set(SV_INSTALL_EXTERNALS ON)
	endif()
	simvascular_external(ITK DOWNLOADABLE)

	set(USE_ITK ON)
	set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/Segmentation/ITK")
        include_directories("${SV_SOURCE_DIR}/Source/Segmentation/ITK/Include")
        include_directories("${SV_SOURCE_DIR}/Source/Segmentation/ITK/ITKCode")
        if(SV_USE_SYSTEM_ITK)
          simvascular_get_external_path_from_include_dir(ITK)
        endif()
endif()

#---
if(SV_USE_MESHSIM OR SV_USE_MESHSIM_DISCRETE_MODEL)
	set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/Mesh/MeshSimMeshObject")
endif()

if(SV_USE_MESHSIM_ADAPTOR)
	set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/Mesh/MeshSimAdapt")
endif()

#-----------------------------------------------------------------------------
# FREETYPE
if(SV_USE_FREETYPE)
	simvascular_external(FREETYPE DOWNLOADABLE)
	set(USE_FREETYPE ON)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_FREETYPE")
	if(SV_USE_FREETYPE_SHARED)
	  set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} FREETYPE)
	  set(SV_INSTALL_EXTERNALS ON)
	endif()
        if(SV_USE_SYSTEM_FREETYPE)
          set(SV_FREETYPE_DIR ${FREETYPE_DIR})
        endif()
  link_directories(${FREETYPE_DIR}/lib)
endif()

#-----------------------------------------------------------------------------
# OpenCASCADE
if(SV_USE_OpenCASCADE)
	simvascular_external(OpenCASCADE DOWNLOADABLE)
	set(USE_OpenCASCADE ON)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_OPENCASCADE")
	set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/Model/OCCTSolidModel")
	if(SV_USE_OpenCASCADE_SHARED)
	  set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} OpenCASCADE)
	  set(SV_INSTALL_EXTERNALS ON)
	endif()
        if(SV_USE_SYSTEM_OpenCASCADE)
          simvascular_get_external_path_from_include_dir(OpenCASCADE)
        endif()
endif()

#-----------------------------------------------------------------------------
# CTK
if(SV_USE_CTK)
    simvascular_external(CTK DOWNLOADABLE)

    if(SV_USE_CTK_SHARED)
      set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} CTK)
      set(SV_INSTALL_EXTERNALS ON)
    endif()
    if(SV_USE_SYSTEM_CTK)
      simvascular_get_external_path_from_include_dir(CTK)
    endif()
endif()

#-----------------------------------------------------------------------------
# SimpleITK
if(SV_USE_SimpleITK)
    simvascular_external(SimpleITK DOWNLOADABLE)

    if(SV_USE_SimpleITK_SHARED)
      set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} SimpleITK)
      set(SV_INSTALL_EXTERNALS ON)
    endif()
    if(SV_USE_SYSTEM_SimpleITK)
      simvascular_get_external_path_from_include_dir(SimpleITK)
    endif()
endif()

if(SV_USE_QT_GUI)
    set(CMAKE_PREFIX_PATH "${CMAKE_PREFIX_PATH}" CACHE PATH "")
    set(SimVascular_QT5_COMPONENTS Concurrent Designer OpenGL PrintSupport Script Sql Svg WebKitWidgets Xml XmlPatterns UiTools Help)
    find_package(Qt5 COMPONENTS ${SimVascular_QT5_COMPONENTS} REQUIRED)
    if(Qt5_DIR)
      get_filename_component(_Qt5_DIR "${Qt5_DIR}/../../../" ABSOLUTE)
      list(FIND CMAKE_PREFIX_PATH "${_Qt5_DIR}" _result)
      if(_result LESS 0)
        set(CMAKE_PREFIX_PATH "${_Qt5_DIR};${CMAKE_PREFIX_PATH}" CACHE PATH "" FORCE)
      endif()
    endif()
    if(NOT SV_USE_MITK_CONFIG)
      set(QT_LIBRARIES "")
      set(QT_INCLUDE_DIRS "")
      foreach(comp ${SimVascular_QT5_COMPONENTS})
        if(Qt5${comp}_LIBRARIES)
          set(QT_LIBRARIES ${QT_LIBRARIES} ${Qt5${comp}_LIBRARIES})
        endif()
        if(Qt5${comp}_INCLUDE_DIRS)
          set(QT_INCLUDE_DIRS ${QT_INCLUDE_DIRS} ${Qt5${comp}_INCLUDE_DIRS})
        endif()
      endforeach()
      include_directories(${QT_INCLUDE_DIRS})

      #CppMicroServices
      find_package(CppMicroServices)
    endif()
endif()

#-----------------------------------------------------------------------------
# MITK
if(SV_USE_MITK)
  if(SV_USE_MITK_CONFIG)
    simvascular_external(MITK DOWNLOADABLE NO_MODULE)
  else()
    simvascular_external(MITK DOWNLOADABLE)
  endif()

    if(SV_USE_MITK_SHARED)
      set(SV_EXTERNAL_SHARED_LIBS ${SV_EXTERNAL_SHARED_LIBS} MITK)
      set(SV_INSTALL_EXTERNALS ON)
    endif()
endif()

# Licensed Package Additions
#-----------------------------------------------------------------------------
# Parasolid
if(SV_USE_PARASOLID)
		if(EXISTS ${SV_SOURCE_DIR}/Licensed/ParasolidSolidModel/CMakeLists.txt)
			simvascular_external(PARASOLID SYSTEM_DEFAULT SHARED_LIB)
			if(WIN32)
				set(SV_EXTRA_PATHS ${SV_EXTRA_PATHS} ${PARASOLID_DLL_PATH})
			endif()
			SET(USE_PARASOLID ON)
			set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_PARASOLID")
			set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Licensed/ParasolidSolidModel")
			option(SV_USE_PARASOLID_SHARED_LIBRARIES "Build Parasolid as shared libraries" OFF)
			if (BUILD_SHARED_LIBS)
				set(SV_USE_PARASOLID_SHARED_LIBRARIES "ON" CACHE BOOL "Force ON" FORCE)
			endif()
		else()
			message("Parasolid requires an extra license.")
		endif()

endif()
if(SV_USE_MESHSIM_DISCRETE_MODEL)
	set(SV_CORE_LIBRARY_DIRS ${SV_CORE_LIBRARY_DIRS} "Source/Model/MeshSimDiscreteSolidModel")
endif()

#-----------------------------------------------------------------------------
# MeshSim
unset(MESHSIM_MODELER)
if(SV_USE_MESHSIM OR SV_USE_MESHSIM_DISCRETE_MODEL)
	unset(MESHSIM_COMP)
	if(APPLE)
		message(FATAL_ERROR "MeshSim not available on macos")
	endif()
	if(SV_USE_MESHSIM_DISCRETE_MODEL)
		set(USE_DISCRETE_MODEL ON)
		set(MESHSIM_COMP SimDiscrete)
	endif()
	if(SV_USE_PARASOLID)
		set(MESHSIM_MODELER "parasolid")
	endif()
	if(MESHSIM_MODELER MATCHES "parasolid")
        set(MESHSIM_SIMPARASOLID_VERSION SimParasolid270 CACHE STRING "When using parasolid, you need to specify which veriosn of the bridge library to use.")
        set(MESHSIM_ACCEPTED_SIMPARASOLID_VERSIONS SimParasolid271 SimParasolid270 SimParasolid260 SimParasolid251 SimParasolid250 SimParasolid241)
		SET_PROPERTY(CACHE MESHSIM_SIMPARASOLID_VERSION PROPERTY STRINGS ${MESHSIM_ACCEPTED_SIMPARASOLID_VERSIONS})
	endif()
	simvascular_external(MESHSIM COMPONENTS ${MESHSIM_COMP} ${MESHSIM_SIMPARASOLID_VERSION} SHARED_LIB SYSTEM_DEFAULT)
	if(MESHSIM_USE_LICENSE_FILE)
		set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DMESHSIM_USE_LICENSE_FILE")
		find_file(MESHSIM_LICENSE_FILE meshsim-license.dat PATHS ${MESHSIM_LICENSE_DIR})
		if(NOT MESHSIM_LICENSE_FILE)
			message(STATUS "")
			message(STATUS "MeshSim license NOT FOUND or specified.  Build will continue with a placeholder")
			message(STATUS "You will need to copy the license file into the build after compilation to use MeshSim functionality.")

			set(MESHSIM_LICENSE_FILE meshsim-license.dat)
		else()
			message(STATUS "MeshSim License: ${MESHSIM_LICENSE_FILE}")
		endif()
	else()
		unset(MESHSIM_LICENSE_FILE)
	endif()
	if(MESHSIM_EMBED_LICENSE_KEYS)
		set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DMESHSIM_EMBED_LICENSE_KEYS")
	endif()
	set(USE_MESHSIM ON)
endif()
message(STATUS "")
#-----------------------------------------------------------------------------
# Modules (Flowsolver)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# ThreeD Solver
if(SV_USE_THREEDSOLVER)
	option(SV_USE_SPARSE "Use sparse Library" ON)
	option(SV_USE_METIS "Use metis Library" ON)
	option(SV_USE_NSPCG "Use nspcg Library" ON)
	set(SV_USE_FORTRAN ON)
	set(SV_THREEDSOLVER_USE_SVLS ON)
	set(SV_USE_MPI ON)
	set(SV_THREEDSOLVER_USE_SOLVERIO ON)
	#set(FLOWSOLVER_CONFIG_PATHS
	#	"${SV_SOURCE_DIR}/Modules/ThreeDSolver/IncompressibleSolver/Solver" CACHE TYPE PATH)
	# find config file
	#set(FLOWSOLVER_CONFIG_FILE "${SV_SOURCE_DIR}/Modules/ThreeDSolver/IncompressibleSolver/Solver/default.inp")
	#set(FLOWSOLVER_CONFIG_PATH ${OUTBIN_DIR})
	#file(COPY ${FLOWSOLVER_CONFIG_FILE} DESTINATION ${FLOWSOLVER_CONFIG_PATH})
	#install(FILES ${FLOWSOLVER_CONFIG_FILE} DESTINATION ${SV_INSTALL_RUNTIME_DIR})
endif()

#-----------------------------------------------------------------------------
# Solver IO
if(SV_THREEDSOLVER_SOLVERIO_REDIRECT)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DBUILD_WITH_FLOWSOLVER_STDOUT_STDERR_REDIRECT")
endif()

#-----------------------------------------------------------------------------
#  SVLS
# svLS depends on the THREEDSOLVER build state so it must be here.
if(SV_THREEDSOLVER_USE_SVLS)
	set(SVLS_BUILD_TYPE "Source")
	#simvascular_external(svls SVEXTERN_DEFAULT)
	set(SV_USE_FORTRAN ON)
	set(SV_USE_MPI ON)
endif()

if(SV_THREEDSOLVER_USE_LESLIB)
	find_package(LESLIB REQUIRED)
endif()

#-----------------------------------------------------------------------------
# Add libraries for flowsolver
if(SV_USE_SPARSE)
	SET(USE_SPARSE ON)
        simvascular_third_party(sparse SOLVER_DEPEND)
	if(NOT SV_USE_SYSTEM_SPARSE)
          set(SPARSE_LIBRARY ${SV_LIB_THIRDPARTY_SPARSE_NAME})
	else()
		find_package(SPARSE)
	endif()
endif()

if(SV_USE_METIS)
	SET(USE_METIS ON)
        simvascular_third_party(metis SOLVER_DEPEND)
	if(NOT SV_USE_SYSTEM_METIS)
          set(METIS_LIBRARY ${SV_LIB_THIRDPARTY_METIS_NAME})
	else()
		find_package(METIS)
	endif()
endif()

if(SV_USE_NSPCG)
	SET(USE_NSPCG ON)
        simvascular_third_party(nspcg SOLVER_DEPEND)
	if(NOT SV_USE_SYSTEM_NSPCG)
          set(NSPCG_LIBRARY ${SV_LIB_THIRDPARTY_NSPCG_NAME})
	else()
		find_package(NSPCG)
	endif()
endif()

#-----------------------------------------------------------------------------
# Remaining optional dependencies
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Enable Interl Runtime libs if we need or want them
if(SV_USE_INTELRUNTIME OR SV_USE_INTEL)
	simvascular_external(INTELRUNTIME SYSTEM_DEFAULT SHARED_LIB)
	if(SV_USE_SYSTEM_ZLIB)
		set(INTELRUNTIME_LIB_DIR "" CACHE PATH "A directory where intel runtimes may be, use this if its not in the path")
	endif()
	set(SV_USE_INTELRUNTIME ON)
	set(SV_USE_INTEL ON)
else()
	unset(INTELRUNTIME_LIBRARIES CACHE)
endif()

#-----------------------------------------------------------------------------
# Enable Fortran
if(SV_USE_FORTRAN)
	enable_language(Fortran)
	if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
		#set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-fbounds-check")
		set(CMAKE_Fortran_FLAGS "-ffixed-line-length-132 -cpp")
	else()
		#set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-check bounds")
		set(CMAKE_Fortran_FLAGS "-132 -fpp")
	endif()
endif()

#-----------------------------------------------------------------------------
# Enable MPI
if(SV_USE_MPI OR USE_MPI)
	set(SV_USE_MPI ON)
	if (NOT SV_USE_DUMMY_MPICH2)
		simvascular_external(MPI SYSTEM_DEFAULT SHARED_LIB)
		if(MPI_FOUND)
			get_filename_component(MPI_LIBRARY_DIR ${MPI_LIBRARY} PATH)
		endif()
		if(WIN32)
			find_library(MPI_fmpich2_LIBRARY NAMES fmpich2 HINTS ${MPI_LIBRARY_DIR})
			set(MPI_EXTRA_LIBRARY ${MPI_EXTRA_LIBRARY} ${MPI_fmpich2_LIBRARY} ${MPI_CXX_LIBRARIES})
			#message("${MPI_EXTRA_LIBRARY}")
		endif()

		# include_directories(${MPI_Fortran_INCLUDE_PATH})
		if(SV_DEVELOPER_OUTPUT)
			#getListOfVarsPrefix("MPI" _VARLIST)
			#print_vars(_VARLIST)
		endif()
		if(SV_USE_MSMPI)
			# TODO(jmerkow): Change this.
			set(SV_MPI_DIR "${CMAKE_CURRENT_SOURCE_DIR}/ThirdParty/msmpi/")
			set(SV_MPI_LIB_DIR  "${SV_MPI_DIR}/Lib/x64/")
			set(SV_MPI_INCLUDE_PATH "${SV_MPI_DIR}/Include/;${SV_MPI_DIR}/Include/x64/")
			set(SV_MPI_EXTRA_LIBRARY "")
			set(SV_MPI_Fortran_LIBRARIES "${SV_MPI_LIB_DIR}/msmpi.lib;${SV_MPI_LIB_DIR}/msmpifmc.lib;${SV_MPI_LIB_DIR}/msmpifec.lib")
		else()
			set(SV_MPI_EXTRA_LIBRARY ${MPI_EXTRA_LIBRARY})
			set(SV_MPI_Fortran_LIBRARIES ${MPI_Fortran_LIBRARIES})
			set(SV_MPI_INCLUDE_PATH ${MPI_Fortran_INCLUDE_PATH})
		endif()
		include_directories(${SV_MPI_INCLUDE_PATH})
	else()
		set(SV_MPI_EXTRA_LIBRARY lib_extra_simvascular_dummympi)
		set(SV_MPI_Fortran_LIBRARIES lib_fortran_simvascular_dummympi)
	endif()
endif()

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
message(STATUS "Install timestamp: ${SV_RELEASE_TIMESTAMP}")
file(WRITE "${TEMP_DIR}/release-date" "${SV_RELEASE_TIMESTAMP}")
install(FILES "${TEMP_DIR}/release-date" DESTINATION ${SV_INSTALL_HOME_DIR})

if(SV_ENABLE_DISTRIBUTION)
        add_subdirectory(${SV_DISTRIBUTION_DIR} ${SV_BINARY_DISTRIBUTION_DIR})
endif()
#-----------------------------------------------------------------------------
# Now we need to finish configuring now that the external projects are configured
#-----------------------------------------------------------------------------
foreach(proj ${SV_EXTERNALS_LIST})
  if(SV_USE_${proj}_SHARED)
    simvascular_install_external(${proj})
  endif()
endforeach()

#-----------------------------------------------------------------------------
# TCL Setup
# TCL has two include directories, the macro only includes one.
include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})
if(WIN32)
	GET_FILENAME_COMPONENT(TCL_DLL_PATH "${TCL_TCLSH}" PATH)
endif()

find_package(TCL)

#-----------------------------------------------------------------------------
# GDCM Setup
#-----------------------------------------------------------------------------
if(SV_USE_GDCM)
  find_package(GDCM)
endif()

# ITK Setup
#-----------------------------------------------------------------------------
if(SV_USE_ITK)
  find_package(ITK REQUIRED)
  include(${ITK_USE_FILE})
endif()

# FREETYPE Setup
#-----------------------------------------------------------------------------
if(SV_USE_FREETYPE)
  find_package(FREETYPE)
endif()

# OpenCASCADE Setup
#-----------------------------------------------------------------------------
if(SV_USE_OpenCASCADE)
  find_package(OpenCASCADE)
endif()

#-----------------------------------------------------------------------------
# MMG Setup
if(SV_USE_MMG)
  find_package(MMG)
endif()

#-----------------------------------------------------------------------------
# CTK
if(SV_USE_CTK)
  find_package(CTK)
endif()

#-----------------------------------------------------------------------------
# SimpleITK
if(SV_USE_SimpleITK)
  find_package(SimpleITK)
endif()

#-----------------------------------------------------------------------------
# MITK
if(SV_USE_MITK)
  if(SV_USE_MITK_CONFIG)
    find_package(MITK NO_MODULE)
    set(MITK_LIBRARIES MitkCore
                       MitkAppUtil
                       MitkQtWidgets
                       MitkQtWidgetsExt
                       MitkMapperExt
                       MitkImageDenoising
                       MitkSegmentationUI
                       MitkSegmentation
                       MitkSceneSerialization)
  else()
    find_package(MITK)
  endif()
  include_directories(${MITK_INCLUDE_DIRS})
  include_directories(${CTK_INCLUDE_DIRS})
endif()

#-----------------------------------------------------------------------------
# VTK Setup
find_package(VTK COMPONENTS
	vtkFiltersFlowPaths vtkWrappingTcl vtkRenderingTk
	vtkCommonDataModel vtkCommonCore vtkChartsCore vtkCommonExecutionModel
	vtkFiltersCore vtkIOLegacy ${VTK_MODULES_ENABLED})
include(${VTK_USE_FILE})
include(VTKSVConfig)
#Does not pick up vtkCommonCoreTCL, need to add manually
set(VTK_LIBRARIES ${VTK_LIBRARIES} vtkCommonCoreTCL)
if(SV_USE_VTK_SHARED)
  set(SV_INSTALL_EXTERNALS ON)
  set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_VTK_SHARED")
endif()
# SV uses non stanard libraries, this adds them, also adds the intel libraries
#file(GLOB_RECURSE vtk_new_libs RELATIVE ${VTK_DIR}/lib/ "${VTK_DIR}/lib/*TCL*${CMAKE_STATIC_LIBRARY_SUFFIX}")
#file(GLOB_RECURSE vtk_new_libs2 RELATIVE ${VTK_DIR}/lib/ "${VTK_DIR}/lib/*Tcl*${CMAKE_STATIC_LIBRARY_SUFFIX}")
#list(APPEND vtk_new_libs ${vtk_new_libs2})
#string(REGEX REPLACE [a-z|A-Z]*/${CMAKE_STATIC_LIBRARY_PREFIX} "" vtk_new_libs "${vtk_new_libs}")
#if (CMAKE_STATIC_LIBRARY_PREFIX)
#	string(REGEX REPLACE ${CMAKE_STATIC_LIBRARY_PREFIX} "" vtk_new_libs "${vtk_new_libs}")
#endif()
#string(REGEX REPLACE "-[0-9]\\.*[0-9]\\${CMAKE_STATIC_LIBRARY_SUFFIX}" "" vtk_new_libs "${vtk_new_libs}")
#list(REMOVE_DUPLICATES vtk_new_libs)
#
#set(VTK_LIBRARIES ${VTK_LIBRARIES} ${vtk_new_libs})
if(${VTK_DIR} MATCHES "intel")
	#message(STATUS "VTK appears to be built with intel, adding libraries")
	set(VTK_LIBRARIES ${VTK_LIBRARIES} ${INTELRUNTIME_LIBRARIES})
endif()

#-----------------------------------------------------------------------------
# TetGen Setup
if(SV_USE_TETGEN)
	if(TETGEN_VERSION MATCHES "1.5.1")
		set(TETGEN151 ON)
	elseif(TETGEN_VERSION MATCHES "1.5.0")
		set(TETGEN150 ON)
	elseif(TETGEN_VERSION MATCHES "1.4.3")
		set(TETGEN143 ON)
	else()
		message(FATAL_ERROR "Unknown Tetgen versions, please specify!")
	endif()
endif()

if(SV_USE_VMTK)
	# no extra setups
endif()

#-----------------------------------------------------------------------------
# Find parasolid dll on windows to definitions
if(SV_USE_PARASOLID)
	if(PARASOLID_FOUND)
		include_directories(${PARASOLID_INCLUDE_DIR})
	endif()
	if (SV_USE_PARASOLID_SHARED_LIBRARIES)
		set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_PARASOLID_SHARED")
	endif()
endif()

if(SV_USE_OpenCASCADE)
	if(OpenCASCADE_FOUND)
		include_directories(${OpenCASCADE_INCLUDE_DIR})
	endif()
endif()

if(SV_USE_MESHSIM)
	if(MESHSIM_FOUND)
		include_directories(${MESHSIM_INCLUDE_DIR})
	endif()
	if (SV_USE_MESHSIM_SHARED_LIBRARIES)
		set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_MESHSIM_SHARED")
	endif()
endif()

if (SV_USE_MESHSIM_DISCRETE_SMODEL)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_MESHSIM_DISCRETE_MODEL")
	if (SV_USE_MESHSIM_DISCRETE_SHARED_LIBRARIES)
		set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_MESHSIM_DISCRETE_MODEL_SHARED")
	endif()
endif()


#-----------------------------------------------------------------------------
# Add GLOBAL_DEFINES to definitions
dev_message(" Global Flags: ${GLOBAL_DEFINES}")
add_definitions(${GLOBAL_DEFINES})

#-----------------------------------------------------------------------------
# Add Local Includes
set(LOCAL_LIBDIR Lib)
set(SV_SCRIPT_TARGETS "" CACHE INTERNAL "" FORCE)
set(INDENTS "")

#-----------------------------------------------------------------------------
# Include the Core subdirs
# These are handled here because other sub directories depend on them
# set
set(SV_INCLUDE_DIRS ${SV_INCLUDE_DIRS}
			 	"Source/Include"
			 	)
if(SV_USE_THREEDSOLVER)
  set(SV_INCLUDE_DIRS ${SV_INCLUDE_DIRS} "svSolver/Code/FlowSolvers/Include")
endif()

foreach(dir ${SV_INCLUDE_DIRS})
	add_subdirectory(${dir})
	include_directories(${SV_SOURCE_DIR}/${dir}
						${SV_BINARY_DIR}/${dir})
endforeach()

foreach(dir ${SV_CORE_LIBRARY_DIRS})
	include_directories("${dir}")
endforeach()

#--------------------------------------------------------------------
# Includes for QT GUI
if(SV_USE_QT_GUI)
  set(f svConfigure.h)
  configure_file(
    ${SV_SOURCE_DIR}/Source/Include/${f}.in
    ${SV_BINARY_DIR}/Source/Include/${f}
    )
endif()

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
foreach(dir ${SV_PLUGINS_DIRS})
  message(STATUS "	${dir}")
  include_directories("${dir}")
  add_subdirectory("${dir}")
endforeach()

#----------------------------------------------------------------------------
# Setup Plugins for provisioning file, must after add_subdirectory for Modules
include(${CMAKE_CURRENT_SOURCE_DIR}/Source/Plugins/PluginList.cmake)
if(SV_USE_MITK_CONFIG)
  if(SV_USE_QT_GUI)
    macro(GetMyTargetLibraries all_target_libraries varname)
      set(re_ctkplugin "^org_sv_[a-zA-Z0-9_]+$")
      set(_tmp_list)
      list(APPEND _tmp_list ${all_target_libraries})
      ctkMacroListFilter(_tmp_list re_ctkplugin OUTPUT_VARIABLE ${varname})
    endmacro()

    set(sv_app_fullpath "${CMAKE_CURRENT_SOURCE_DIR}/Source/UI^^${PROJECT_NAME}_Apps/simvascular")
    message(STATUS "sv app full path: ${sv_app_fullpath}")

    set(sv_plugins_fullpath "")
    foreach(sv_plugin ${SV_PLUGINS})
      list(APPEND sv_plugins_fullpath "Source/Plugins/${sv_plugin}")
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
# Configure Exe Scripts, this should be the last subdirectory
if(SV_USE_QT_GUI)
  include(SimVascularInstallQT)
endif()
include(SimVascularInstallLibs)
add_subdirectory(Scripts)
