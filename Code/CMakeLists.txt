cmake_minimum_required(VERSION 2.8.12)

if (POLICY CMP0040)
	cmake_policy(SET CMP0040 OLD)
endif()
if (POLICY CMP0046)
	cmake_policy(SET CMP0046 OLD)
endif()
if (POLICY CMP0054)
	cmake_policy(SET CMP0054 OLD)
endif()


#-----------------------------------------------------------------------------
# Superbuild
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/CMake"  
	"${CMAKE_MODULE_PATH}")

set(EXTERNAL_PROJECT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Superbuild)
set(EXTERNAL_PROJECT_FILE_PREFIX "")
include(ExternalProject)
include(ExternalProjectDependency)

#-----------------------------------------------------------------------------
# Do some apple logic
if(APPLE)
	set ( CMAKE_OSX_ARCHITECTURES "" CACHE STRING "" FORCE )
	#	set(CMAKE_OSX_DEPLOYMENT_TARGET "" CACHE STRING "" FORCE )
	#	set(CMAKE_OSX_SYSROOT "" CACHE STRING "" FORCE)
	# Note: By setting CMAKE_OSX_* variables before any enable_language() or project() calls,
	#       we ensure that the bitness will be properly detected.
	mark_as_superbuild(
		VARS 
		CMAKE_OSX_ARCHITECTURES:STRING CMAKE_OSX_SYSROOT:PATH 
		CMAKE_OSX_DEPLOYMENT_TARGET:STRING)

endif()
#-----------------------------------------------------------------------------
# Developer flag (Output extra info during configure)
option(SimVascular_DEVELOPER_OUTPUT "This is a developer mode to print extra messages during configure" OFF)
mark_as_superbuild(SimVascular_DEVELOPER_OUTPUT)
#-----------------------------------------------------------------------------
# Include Macros
include(SimVascularMacros)
include(SimVascularSuperBuildURLs)

#-----------------------------------------------------------------------------
Project(SimVascular)
#-----------------------------------------------------------------------------
if(APPLE)
	set(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH} "/opt/local/lib")
endif()
#-----------------------------------------------------------------------------
# Set a default build type (if none was specified)
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
	message(STATUS "Setting build type to 'RelWithDebInfo' as none was specified.")
	set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Choose the type of build." FORCE)
	mark_as_advanced(CMAKE_BUILD_TYPE)
	# Set the possible values of build type for cmake-gui
	set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
		"MinSizeRel" "RelWithDebInfo")
endif()
if(NOT CMAKE_CONFIGURATION_TYPES)
	mark_as_superbuild(VARS CMAKE_BUILD_TYPE ALL_PROJECTS)
endif()

#-----------------------------------------------------------------------------
# Superbuild Option
option(SimVascular_SUPERBUILD
	"Build ${PROJECT_NAME} and the projects it depends on.
	This must be turn on if your wish to download any packages." ON)

set(SuperBuild_Libraries_Dir "${svlibs_OPEN_URLBASE}" CACHE TYPE PATH)
mark_as_superbuild(SuperBuild_Libraries_Dir)

#-----------------------------------------------------------------------------
# unset NEED variables
unset(USE_Fortran)
unset(USE_MPI)
unset(USE_INTEL)
unset(USE_SVLS)

#-----------------------------------------------------------------------------
# Set EXE names for the executables
set(SIMVASCULAR_EXE "simvascular")
set(ADAPTOR_EXE "svadapt")
set(PRESOLVER_EXE "svpre")
set(FLOWSOLVER_EXE "svsolver")
set(POSTSOLVER_EXE "svpost")
set(TET_ADAPTOR_EXE "svtetadapt")

#-----------------------------------------------------------------------------
# start internal cache variables
set(SIMVASCULAR_INCLUDE_DIRS "")
set(SIMVASCULAR_CORE_LIBRARY_DIRS "")
set(SIMVASCULAR_CORE_EXECUTABLE_DIRS "")

#-----------------------------------------------------------------------------
# Includes
#-----------------------------------------------------------------------------
include(CheckLibraryExists)
include(SimVascularSystemCheck)
include(SimVascularVersion)
include(SimVascularInstallSetup)
include(SimVascularHomeSetup)
include(SimVascularTclSetup)

#-----------------------------------------------------------------------------
# Setting up default directories for SimVascular Externals
# Note: *These can be changes by the user if they have moved the sv_extern directories*
# This is part of he legacy build system

include(SetupSimVascularExternalsDefaults)

#-----------------------------------------------------------------------------
# Start Option Section
#-----------------------------------------------------------------------------
# This section sets up all the build options that are presented to the user and
# sets their default values

#-----------------------------------------------------------------------------
# Basic Options (CMAKE Defaults)
option(BUILD_SHARED_LIBS "Build ${PROJECT_NAME} as shared libraries." OFF)
mark_as_superbuild(VARS BUILD_SHARED_LIBS ALL_PROJECTS)

find_package(Doxygen)
if(DOXYGEN_FOUND)
	file(TO_NATIVE_PATH "${SimVascular_BINARY_DIR}/Doxygen/" SimVascular_DOCS_DIR_WORK)
	set(SimVascular_DOCS_DIR ${SimVascular_DOCS_DIR_WORK} CACHE PATH "Location to place docs")
	configure_file(${SimVascular_SOURCE_DIR}/../Documentation/simvascular.Doxyfile.in 
		${SimVascular_BINARY_DIR}/simvascular.Doxyfile @ONLY)
	add_custom_target(doc
		${DOXYGEN_EXECUTABLE} ${SimVascular_BINARY_DIR}/simvascular.Doxyfile
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
		COMMENT "Generating API documentation with Doxygen" VERBATIM
		)
endif(DOXYGEN_FOUND)

if(NOT BUILD_SHARED_LIBS)
	set(SIMVASCULAR_INSTALL_HEADERS OFF)
	set(SIMVASCULAR_INSTALL_EXTERNALS OFF)
	set(SIMVASCULAR_INSTALL_LIBS OFF)
endif()

#-----------------------------------------------------------------------------
# SimVascular Build options
option(SimVascular_ONLY_BUILD_FLOWSOLVER "Option ONLY build the 3D Solver (No GUI)" OFF)
option(SimVascular_SUPPRESS_WARNINGS "Option to suppress all compiler warnings while compiling" ON)

mark_as_superbuild(SimVascular_ONLY_BUILD_FLOWSOLVER)
mark_as_superbuild(SimVascular_SUPPRESS_WARNINGS)

if(SimVascular_ONLY_BUILD_FLOWSOLVER)
	set(BUILD_ThreeDSolver ON)
endif()

if(WIN32)
	option(SimVascular_USE_WIN32_REGISTRY "Use Windows registry to obtain certain settings (install mode)" OFF)

	mark_as_advanced(SimVascular_USE_WIN32_REGISTRY)
	mark_as_superbuild(SimVascular_USE_WIN32_REGISTRY:BOOL)
endif()

#-----------------------------------------------------------------------------
# FlowSolver Options 
if(BUILD_ThreeDSolver)

endif()

#-----------------------------------------------------------------------------
# General Library Options
option(SimVascular_USE_ZLIB "Use ZLib" ON)
option(SimVascular_USE_MPICH2 "Use MPICH2" ON)
option(SimVascular_USE_DUMMY_MPICH2 "Use Dummy MPICH2" OFF)
mark_as_superbuild(SimVascular_USE_ZLIB)
mark_as_superbuild(SimVascular_USE_MPICH2)
mark_as_superbuild(SimVascular_USE_DUMMY_MPICH2)

option(SimVascular_USE_GLIB "Use GLIB Library" OFF)
option(SimVascular_USE_GTS "Use GTS Library" OFF)
mark_as_superbuild(SimVascular_USE_GLIB)
mark_as_superbuild(SimVascular_USE_GTS)

#-----------------------------------------------------------------------------
# Plugins
option(Plugin_ITK "Enable ITK Plugin" ON)
mark_as_superbuild(Plugin_ITK)

option(Plugin_VMTK "Enable VMTK Plugin" ON)
mark_as_superbuild(Plugin_VMTK)

option(Plugin_TETGEN "Enable Tetgen Meshing Plugin" ON)
mark_as_superbuild(Plugin_TETGEN)
if(Plugin_TETGEN)
	option(Plugin_TET_ADAPTOR "Option to use open source mesh adaption" OFF)
	mark_as_superbuild(Plugin_TET_ADAPTOR)
endif()

if(Plugin_TET_ADAPTOR)
	#set(USE_Fortran ON)
	set(BUILD_ThreeDSolver ON)
endif()

#-----------------------------------------------------------------------------
# Adaptor Options
# Adaptor converts objects between the different solid models.
option(Licensed_ADAPTOR "Build the adapter (Requires Fortran and MeshSim)" OFF)
mark_as_superbuild(Licensed_ADAPTOR)

if(Licensed_ADAPTOR)
	set(BUILD_ThreeDSolver ON)
	set(Licensed_MESHSIM ON)
endif()


#-----------------------------------------------------------------------------
# Commercial Software Options: Solid Models - Parasolid
option(Licensed_PARASOLID "Parasolid" OFF)
mark_as_superbuild(Licensed_PARASOLID)

#-----------------------------------------------------------------------------
# Commercial Software Options: Meshing - MeshSim
option(Licensed_MESHSIM "Use MeshSim commercial libraries.  Requires licenese" OFF)
if(Licensed_MESHSIM)
	option(Licensed_MESHSIM_DISCRETE_SOLIDMODEL "" ON)
	if(WIN32)
		set(CMAKE_CXX_STANDARD_LIBRARIES ${CMAKE_CXX_STANDARD_LIBRARIES} rpcrt4.lib)
		option(MESHSIM_LICENSE_IN_WIN32_REGISTRY "Use system registry in windows" ON)
		option(MESHSIM_USE_LICENSE_FILE "Use a license file for MeshSim" OFF)
		option(MESHSIM_EMBED_LICENSE_KEYS "Embed License information for MeshSim" OFF)
	else()
		option(MESHSIM_USE_LICENSE_FILE "Use a license file for MeshSim" ON)
		option(MESHSIM_EMBED_LICENSE_KEYS "Embed License information for MeshSim" OFF)
	endif()
else()
	unset(MESHSIM_USE_LICENSE_FILE  CACHE)
	unset(MESHSIM_EMBED_LICENSE_KEYS  CACHE)
	unset(MESHSIM_LICENSE_IN_WIN32_REGISTRY  CACHE)
	UNset(MESHSIM_USE_DISCRETE_MODEL CACHE)
endif()
mark_as_superbuild(Licensed_MESHSIM:BOOL)
mark_as_superbuild(Licensed_MESHSIM_DISCRETE_SOLIDMODEL:BOOL)
mark_as_superbuild(MESHSIM_LICENSE_IN_WIN32_REGISTRY:BOOL)
mark_as_superbuild(MESHSIM_USE_LICENSE_FILE:BOOL)
mark_as_superbuild(MESHSIM_EMBED_LICENSE_KEYS:BOOL)


#-----------------------------------------------------------------------------
# Solver Build Options (Modules)
#-----------------------------------------------------------------------------
option(BUILD_ThreeDSolver "Option to build flowsolver modules (requires Fortran)" OFF)
if(BUILD_ThreeDSolver)
	#option(BUILD_ThreeDSolver "Option to build 3D-solver module (requires Fortran)" ON)
	option(ThreeDSolver_BUILD_SolverIO "Option to build solverIO module (requires Fortran)" ON)
	option(ThreeDSolver_BUILD_svPre "Option to build Pre-solver module (requires Fortran)" ON)
	option(ThreeDSolver_BUILD_svPost "Option to build post-solver module" ON)
	option(ThreeDSolver_SOLVER_IO_REDIRECT "Option to redirect solver IO" OFF)
	option(ThreeDSolver_USE_CORONARY "" ON)
	option(ThreeDSolver_USE_CLOSEDLOOP "" ON)
	option(ThreeDSolver_USE_VARWALL "" ON)
	option(ThreeDSolver_USE_VTK "" OFF)
endif()
mark_as_superbuild(BUILD_ThreeDSolver)
mark_as_superbuild(ThreeDSolver_BUILD_SolverIO)
mark_as_superbuild(ThreeDSolver_BUILD_svPre)
mark_as_superbuild(ThreeDSolver_BUILD_svPost)
mark_as_superbuild(ThreeDSolver_USE_IO_REDIRECT)
mark_as_superbuild(ThreeDSolver_USE_CORONARY)
mark_as_superbuild(ThreeDSolver_USE_CLOSEDLOOP)
mark_as_superbuild(ThreeDSolver_USE_VARWALL)
mark_as_superbuild(ThreeDSolver_USE_VTK)


#-----------------------------------------------------------------------------
# Linear Solver Options: SVLS
if(BUILD_ThreeDSolver)
	option(ThreeDSolver_USE_SVLS "Use svLS as linear solver" ON )
	if(ThreeDSolver_USE_SVLS)
		set(USE_SVLS 1)
	endif()
	mark_as_superbuild(ThreeDSolver_USE_SVLS)
	set(SVLS_BUILD_TYPE "Source")

	option(ThreeDSolver_USE_LESLIB "Use leslib as linear solver" OFF )
	if(ThreeDSolver_USE_LESLIB)
		set(USE_LESLIB 1)
	endif()
	mark_as_superbuild(ThreeDSolver_USE_LESLIB)
endif()

#-----------------------------------------------------------------------------
# Start Configuration Steps and Second Generation Options
#-----------------------------------------------------------------------------
# Compiler Flags
if(SimVascular_SUPPRESS_WARNINGS) 
	add_definitions("-w")
endif()
if(LINUX)
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pthread")
	set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -pthread -static")
endif()
if(CMAKE_COMPILER_IS_GNUCXX)
	add_definitions("-fpermissive")
endif()
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -pthread -static")
#-----------------------------------------------------------------------------
# Start Global Defines
#-----------------------------------------------------------------------------
# Global definesis a catch variable for all the compiler definitions.
# These flags are stored (as opposed to adding directly with ADD_DEFINITIONS)
# so the flags may be printed for debuging purposes.
if(CMAKE_COMPILER_IS_GNUCXX)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DGCC")
endif()

if(NOT BUILD_SHARED_LIBS)
	set(SIMVASCULAR_STATIC_BUILD "1")
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DCV_STATIC_LINK -DSIMVASCULAR_STATIC_BUILD")
elseif()
	set(SIMVASCULAR_STATIC_BUILD "0")
endif()

#-----------------------------------------------------------------------------
# OS Specific build flags
#-----------------------------------------------------------------------------
# All OS
set(USE_NOTIMER ON)

# Windows (32 and 64)
if(WIN32)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DWINDOWS -DWIN32")
	if(NOT IS64)
		set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -D_X86_")
	endif()
	if(CYGWIN)
		set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DCYGWIN")
	endif()
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DCV_WRAP_FORTRAN_IN_CAPS_NO_UNDERSCORE")
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -D__VC__")
	check_library_exists("${CMAKE_CXX_STANDARD_LIBRARIES}" gethostname "" HAVE_STDGETHOSTNAME)
	if(NOT HAVE_STDGETHOSTNAME)
		check_library_exists("wsock32.lib" gethostname "" HAVE_WSOCK_GETHOSTNAME)
		if(HAVE_WSOCK_GETHOSTNAME)
			set (CMAKE_CXX_STANDARD_LIBRARIES "${CMAKE_CXX_STANDARD_LIBRARIES} wsock32.lib")
		else()
			MESSAGE(AUTHOR_WARNING "gethostname has not beed found! The flowsolver will not compile")
		endif()
	endif()
	set (CMAKE_CXX_STANDARD_LIBRARIES "${CMAKE_CXX_STANDARD_LIBRARIES} Shlwapi.lib")
	mark_as_superbuild(CMAKE_CXX_STANDARD_LIBRARIES)


	#set (CMAKE_CXX_STANDARD_LIBRARIES "${CMAKE_CXX_STANDARD_LIBRARIES} ws2_32.lib wsock32.lib" CACHE)
	
	#set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DCV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE")
endif()

# Visual Studio flags
if(MSVC)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DMSVC /EHsc")
endif()

# Linux
if(UNIX)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DUNIX")
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DCV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE")
endif()

# Visual Studio Linker Flags
if(MSVC)
	set(CMAKE_EXE_LINKER_FLAGS "/LARGEADDRESSAWARE /INCREMENTAL:NO /FIXED:NO /RELEASE /NOLOGO")
	set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /VERBOSE:LIB")
	set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:libc.lib /NODEFAULTLIB:libcd.lib")
	set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libcpmt.lib")
	set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:libcmtd.lib /NODEFAULTLIB:libcpmtd.lib")
	set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:msvcrtd.lib")
	set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /MACHINE:X64 /subsystem:console /D__VC__")
	set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /STACK:10000000,10000000")
endif()

#-----------------------------------------------------------------------------
# Package and Subdirectory Logic
# Now we begin actually doing the logic to figure out which directories to add#
# and finding the libraries that those parts need.
#-----------------------------------------------------------------------------
# Basic Core Libs
set(SIMVASCULAR_CORE_LIBRARY_DIRS ${SIMVASCULAR_CORE_LIBRARY_DIRS} 
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

set(SIMVASCULAR_CORE_EXECUTABLE_DIRS ${SIMVASCULAR_CORE_EXECUTABLE_DIRS} "Source/UI")

#-----------------------------------------------------------------------------
# We can not begin enabling libraries
# Libraries that we always need...
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# TCL
if(WIN32)
	simvascular_external(TCL DOWNLOADABLE SHARED_LIB)
else()
	simvascular_external(TCL SYSTEM_DEFAULT SHARED_LIB)
endif()

STRING(REGEX REPLACE
	"^.*libtcl([0-9]\\.*[0-9]).*$" "\\1" TCL_VERSION "${TCL_LIBRARY}")
mark_as_superbuild(
	VARS 
	TCL_INCLUDE_PATH:PATH 
	TCL_LIBRARY:FILEPATH
	TCL_TCLSH:FILEPATH
	TK_INCLUDE_PATH:PATH 
	TK_LIBRARY:FILEPATH
	TK_WISH:FILEPATH 
	ALL_PROJECTS)


#-----------------------------------------------------------------------------
# VTK
set(SimVascular_DEPENDS "TCL")
simvascular_external(VTK DOWNLOADABLE)

#-----------------------------------------------------------------------------
# tkcximage (Legacy)
if(WIN32)
	option(SimVascular_USE_TKCXIMAGE "Use TKCXImage (Legacy)" OFF)
	mark_as_superbuild(SimVascular_USE_TKCXIMAGE)
	if(SimVascular_USE_TKCXIMAGE)
		find_library(TKCXIMAGE_DLL tkcximage)
		if(TKCXIMAGE_DLL)
			set(TKCXIMAGE_DLL_LIBRARY ${TKCXIMAGE_DLL})
			get_filename_component(TKCXIMAGE_DLL_PATH ${TKCXIMAGE_DLL} DIRECTORY CACHE)
			mark_as_superbuild(TKCXIMAGE_DLL)
			mark_as_superbuild(TKCXIMAGE_DLL_PATH)
			set(SIMVASCULAR_EXTERNAL_SHARED_LIBS ${SIMVASCULAR_EXTERNAL_SHARED_LIBS} "TKCXIMAGE")
		endif()
	endif()
endif()

#-----------------------------------------------------------------------------
# ZLIB
if(SimVascular_USE_ZLIB)
	SET(USE_ZLIB ON)
	simvascular_third_party(zlib)
	if(NOT SimVascular_USE_SYSTEM_ZLIB)
		set(ZLIB_LIBRARY lib_simvascular_zlib)
	else()
		find_package(ZLIB)
	endif()
else()
	unset(ZLIB_LIBRARY CACHE)
endif()

#-----------------------------------------------------------------------------
# Begin Plugin Logic
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Plugin Tetgen
if(Plugin_TETGEN)
	SET(USE_TETGEN ON)
	simvascular_third_party(tetgen)
	if(NOT SimVascular_USE_SYSTEM_TETGEN)
		set(TETGEN_VERSION "1.5.1")
		set(TETGEN_LIBRARY lib_simvascular_tet)
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
	set(SIMVASCULAR_CORE_LIBRARY_DIRS ${SIMVASCULAR_CORE_LIBRARY_DIRS} "Source/Mesh/TetGenMeshObject")
else()
	unset(TETGEN_LIBRARY CACHE)
endif()

if(Licensed_ADAPTOR)
    set(SIMVASCULAR_CORE_LIBRARY_DIRS ${SIMVASCULAR_CORE_LIBRARY_DIRS} "Source/Mesh/AdaptObject")
elseif(Plugin_TET_ADAPTOR)
    set(SIMVASCULAR_CORE_LIBRARY_DIRS ${SIMVASCULAR_CORE_LIBRARY_DIRS} "Source/Mesh/AdaptObject")
endif()

if(Plugin_TET_ADAPTOR)
	SET(USE_TET_ADAPTOR ON)
	set(SIMVASCULAR_CORE_LIBRARY_DIRS ${SIMVASCULAR_CORE_LIBRARY_DIRS} "Source/Mesh/TetGenAdapt")
endif()

#-----------------------------------------------------------------------------
# VMTK
if(Plugin_VMTK)
	set(USE_VMTK ON)
	set(SIMVASCULAR_CORE_LIBRARY_DIRS ${SIMVASCULAR_CORE_LIBRARY_DIRS} "ThirdParty/vmtk")
endif()

#-----------------------------------------------------------------------------
# Plugin LevelSets
if(Plugin_ITK)
	simvascular_external(ITK DOWNLOADABLE)
	set(USE_ITK ON)
	set(SIMVASCULAR_CORE_LIBRARY_DIRS ${SIMVASCULAR_CORE_LIBRARY_DIRS} "Source/Segmentation/ITK")
endif()

#---
if(Licensed_MESHSIM OR Licensed_MESHSIM_DISCRETE_SOLIDMODEL)
	set(SIMVASCULAR_CORE_LIBRARY_DIRS ${SIMVASCULAR_CORE_LIBRARY_DIRS} "Source/Mesh/MeshSimMeshObject")
endif()

if(Licensed_ADAPTOR)
	set(USE_MESHSIM_ADAPTOR ON)
	set(SIMVASCULAR_CORE_LIBRARY_DIRS ${SIMVASCULAR_CORE_LIBRARY_DIRS} "Source/Mesh/MeshSimAdapt")
endif()



#-----------------------------------------------------------------------------
# GTS
if(SimVascular_USE_GTS)
	simvascular_external(GTS DOWNLOADABLE SHARED_LIB)
	set(SimVascular_USE_GLIB ON)
	set(USE_GTS ON)
endif()

if(SimVascular_USE_GLIB)
	if(UNIX)
		simvascular_external(GLIB SYSTEM_DEFAULT SHARED_LIB)
	endif()
	if(MSVC)
		simvascular_external(GLIB DOWNLOADABLE SHARED_LIB)
	endif()
	set(USE_GLIB ON)
endif()

#-----------------------------------------------------------------------------
# Licensed Package Additions
#-----------------------------------------------------------------------------
if(Licensed_ADAPTOR)
	set(USE_SVLS ON)
	set(USE_Fortran ON)
	set(USE_MPI ON)
	set(BUILD_ThreeDSolver ON)
endif()

#-----------------------------------------------------------------------------
# Parasolid
if(NOT SimVascular_EXCLUDE_SOLID_MODEL)
	if(Licensed_PARASOLID)
			if(EXISTS ${SimVascular_SOURCE_DIR}/Licensed/ParasolidSolidModel/CMakeLists.txt)
				simvascular_external(PARASOLID SYSTEM_DEFAULT SHARED_LIB)
				if(WIN32)
					set(SIMVASCULAR_EXTRA_PATHS ${SIMVASCULAR_EXTRA_PATHS} ${PARASOLID_DLL_PATH})
					mark_as_superbuild(PARASOLID_DLL_PATH:PATH)
				endif()
				mark_as_superbuild(PARASOLID_DIR)
				mark_as_superbuild(PARASOLID_INCLUDE_DIR)
				mark_as_superbuild(PARASOLID_LIBRARIES)
				mark_as_superbuild(PARASOLID_LIBRARY)
				mark_as_superbuild(PARASOLID_SCHEMA_DIR)
				SET(USE_PARASOLID ON)
				set(SIMVASCULAR_CORE_LIBRARY_DIRS ${SIMVASCULAR_CORE_LIBRARY_DIRS} "Licensed/ParasolidSolidModel")
			else()
				message("Parasolid requires an extra license.")
			endif()

	endif()
	if(Licensed_MESHSIM_DISCRETE_SOLIDMODEL)
		set(SIMVASCULAR_CORE_LIBRARY_DIRS ${SIMVASCULAR_CORE_LIBRARY_DIRS} "Source/Model/MeshSimDiscreteSolidModel")
	endif()
else()
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DEXCLUDE_SOLID_MODEL")
endif()

#-----------------------------------------------------------------------------
# MeshSim
unset(MESHSIM_MODELER)
if(Licensed_MESHSIM OR Licensed_MESHSIM_DISCRETE_SOLIDMODEL)
	unset(MESHSIM_COMP)
	if(APPLE)
		message(FATAL_ERROR "MeshSim not available on macos")
	endif()
	if(Licensed_MESHSIM_DISCRETE_SOLIDMODEL)
		set(USE_DISCRETE_MODEL ON)
		set(MESHSIM_COMP SimDiscrete)
	endif()
	if(Licensed_PARASOLID)
		set(MESHSIM_MODELER "parasolid")
		mark_as_superbuild(MESHSIM_MODELER:STRING)
	endif()
	if(MESHSIM_MODELER MATCHES "parasolid")
        set(MESHSIM_SIMPARASOLID_VERSION SimParasolid270 CACHE STRING "When using parasolid, you need to specify which veriosn of the bridge library to use.")
        set(MESHSIM_ACCEPTED_SIMPARASOLID_VERSIONS SimParasolid271 SimParasolid270 SimParasolid260 SimParasolid251 SimParasolid250 SimParasolid241)
		SET_PROPERTY(CACHE MESHSIM_SIMPARASOLID_VERSION PROPERTY STRINGS ${MESHSIM_ACCEPTED_SIMPARASOLID_VERSIONS})
		mark_as_superbuild(MESHSIM_SIMPARASOLID_VERSION)
	endif()
	simvascular_external(MESHSIM COMPONENTS ${MESHSIM_COMP} ${MESHSIM_SIMPARASOLID_VERSION} SHARED_LIB SYSTEM_DEFAULT)
	mark_as_superbuild(MESHSIM_DIR)
	mark_as_superbuild(MESHSIM_INCLUDE_DIR)
	mark_as_superbuild(MESHSIM_LIBRARIES)
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
		mark_as_superbuild(MESHSIM_LICENSE_FILE)
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
# GTS
if(SimVascular_USE_GTS)
	set(USE_GTS ON)
	if(WINDOWS)
		set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DNATIVE_WIN32")
	else()
	endif()
endif()

#-----------------------------------------------------------------------------
# Modules (Flowsolver)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# ThreeD Solver
if(BUILD_ThreeDSolver)
	option(SimVascular_USE_SPARSE "Use sparse Library" ON)
	option(SimVascular_USE_METIS "Use metis Library" ON)
	option(SimVascular_USE_NSPCG "Use nspcg Library" ON)
	set(USE_Fortran ON)
	set(USE_SVLS ON)
	set(USE_MPI ON)
	set(ThreeDSolver_BUILD_SolverIO ON)
	#set(FLOWSOLVER_CONFIG_PATHS 
	#	"${SimVascular_SOURCE_DIR}/Modules/ThreeDSolver/IncompressibleSolver/Solver" CACHE TYPE PATH)
	# find config file		
	#set(FLOWSOLVER_CONFIG_FILE "${SimVascular_SOURCE_DIR}/Modules/ThreeDSolver/IncompressibleSolver/Solver/default.inp")
	#mark_as_superbuild(FLOWSOLVER_CONFIG_FILE)
	#set(FLOWSOLVER_CONFIG_PATH ${OUTBIN_DIR})
	#file(COPY ${FLOWSOLVER_CONFIG_FILE} DESTINATION ${FLOWSOLVER_CONFIG_PATH})
	#install(FILES ${FLOWSOLVER_CONFIG_FILE} DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR})
endif()

#-----------------------------------------------------------------------------
# Solver IO
if(ThreeDSolver_SOLVER_IO_REDIRECT)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DBUILD_WITH_FLOWSOLVER_STDOUT_STDERR_REDIRECT")
endif()

#-----------------------------------------------------------------------------
#  SVLS
# svLS depends on the THREEDSOLVER build state so it must be here.
if(USE_SVLS)
	set(SVLS_BUILD_TYPE "Source")
	#simvascular_external(svls SVEXTERN_DEFAULT)
	set(USE_Fortran ON)
	set(USE_MPI ON)
endif()

if(ThreeDSolver_USE_LESLIB)
	find_package(LESLIB REQUIRED)
	mark_as_superbuild(VARS LESLIB_LIBRARY LESLIB_INCLUDE_DIR)
endif()

#-----------------------------------------------------------------------------
# Add libraries for flowsolver
if(SimVascular_USE_SPARSE)
	SET(USE_SPARSE ON)
	simvascular_third_party(sparse)
	if(NOT SimVascular_USE_SYSTEM_SPARSE)
		set(SPARSE_LIBRARY lib_simvascular_sparse)
	else()
		find_package(METIS)
	endif()
endif()

if(SimVascular_USE_METIS)
	SET(USE_METIS ON)
	simvascular_third_party(metis)
	if(NOT SimVascular_USE_SYSTEM_METIS)
		set(METIS_LIBRARY lib_simvascular_metis)
	else()
		find_package(METIS)
	endif()
endif()

if(SimVascular_USE_NSPCG)
	SET(USE_NSPCG ON)
	simvascular_third_party(nspcg)
	if(NOT SimVascular_USE_SYSTEM_NSPCG)
		set(NSPCG_LIBRARY lib_simvascular_nspcg)
	else()
		find_package(NSPCG)
	endif()
endif()

#-----------------------------------------------------------------------------
# Remaining optional dependencies
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Enable Interl Runtime libs if we need or want them
option(SimVascular_USE_INTELRUNTIME "Add Intel Runtime Libraries (these may be needed by some libraries)" OFF)
mark_as_superbuild(SimVascular_USE_INTELRUNTIME)

if(SimVascular_USE_INTELRUNTIME OR USE_INTEL)
	simvascular_external(INTELRUNTIME SYSTEM_DEFAULT SHARED_LIB)
	if(SimVascular_USE_SYSTEM_ZLIB)
		set(INTELRUNTIME_LIB_DIR "" CACHE PATH "A directory where intel runtimes may be, use this if its not in the path")
	endif()
	set(USE_INTELRUNTIME ON)
	set(USE_INTEL ON)
	mark_as_superbuild(VARS 
		INTELRUNTIME_LIB_DIR:PATH
		INTELRUNTIME_LIBRARIES)
else()
	unset(INTELRUNTIME_LIBRARIES CACHE)
endif()

#-----------------------------------------------------------------------------
# Enable Fortran
if(USE_Fortran)
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
if(USE_MPI OR SimVascular_USE_MPI)
	set(USE_MPI ON)
	if (NOT SimVascular_USE_DUMMY_MPICH2)
		simvascular_external(MPI SYSTEM_DEFAULT SHARED_LIB)
		if(MPI_FOUND)
			get_filename_component(MPI_LIBRARY_DIR ${MPI_LIBRARY} PATH)
		endif()
		if(WIN32)
			find_library(MPI_fmpich2_LIBRARY NAMES fmpich2 HINTS ${MPI_LIBRARY_DIR})
			set(MPI_EXTRA_LIBRARY ${MPI_EXTRA_LIBRARY} ${MPI_fmpich2_LIBRARY} ${MPI_CXX_LIBRARIES})
			#message("${MPI_EXTRA_LIBRARY}")
		endif()

		mark_as_superbuild(
			VARS 
			MPI_Fortran_COMPILER:FILEPATH 
			MPI_C_COMPILER:FILEPATH
			MPI_CXX_COMPILER:FILEPATH
			MPIEXEC
			ALL_PROJECTS)

		include_directories(${MPI_Fortran_INCLUDE_PATH})
		if(SimVascular_DEVELOPER_OUTPUT)
			#getListOfVarsPrefix("MPI" _VARLIST)
			#print_vars(_VARLIST)
		endif()

		set(svMPI_EXTRA_LIBRARY ${MPI_EXTRA_LIBRARY})
		set(svMPI_Fortran_LIBRARIES ${MPI_Fortran_LIBRARIES})
	else()
		set(svMPI_EXTRA_LIBRARY lib_extra_simvascular_dummympi)
		set(svMPI_Fortran_LIBRARIES lib_fortran_simvascular_dummympi)
	endif()
endif()

#-----------------------------------------------------------------------------
# Enable Testing
option(BUILD_TESTING "Build ${PROJECT_NAME} testing" OFF)
mark_as_superbuild(VARS BUILD_TESTING)

if(BUILD_TESTING)
	enable_testing()
	add_subdirectory(Testing)
endif()

if(WIN32)
	if(SimVascular_ENABLE_DISTRIBUTION)
		set(SIMVASCULAR_USE_WIN32_REGISTRY ON)
	endif()
endif()
if(SIMVASCULAR_USE_WIN32_REGISTRY_FORCE_OFF)
	set(SIMVASCULAR_USE_WIN32_REGISTRY OFF)
	message(STATUS "Forcing SIMVASCULAR_USE_WIN32_REGISTRY off, even in distribution mode")
	message(STATUS "SIMVASCULAR_USE_WIN32_REGISTRY: ${SIMVASCULAR_USE_WIN32_REGISTRY}")
	mark_as_superbuild(SIMVASCULAR_USE_WIN32_REGISTRY_FORCE_OFF:BOOL)
endif()
mark_as_superbuild(SIMVASCULAR_USE_WIN32_REGISTRY:BOOL)
#-----------------------------------------------------------------------------
# Superbuild
#-----------------------------------------------------------------------------
# Anything after this will not be executed if in 
# superbuild mode.
#-----------------------------------------------------------------------------
set(git_protocol "git")
mark_as_superbuild(git_protocol)

ExternalProject_Include_Dependencies(${PROJECT_NAME} 
	PROJECT_VAR PROJECT_NAME)
if(SimVascular_SUPERBUILD)

	message(STATUS "Install timestamp: ${SIMVASCULAR_RELEASE_TIMESTAMP}")
	file(WRITE "${TEMP_DIR}/release-date" "${SIMVASCULAR_RELEASE_TIMESTAMP}")
	install(FILES "${TEMP_DIR}/release-date" DESTINATION ${SIMVASCULAR_INSTALL_HOME_DIR})

	# Git Check (superbuild)
	find_package(Git REQUIRED)
	option(SimVascular_USE_GIT
		"If behind a firewall turn this off to use http instead." ON)

	dev_message("Superbuild Mode!")
	if(SimVascular_ENABLE_DISTRIBUTION)
		add_subdirectory(${SimVascular_DISTRIBUTION_DIR} ${SimVascular_BINARY_DISTRIBUTION_DIR})
	endif()
	include(Superbuild)
	RETURN()
endif()
#-----------------------------------------------------------------------------
# Now we need to finish configuring now that the external projects are configured
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# TCL Setup 
# TCL has two include directories, the macro only includes one.
include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})
if(WIN32)
	GET_FILENAME_COMPONENT(TCL_DLL_PATH "${TCL_TCLSH}" PATH)
endif()

#-----------------------------------------------------------------------------
# VTK Setup
include(${VTK_USE_FILE})

# SV uses non stanard libraries, this adds them, also adds the intel libraries
include(VTKSVConfig) 
set(VTK_LIBRARIES ${VTK_LIBRARIES} ${VTK_EXTRA_LIBS})
if(${VTK_DIR} MATCHES "intel")
	#message(STATUS "VTK appears to be built with intel, adding libraries")
	set(VTK_LIBRARIES ${VTK_LIBRARIES} ${INTELRUNTIME_LIBRARIES})
endif()


#-----------------------------------------------------------------------------
# Plugin setup
if(Plugin_ITK)
	include(${ITK_USE_FILE})
endif()

if(Plugin_TETGEN)
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

if(Plugin_TET_ADAPTOR)
	#set(USE_Fortran ON)
	set(ThreeDSolver_BUILD_SolverIO ON)
endif()


if(Plugin_VMTK)
	# no extra setups
endif()

#-----------------------------------------------------------------------------
# Find parasolid dll on windows to definitions
if(Licensed_PARASOLID)
	if(PARASOLID_FOUND)
		include_directories(${PARASOLID_INCLUDE_DIR})
	endif()
endif()

if(Licensed_MESHSIM)
	if(MESHSIM_FOUND)
		include_directories(${MESHSIM_INCLUDE_DIR})
	endif()
endif()


#-----------------------------------------------------------------------------
# Add GLOBAL_DEFINES to definitions
dev_message(" Global Flags: ${GLOBAL_DEFINES}")
add_definitions(${GLOBAL_DEFINES})

#-----------------------------------------------------------------------------
# Add Local Includes
set(LOCAL_LIBDIR Lib)
set(SIMVASCULAR_SCRIPT_TARGETS "" CACHE INTERNAL "" FORCE)
set(INDENTS "")

#-----------------------------------------------------------------------------
# Include the Core subdirs 
# These are handled here because other sub directories depend on them
# set
set(SIMVASCULAR_INCLUDE_DIRS 
			 	"Source/Include"
			 	"FlowSolvers/Include"
			 	)

foreach(dir ${SIMVASCULAR_INCLUDE_DIRS})
	add_subdirectory(${dir})
	include_directories(${SimVascular_SOURCE_DIR}/${dir} 
						${SimVascular_BINARY_DIR}/${dir})
endforeach()

foreach(dir ${SIMVASCULAR_CORE_LIBRARY_DIRS})
	include_directories("${dir}")
endforeach()



#-----------------------------------------------------------------------------
# Add directories
# Modules
if(BUILD_ThreeDSolver)
	add_subdirectory(FlowSolvers)
endif()

if(NOT SimVascular_ONLY_BUILD_FLOWSOLVER)
	# Add the Core Direcotories
	message(STATUS "Libraries:")
	foreach(dir ${SIMVASCULAR_CORE_LIBRARY_DIRS})
		message(STATUS "	${dir}")
		include_directories("${dir}")
		add_subdirectory("${dir}")
	endforeach()
	message(STATUS "Executables")
	foreach(dir ${SIMVASCULAR_CORE_EXECUTABLE_DIRS})
		message(STATUS "	${dir}")
		add_subdirectory("${dir}")
	endforeach()
endif()
#-----------------------------------------------------------------------------
# Configure Exe Scripts, this should be the last subdirectory
include(SimVascularInstallLibs)
add_subdirectory(Scripts)
