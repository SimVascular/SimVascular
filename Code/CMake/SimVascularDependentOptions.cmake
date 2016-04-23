# Options and defined depending on the system, and the intial options
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# APPLE
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

	set(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH} "/opt/local/lib")
endif()

#-----------------------------------------------------------------------------
# WIN32
if(WIN32)
	option(SV_USE_WIN32_REGISTRY "Use Windows registry to obtain certain settings (install mode)" OFF)
	mark_as_advanced(SV_USE_WIN32_REGISTRY)
	mark_as_superbuild(SV_USE_WIN32_REGISTRY:BOOL)

	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DWINDOWS -DWIN32")
	if(NOT IS64)
		set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -D_X86_")
	endif()
	if(CYGWIN)
		set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DCYGWIN")
	endif()
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_WRAP_FORTRAN_IN_CAPS_NO_UNDERSCORE")
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

	option(SV_USE_TKCXIMAGE "Use TKCXImage (Legacy)" OFF)
	mark_as_superbuild(SV_USE_TKCXIMAGE)
endif()

#-----------------------------------------------------------------------------
# LINUX
if(UNIX)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DUNIX")
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE")
endif()
if(LINUX)
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pthread")
	set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -pthread -static")
endif()

#-----------------------------------------------------------------------------
# Visual Studio flags
if(MSVC)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DMSVC /EHsc")
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
# All OS
set(SV_USE_NOTIMER ON)

# Compiler Flags
#-----------------------------------------------------------------------------
if(SV_SUPPRESS_WARNINGS)
	add_definitions("-w")
endif()
if(CMAKE_COMPILER_IS_GNUCXX)
	add_definitions("-fpermissive")
endif()
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -pthread -static")

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
# Renderer
if (SV_NO_RENDERER)
	# Needs to be 1 not 'true'
	set(SV_NO_RENDERER 1)
	mark_as_superbuild(SV_NO_RENDERER)
endif()

#-----------------------------------------------------------------------------
# These flags are stored (as opposed to adding directly with ADD_DEFINITIONS)
# so the flags may be printed for debuging purposes.
if(CMAKE_COMPILER_IS_GNUCXX)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DGCC")
endif()

#-----------------------------------------------------------------------------
# Shared Libs
#-----------------------------------------------------------------------------
if(SV_BUILD_SHARED_LIBS)
	set(SV_LIBRARY_TYPE "SHARED" CACHE STRING "Shared cache" FORCE)
	set(SV_STATIC_BUILD "0")
	set(SV_INSTALL_HEADERS ON)
	set(SV_INSTALL_LIBS ON)
else()
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_STATIC_LINK -DSV_STATIC_BUILD")
	set(SV_STATIC_BUILD "1")
	set(SV_INSTALL_HEADERS OFF)
	set(SV_INSTALL_EXTERNALS OFF)
	set(SV_INSTALL_LIBS OFF)
endif()

# Only FLowsolver
if(SV_ONLY_BUILD_FLOWSOLVER)
	set(SV_USE_THREEDSOLVER ON)
endif()

#-----------------------------------------------------------------------------
# Plugins
#find_package(FreeType)
#if(FREETYPE_FOUND)
#	mark_as_superbuild(FREETYPE_DIR)
#	mark_as_superbuild(FREETYPE_INCLUDE_DIRS)
#	mark_as_superbuild(FREETYPE_LIBRARY)
#	mark_as_superbuild(FREETYPE_INCLUDE_DIR_freetype2)
#	mark_as_superbuild(FREETYPE_INCLUDE_DIR_ft2build)
#	include_directories(${FREETYPE_INCLUDE_DIR_freetype2})
#	include_directories(${FREETYPE_INCLUDE_DIR_ft2build})
#endif()

if(SV_USE_TETGEN)
	option(SV_USE_TET_ADAPTOR "Option to use open source mesh adaption" OFF)
	mark_as_superbuild(SV_USE_TET_ADAPTOR)
endif()

if(SV_USE_TET_ADAPTOR)
	set(SV_USE_THREEDSOLVER ON)
endif()

if(SV_USE_PYTHON)
	set(VTK_SHARED_LIBRARIES "ON" CACHE BOOL "Initial cache" FORCE)
endif()

#-----------------------------------------------------------------------------
# Open Source Software Options: Solid Models - OPENCASCADE
if(SV_USE_OPENCASCADE)
    option(OPENCASCADE_SHARED_LIBRARIES "Build opencascade as shared libs" OFF)
    mark_as_superbuild(OPENCASCADE_SHARED_LIBRARIES)
endif()

if(${OPENCASCADE_SHARED_LIBRARIES})
    set(VTK_SHARED_LIBRARIES "ON" CACHE BOOL "Initial cache" FORCE)
endif()

if(${VTK_SHARED_LIBRARIES})
  if(UNIX AND NOT APPLE)
  	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=gnu++11")
  endif()
endif()

#-----------------------------------------------------------------------------
# Commercial Software Options: Meshing - MeshSim
if(SV_USE_MESHSIM)
	option(SV_USE_MESHSIM_DISCRETE_MODEL "" ON)
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
	unset(SV_USE_MESHSIM_DISCRETE_MODEL CACHE)
endif()
mark_as_superbuild(SV_USE_MESHSIM:BOOL)
mark_as_superbuild(SV_USE_MESHSIM_DISCRETE_MODEL:BOOL)
mark_as_superbuild(MESHSIM_LICENSE_IN_WIN32_REGISTRY:BOOL)
mark_as_superbuild(MESHSIM_USE_LICENSE_FILE:BOOL)
mark_as_superbuild(MESHSIM_EMBED_LICENSE_KEYS:BOOL)

#---
if(SV_USE_MESHSIM OR SV_USE_MESHSIM_DISCRETE_MODEL)
	if(SV_USE_MESHSIM)
		option(SV_USE_MESHSIM_SHARED_LIBRARIES "Build MeshSim as shared libraries" OFF)
		if(SV_BUILD_SHARED_LIBS)
			set(SV_USE_MESHSIM_SHARED_LIBRARIES "ON" CACHE BOOL "Force ON" FORCE)
		endif()
		mark_as_superbuild(SV_USE_MESHSIM_SHARED_LIBRARIES)
	endif()
	if(SV_USE_MESHSIM_DISCRETE_MODEL)
		option(SV_USE_MESHSIM_DISCRETE_SHARED_LIBRARIES "Build MeshSim Discrete Solid as shared libraries" OFF)
		if(SV_BUILD_SHARED_LIBS)
			set(SV_USE_MESHSIM_DISCRETE_SHARED_LIBRARIES "ON" CACHE BOOL "Force ON" FORCE)
		endif()
		mark_as_superbuild(SV_USE_MESHSIM_DISCRETE_SHARED_LIBRARIES)
	endif()
endif()

#-----------------------------------------------------------------------------
# Adaptor Options
# Adaptor converts objects between the different solid models.
if(SV_USE_MESHSIM_ADAPTOR)
	set(SV_USE_THREEDSOLVER ON)
	set(SV_USE_MESHSIM ON)
endif()

#-----------------------------------------------------------------------------
# Solver Build Options (Modules)
if(SV_USE_THREEDSOLVER)
	#option(SV_USE_THREEDSOLVER "Option to build 3D-solver module (requires Fortran)" ON)
	option(SV_THREEDSOLVER_USE_SOLVERIO "Option to build solverIO module (requires Fortran)" ON)
	option(SV_THREEDSOLVER_USE_SVPRE "Option to build Pre-solver module (requires Fortran)" ON)
	option(SV_THREEDSOLVER_USE_SVPOST "Option to build post-solver module" ON)
	option(SV_THREEDSOLVER_SOLVERIO_REDIRECT "Option to redirect solver IO" OFF)
	option(SV_THREEDSOLVER_USE_CORONARY "" ON)
	option(SV_THREEDSOLVER_USE_CLOSEDLOOP "" ON)
	option(SV_THREEDSOLVER_USE_VARWALL "" ON)
	option(SV_THREEDSOLVER_USE_VTK "" OFF)
	mark_as_superbuild(SV_USE_THREEDSOLVER)
	mark_as_superbuild(SV_THREEDSOLVER_USE_SOLVERIO)
	mark_as_superbuild(SV_THREEDSOLVER_USE_SVPRE)
	mark_as_superbuild(SV_THREEDSOLVER_USE_SVPOST)
	mark_as_superbuild(SV_THREEDSOLVER_SOLVERIO_REDIRECT)
	mark_as_superbuild(SV_THREEDSOLVER_USE_CORONARY)
	mark_as_superbuild(SV_THREEDSOLVER_USE_CLOSEDLOOP)
	mark_as_superbuild(SV_THREEDSOLVER_USE_VARWALL)
	mark_as_superbuild(SV_THREEDSOLVER_USE_VTK)
endif()

#-----------------------------------------------------------------------------
# Linear Solver Options: SVLS
if(SV_USE_THREEDSOLVER)
	option(SV_THREEDSOLVER_USE_SVLS "Use svLS as linear solver" ON )
	if(SV_THREEDSOLVER_USE_SVLS)
		set(USE_SVLS 1)
	endif()
	mark_as_superbuild(SV_THREEDSOLVER_USE_SVLS)
	set(SVLS_BUILD_TYPE "Source")

	option(SV_THREEDSOLVER_USE_LESLIB "Use leslib as linear solver" OFF )
	if(SV_THREEDSOLVER_USE_LESLIB)
		set(SV_USE_LESLIB 1)
	endif()
	mark_as_superbuild(SV_THREEDSOLVER_USE_LESLIB)
endif()

#-----------------------------------------------------------------------------
# Set flags for shared libs
if(NOT MSVC)
if (SV_USE_PARASOLID_SHARED_LIBRARIES OR SV_USE_MESHSIM_SHARED_LIBRARIES OR
		ITK_SHARED_LIBRARIES OR VTK_SHARED_LIBRARIES OR
		SV_BUILD_SHARED_LIBS)
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC" CACHE STRING "Need for shared libs" FORCE)
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC" CACHE STRING "Need for shared libs" FORCE)
endif()
endif()

