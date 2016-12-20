# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved.
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
# Options and defined depending on the system, and the intial options
#-----------------------------------------------------------------------------
# APPLE
if(APPLE)
	set ( CMAKE_OSX_ARCHITECTURES "" CACHE STRING "" FORCE )
	# Note: By setting CMAKE_OSX_* variables before any enable_language() or project() calls,
	#       we ensure that the bitness will be properly detected.
	set(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH} "/opt/local/lib")
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# WIN32
if(WIN32)
	option(SV_USE_WIN32_REGISTRY "Use Windows registry to obtain certain settings (install mode)" OFF)
	mark_as_advanced(SV_USE_WIN32_REGISTRY)

	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DWINDOWS -DWIN32")
	if(NOT IS64)
	  if(NOT "${CMAKE_GENERATOR}" MATCHES ".*Win64")
		  set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -D_X86_")
		endif()
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
			message(AUTHOR_WARNING "gethostname has not beed found! The flowsolver will not compile")
		endif()
	endif()

	set (CMAKE_CXX_STANDARD_LIBRARIES "${CMAKE_CXX_STANDARD_LIBRARIES} Shlwapi.lib")

	option(SV_USE_TKCXIMAGE "Use TKCXImage (Legacy)" OFF)
endif()
#-----------------------------------------------------------------------------

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

#-----------------------------------------------------------------------------
# Visual Studio flags
if(MSVC)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DMSVC /EHsc")
  # SUPPRESS_VC_DEPRECATED_WARNINGS
  add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_WARNINGS -D_SCL_SECURE_NO_WARNINGS)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# All OS
set(SV_USE_NOTIMER ON)
#-----------------------------------------------------------------------------

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
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Renderer
if (SV_NO_RENDERER)
	# Needs to be 1 not 'true'
	set(SV_NO_RENDERER 1)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# These flags are stored (as opposed to adding directly with ADD_DEFINITIONS)
# so the flags may be printed for debuging purposes.
if(CMAKE_COMPILER_IS_GNUCXX)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DGCC")
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Shared Libs
if(BUILD_SHARED_LIBS)
	set(SV_LIBRARY_TYPE "SHARED" CACHE STRING "Shared cache" FORCE)
	set(SV_STATIC_BUILD "0")
	set(SV_INSTALL_HEADERS ON)
	set(SV_INSTALL_LIBS ON)
else()
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_STATIC_LINK -DSV_STATIC_BUILD")
	set(SV_STATIC_BUILD "1")
  set(SV_INSTALL_HEADERS OFF)
  set(SV_INSTALL_LIBS ON)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# SolverIO
if(SV_USE_MESHSIM_ADAPTOR OR SV_USE_TETGEN_ADAPTOR)
  set(SV_USE_SOLVERIO ON)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Open Source Software Options: Solid Models - OpenCASCADE
if(SV_USE_OpenCASCADE)
  # OpenCASCADE links to freetype, must be used too
  set(SV_USE_FREETYPE "ON" CACHE BOOL "Force freetype on" FORCE)
endif()
#-----------------------------------------------------------------------------

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

if(SV_USE_MESHSIM OR SV_USE_MESHSIM_DISCRETE_MODEL)
	if(SV_USE_MESHSIM)
		option(SV_USE_MESHSIM_SHARED_LIBRARIES "Build MeshSim as shared libraries" OFF)
		if(BUILD_SHARED_LIBS)
			set(SV_USE_MESHSIM_SHARED_LIBRARIES "ON" CACHE BOOL "Force ON" FORCE)
		endif()
	endif()
	if(SV_USE_MESHSIM_DISCRETE_MODEL)
		option(SV_USE_MESHSIM_DISCRETE_SHARED_LIBRARIES "Build MeshSim Discrete Solid as shared libraries" OFF)
		if(BUILD_SHARED_LIBS)
			set(SV_USE_MESHSIM_DISCRETE_SHARED_LIBRARIES "ON" CACHE BOOL "Force ON" FORCE)
		endif()
	endif()
endif()

if(SV_USE_MESHSIM_ADAPTOR)
  set(SV_USE_THREEDSOLVER "ON" CACHE BOOL "Force ON" FORCE)
  set(SV_USE_MESHSIM "ON" CACHE BOOL "Force ON" FORCE)
endif()
#-----------------------------------------------------------------------------

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
  option(SV_THREEDSOLVER_USE_VTK "" ON)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Linear Solver Options: SVLS
if(SV_USE_THREEDSOLVER)
	option(SV_THREEDSOLVER_USE_SVLS "Use svLS as linear solver" ON )
	if(SV_THREEDSOLVER_USE_SVLS)
		set(USE_SVLS 1)
	endif()
	set(SVLS_BUILD_TYPE "Source")

	option(SV_THREEDSOLVER_USE_LESLIB "Use leslib as linear solver" OFF )
	if(SV_THREEDSOLVER_USE_LESLIB)
		set(SV_USE_LESLIB 1)
	endif()
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Set flags for shared libs
if(NOT MSVC)
  if (SV_USE_PARASOLID_SHARED_LIBRARIES OR SV_USE_MESHSIM_SHARED_LIBRARIES OR
    SV_USE_ITK_SHARED OR SV_USE_VTK_SHARED OR BUILD_SHARED_LIBS)
	  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")
	  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC")
  endif()
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Gui Options: Qt GUI
if(SV_USE_QT_GUI OR (SV_USE_OpenCASCADE AND SV_USE_OpenCASCADE_SHARED))
  SimVascularFunctionCheckCompilerFlags("-std=c++11" SimVascular_CXX11_FLAG)
  if(NOT SimVascular_CXX11_FLAG)
    # Older gcc compilers use -std=c++0x
    SimVascularFunctionCheckCompilerFlags("-std=c++0x" SimVascular_CXX11_FLAG)
  endif()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${SimVascular_CXX11_FLAG}")
  if(WIN32)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -D_WIN32_WINNT=0x0501 -DPOCO_NO_UNWINDOWS -DWIN32_LEAN_AND_MEAN -DNOMINMAX")
  endif()
endif()

if(SV_USE_QT_GUI)
  set(SV_USE_VTK_SHARED "ON" CACHE BOOL "Force ON" FORCE)

  set(SV_USE_ITK "ON" CACHE BOOL "Force ON" FORCE)
  set(SV_USE_ITK_SHARED "ON" CACHE BOOL "Force ON" FORCE)

  set(SV_USE_MITK "ON" CACHE BOOL "Force ON" FORCE)
  set(SV_USE_MITK_SHARED "ON" CACHE BOOL "Force ON" FORCE)

  set(SV_USE_GDCM "ON" CACHE BOOL "Force ON" FORCE)
  set(SV_USE_GDCM_SHARED "ON" CACHE BOOL "Force ON" FORCE)

  set(SV_USE_PYTHON "ON" CACHE BOOL "Force ON" FORCE)
  set(SV_USE_PYTHON_SHARED "ON" CACHE BOOL "Force ON" FORCE)
endif()
#-----------------------------------------------------------------------------
