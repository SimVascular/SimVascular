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

# Set external project directory
# Set src, build, bin dirs for externals
# Set options for externals
#-----------------------------------------------------------------------------
if(NOT SV_INSTALL_ROOT_DIR)
  set(SV_INSTALL_ROOT_DIR "SV")
endif()
mark_as_superbuild(SV_INSTALL_ROOT_DIR:PATH)
if(NOT WIN32)
  if(NOT CMAKE_INSTALL_PREFIX MATCHES "${SV_INSTALL_ROOT_DIR}")
    set(CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX}/${SV_INSTALL_ROOT_DIR})
  endif()
endif()

set(SV_EXTERNALS_TOPLEVEL_SRC_DIR "src")

set(SV_EXTERNALS_TOPLEVEL_BIN_DIR "bin")

set(SV_EXTERNALS_TOPLEVEL_BLD_DIR "build")

set(SV_EXTERNALS_TOPLEVEL_PFX_DIR "prefix")

if(APPLE)
  set(SV_PLATFORM_DIR "mac_osx")
elseif(LINUX)
  set(SV_PLATFORM_DIR "linux")
elseif(WIN64)
  set(SV_PLATFORM_DIR "windows")
else()
  set(SV_PLATFORM_DIR "unsupported")
endif()
mark_as_superbuild(SV_PLATFORM_DIR)

set(SV_COMPILER_DIR "")
string(TOLOWER "${COMPILER_VERSION}" SV_COMPILER_DIR)
mark_as_superbuild(SV_COMPILER_DIR)

set(SV_ARCH_DIR "x64")

set(SV_EXTERNALS_SRC_DIR "${SV_EXTERNALS_TOPLEVEL_SRC_DIR}")
set(SV_EXTERNALS_BLD_DIR "${SV_EXTERNALS_TOPLEVEL_BLD_DIR}")
set(SV_EXTERNALS_PFX_DIR "${SV_EXTERNALS_TOPLEVEL_PFX_DIR}")
set(SV_EXTERNALS_BIN_DIR "${SV_EXTERNALS_TOPLEVEL_BIN_DIR}/${SV_PLATFORM_DIR}/${SV_COMPILER_DIR}/${SV_ARCH_DIR}")
mark_as_superbuild(SV_EXTERNALS_SRC_DIR:PATH)
mark_as_superbuild(SV_EXTERNALS_BLD_DIR:PATH)
mark_as_superbuild(SV_EXTERNALS_PFX_DIR:PATH)
mark_as_superbuild(SV_EXTERNALS_BIN_DIR:PATH)

macro(simvascular_add_new_external proj version use shared dirname) 
  option(SV_USE_${proj} "Enable ${proj} Plugin" ${use})
  mark_as_superbuild(SV_USE_${proj})
  option(SV_USE_${proj}_SHARED "Build ${proj} libraries as shared libs" ${shared})
  mark_as_superbuild(SV_USE_${proj}_SHARED)

  set(${proj}_VERSION "${version}" CACHE TYPE STRING)
  mark_as_superbuild(${proj}_VERSION)
  mark_as_advanced(${proj}_VERSION)
  set(SV_EXT_${proj}_SRC_DIR ${SV_EXTERNALS_SRC_DIR}/${dirname}-${${proj}_VERSION})
  set(SV_EXT_${proj}_BIN_DIR ${SV_EXTERNALS_BIN_DIR}/${dirname}-${${proj}_VERSION})
  set(SV_EXT_${proj}_BLD_DIR ${SV_EXTERNALS_BLD_DIR}/${dirname}-${${proj}_VERSION})
  set(SV_EXT_${proj}_PFX_DIR ${SV_EXTERNALS_PFX_DIR}/${dirname}-${${proj}_VERSION})
  mark_as_superbuild(SV_EXT_${proj}_SRC_DIR:PATH)
  mark_as_superbuild(SV_EXT_${proj}_BIN_DIR:PATH)
  mark_as_superbuild(SV_EXT_${proj}_BLD_DIR:PATH)
  mark_as_superbuild(SV_EXT_${proj}_PFX_DIR:PATH)

  if(NOT SV_INSTALL_${proj}_RUNTIME_DIR)
    set(SV_INSTALL_${proj}_RUNTIME_DIR ${SV_EXT_${proj}_BIN_DIR}/bin)
  endif()
  mark_as_superbuild(SV_INSTALL_${proj}_RUNTIME_DIR:PATH)

  if(NOT SV_INSTALL_${proj}_LIBRARY_DIR)
    set(SV_INSTALL_${proj}_LIBRARY_DIR ${SV_EXT_${proj}_BIN_DIR}/lib)
  endif()
  mark_as_superbuild(SV_INSTALL_${proj}_LIBRARY_DIR:PATH)

  if(NOT SV_INSTALL_${proj}_ARCHIVE_DIR)
    set(SV_INSTALL_${proj}_ARCHIVE_DIR ${SV_EXT_${proj}_BIN_DIR}/lib)
  endif()
  mark_as_superbuild(SV_INSTALL_${proj}_ARCHIVE_DIR:PATH)

  if(NOT SV_INSTALL_${proj}_INCLUDE_DIR)
    set(SV_INSTALL_${proj}_INCLUDE_DIR ${SV_EXT_${proj}_BIN_DIR}/include)
  endif()
  mark_as_superbuild(SV_INSTALL_${proj}_INCLUDE_DIR:PATH)

  if(SV_USE_${proj})
    list(APPEND SV_EXTERNALS_LIST ${proj})
    list(REMOVE_DUPLICATES SV_EXTERNALS_LIST)
    set(SV_${proj}_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BIN_DIR})
    mark_as_superbuild(SV_${proj}_DIR)
    if(NOT SV_SUPERBUILD)
      set(SV_USE_SYSTEM_${proj} "ON" CACHE BOOL "External ${proj} must be used" FORCE)
    endif()
  endif()
endmacro()

# VTK
#-----------------------------------------------------------------------------
# VTK is required
set(SV_USE_VTK ON CACHE BOOL "Must build with vtk" FORCE)
simvascular_add_new_external(VTK 6.2.0 ON ON vtk)
if(NOT SV_INSTALL_VTK_TCL_DIR)
  set(SV_INSTALL_VTK_TCL_DIR ${SV_EXT_VTK_BIN_DIR}/lib/tcltk/vtk-6.2)
endif()
mark_as_superbuild(SV_INSTALL_VTK_TCL_DIR:PATH)
if(SV_EXTERNALS_USE_TOPLEVEL_DIR AND NOT SV_SUPERBUILD)
  set(VTK_DIR ${SV_VTK_DIR}/lib/cmake/vtk-6.2)
endif()


# ITK
#-----------------------------------------------------------------------------
simvascular_add_new_external(ITK 4.8.0 ON ON itk)
# If using extern directory, set dir with Config file!
if(SV_USE_ITK)
  if(SV_EXTERNALS_USE_TOPLEVEL_DIR AND NOT SV_SUPERBUILD)
    set(ITK_DIR ${SV_ITK_DIR}/lib/cmake/ITK-4.8)
  endif()
endif()

# TCL is required
#-----------------------------------------------------------------------------
set(SV_USE_TCL ON CACHE BOOL "Must build with tcl" FORCE)
simvascular_add_new_external(TCL 8.6.0 ON ON tcltk)
if(SV_EXTERNALS_USE_TOPLEVEL_DIR AND NOT SV_SUPERBUILD)
  set(TCL_DIR ${SV_TCL_DIR})
endif()

#PYTHON
#-----------------------------------------------------------------------------
simvascular_add_new_external(PYTHON 2.7 OFF ON python)

# OpenCASCADE
#-----------------------------------------------------------------------------
simvascular_add_new_external(OpenCASCADE 7.0.0 OFF ON opencascade)
# If using extern directory, set dir with Config file!
if(SV_USE_OpenCASCADE)
  if(SV_EXTERNALS_USE_TOPLEVEL_DIR AND NOT SV_SUPERBUILD)
    set(OpenCASCADE_DIR ${SV_OpenCASCADE_DIR}/lib/cmake/opencascade)
  endif()
endif()

# GDCM
#-----------------------------------------------------------------------------
simvascular_add_new_external(GDCM 2.6.1 OFF ON gdcm)
# If using extern directory, set dir with Config file!
if(SV_USE_GDCM)
  if(SV_EXTERNALS_USE_TOPLEVEL_DIR AND NOT SV_SUPERBUILD)
    set(GDCM_DIR ${SV_GDCM_DIR}/lib/gdcm-2.6)
  endif()
endif()

# FREETYPE
#-----------------------------------------------------------------------------
simvascular_add_new_external(FREETYPE 2.6.3 OFF ON freetype)
if(SV_USE_FREETYPE)
  if(SV_EXTERNALS_USE_TOPLEVEL_DIR AND NOT SV_SUPERBUILD)
    set(FREETYPE_DIR ${SV_FREETYPE_DIR})
  endif()
endif()

# MMG
#-----------------------------------------------------------------------------
simvascular_add_new_external(MMG 5.1.0 OFF OFF mmg)
if(SV_USE_MMG)
  if(SV_EXTERNALS_USE_TOPLEVEL_DIR AND NOT SV_SUPERBUILD)
    set(MMG_DIR ${SV_MMG_DIR})
  endif()
endif()

if(SV_DOWNLOAD_EXTERNALS)
  simvascular_download_and_extract_tar(${SV_EXTERNALS_DOWNLOAD_URL} "${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXTERNALS_BIN_DIR}")
endif()
