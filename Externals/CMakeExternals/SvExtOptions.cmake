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
#
#-----------------------------------------------------------------------------
# Toplevel directories for src, bin, build or externals
set(SV_EXTERNALS_TOPLEVEL_DIR "${CMAKE_BINARY_DIR}/svExternals")

set(SV_EXTERNALS_TOPLEVEL_SRC_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/src"
  CACHE PATH "Directory where source files for externals will be put")
set(SV_EXTERNALS_TOPLEVEL_BIN_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/bin/${SV_COMPILER_DIR}/${SV_COMPILER_VERSION_DIR}/${SV_ARCH_DIR}/${SV_BUILD_TYPE_DIR}"
  CACHE PATH "Directory where install files for externals will be put")
set(SV_EXTERNALS_TOPLEVEL_BLD_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/build"
  CACHE PATH "Directory where build files for externals will be put")
set(SV_EXTERNALS_TOPLEVEL_PFX_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/prefix"
  CACHE PATH "Directory where prefix files for externals will be put")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Tar install directory
simvascular_today(YEAR MONTH DAY)
option(SV_EXTERNALS_TAR_BINARIES "Tar up the final built or downloaded externals" OFF)
set(SV_EXTERNALS_TAR_INSTALL_DIR "${CMAKE_BINARY_DIR}/tar_output/${YEAR}.${MONTH}.${DAY}"
  CACHE PATH "Directory where source files for externals will be put")
file(MAKE_DIRECTORY "${SV_EXTERNALS_TAR_INSTALL_DIR}")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# URLs for external downloads and git repositories
set(SV_EXTERNALS_URL  "http://simvascular.stanford.edu/downloads/public/simvascular/externals" CACHE STRING "SimVascular Externals")
set(SV_EXTERNALS_VERSION_NUMBER  "2018.03" CACHE STRING "SimVascular Externals version")
set_property(CACHE SV_EXTERNALS_VERSION_NUMBER PROPERTY STRINGS "2017.01" "2018.01" "2018.03")
set(SV_EXTERNALS_ORIGINALS_URL "${SV_EXTERNALS_URL}/${SV_EXTERNALS_VERSION_NUMBER}/src/originals" CACHE STRING "URL with source downloads for externals")
#set(SV_EXTERNALS_ORIGINALS_URL "/Users/adamupdegrove/Documents/simvascular_externals_srcs/originals" CACHE STRING "URL with source downloads for externals")
mark_as_advanced(SV_EXTERNALS_ORIGINALS_URL)
set(SV_EXTERNALS_BINARIES_URL_PREFIX "${SV_PLATFORM_DIR}.${SV_COMPILER_DIR}-${SV_COMPILER_VERSION_DIR}.${SV_ARCH_DIR}" CACHE STRING "String that gets appended to the beginning of each individual external pre-built binary download")
mark_as_advanced(SV_EXTERNALS_BINARIES_URL_PREFIX)
set(SV_EXTERNALS_GIT_URL "http://github.com/SimVascular" CACHE STRING "Git URL for SimVascular")
mark_as_advanced(SV_EXTERNALS_GIT_URL)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# SV_EXTERNALS_MAC_PACKAGE_MANAGER
if (APPLE)
  message(WARNING "Need to make sure to have openssl from homebrew or macports. Specify which is used with SV_EXTERNALS_MAC_PACKAGE_MANAGER variable.")
  set(SV_EXTERNALS_MAC_PACKAGE_MANAGER "HOMEBREW" CACHE STRING "Options are HOMEBREW OR MACPORTS")
  set_property(CACHE SV_EXTERNALS_MAC_PACKAGE_MANAGER PROPERTY STRINGS HOMEBREW MACPORTS)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Qt
# First external is not built by the project but is required if building
# MITK
option(SV_EXTERNALS_USE_QT "Enable QT Plugin" ON)
option(SV_EXTERNALS_BUILD_MITK_WITH_PYTHON "Build MITK without python" ON)

# Add externals with default values of version, build_with, shared, dirname,
# and optional install dirname. Order matters; put independent packages first
# Must have existing "EXTERNAL_NAME.cmake" file underneath CMakeExternals
# (i.e. Qt.cmake for Qt)
# "EXTERNAL_NAME" "ENABLE_EXTERNAL" "BUILD_SHARED" "BUILD_DIR_NAME" "INSTALL_DIR_NAME"
#-----------------------------------------------------------------------------
# QT
sv_externals_add_new_external(Qt 5.6.3 ON ON qt qt)
set_property(CACHE SV_EXTERNALS_Qt_VERSION PROPERTY STRINGS "5.4.2" "5.6.0" "5.6.3")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# TCL
sv_externals_add_new_external(TCL 8.6.8 ON ON tcl tcltk)
set_property(CACHE SV_EXTERNALS_TCL_VERSION PROPERTY STRINGS "8.6.4" "8.6.8")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# TK
sv_externals_add_new_external(TK 8.6.8 ON ON tk tcltk)
set_property(CACHE SV_EXTERNALS_TK_VERSION PROPERTY STRINGS "8.6.4" "8.6.8")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# TCLLIB
sv_externals_add_new_external(TCLLIB 1.17 ON ON tcllib none)
set_property(CACHE SV_EXTERNALS_TCLLIB_VERSION PROPERTY STRINGS "1.17")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# TKLIB
sv_externals_add_new_external(TKLIB 0.6 ON ON tklib none)
set_property(CACHE SV_EXTERNALS_TKLIB_VERSION PROPERTY STRINGS "0.6")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# PYTHON
sv_externals_add_new_external(PYTHON 3.5.2 ON ON python python)
set_property(CACHE SV_EXTERNALS_PYTHON_VERSION PROPERTY STRINGS "2.7.11" "2.7.13" "3.5.2")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# PIP
sv_externals_add_new_external(PIP 0.0.0 ON ON pip none)
set_property(CACHE SV_EXTERNALS_PIP_VERSION PROPERTY STRINGS "0.0.0")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# NUMPY
sv_externals_add_new_external(NUMPY 1.11.1 ON ON numpy none)
set_property(CACHE SV_EXTERNALS_NUMPY_VERSION PROPERTY STRINGS "1.11.1")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# FREETYPE
sv_externals_add_new_external(FREETYPE 2.6.3 ON ON freetype freetype)
set_property(CACHE SV_EXTERNALS_FREETYPE_VERSION PROPERTY STRINGS "2.6.3")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# SWIG
sv_externals_add_new_external(SWIG 3.0.12 OFF ON swig swig)
set_property(CACHE SV_EXTERNALS_SWIG_VERSION PROPERTY STRINGS "3.0.12")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# MMG
sv_externals_add_new_external(MMG 5.3.9 ON OFF mmg mmg)
set_property(CACHE SV_EXTERNALS_MMG_VERSION PROPERTY STRINGS "5.1.0" "5.3.9")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# GDCM
sv_externals_add_new_external(GDCM 2.6.3 ON ON gdcm gdcm)
set_property(CACHE SV_EXTERNALS_GDCM_VERSION PROPERTY STRINGS "2.6.1" "2.6.3")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# VTK
sv_externals_add_new_external(VTK 8.0.0 ON ON vtk vtk)
set_property(CACHE SV_EXTERNALS_VTK_VERSION PROPERTY STRINGS "6.2.0" "8.0.0")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# ITK
sv_externals_add_new_external(ITK 4.12.2 ON ON itk itk)
set_property(CACHE SV_EXTERNALS_ITK_VERSION PROPERTY STRINGS "4.7.1" "4.12.2")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# OpenCASCADE
sv_externals_add_new_external(OpenCASCADE 7.2.0 ON ON opencascade opencascade)
set_property(CACHE SV_EXTERNALS_OpenCASCADE_VERSION PROPERTY STRINGS "7.0.0" "7.2.0")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# MITK
sv_externals_add_new_external(MITK 2018.02 ON ON mitk mitk)
set_property(CACHE SV_EXTERNALS_MITK_VERSION PROPERTY STRINGS "2016.03" "2018.02")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#Download options for tcltk
option(SV_EXTERNALS_DOWNLOAD_TCLTK "Download instead of build TCLTK" ON)
