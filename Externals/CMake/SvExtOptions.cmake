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
set(SV_EXTERNALS_TOPLEVEL_DIR "${CMAKE_BINARY_DIR}/sv_externals" CACHE PATH "Externals toplevel directory")

set(SV_EXTERNALS_TOPLEVEL_SRC_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/src"
  CACHE PATH "Directory where source files for externals will be put")
set(SV_EXTERNALS_TOPLEVEL_BIN_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/bin/${SV_EXTERNALS_COMPILER_DIR}/${SV_EXTERNALS_ARCH_DIR}"
  CACHE PATH "Directory where install files for externals will be put")
set(SV_EXTERNALS_TOPLEVEL_BLD_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/build"
  CACHE PATH "Directory where build files for externals will be put")
set(SV_EXTERNALS_TOPLEVEL_PFX_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/prefix"
  CACHE PATH "Directory where prefix files for externals will be put")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Tar install directory
option(SV_EXTERNALS_TAR_BINARIES "Tar up the final built or downloaded externals" OFF)
set(SV_EXTERNALS_TAR_INSTALL_DIR "${CMAKE_BINARY_DIR}/tar_output"
  CACHE PATH "Directory where source files for externals will be put")
file(MAKE_DIRECTORY "${SV_EXTERNALS_TAR_INSTALL_DIR}")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# URLs for external downloads and git repositories
set(SV_EXTERNALS_URL  "http://simvascular.stanford.edu/downloads/public/simvascular/externals")
set(SV_EXTERNALS_ORIGINALS_URL "${SV_EXTERNALS_URL}/src/originals" CACHE STRING "URL with source downloads for externals")
mark_as_advanced(SV_EXTERNALS_ORIGINALS_URL)
set(SV_EXTERNALS_DOWNLOADS_DATE "latest" CACHE STRING "Date that downloadable binaries were built")
mark_as_advanced(SV_EXTERNALS_DOWNLOADS_DATE)
set(SV_EXTERNALS_BINARIES_URL_BASE "${SV_EXTERNALS_URL}/${SV_EXTERNALS_DOWNLOADS_PLATFORM_DIRS}/${SV_EXTERNALS_DOWNLOADS_DATE}" CACHE STRING "URL base where pre-built externals are located")
mark_as_advanced(SV_EXTERNALS_BINARIES_URL_BASE)
set(SV_EXTERNALS_BINARIES_URL_PREFIX "${SV_EXTERNALS_PLATFORM_DIR}.${SV_EXTERNALS_COMPILER_DIR}.${SV_EXTERNALS_ARCH_DIR}" CACHE STRING "String that gets appended to the beginning of each individual external pre-built binary download")
mark_as_advanced(SV_EXTERNALS_BINARIES_URL_PREFIX)
set(SV_EXTERNALS_GIT_URL "http://github.com/SimVascular" CACHE STRING "Git URL for SimVascular")
mark_as_advanced(SV_EXTERNALS_GIT_URL)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Qt
# First external is not built by the project but is required if building
# MITK
option(SV_EXTERNALS_USE_QT "Enable QT Plugin" ON)

# Add externals with default values of version, build_with, shared, dirname,
# and optional install dirname. Order matters; put independent packages first
#-----------------------------------------------------------------------------
# TCL
set(SV_EXTERNALS_TCL_DEFAULT tcltk-8.6.4)
sv_externals_add_new_external(TCL 8.6.4 ON ON tcl tcltk)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# TK
set(SV_EXTERNALS_TK_DEFAULT tcltk-8.6.4)
sv_externals_add_new_external(TK 8.6.4 ON ON tk tcltk)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# TCLLIB
sv_externals_add_new_external(TCLLIB 1.17 ON ON tcllib none)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# TKLIB
sv_externals_add_new_external(TKLIB 0.6 ON ON tklib none)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#PYTHON
set(SV_EXTERNALS_PYTHON_DEFAULT python-2.7.11)
sv_externals_add_new_external(PYTHON 2.7.11 ON ON python python)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#PIP
sv_externals_add_new_external(PIP 0.0.0 ON ON pip none)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#NUMPY
sv_externals_add_new_external(NUMPY 1.11.1 ON ON numpy none)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#FREETYPE
set(SV_EXTERNALS_FREETYPE_DEFAULT freetype-2.6.3)
sv_externals_add_new_external(FREETYPE 2.6.3 ON ON freetype freetype)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# MMG
set(SV_EXTERNALS_MMG_DEFAULT mmg-5.1.0)
sv_externals_add_new_external(MMG 5.1.0 ON OFF mmg mmg)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# GDCM
set(SV_EXTERNALS_GDCM_DEFAULT gdcm-2.6.1)
sv_externals_add_new_external(GDCM 2.6.1 ON ON gdcm gdcm)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# VTK
set(SV_EXTERNALS_VTK_DEFAULT vtk-6.2.0)
sv_externals_add_new_external(VTK 6.2.0 ON ON vtk vtk)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# ITK
set(SV_EXTERNALS_ITK_DEFAULT itk-4.7.1)
sv_externals_add_new_external(ITK 4.7.1 ON ON itk itk)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# OpenCASCADE
set(SV_EXTERNALS_OpenCASCADE_DEFAULT opencascade-7.0.0)
sv_externals_add_new_external(OpenCASCADE 7.0.0 ON ON opencascade opencascade)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# MITK
set(SV_EXTERNALS_MITK_DEFAULT mitk-2016.03)
sv_externals_add_new_external(MITK 2016.03 ON ON mitk mitk)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#Download options for tcltk
option(SV_EXTERNALS_DOWNLOAD_TCLTK "Download instead of build TCLTK" ON)
