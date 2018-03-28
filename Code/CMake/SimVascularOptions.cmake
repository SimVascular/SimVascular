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
# Initial SV Options
#-----------------------------------------------------------------------------
# Developer flag (Output extra info during configure)
option(SV_DEVELOPER_OUTPUT "This is a developer mode to print extra messages during configure" OFF)

# Setup components to pack
set(SV_DISTRIBUTE_COMPONENTS OpenSource CACHE STRING "When distributing, specify which components to distribute.")
set_property(CACHE SV_DISTRIBUTE_COMPONENTS PROPERTY STRINGS OpenSource Licensed All)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Distribution
option(SV_ENABLE_DISTRIBUTION "Distribute" OFF)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Enable Testing
option(BUILD_TESTING "Build ${PROJECT_NAME} testing" OFF)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Libs options
option(BUILD_SHARED_LIBS "Build ${PROJECT_NAME} as shared libraries." OFF)

set(SV_LIBRARY_TYPE "STATIC" CACHE STRING "Options are STATIC or SHARED" FORCE)
set_property(CACHE SV_LIBRARY_TYPE PROPERTY STRINGS STATIC SHARED)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Compiler warning suppression
option(SV_SUPPRESS_WARNINGS "Option to suppress all compiler warnings while compiling" ON)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Solver stuff
option(SV_USE_THREEDSOLVER "Option to build flowsolver modules (requires Fortran)" OFF)

option(SV_USE_THREEDSOLVER_SHARED_LIBRARIES "Option to build flowsolver libs as shared" OFF)

option(SV_USE_MPICH2 "Use MPICH2" ON)

option(SV_USE_DUMMY_MPICH2 "Use Dummy MPICH2" OFF)

option(SV_USE_MSMPI "Use MSMPI" OFF)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Externals
set(SV_EXTERNALS_INSTALL_PREFIX "svExternals" CACHE PATH "Externals toplevel directory")

set(SV_EXTERNALS_TOPLEVEL_BIN_DIR "${CMAKE_BINARY_DIR}/svExternals/bin/${SV_COMPILER_DIR}/${SV_COMPILER_VERSION_DIR}/${SV_ARCH_DIR}/${SV_BUILD_TYPE_DIR}" CACHE PATH "Externals toplevel bin dir")

option(SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR "If ON, SV_EXTERNALS_TOPLEVEL_BIN_DIR will be used as location used to find external packages" OFF)

option(SV_EXTERNALS_INSTALL_HEADERS "If ON, The externals headers will be included in the installation" OFF)

# Qt
simvascular_add_new_external(Qt5 5.6.3 ON ON qt)
set_property(CACHE Qt5_VERSION PROPERTY STRINGS "5.4.2" "5.6.0" "5.6.3")

# TCL
simvascular_add_new_external(TCL 8.6.8 ON ON tcltk)
set_property(CACHE TCL_VERSION PROPERTY STRINGS "8.6.4" "8.6.8")

#PYTHON
simvascular_add_new_external(PYTHON 3.5.2 ON ON python)
set_property(CACHE PYTHON_VERSION PROPERTY STRINGS "2.7.11" "2.7.13" "3.5.2")

#FREETYPE
simvascular_add_new_external(FREETYPE 2.6.3 ON ON freetype)
set_property(CACHE FREETYPE_VERSION PROPERTY STRINGS "2.6.3")

# MMG
simvascular_add_new_external(MMG 5.3.9 ON OFF mmg)
set_property(CACHE MMG_VERSION PROPERTY STRINGS "5.1.0" "5.3.9")

# VTK
simvascular_add_new_external(VTK 8.0.0 ON ON vtk)
set_property(CACHE VTK_VERSION PROPERTY STRINGS "6.2.0" "8.0.0")

# GDCM
simvascular_add_new_external(GDCM 2.6.3 ON ON gdcm)
set_property(CACHE GDCM_VERSION PROPERTY STRINGS "2.6.1" "2.6.3")

# ITK
simvascular_add_new_external(ITK 4.12.2 ON ON itk)
set_property(CACHE ITK_VERSION PROPERTY STRINGS "4.7.1" "4.12.2")

# OpenCASCADE
simvascular_add_new_external(OpenCASCADE 7.2.0 ON ON opencascade)
set_property(CACHE OpenCASCADE_VERSION PROPERTY STRINGS "7.0.0" "7.2.0")

# MITK
simvascular_add_new_external(MITK 2018.02 ON ON mitk)
set_property(CACHE MITK_VERSION PROPERTY STRINGS "2016.03" "2018.02")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# ThirdParty
option(SV_USE_THIRDPARTY_SHARED_LIBRARIES "Option to build the thirdparty libs as shared" OFF)

option(SV_USE_ZLIB "Use ZLib" ON)

option(SV_USE_VMTK "Enable VMTK Plugin" ON)

option(SV_USE_TETGEN "Enable Tetgen Meshing Plugin" ON)

option(SV_USE_TETGEN_ADAPTOR "Option to use open source mesh adaption" ON)

option(SV_USE_PYTHON "Use Tcl Python" OFF)

option(SV_USE_SOLVERIO "Use SolverIO" OFF)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Commercial Software Options: Solid Models - Parasolid
option(SV_USE_PARASOLID "Parasolid" OFF)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# MeshSim
option(SV_USE_MESHSIM "Use MeshSim commercial libraries.  Requires licenese" OFF)
option(SV_USE_MESHSIM_ADAPTOR "Build the adapter (Requires Fortran and MeshSim)" OFF)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Modules Shared
option(SV_USE_MODULES_SHARED_LIBRARIES "Option to build the thirdparty libs as shared" ON)

#-----------------------------------------------------------------------------
# Option to build qt GUI
option(SV_USE_QT_GUI "Option to build the SimVascular QT GUI" ON)

option(SV_USE_QT "Option to build the SimVascular QT" ON)

option(SV_NO_PYTHONQT_ALL "Option to use PythonQt_all" ON)

option(SV_USE_MITK_CONFIG "Option to use MITKConfig.cmake" OFF)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# vtkSVOptions
# Build regular libraries instead of typical build system
option(VTKSV_BUILD_LIBS_AS_VTK_MODULES "Option to build the vtkSV libs as vtk modules" OFF)

# Different modules in vtkSV
option(VTKSV_BUILD_FILTERS                 "Option to build the filters"        ON)
set(VTKSV_BUILD_FILTERS ON CACHE BOOL      "Always ON, but needed for vtkSV" FORCE)
mark_as_advanced(VTKSV_BUILD_FILTERS)

option(VTKSV_BUILD_MODULE_NURBS            "Option to build the NURBS code"     ON)
set(VTKSV_BUILD_NURBS ON CACHE BOOL        "Always ON, but needed for vtkSV" FORCE)
mark_as_advanced(VTKSV_BUILD_NURBS)

option(VTKSV_BUILD_MODULE_BOOLEAN          "Option to build the Boolean code"    ON)
set(VTKSV_BUILD_BOOLEAN ON CACHE BOOL      "Always ON, but needed for vtkSV"  FORCE)
mark_as_advanced(VTKSV_BUILD_BOOLEAN)

option(VTKSV_BUILD_MODULE_PARAMETERIZATION     "Option to build the Parameterization code" ON)
set(VTKSV_BUILD_PARAMETERIZATION ON CACHE BOOL "Always ON, but needed for vtkSV"        FORCE)
mark_as_advanced(VTKSV_BUILD_PARAMETERIZATION)

option(VTKSV_BUILD_MODULE_GEOMETRY     "Option to build the Geometry code"    ON)
set(VTKSV_BUILD_GEOMETRY ON CACHE BOOL "Always ON, but needed for vtkSV"   FORCE)
mark_as_advanced(VTKSV_BUILD_GEOMETRY)

# Needed install dirs (not really options, but easier to place here with everything else for vtksV)
set(VTKSV_INSTALL_RUNTIME_DIR ${SV_INSTALL_RUNTIME_DIR})
set(VTKSV_INSTALL_LIBRARY_DIR ${SV_INSTALL_LIBRARY_DIR})
set(VTKSV_INSTALL_ARCHIVE_DIR ${SV_INSTALL_ARCHIVE_DIR})
set(VTKSV_INSTALL_INCLUDE_DIR ${SV_INSTALL_INCLUDE_DIR})
#-----------------------------------------------------------------------------
