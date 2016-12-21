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
# Initial SV Options
#-----------------------------------------------------------------------------
# Developer flag (Output extra info during configure)
option(SV_DEVELOPER_OUTPUT "This is a developer mode to print extra messages during configure" OFF)
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
set(SV_EXTERNALS_TOPLEVEL_DIR "${CMAKE_BINARY_DIR}/sv_externals" CACHE PATH "Externals toplevel directory")
set(SV_EXTERNALS_SRC_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/src" CACHE PATH "Externals toplevel src dir")
set(SV_EXTERNALS_BLD_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/build" CACHE PATH "Externals toplevel build dir")
set(SV_EXTERNALS_PFX_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/prefix" CACHE PATH "Externals toplevel prefix dir")
set(SV_EXTERNALS_BIN_DIR "${SV_EXTERNALS_TOPLEVEL_DIR}/bin/${SV_COMPILER_DIR}/${SV_ARCH_DIR}" CACHE PATH "Externals toplevel bin dir")

set(SV_EXTERNALS_INSTALL_PREFIX "sv_externals" CACHE PATH "Externals toplevel directory")

option(SV_EXTERNALS_USE_TOPLEVEL_DIR "If ON, SV_EXTERNALS_TOPLEVEL_DIR will be used as location for external packages" OFF)

# TCL
simvascular_add_new_external(TCL 8.6.4 ON ON tcltk)

#PYTHON
simvascular_add_new_external(PYTHON 2.7.11 OFF ON python)

#FREETYPE
simvascular_add_new_external(FREETYPE 2.6.3 OFF ON freetype)

# MMG
simvascular_add_new_external(MMG 5.1.0 OFF OFF mmg)

# VTK
simvascular_add_new_external(VTK 6.2.0 ON ON vtk)

# GDCM
simvascular_add_new_external(GDCM 2.6.1 OFF ON gdcm)

# ITK
simvascular_add_new_external(ITK 4.7.1 ON ON itk)

# OpenCASCADE
simvascular_add_new_external(OpenCASCADE 7.0.0 OFF ON opencascade)

# MITK
simvascular_add_new_external(MITK 2016.03 OFF ON mitk)
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
option(SV_USE_QT_GUI "Option to build the SimVascular QT GUI" OFF)

option(SV_USE_QT "Option to build the SimVascular QT" OFF)

option(SV_NO_PYTHONQT_ALL "Option to use PythonQt_all" ON)

option(SV_USE_MITK_CONFIG "Option to use MITKConfig.cmake" OFF)
#-----------------------------------------------------------------------------
