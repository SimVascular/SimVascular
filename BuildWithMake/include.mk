# Hey emacs, this is a -*- makefile -*-

# Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                         University of California, San Diego.
# 
# All rights reserved. 
#
# Portions copyright (c) 1999-2007 Stanford University,
# Nathan Wilson, Ken Wang, Charles Taylor.
# 
# See SimVascular Acknowledgements file for additional
# contributors to the source code. 
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
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# This is where we want to define things that will be useful for
# multiple packages.  Vtk, for example, is used by both the level set
# package for front extraction and by the utils package to implement
# general vtk helper functions.

# Important notes:
#   - set TARGETDIR
#   - specify inclusion of { Discrete, Parasolid, Irit } via SV_USE_*
#     vars

# ----------------------------------------------------
# by default, no check for dependancies when compiling
# ----------------------------------------------------

NO_DEPEND = 1

# -----------------------------------------------------------
# CLUSTER = { x64_cygwin, x64_linux }
# -----------------------------------------------------------

CLUSTER = x64_cygwin
#CLUSTER = x64_linux

# ---------------------------------------------------------------------
# CXX_COMPILER_VERSION = { icpc, vs10.1, vs12.5, mingw-gcc, gcc}
# FORTRAN_COMPILER_VERSION = { ifort, mingw-gfortran, gfortran }
# ---------------------------------------------------------------------

CXX_COMPILER_VERSION = vs12.5
FORTRAN_COMPILER_VERSION = ifort

ifeq ($(LOCAL_DIR_CLUSTER_OVERRIDES),1)
-include cluster_overrides.mk
else
-include $(TOP)/cluster_overrides.mk
endif

# -------
# globals
# -------

SV_USE_SHARED = 1
SV_USE_GLOBALS_SHARED = 1

# ---------------------------------------
# Control solid modeling kernel inclusion
# ---------------------------------------
# Note that Geodesic allows for multiple kernels to be built into a
# single executable.  In order to include a kernel, uncomment its line
# in the following short section.  In order to exclude a kernel,
# set the value to something other than 1 in global_overrides.mk or 
# via the make command line.

SV_USE_PARASOLID = 0
SV_USE_PARASOLID_SHARED = 1

# ------------
# Open Cascade
# ------------

SV_USE_OPENCASCADE = 1
SV_USE_OPENCASCADE_SHARED = 1

# --------------------------------------
# Control inclusion of meshSim functions
# --------------------------------------

SV_USE_MESHSIM = 0
SV_USE_MESHSIM_DISCRETE_MODEL = 0
SV_USE_MESHSIM_DISCRETE_MODEL_SHARED = 1
SV_USE_MESHSIM_SOLID_MODEL = 0
SV_USE_MESHSIM_SOLID_MODEL_SHARED = 1
SV_USE_MESHSIM_ADAPTOR = 0
SV_USE_MESHSIM_SHARED = 1
MESHSIM_USE_LICENSE_FILE = 1
MESHSIM_EMBED_LICENSE_KEYS = 0
MESHSIM_LICENSE_IN_WIN32_REGISTRY = 0

# -------------------------------------
# Control inclusion of tetgen functions
# -------------------------------------

SV_USE_TETGEN = 1
SV_USE_TETGEN_ADAPTOR = 1

# ------------------------
# Control inclusion of mmg
# ------------------------

SV_USE_MMG = 1
SV_USE_MMG_SHARED = 1

# ----------------------------------------------
# Control inclusion of leslib
# {binary and dummy} are mutually exclusive opts
# ----------------------------------------------

SV_USE_BINARY_LESLIB = 0
SV_USE_DUMMY_LESLIB = 1

# --------------------------------------------------------
# Control inclusion of svLS
# {binary, dummy, source code} are mutually exclusive opts
# --------------------------------------------------------

SV_USE_DUMMY_SVLS = 0
SV_USE_SOURCE_CODE_SVLS = 1

# -----------------------------------------------------
# Compile with zlib
# -----------------------------------------------------

SV_USE_ZLIB = 1

# -----------------------------------------------------
# Compile with python interpreter
# -----------------------------------------------------

SV_USE_PYTHON = 1
SV_USE_PYTHON_SHARED = 1
SV_USE_SYSTEM_PYTHON = 1

# -----------------------------------------------------
# Compile with python interpreter
# -----------------------------------------------------

SV_USE_QT = 1
SV_USE_SYSTEM_QT = 1
SV_USE_QT_GUI = 0
SV_USE_QT_GUI_SHARED = 1

# -----------------------------------------------------
# Compile with freetype
# -----------------------------------------------------

SV_USE_FREETYPE = 1
SV_USE_SYSTEM_FREETYPE = 1

# -----------------------------------------------------
# system tcltk
# -----------------------------------------------------

SV_USE_SYSTEM_TCLTK = 0

# -----------------------------------------------------
# Compile with 3-D Solver and Related Programs
# -----------------------------------------------------

SV_USE_SOLVERIO = 1
SV_USE_THREEDSOLVER = 1
SV_USE_PRESOLVER = 1
SV_USE_POSTSOLVER = 1

# -----------------------------------------------------
# Compile Flowsolver Modules
# -----------------------------------------------------

SV_THREEDSOLVER_USE_CORONARY = 1
SV_THREEDSOLVER_USE_CLOSEDLOOP = 1
SV_THREEDSOLVER_USE_VARWALL = 1
SV_THREEDSOLVER_USE_VTK = 1

# -----------------------------------------------------
# Compile with MPI
# -----------------------------------------------------

SV_USE_MPI = 1
SV_USE_OPENMPI = 0
SV_USE_MPICH = 0

# by default, only build with real mpi on windows
ifeq ($(CLUSTER), x64_cygwin)
  SV_USE_DUMMY_MPI = 0
endif
ifeq ($(CLUSTER), x64_linux)
  SV_USE_DUMMY_MPI = 1
endif
ifeq ($(CLUSTER), x64_macosx)
  SV_USE_DUMMY_MPI = 1
endif

# -----------------------------------------------------
# Build only the 3D Solver
# -----------------------------------------------------

EXCLUDE_ALL_BUT_THREEDSOLVER ?= 0

# -----------------------------------------------------
# Compile with VTK
# -----------------------------------------------------

SV_USE_VTK = 1

# -----------------------------------------------------
# Compile with ITK
# -----------------------------------------------------

SV_USE_ITK = 1

# -----------------------------------------------------
# Compile with VMTK
# -----------------------------------------------------

SV_USE_VMTK = 1

# -----------------------------------------------------
# Compile with GDCM
# -----------------------------------------------------

SV_USE_GDCM = 1

# -----------------------------------------------------
# Compile with MITK
# -----------------------------------------------------

SV_USE_MITK = 1

# -----------------------------------------------------
# Compile with glib & gts
# -----------------------------------------------------

SV_USE_GLIB = 0
SV_USE_GTS  = 0

# -----------------------------------------------------
# Compile with sparse, metis, nspcg
# -----------------------------------------------------

SV_USE_SPARSE = 1
SV_USE_METIS = 1
SV_USE_NSPCG = 1

# -----------------------------------------------------
# Compile with Optimization
# -----------------------------------------------------

MAKE_OPTIMIZED = 1
LINK_WITH_DEBUG = 1

# -----------------------------------------------------
# Static link
# -----------------------------------------------------

#MAKE_STATIC_BUILD = 1

# if you need to override anything above for a given site, do it here
# -----------------------------------------------------------------------

ifeq ($(LOCAL_DIR_SITE_OVERRIDES),1)
-include site_overrides.mk
else
-include $(TOP)/site_overrides.mk
endif

# ----------------
# Target directory
# ----------------

TARGETDIR = .

ifeq ($(CLUSTER), x64_cygwin)
  SVEXTERN_COMPILER_VERSION = $(CXX_COMPILER_VERSION)
endif
ifeq ($(CLUSTER), x64_linux)
  SVEXTERN_COMPILER_VERSION = gcc-4.8
endif
ifeq ($(CLUSTER), x64_macosx)
  SVEXTERN_COMPILER_VERSION = clang-7.0
endif

ifeq ($(CLUSTER), x64_cygwin)
    OPEN_SOFTWARE_BINARIES_TOPLEVEL = C:/cygwin64/SV16/bin/$(SVEXTERN_COMPILER_VERSION)/x64
    OPEN_SOFTWARE_BUILDS_TOPLEVEL = C:/cygwin64/SV16/build/$(SVEXTERN_COMPILER_VERSION)/x64
    OPEN_SOFTWARE_SOURCES_TOPLEVEL = C:/cygwin64/SV16/src
    LICENSED_SOFTWARE_TOPLEVEL = C:/cygwin64/SV16/licensed
endif

ifeq ($(CLUSTER), x64_linux)
    OPEN_SOFTWARE_BINARIES_TOPLEVEL = /usr/local/SV16
    OPEN_SOFTWARE_BUILDS_TOPLEVEL =
    OPEN_SOFTWARE_SOURCES_TOPLEVEL  = 
    LICENSED_SOFTWARE_TOPLEVEL      = /SV16/licensed
endif

ifeq ($(CLUSTER), x64_macosx)
    OPEN_SOFTWARE_BINARIES_TOPLEVEL = /usr/local/SV16/bin/$(SVEXTERN_COMPILER_VERSION)/x64
    OPEN_SOFTWARE_BUILDS_TOPLEVEL = /usr/local/SV16/build/$(SVEXTERN_COMPILER_VERSION)/x64
    OPEN_SOFTWARE_SOURCES_TOPLEVEL  = /usr/local/SV16/src
    LICENSED_SOFTWARE_TOPLEVEL      = /usr/local/SV16/licensed
endif

# -------------------------------------------
#   Release version numbers for SimVascular 
# -------------------------------------------

SV_MAJOR_VER_NO = "2.16"
SV_FULL_VER_NO = "2.16.0719"
SV_USE_WIN32_REGISTRY=0
SV_REGISTRY_TOPLEVEL=SIMVASCULAR

# if you need to override anything above, stuff it in global_overrides.mk
# -----------------------------------------------------------------------

ifeq ($(LOCAL_DIR_GLOBAL_OVERRIDES),1)
-include global_overrides.mk
else
-include $(TOP)/global_overrides.mk
endif

ifeq ($(CLUSTER),x64_cygwin) 
  SV_VERSION  = simvascular
  SV_PLATFORM = x64
  SV_POSTFIX=
  SV_OS=windows
endif
ifeq ($(CLUSTER),x64_linux) 
  SV_VERSION  = simvascular
  SV_PLATFORM = x64
  SV_POSTFIX=
  SV_OS=linux
endif

# by default don't build most third party
# if we are only building the flow solver
ifeq ($(EXCLUDE_ALL_BUT_THREEDSOLVER), 1)
    ifeq ($(SV_THREEDSOLVER_USE_VTK), 0)
        SV_USE_VTK = 0
    endif

    SV_USE_ITK = 0
    SV_USE_MITK = 0
    SV_USE_GDCM = 0
    SV_USE_VMTK = 0
    SV_USE_TETGEN = 0
    SV_USE_SPARSE = 0
    SV_USE_NSPCG = 0
endif

# --------------
# Global defines
# --------------

GLOBAL_DEFINES = -DSV_VERSION=\"$(SV_VERSION)\" -DSV_MAJOR_VER_NO=\"$(SV_MAJOR_VER_NO)\" -DSV_FULL_VER_NO=\"$(SV_FULL_VER_NO)\" -DSV_REGISTRY_TOPLEVEL=\"$(SV_REGISTRY_TOPLEVEL)\"

ifeq ($(SV_USE_WIN32_REGISTRY), 1)
  GLOBAL_DEFINES += -DSV_USE_WIN32_REGISTRY
endif

ifeq ($(SV_STATIC_BUILD),1)
  GLOBAL_DEFINES += -DSV_STATIC_LINK -DSV_STATIC_BUILD
endif

ifeq ($(SV_GLOBALS_SHARED),1)
  GLOBAL_DEFINES += -DSV_GLOBALS_SHARED
endif

ifeq ($(CLUSTER), x64_cygwin)
   GLOBAL_DEFINES += -DSV_USE_NOTIMER -DWINDOWS -DWIN32
endif

ifeq ($(CLUSTER), x64_linux)
   GLOBAL_DEFINES += -DSV_USE_NOTIMER -DUNIX
endif

ifeq ($(CLUSTER), x64_macosx)
   GLOBAL_DEFINES += -DSV_USE_NOTIMER -DUNIX
endif


ifeq ($(SV_USE_PARASOLID),1)
    GLOBAL_DEFINES += -DSV_USE_PARASOLID
endif
ifeq ($(SV_USE_PARASOLID_SHARED),1)
    GLOBAL_DEFINES += -DSV_USE_PARASOLID_SHARED
endif
ifeq ($(SV_USE_OPENCASCADE),1)
    GLOBAL_DEFINES += -DSV_USE_OPENCASCADE
endif
ifeq ($(SV_USE_OPENCASCADE_SHARED),1)
    GLOBAL_DEFINES += -DSV_USE_OPENCASCADE_SHARED
endif

ifeq ($(SV_USE_MESHSIM),1) 
  GLOBAL_DEFINES += -DSV_USE_MESHSIM
  ifeq ($(SV_USE_MESHSIM_SHARED),1) 
    GLOBAL_DEFINES += -DSV_USE_MESHSIM_SHARED
  endif
  ifeq ($(SV_USE_MESHSIM_DISCRETE_MODEL),1)
    GLOBAL_DEFINES += -DSV_USE_MESHSIM_DISCRETE_MODEL
  endif
  ifeq ($(SV_USE_MESHSIM_DISCRETE_MODEL_SHARED),1)
    GLOBAL_DEFINES += -DSV_USE_MESHSIM_DISCRETE_MODEL_SHARED
  endif
  ifeq ($(SV_USE_MESHSIM_SOLID_MODEL),1)
    GLOBAL_DEFINES += -DSV_USE_MESHSIM_SOLID_MODEL
  endif
  ifeq ($(SV+USE_MESHSIM_SOLID_MODEL_SHARED),1)
    GLOBAL_DEFINES += -DSV_USE_MESHSIM_SOLID_MODEL_SHARED
  endif
  ifeq ($(SV_USE_MESHSIM_ADAPTOR),1) 
    GLOBAL_DEFINES += -DSV_USE_MESHSIM_ADAPTOR
  endif
  ifeq ($(SV_USE_WIN32_REGISTRY),1)
    GLOBAL_DEFINES += -DMESHSIM_LICENSE_IN_WIN32_REGISTRY
  else
    ifeq ($(MESHSIM_USE_LICENSE_FILE),1)
      GLOBAL_DEFINES += -DMESHSIM_USE_LICENSE_FILE
    endif
    ifeq ($(MESHSIM_EMBED_LICENSE_KEYS),1)
      GLOBAL_DEFINES += -DMESHSIM_EMBED_LICENSE_KEYS
    endif
  endif
endif

ifeq ($(SV_USE_TETGEN),1) 
  GLOBAL_DEFINES += -DSV_USE_TETGEN
  ifeq ($(SV_USE_TETGEN_ADAPTOR),1) 
    GLOBAL_DEFINES += -DSV_USE_TET_ADAPTOR
  endif
endif

ifeq ($(SV_USE_MMG),1)
  GLOBAL_DEFINES += -DSV_USE_MMG
endif

ifeq ($(SV_USE_PYTHON),1)
    GLOBAL_DEFINES += -DSV_USE_PYTHON
endif

ifeq ($(SV_USE_QT),1)
    GLOBAL_DEFINES += -DSV_USE_QT
  ifeq ($(SV_USE_QT_GUI),1)
    GLOBAL_DEFINES += -DSV_USE_QT_GUI
  endif
endif

ifeq ($(SV_USE_ZLIB),1)
  GLOBAL_DEFINES += -DSV_USE_ZLIB
endif

ifeq ($(SV_USE_ITK),1)
  GLOBAL_DEFINES += -DSV_USE_ITK
endif

ifeq ($(SV_USE_MITK),1)
  GLOBAL_DEFINES += -DSV_USE_MITK
endif

ifeq ($(SV_USE_GDCM),1)
  GLOBAL_DEFINES += -DSV_USE_GDCM
endif

ifeq ($(SV_USE_VMTK),1)
  GLOBAL_DEFINES += -DSV_USE_VMTK
endif

ifeq ($(SV_USE_GTS),1)
  GLOBAL_DEFINES += -DSV_USE_GTS -DNATIVE_WIN32
endif

# ----------------------------------
# Platform-specific compiler options
# ----------------------------------

ifeq ($(CLUSTER), x64_cygwin)
  ifeq ($(CXX_COMPILER_VERSION), vs10.1)
	include $(TOP)/MakeHelpers/compiler.vs10.1.x64_cygwin.mk
  endif
  ifeq ($(CXX_COMPILER_VERSION), vs12.5)
	include $(TOP)/MakeHelpers/compiler.vs12.5.x64_cygwin.mk
  endif
  ifeq ($(FORTRAN_COMPILER_VERSION), ifort)
	include $(TOP)/MakeHelpers/compiler.ifort.x64_cygwin.mk
        GLOBAL_DEFINES += -DSV_WRAP_FORTRAN_IN_CAPS_NO_UNDERSCORE
  endif
  ifeq ($(CXX_COMPILER_VERSION), mingw-gcc)
	include $(TOP)/MakeHelpers/compiler.mingw-gcc.x64_cygwin.mk
  endif
  ifeq ($(FORTRAN_COMPILER_VERSION), mingw-gfortran)
	include $(TOP)/MakeHelpers/compiler.mingw-gfortran.x64_cygwin.mk
        GLOBAL_DEFINES += -DSV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE
  endif
endif

ifeq ($(CLUSTER), x64_linux)
  ifeq ($(CXX_COMPILER_VERSION), icpc)
	include $(TOP)/MakeHelpers/compiler.icpc.x64_linux.mk
  endif
  ifeq ($(FORTRAN_COMPILER_VERSION), ifort)
	include $(TOP)/MakeHelpers/compiler.ifort.x64_linux.mk
        GLOBAL_DEFINES += -DSV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE
  endif
  ifeq ($(CXX_COMPILER_VERSION), gcc)
	include $(TOP)/MakeHelpers/compiler.gcc.x64_linux.mk
  endif
  ifeq ($(FORTRAN_COMPILER_VERSION), gfortran)
	include $(TOP)/MakeHelpers/compiler.gfortran.x64_linux.mk
        GLOBAL_DEFINES += -DSV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE
  endif
endif

ifeq ($(CLUSTER), x64_macosx)
  ifeq ($(CXX_COMPILER_VERSION), clang)
	include $(TOP)/MakeHelpers/compiler.clang.x64_macosx.mk
  endif
  ifeq ($(FORTRAN_COMPILER_VERSION), ifort)
	include $(TOP)/MakeHelpers/compiler.ifort.x64_macosx.mk
        GLOBAL_DEFINES += -DSV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE
  endif
  ifeq ($(CXX_COMPILER_VERSION), gcc)
	include $(TOP)/MakeHelpers/compiler.gcc.x64_macosx.mk
  endif
  ifeq ($(FORTRAN_COMPILER_VERSION), gfortran)
	include $(TOP)/MakeHelpers/compiler.gfortran.x64_macosx.mk
        GLOBAL_DEFINES += -DSV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE
  endif
endif

# --------------------------------
# build directory for object files
# --------------------------------

BUILD_DIR = obj/$(CLUSTER)/$(CXX_COMPILER_VERSION)-$(FORTRAN_COMPILER_VERSION)
LIB_BUILD_DIR = $(CLUSTER)/$(CXX_COMPILER_VERSION)-$(FORTRAN_COMPILER_VERSION)
BUILD_MPI_DIR = obj/$(CLUSTER)/$(CXX_COMPILER_VERSION)-$(FORTRAN_COMPILER_VERSION)/$(MPI_NAME)
LIB_MPI_BUILD_DIR = $(CLUSTER)/$(CXX_COMPILER_VERSION)-$(FORTRAN_COMPILER_VERSION)

# ---------------------
# Local lib directories
# ---------------------

LIBDIRS =
SHARED_LIBDIRS =

ifeq ($(SV_USE_GLOBALS_SHARED),1)
  SHARED_LIBDIRS = ../Code/Source/Common/Globals
else
  LIBDIRS = ../Code/Source/Common/Globals
endif

ifeq ($(SV_USE_SHARED),1)
  SHARED_LIBDIRS += ../Code/Source/Common/Utils \
	  ../Code/Source/Common/Repository \
	  ../Code/Source/Model/SolidModel \
	  ../Code/Source/Mesh/MeshObject \
	  ../Code/Source/Common/Geometry \
	  ../Code/Source/ImageProcessing \
	  ../Code/Source/PostProcessing \
	  ../Code/Source/Model/PolyDataSolidModel \
	  ../Code/Source/Legacy/LevelSet
else
  LIBDIRS += ../Code/Source/Common/Utils \
	  ../Code/Source/Common/Repository \
	  ../Code/Source/Model/SolidModel \
	  ../Code/Source/Mesh/MeshObject \
	  ../Code/Source/Common/Geometry \
	  ../Code/Source/ImageProcessing \
	  ../Code/Source/PostProcessing \
	  ../Code/Source/Model/PolyDataSolidModel \
	  ../Code/Source/Legacy/LevelSet
endif

ifeq ($(SV_USE_ITK),1)
  ifeq ($(SV_USE_SHARED),1)
     SHARED_LIBDIRS += ../Code/Source/Segmentation/ITK
  else
     LIBDIRS += ../Code/Source/Segmentation/ITK
  endif
endif

# for now, combine mitk code qt gui code
ifeq ($(SV_USE_MITK),1)
  ifeq ($(SV_USE_SHARED),1)
     SHARED_LIBDIRS += ../Code/Source/QtCode
  else
     LIBDIRS += ../Code/Source/QtCode
  endif
endif

ifeq ($(SV_USE_MMG),1)
  ifeq ($(SV_USE_MMG_SHARED),1)
     SHARED_LIBDIRS += ../Code/Source/Mesh/MMGMeshUtils
  else
     LIBDIRS += ../Code/Source/Mesh/MMGMeshUtils
  endif
endif

ifeq ($(SV_USE_TETGEN),1)
  ifeq ($(SV_USE_SHARED),1)
     SHARED_LIBDIRS += ../Code/Source/Mesh/TetGenMeshObject
  else
     LIBDIRS += ../Code/Source/Mesh/TetGenMeshObject
  endif
endif

#  solid modeling

ifeq ($(SV_USE_PARASOLID),1)
  ifeq ($(SV_USE_PARASOLID_SHARED),1)
    SHARED_LIBDIRS += ../Code/Licensed/ParasolidSolidModel
  else
    LIBDIRS += ../Code/Licensed/ParasolidSolidModel
  endif
endif

ifeq ($(SV_USE_MESHSIM_DISCRETE_MODEL),1)
  ifeq ($(SV_USE_MESHSIM_SHARED),1)
    SHARED_LIBDIRS += ../Code/Source/Model/MeshSimDiscreteSolidModel
  else
    LIBDIRS += ../Code/Source/Model/MeshSimDiscreteSolidModel
  endif
endif

    ifeq ($(SV_USE_MESHSIM_SOLID_MODEL),1)
      ifeq ($(SV_USE_MESHSIM_SOLID_MODEL_SHARED),1)
        SHARED_LIBDIRS += ../Code/Source/Model/MeshSimSolidModel
      else
        LIBDIRS += ../Code/Source/Model/MeshSimSolidModel
      endif
    endif

ifeq ($(SV_USE_OPENCASCADE),1)
  ifeq ($(SV_USE_OPENCASCADE_SHARED),1)
    SHARED_LIBDIRS += ../Code/Source/Model/OCCTSolidModel
  else
    LIBDIRS += ../Code/Source/Model/OCCTSolidModel
  endif
endif

# meshing

ifeq ($(SV_USE_MESHSIM),1)
  ifeq ($(SV_USE_MESHSIM_SHARED),1)
     SHARED_LIBDIRS += ../Code/Source/Mesh/MeshSimMeshObject
  else
     LIBDIRS += ../Code/Source/Mesh/MeshSimMeshObject
  endif
endif

ifneq ($(EXCLUDE_ALL_BUT_THREEDSOLVER),1)
  EXECDIRS = ../Code/Source/UI
else
  EXECDIRS = 
endif

ifeq ($(SV_USE_THREEDSOLVER),1)
     LIBDIRS += ../Code/FlowSolvers/ThreeDSolver
     SOLVERIO_INCDIR = -I $(TOP)/../Code/FlowSolvers/ThreeDSolver/SolverIO -I $(TOP)/../Code/FlowSolvers/Include/Make
     THREEDSOLVER_INCDIR = -I $(TOP)/../Code/FlowSolvers/ThreeDSolver
     EXECDIRS += ../Code/FlowSolvers/ThreeDSolver
endif

ifeq ($(SV_USE_SOLVERIO),1)
     LIBDIRS += ../Code/FlowSolvers/ThreeDSolver/SolverIO
     THREEDSOLVER_INCDIR = -I $(TOP)/../Code/FlowSolvers/ThreeDSolver
endif

# need solverio for adaptor classes so add them after adding solverio

ifeq ($(SV_USE_TETGEN_ADAPTOR),1)
  ifeq ($(SV_USE_SHARED),1)
    SHARED_LIBDIRS += ../Code/Source/Mesh/AdaptObject
  else
    LIBDIRS += ../Code/Source/Mesh/AdaptObject
  endif
else
  ifeq ($(SV_USE_MESHSIM_ADAPTOR),1)
    ifeq ($(SV_USE_SHARED),1)
      SHARED_LIBDIRS += ../Code/Source/Mesh/AdaptObject
    else
      LIBDIRS += ../Code/Source/Mesh/AdaptObject
    endif
  endif
endif

ifeq ($(SV_USE_TETGEN_ADAPTOR),1)
  ifeq ($(SV_USE_SHARED),1)
    SHARED_LIBDIRS += ../Code/Source/Mesh/TetGenAdapt
  else
    LIBDIRS += ../Code/Source/Mesh/TetGenAdapt
  endif
endif

ifeq ($(SV_USE_MESHSIM_ADAPTOR),1)
  ifeq ($(SV_USE_MESHSIM_SHARED),1)
     SHARED_LIBDIRS += ../Code/Source/Mesh/MeshSimAdapt
  else
     LIBDIRS += ../Code/Source/Mesh/MeshSimAdapt
  endif
endif

# -------------------------
# Build a python interpreter
# -------------------------

ifeq ($(SV_USE_PYTHON),1)
  ifeq ($(SV_USE_PYTHON_SHARED),1)
     SHARED_LIBDIRS += ../Code/Source/TclPython
  else
     LIBDIRS += ../Code/Source/TclPython
  endif
endif

# -------------------------
# Qt
# -------------------------

ifeq ($(SV_USE_QT_GUI),1)
  ifeq ($(SV_USE_QT_GUI_SHARED),1)
     SHARED_LIBDIRS += ../Code/Source/QtCode
  else
     LIBDIRS += ../Code/Source/QtCode
  endif
endif

#
#  override other options to build solver only!
#

ifeq ($(EXCLUDE_ALL_BUT_THREEDSOLVER),1)
  LIBDIRS = ../Code/FlowSolvers/ThreeDSolver
  EXECDIRS = ../Code/FlowSolvers/ThreeDSolver
endif

SUBDIRS         = $(LIBDIRS) $(EXECDIRS)

# -------------------------
# Local include directories
# -------------------------

LOCAL_SUBDIRS   = $(LIBDIRS) $(SHARED_LIBDIRS) ../Code/Source/Include ../Code/Source/Include/Make
LOCAL_INCDIR    := $(foreach i, ${LOCAL_SUBDIRS}, -I$(TOP)/$(i))

ifeq ($(SV_USE_ITK),1)
     LOCAL_INCDIR += -I$(TOP)/../Code/Source/Segmentation/ITK/Include
endif

# for now, combine the mitk and qt gui include dirs
ifeq ($(SV_USE_MITK),1)
     LOCAL_INCDIR += -I$(TOP)/../Code/Source/QtCode/Extensions/sv.general \
                     -I$(TOP)/../Code/Source/QtCode/Extensions/sv.image \
                     -I$(TOP)/../Code/Source/QtCode/Extensions/sv.mitksegmentation \
                     -I$(TOP)/../Code/Source/QtCode/Extensions/sv.pathplanning \
                     -I$(TOP)/../Code/Source/QtCode/Extensions/sv.segmentation \
                     -I$(TOP)/../Code/Source/QtCode/Extensions/sv.test \
                     -I$(TOP)/../Code/Source/QtCode/Modules/AppBase \
                     -I$(TOP)/../Code/Source/QtCode/Modules/Common \
                     -I$(TOP)/../Code/Source/QtCode/Modules/PathPlanning \
                     -I$(TOP)/../Code/Source/QtCode/Modules/Segmentation \
                     -I$(TOP)/../Code/Source/QtCode/Applications
endif

# Link flags, which also need to be dealt with conditionally depending
# on which concrete classes derived from SolidModel are being
# included.
# -----

# include path to find libs when linking
GLOBAL_LFLAGS 	 += $(LIBPATH_COMPILER_FLAG)$(TOP)/Lib/$(LIB_BUILD_DIR)

LFLAGS 	 = $(GLOBAL_LFLAGS) $(VTK_LIBS) $(TCLTK_LIBS) $(PYTHON_LIB)

ifneq ($(SV_USE_SHARED),1)
  LFLAGS     += $(SVLIBFLAG)_simvascular_lset$(LIBLINKEXT) \
              $(SVLIBFLAG)_simvascular_image$(LIBLINKEXT) \
              $(SVLIBFLAG)_simvascular_mesh$(LIBLINKEXT) \
              $(SVLIBFLAG)_simvascular_solid$(LIBLINKEXT) \
              $(SVLIBFLAG)_simvascular_geom$(LIBLINKEXT) \
              $(SVLIBFLAG)_simvascular_repository$(LIBLINKEXT) \
              $(SVLIBFLAG)_simvascular_utils$(LIBLINKEXT) \
              $(SVLIBFLAG)_simvascular_post$(LIBLINKEXT) \
              $(SVLIBFLAG)_simvascular_polydata_solid$(LIBLINKEXT)
endif

#
# ThirdParty software that must be built
# from source if used.
#

# -----------------------------------------
# ***  Optional Open Source Packages    ***
# ***   (less restrictive licenses)     ***
# *** (e.g. MIT or BSD or Apache 2.0)   ***
# -----------------------------------------

# ----
# VMTK
# ----

ifeq ($(SV_USE_VMTK),1)
     THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/vmtk
     VMTK_TOP = $(TOP)/../Code/ThirdParty/vmtk
     VMTK_INCDIR  = -I $(VMTK_TOP)
     VMTK_LIBS    = $(SVLIBFLAG)_simvascular_thirdparty_vmtk$(LIBLINKEXT)
endif

# ------
# Sparse
# ------

ifeq ($(SV_USE_SPARSE),1)
  THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/sparse
  SPARSE_TOP = $(TOP)/../Code/ThirdParty/sparse
  SPARSE_INCDIR  = -I $(SPARSE_TOP)
  SPARSE_LIBS    = $(SVLIBFLAG)_simvascular_thirdparty_sparse$(LIBLINKEXT)
endif

# ----
# zlib
# ----

ifeq ($(SV_USE_ZLIB),1)
  THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/zlib
  ZLIB_TOP = $(TOP)/../Code/ThirdParty/zlib
  ZLIB_INCDIR  = -I $(ZLIB_TOP)
  ZLIB_LIBS    = $(SVLIBFLAG)_simvascular_thirdparty_zlib$(LIBLINKEXT)
endif

# -----------------------------------------
# ***  Optional Open Source Packages    ***
# ***           (LGPL code)             ***
# -----------------------------------------

# ------
# NSPCG
# ------

ifeq ($(SV_USE_NSPCG),1)
  THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/nspcg
  NSPCG_TOP = $(TOP)/../Code/ThirdParty/nspcg
  NSPCG_INCDIR  = -I $(NSPCG_TOP)
  NSPCG_LIBS    = $(SVLIBFLAG)_simvascular_thirdparty_nspcg$(LIBLINKEXT)
endif

# -----------------------------------------
# ***  Optional Open Source Packages    ***
# ***  (not free for commercial use)    ***
# -----------------------------------------

ifeq ($(SV_USE_TETGEN),1)
  TETGEN150       = 1
  GLOBAL_DEFINES += -DTETLIBRARY
  THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/tetgen
  TETGEN_TOP = $(TOP)/../Code/ThirdParty/tetgen
  TETGEN_INCDIR  = -I $(TETGEN_TOP)
  TETGEN_LIBS    = $(SVLIBFLAG)_simvascular_thirdparty_tetgen$(LIBLINKEXT)
endif

# ----
# svLS
# ----

ifeq ($(SV_USE_DUMMY_SVLS),1)
    SVLS_DEFS   = 
    SVLS_INCDIR = -I ../svLS
    SVLS_LIBS   = $(SVLIBFLAG)_simvascular_dummy_svLS$(LIBLINKEXT)
endif

ifeq ($(SV_USE_SOURCE_CODE_SVLS),1)
    SVLS_DEFS   = 
    SVLS_INCDIR = -I ../svLS
    SVLS_LIBS   = $(SVLIBFLAG)_simvascular_svLS_$(MPI_NAME)$(LIBLINKEXT)
endif

# -----
# Metis
# -----

ifeq ($(SV_USE_METIS),1)
  THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/metis
  METIS_TOP = $(TOP)/../Code/ThirdParty/metis
  METIS_INCDIR  = -I $(METIS_TOP)
  METIS_LIBS    = $(SVLIBFLAG)_simvascular_thirdparty_metis$(LIBLINKEXT)
endif


#
# ThirdParty software included from /sv_extern
#

# ---------------------------------------
# ***  Required Open Source Packages  ***
# ***  (no commercial restrictions)   ***
# ---------------------------------------

# ------------------
# Tcl/Tk & Tkcximage
# ------------------

ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/tcltk-8.6.4.x64_cygwin.mk
endif

ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/tcltk-8.6.4.x64_linux.mk
endif

ifeq ($(CLUSTER), x64_macosx)
	ifeq ($(SV_USE_SYSTEM_TCLTK),0)
	  include $(TOP)/MakeHelpers/tcltk-8.6.4.x64_macosx.mk
	endif
endif

# ---------------------
# Visualization toolkit
# ---------------------

ifeq ($(SV_USE_VTK),1)

ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/vtk-6.2.0.x64_cygwin.mk
endif

ifeq ($(CLUSTER), x64_linux)
	ifeq ($(SV_USE_PYTHON),1)
	  include $(TOP)/MakeHelpers/vtk-6.2.0.x64_linux.mk
        else
	  include $(TOP)/MakeHelpers/vtk-6.2.0.x64_linux.mk
        endif
endif

ifeq ($(CLUSTER), x64_macosx)
	ifeq ($(SV_USE_PYTHON),1)
	  include $(TOP)/MakeHelpers/vtk-6.2.0.x64_macosx.mk
        else
	  include $(TOP)/MakeHelpers/vtk-6.2.0.x64_macosx.mk
	endif
endif

endif

# -----------------------------------------
# ***  Optional Open Source Packages    ***
# ***   (less restrictive licenses)     ***
# *** (e.g. MIT or BSD or Apache 2.0)   ***
# -----------------------------------------

# ----
# GDCM
# ----

ifeq ($(SV_USE_GDCM),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/gdcm-2.6.1.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/gdcm-2.6.1.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/gdcm-2.6.1.x64_macosx.mk
  endif

endif

# ---------------
# Insight ToolKit
# ---------------

ifeq ($(SV_USE_ITK),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/itk-4.7.1.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/itk-4.7.1.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/itk-4.7.1.x64_macosx.mk
  endif

endif

# ----
# MITK
# ----

ifeq ($(SV_USE_MITK),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/mitk-2016.03.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/mitk-2016.03.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/mitk-2016.03.x64_macosx.mk
  endif

endif

# -----
# MPI
# -----

ifeq ($(SV_USE_MPI),1)

ifeq ($(SV_USE_DUMMY_MPI),1)

  MPI_NAME      = nompi
  MPI_TOP       = ../dummyMPI
  MPI_INCDIR    = -I $(MPI_TOP)
  MPI_LIBS      = $(LIBFLAG)_simvascular_dummy_mpi$(LIBLINKEXT)
  MPI_SO_PATH   = 
  MPIEXEC_PATH  = 
  MPIEXEC       =

else

  MPI_NAME ?= mpi

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/msmpi.x64_cygwin.mk
  endif

  # on linux, use the OS installed version of mpich2
  ifeq ($(CLUSTER), x64_linux)
    ifeq ($(SV_USE_OPENMPI),1)
      include $(TOP)/MakeHelpers/openmpi.x64_linux.mk
    endif
    ifeq ($(SV_USE_MPICH),1)
      include $(TOP)/MakeHelpers/mpich.x64_linux.mk
    endif
  endif

  ifeq ($(CLUSTER), x64_macosx)
    ifeq ($(SV_USE_OPENMPI),1)
      include $(TOP)/MakeHelpers/openmpi.x64_macosx.mk
    endif
    ifeq ($(SV_USE_MPICH),1)
      include $(TOP)/MakeHelpers/mpich.x64_macosx.mk
    endif
  endif

endif

endif

# -----------------------------------------
# ***  Optional Open Source Packages    ***
# ***           (GPL code)              ***
# -----------------------------------------

# ------------
# Open Cascade
# ------------

ifeq ($(SV_USE_OPENCASCADE),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/opencascade-7.0.0.x64_cygwin.mk
         OPENCASCADE_DEFS = -DWNT
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/opencascade-7.0.0.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/opencascade-7.0.0.x64_macosx.mk
  endif

endif

# ----
# glib
# ----

ifeq ($(SV_USE_GLIB),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/glib-2.36.4.x64_cygwin.mk
  endif

###  ifeq ($(CLUSTER), x64_linux)
###	include $(TOP)/MakeHelpers/glib-??????.x64_linux.mk
###  endif

endif

# ---
# gts
# ---

ifeq ($(SV_USE_GTS),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/gts-2010.03.21.x64_cygwin.mk
  endif

###  ifeq ($(CLUSTER), x64_linux)
###	include $(TOP)/MakeHelpers/gts-2010.03.21.x64_linux.mk
###  endif

endif

# ------------------
# Python
# ------------------

ifeq ($(SV_USE_PYTHON),1)
  ifeq ($(CLUSTER), x64_cygwin)
	  include $(TOP)/MakeHelpers/python-2.7.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	  include $(TOP)/MakeHelpers/python-2.7.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/python-2.7.x64_macosx.mk
  endif
endif

# --------
# Freetype
# --------

ifeq ($(SV_USE_FREETYPE),1)
  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/freetype-2.6.3.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/freetype-2.6.3.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/freetype-2.6.3.x64_macosx.mk
  endif
endif

# ---
# MMG
# ---

ifeq ($(SV_USE_MMG),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/mmg-5.1.0.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/mmg-5.1.0.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/mmg-5.1.0.x64_macosx.mk
  endif

endif

# -----------------------------------------
# ***  Optional Open Source Packages    ***
# ***  (not free for commercial use)    ***
# -----------------------------------------

# ------------------
# Qt
# ------------------

ifeq ($(SV_USE_QT),1)
  ifeq ($(CLUSTER), x64_cygwin)
	  include $(TOP)/MakeHelpers/qt-5.4.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	  include $(TOP)/MakeHelpers/qt-5.4.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	  include $(TOP)/MakeHelpers/qt-5.4.x64_macosx.mk
  endif
endif

# --------------------------------------
# ***  Optional Commercial Packages  ***
# --------------------------------------

# ---------
# Parasolid
# ---------

ifeq ($(SV_USE_PARASOLID),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/parasolid-26.1.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/parasolid-26.1.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/parasolid-26.1.x64_macosx.mk
  endif

endif

# -------
# MeshSim
# -------

ifeq ($(SV_USE_MESHSIM),1)

  SIM_LICENSE_FILE = Licenses/MeshSim/license.dat

  ifeq ($(SV_USE_PARASOLID),1)
    MESHSIM_MODELER=parasolid
  endif

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/meshsim-9.0-151017-vs12.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/meshsim-9.0-150704.x64_linux.mk
  endif

  #No meshsim for mac osx

endif

# ------
# LesLib
# ------

ifeq ($(SV_USE_BINARY_LESLIB),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/leslib-1.5.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/leslib-1.5.x64_linux.mk
  endif

  #No leslib for mac osx

endif

ifeq ($(SV_USE_DUMMY_LESLIB),1)

  LESLIB_INCDIR = 
  LESLIB_LIBS   = 

  ifeq ($(CLUSTER), x64_cygwin)
    LESLIB_DEFS   = -DACUSIM_NT -DACUSIM_WIN -DACUSIM_WIN64
    LESLIB_LIBS   = 
  endif

  ifeq ($(CLUSTER), x64_linux)
    LESLIB_DEFS   = -DACUSIM_LINUX
  endif

  ifeq ($(CLUSTER), x64_macosx)
    LESLIB_DEFS   = -DACUSIM_LINUX
  endif

endif

# here's your chance to override package locations
# ------------------------------------------------

ifeq ($(LOCAL_DIR_PKG_OVERRIDES),1)
-include pkg_overrides.mk
else
-include $(TOP)/pkg_overrides.mk
endif

# --------------------------------
# define rules for file extensions
# --------------------------------

ifeq ($(CLUSTER), x64_cygwin)
	  include $(TOP)/MakeHelpers/rules.x64_cygwin.mk
endif

ifeq ($(CLUSTER), x64_linux)
	  include $(TOP)/MakeHelpers/rules.x64_linux.mk
endif

ifeq ($(CLUSTER), x64_macosx)
	  include $(TOP)/MakeHelpers/rules.x64_macosx.mk
endif
