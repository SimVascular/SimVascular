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
# CXX_COMPILER_VERSION = { icpc, vs10.1, msvc-12.5, mingw-gcc, gcc}
# FORTRAN_COMPILER_VERSION = { ifort, mingw-gfortran, gfortran }
# ---------------------------------------------------------------------

CXX_COMPILER_VERSION = msvc-12.5
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

# --------
# SolverIO
# --------

SV_USE_SOLVERIO = 1

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
SV_USE_QT_GUI = 1
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
SV_IGNORE_PROVISIONING_FILE = 1

# -----------------------------------------------------
# Compile with Optimization
# -----------------------------------------------------

MAKE_OPTIMIZED = 1
LINK_WITH_DEBUG = 1

# -----------------------------------------------------
# Static link
# -----------------------------------------------------

#SV_STATIC_BUILD = 1

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
  SVEXTERN_COMPILER_VERSION = gnu-4.8
endif
ifeq ($(CLUSTER), x64_macosx)
  SVEXTERN_COMPILER_VERSION = clang-7.0
endif

ifeq ($(CLUSTER), x64_cygwin)
    OPEN_SOFTWARE_BINARIES_TOPLEVEL = C:/cygwin64/usr/local/sv/ext/bin/$(SVEXTERN_COMPILER_VERSION)/x64
    OPEN_SOFTWARE_BUILDS_TOPLEVEL   = C:/sv/build/$(SVEXTERN_COMPILER_VERSION)/x64
    OPEN_SOFTWARE_SOURCES_TOPLEVEL  = C:/cygwin64/usr/local/sv/ext/src
    LICENSED_SOFTWARE_TOPLEVEL      = C:/cygwin64/usr/local/sv/licensed
endif

ifeq ($(CLUSTER), x64_linux)
    OPEN_SOFTWARE_BINARIES_TOPLEVEL = /usr/local/sv/ext/bin/$(SVEXTERN_COMPILER_VERSION)/x64
    OPEN_SOFTWARE_BUILDS_TOPLEVEL   = /usr/local/sv/ext/build/$(SVEXTERN_COMPILER_VERSION)/x64
    OPEN_SOFTWARE_SOURCES_TOPLEVEL  = /usr/local/sv/ext/src
    LICENSED_SOFTWARE_TOPLEVEL      = /usr/local/sv/licensed
endif

ifeq ($(CLUSTER), x64_macosx)
    OPEN_SOFTWARE_BINARIES_TOPLEVEL = /usr/local/sv/ext/bin/$(SVEXTERN_COMPILER_VERSION)/x64
    OPEN_SOFTWARE_BUILDS_TOPLEVEL   = /usr/local/sv/ext/build/$(SVEXTERN_COMPILER_VERSION)/x64
    OPEN_SOFTWARE_SOURCES_TOPLEVEL  = /usr/local/sv/ext/src
    LICENSED_SOFTWARE_TOPLEVEL      = /usr/local/sv/licensed
endif

# -------------------------------------------
#   Release version numbers for SimVascular
# -------------------------------------------

SV_MAJOR_VER_NO = "2.16"
SV_FULL_VER_NO = "2.16.0918"
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
    GLOBAL_DEFINES += -DSV_USE_OpenCASCADE
endif
ifeq ($(SV_USE_OPENCASCADE_SHARED),1)
    GLOBAL_DEFINES += -DSV_USE_OpenCASCADE_SHARED
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
    GLOBAL_DEFINES += -DSV_USE_TETGEN_ADAPTOR
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

ifeq ($(SV_IGNORE_PROVISIONING_FILE),1)
  GLOBAL_DEFINES += -DSV_IGNORE_PROVISIONING_FILE
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
  ifeq ($(CXX_COMPILER_VERSION), msvc-12.5)
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

EXECDIRS = ../Code/Source/UI

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
# Qt & MITK
# -------------------------

# for now, combine mitk code qt gui code
ifeq ($(SV_USE_MITK),1)
  ifeq ($(SV_USE_SHARED),1)
     SHARED_LIBDIRS += ../Code/Source/Modules
  else
     LIBDIRS += ../Code/Source/Modules
  endif
endif

ifeq ($(SV_USE_QT_GUI),1)
  ifeq ($(SV_USE_QT_GUI_SHARED),1)
     SHARED_LIBDIRS += ../Code/Source/Plugins
  else
     LIBDIRS += ../Code/Source/Plugins
  endif
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
     LOCAL_INCDIR += -I$(TOP)/../Code/Source/Plugins/org.sv.projectdatanodes/src/internal \
                     -I$(TOP)/../Code/Source/Plugins/org.sv.gui.qt.projectmanager/src/internal \
                     -I$(TOP)/../Code/Source/Plugins/org.sv.gui.qt.meshing/src/internal \
                     -I$(TOP)/../Code/Source/Plugins/org.sv.gui.qt.modeling/src/internal \
                     -I$(TOP)/../Code/Source/Plugins/org.sv.gui.qt.pathplanning/src/internal \
                     -I$(TOP)/../Code/Source/Plugins/org.sv.gui.qt.segmentation/src/internal \
                     -I$(TOP)/../Code/Source/Plugins/org.sv.gui.qt.simulation/src/internal \
                     -I$(TOP)/../Code/Source/Plugins/org.sv.gui.qt.application/src/internal \
                     -I$(TOP)/../Code/Source/Modules/Common \
                     -I$(TOP)/../Code/Source/Modules/Model \
                     -I$(TOP)/../Code/Source/Modules/Mesh \
                     -I$(TOP)/../Code/Source/Modules/Path \
                     -I$(TOP)/../Code/Source/Modules/ProjectManagement \
                     -I$(TOP)/../Code/Source/Modules/QtWidgets \
                     -I$(TOP)/../Code/Source/Modules/Segmentation \
                     -I$(TOP)/../Code/Source/Modules/Simulation
endif

# Link flags, which also need to be dealt with conditionally depending
# on which concrete classes derived from SolidModel are being
# included.
# -----

# include path to find libs when linking
ifeq ($(CLUSTER), x64_cygwin)
  TOP_WINDOWS_STYLE=$(shell cygpath -m $(TOP))
  GLOBAL_LFLAGS 	 += $(LIBPATH_COMPILER_FLAG)$(TOP_WINDOWS_STYLE)/Lib/$(LIB_BUILD_DIR)
else
  GLOBAL_LFLAGS 	 += $(LIBPATH_COMPILER_FLAG)$(TOP)/Lib/$(LIB_BUILD_DIR)
endif

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

# ----
# zlib
# ----

ifeq ($(SV_USE_ZLIB),1)
  THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/zlib
  ZLIB_TOP = $(TOP)/../Code/ThirdParty/zlib
  ZLIB_INCDIR  = -I $(ZLIB_TOP)
  ZLIB_LIBS    = $(SVLIBFLAG)_simvascular_thirdparty_zlib$(LIBLINKEXT)
endif

# --------
# SolverIO
# --------

ifeq ($(SV_USE_SOLVERIO),1)
  THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/SolverIO
  SOLVERIO_TOP = $(TOP)/../Code/ThirdParty/SolverIO
  SOLVERIO_INCDIR  = -I $(SOLVERIO_TOP)
  SOLVERIO_LIB     = $(SVLIBFLAG)_simvascular_thirdparty_solverio$(LIBLINKEXT)
endif

# -----------------------------------------
# ***  Optional Open Source Packages    ***
# ***           (LGPL code)             ***
# -----------------------------------------

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

#
# ThirdParty software included from externals
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
