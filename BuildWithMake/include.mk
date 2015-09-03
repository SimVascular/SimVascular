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
#   - specify inclusion of { Discrete, Parasolid, Irit } via MAKE_WITH_*
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
# CXX_COMPILER_VERSION = { vs10.1, vs12.5, mingw-gcc, gcc}
# FORTRAN_COMPILER_VERSION = { ifort, mingw-gfortran, gfortran }
# ---------------------------------------------------------------------

CXX_COMPILER_VERSION = vs12.5
FORTRAN_COMPILER_VERSION = ifort

# ---------------------------------------------------------------------
# COMPILER_VERSION = { intel, gnu }
# for compilation on linux cluster with intel or gun compiler
# ---------------------------------------------------------------------

COMPILER_VERSION = intel

ifeq ($(LOCAL_DIR_CLUSTER_OVERRIDES),1)
-include cluster_overrides.mk
else
-include $(TOP)/cluster_overrides.mk
endif

# ---------------------------------------
# Control solid modeling kernel inclusion
# ---------------------------------------
# Note that Geodesic allows for multiple kernels to be built into a
# single executable.  In order to include a kernel, uncomment its line
# in the following short section.  In order to exclude a kernel,
# set the value to something other than 1 in global_overrides.mk or 
# via the make command line.

MAKE_WITH_PARASOLID = 0
MAKE_WITH_PARASOLID_SHARED = 1

# You can also exclude the SolidModel module entirely.  Be aware that
# this leads to the exclusion of certain LevelSet functionality as
# well.

EXCLUDE_SOLID_MODEL = 0


# --------------------------------------
# Control inclusion of meshSim functions
# --------------------------------------

MAKE_WITH_MESHSIM = 0
MAKE_WITH_MESHSIM_DISCRETE_MODEL = 0
MAKE_WITH_MESHSIM_DISCRETE_MODEL_SHARED = 1
MAKE_WITH_MESHSIM_ADAPTOR = 0
MAKE_WITH_MESHSIM_SHARED = 1
MESHSIM_USE_LICENSE_FILE = 1
MESHSIM_EMBED_LICENSE_KEYS = 0
MESHSIM_USE_SIMVASCULAR_USE_WIN32_REGISTRY = 0

# -------------------------------------
# Control inclusion of tetgen functions
# -------------------------------------

MAKE_WITH_TETGEN = 1
MAKE_WITH_TETGEN_ADAPTOR = 1

# ----------------------------------------------
# Control inclusion of leslib
# {binary and dummy} are mutually exclusive opts
# ----------------------------------------------

MAKE_WITH_BINARY_LESLIB = 0
MAKE_WITH_DUMMY_LESLIB = 1

# --------------------------------------------------------
# Control inclusion of svLS
# {binary, dummy, source code} are mutually exclusive opts
# --------------------------------------------------------

MAKE_WITH_DUMMY_SVLS = 0
MAKE_WITH_SOURCE_CODE_SVLS = 1

# -----------------------------------------------------
# Compile with zlib
# -----------------------------------------------------

MAKE_WITH_ZLIB = 1

# -----------------------------------------------------
# Compile with 3-D Solver and Related Programs
# -----------------------------------------------------

MAKE_WITH_SOLVERIO = 1
MAKE_WITH_THREEDSOLVER = 1
MAKE_WITH_PRESOLVER = 1
MAKE_WITH_POSTSOLVER = 1

# -----------------------------------------------------
# Compile Flowsolver Modules
# -----------------------------------------------------

FLOWSOLVER_VERSION_CORONARY_ACTIVATE = 1
FLOWSOLVER_VERSION_CLOSEDLOOP_ACTIVATE = 1
FLOWSOLVER_VERSION_VARWALL_ACTIVATE = 1
FLOWSOLVER_VERSION_USE_VTK_ACTIVATE = 1

# -----------------------------------------------------
# Compile with MPICH2
# -----------------------------------------------------

MAKE_WITH_MPICH2 = 1

# -----------------------------------------------------
# Build only the 3D Solver
# -----------------------------------------------------

EXCLUDE_ALL_BUT_THREEDSOLVER = 0

# -----------------------------------------------------
# Compile with VTK
# -----------------------------------------------------

MAKE_WITH_VTK = 1

# -----------------------------------------------------
# Compile with ITK
# -----------------------------------------------------

MAKE_WITH_ITK = 1

# -----------------------------------------------------
# Compile with VMTK
# -----------------------------------------------------

MAKE_WITH_VMTK = 1

# -----------------------------------------------------
# Compile with glib & gts
# -----------------------------------------------------

MAKE_WITH_GLIB = 0
MAKE_WITH_GTS  = 0

# -----------------------------------------------------
# Compile with sparse, metis, nspcg
# -----------------------------------------------------

MAKE_WITH_SPARSE = 1
MAKE_WITH_METIS = 1
MAKE_WITH_NSPCG = 1

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

# by default don't build most third party
# if we are only building the flow solver
ifeq ($(EXCLUDE_ALL_BUT_THREEDSOLVER), 1)
    ifeq ($(FLOWSOLVER_VERSION_USE_VTK_ACTIVATE), 0)
        MAKE_WITH_VTK = 0
    endif

    MAKE_WITH_ITK = 0
    MAKE_WITH_VMTK = 0
    MAKE_WITH_TETGEN = 0
    MAKE_WITH_SPARSE = 0
    MAKE_WITH_NSPCG = 0
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

ifeq ($(CLUSTER), x64_cygwin)
    OPEN_SOFTWARE_BINARIES_TOPLEVEL = C:/cygwin64/SV15/bin/$(SVEXTERN_COMPILER_VERSION)/x64
    OPEN_SOFTWARE_SOURCES_TOPLEVEL = C:/cygwin64/SV15/src
    LICENSED_SOFTWARE_TOPLEVEL = C:/cygwin64/SV15/licensed
endif

ifeq ($(CLUSTER), x64_linux)
    OPEN_SOFTWARE_BINARIES_TOPLEVEL = /SV15/bin/$(SVEXTERN_COMPILER_VERSION)/x64
    OPEN_SOFTWARE_SOURCES_TOPLEVEL  = /SV15/src
    LICENSED_SOFTWARE_TOPLEVEL      = /SV15/licensed
endif

# -------------------------------------------
#   Release version numbers for SimVascular 
# -------------------------------------------

SIMVASCULAR_MAJOR_VER_NO = "2.0"
SIMVASCULAR_FULL_VER_NO = "2.0.20"
SIMVASCULAR_USE_WIN32_REGISTRY=0
SIMVASCULAR_REGISTRY_TOPLEVEL=SIMVASCULAR

# if you need to override anything above, stuff it in global_overrides.mk
# -----------------------------------------------------------------------

ifeq ($(LOCAL_DIR_GLOBAL_OVERRIDES),1)
-include global_overrides.mk
else
-include $(TOP)/global_overrides.mk
endif

ifeq ($(CLUSTER),x86_cygwin) 
  SIMVASCULAR_VERSION  = simvascular32
  SIMVASCULAR_PLATFORM = x86
  SIMVASCULAR_POSTFIX=32
  SIMVASCULAR_OS=windows
endif
ifeq ($(CLUSTER),x64_cygwin) 
  SIMVASCULAR_VERSION  = simvascular
  SIMVASCULAR_PLATFORM = x64
  SIMVASCULAR_POSTFIX=
  SIMVASCULAR_OS=windows
endif
ifeq ($(CLUSTER),x64_linux) 
  SIMVASCULAR_VERSION  = simvascular
  SIMVASCULAR_PLATFORM = x64
  SIMVASCULAR_POSTFIX=
  SIMVASCULAR_OS=linux
endif

# --------------
# Global defines
# --------------

GLOBAL_DEFINES = -DSIMVASCULAR_VERSION=\"$(SIMVASCULAR_VERSION)\" -DSIMVASCULAR_MAJOR_VER_NO=\"$(SIMVASCULAR_MAJOR_VER_NO)\" -DSIMVASCULAR_FULL_VER_NO=\"$(SIMVASCULAR_FULL_VER_NO)\" -DSIMVASCULAR_REGISTRY_TOPLEVEL=\"$(SIMVASCULAR_REGISTRY_TOPLEVEL)\"

ifeq ($(SIMVASCULAR_USE_WIN32_REGISTRY), 1)
  GLOBAL_DEFINES += -DSIMVASCULAR_USE_WIN32_REGISTRY
endif

ifeq ($(MAKE_STATIC_BUILD),1)
  GLOBAL_DEFINES += -DCV_STATIC_LINK -DSIMVASCULAR_STATIC_BUILD
endif

ifeq ($(CLUSTER), x64_cygwin)
   GLOBAL_DEFINES += -DUSE_NOTIMER -DWINDOWS -DWIN32
endif

ifeq ($(CLUSTER), x64_linux)
   GLOBAL_DEFINES += -DUSE_NOTIMER -DUNIX
endif


ifeq ($(EXCLUDE_SOLID_MODEL),0)
    ifeq ($(MAKE_WITH_PARASOLID),1)
        GLOBAL_DEFINES += -DUSE_PARASOLID
    endif
    ifeq ($(MAKE_WITH_PARASOLID_SHARED),1)
        GLOBAL_DEFINES += -DUSE_PARASOLID_SHARED
    endif
else
    GLOBAL_DEFINES += -DEXCLUDE_SOLID_MODEL
endif

ifeq ($(MAKE_WITH_MESHSIM),1) 
  GLOBAL_DEFINES += -DUSE_MESHSIM
  ifeq ($(MAKE_WITH_MESHSIM_SHARED),1) 
    GLOBAL_DEFINES += -DUSE_MESHSIM_SHARED
  endif
  ifeq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL),1)
    GLOBAL_DEFINES += -DUSE_DISCRETE_MODEL
  endif
  ifeq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL_SHARED),1)
    GLOBAL_DEFINES += -DUSE_DISCRETE_MODEL_SHARED
  endif
  ifeq ($(SIMVASCULAR_USE_WIN32_REGISTRY),1)
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

ifeq ($(MAKE_WITH_TETGEN),1) 
  GLOBAL_DEFINES += -DUSE_TETGEN
endif

ifeq ($(MAKE_WITH_ZLIB),1)
  GLOBAL_DEFINES += -DUSE_ZLIB
endif

ifeq ($(MAKE_WITH_ITK),1)
  GLOBAL_DEFINES += -DUSE_ITK
endif

ifeq ($(MAKE_WITH_VMTK),1)
  GLOBAL_DEFINES += -DUSE_VMTK
endif

ifeq ($(MAKE_WITH_GTS),1)
  GLOBAL_DEFINES += -DUSE_GTS -DNATIVE_WIN32
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
        GLOBAL_DEFINES += -DCV_WRAP_FORTRAN_IN_CAPS_NO_UNDERSCORE
  endif
  ifeq ($(CXX_COMPILER_VERSION), mingw-gcc)
	include $(TOP)/MakeHelpers/compiler.mingw-gcc.x64_cygwin.mk
  endif
  ifeq ($(FORTRAN_COMPILER_VERSION), mingw-gfortran)
	include $(TOP)/MakeHelpers/compiler.mingw-gfortran.x64_cygwin.mk
        GLOBAL_DEFINES += -DCV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE
  endif
endif

# Use COMPILER_VERSION for compilation on linux clusters
ifeq ($(CLUSTER), x64_linux)
	ifeq ($(COMPILER_VERSION), intel)
      include $(TOP)/MakeHelpers/compiler.intel.x64_linux.mk
	endif
	ifeq ($(COMPILER_VERSION), gnu)
	  include $(TOP)/MakeHelpers/compiler.gnu.x64_linux.mk
	endif
        GLOBAL_DEFINES += -DCV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE
endif

# -----------------------
# Intel Compiler Runtimes
# -----------------------

ifeq ($(CLUSTER), x64_cygwin)
    INTEL_COMPILER_SO_PATH  = $(LICENSED_SOFTWARE_TOPLEVEL)/intel_compiler_runtime_libs/2013.1.117/win/intel64
endif

ifeq ($(CLUSTER), x64_linux)
    INTEL_COMPILER_SO_PATH  = $(LICENSED_SOFTWARE_TOPLEVEL)/intel_compiler_runtime_libs/2013.1.117/linux/intel64
endif

# --------------------------------
# define rules for file extensions
# --------------------------------

ifeq ($(CLUSTER), x86_cygwin)
	  include $(TOP)/MakeHelpers/rules.x86_cygwin.mk
endif

ifeq ($(CLUSTER), x64_cygwin)
	  include $(TOP)/MakeHelpers/rules.x64_cygwin.mk
endif

ifeq ($(CLUSTER), x64_linux)
	  include $(TOP)/MakeHelpers/rules.x64_linux.mk
endif

# ---------------------
# Local lib directories
# ---------------------

SHARED_LIBDIRS =

LIBDIRS = ../Code/Source/Common/Globals \
	  ../Code/Source/Common/Utils \
	  ../Code/Source/Common/Repository \
	  ../Code/Source/Common/Geometry \
	  ../Code/Source/ImageProcessing \
	  ../Code/Source/PostProcessing \
	  ../Code/Source/Mesh/MeshObject \
	  ../Code/Source/Model/SolidModel \
	  ../Code/Source/Legacy/LevelSet \
	  ../Code/Source/Model/PolyDataSolidModel

ifeq ($(MAKE_WITH_ITK),1)
     LIBDIRS += ../Code/Source/Segmentation/ITK ../Code/Source/Segmentation/ITK/Util
endif

ifeq ($(MAKE_WITH_TETGEN),1)
     LIBDIRS += ../Code/Source/Mesh/TetGenMeshObject
endif

ifeq ($(MAKE_WITH_VMTK),1)
     LIBDIRS += ../Code/ThirdParty/vmtk
endif

#  solid modeling

ifeq ($(EXCLUDE_SOLID_MODEL),0)

    ifeq ($(MAKE_WITH_PARASOLID),1)
      ifeq ($(MAKE_WITH_PARASOLID_SHARED),1)
        SHARED_LIBDIRS += ../Code/Licensed/ParasolidSolidModel
      else
        LIBDIRS += ../Code/Licensed/ParasolidSolidModel
      endif
    endif

    ifeq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL),1)
      ifeq ($(MAKE_WITH_MESHSIM_SHARED),1)
        SHARED_LIBDIRS += ../Code/Source/Model/MeshSimDiscreteSolidModel
      else
        LIBDIRS += ../Code/Source/Model/MeshSimDiscreteSolidModel
      endif
    endif

endif

# meshing

ifeq ($(MAKE_WITH_MESHSIM),1)
  ifeq ($(MAKE_WITH_MESHSIM_SHARED),1)
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

#
#  override other options to build solver only!
#

ifeq ($(EXCLUDE_ALL_BUT_THREEDSOLVER),0)
  LIBDIRS += ../Code/FlowSolvers/ThreeDSolver
  SOLVERIO_INCDIR = -I $(TOP)/../Code/FlowSolvers/ThreeDSolver/SolverIO
  THREEDSOLVER_INCDIR = -I $(TOP)/../Code/FlowSolvers/ThreeDSolver
  # must build solver before adaptor because of SolverIO
  ifeq ($(MAKE_WITH_MESHSIM_ADAPTOR),1)
     EXECDIRS += ../Code/Source/Mesh/MeshSimAdapt
  endif
  ifeq ($(MAKE_WITH_TETGEN_ADAPTOR),1)
     EXECDIRS += ../Code/Source/Mesh/TetGenAdapt
  endif
else
  ifeq ($(MAKE_WITH_THREEDSOLVER),1)
     LIBDIRS = ../Code/FlowSolvers/ThreeDSolver
     SOLVERIO_INCDIR = -I $(TOP)/../Code/FlowSolvers/ThreeDSolver/SolverIO
     THREEDSOLVER_INCDIR = -I $(TOP)/../Code/FlowSolvers/ThreeDSolver
  else
     LIBDIRS = 		
  endif
endif

SUBDIRS         = $(LIBDIRS) $(EXECDIRS)

# -------------------------
# Local include directories
# -------------------------

LOCAL_SUBDIRS   = $(LIBDIRS) $(SHARED_LIBDIRS) ../Code/Source/Include
LOCAL_INCDIR    := $(foreach i, ${LOCAL_SUBDIRS}, -I$(TOP)/$(i))
LOCAL_LIBDIR	=  -L$(TOP)/Lib
LOCAL_LIBS	=  $(LOCAL_LIBDIR) -lsimvascular_utils

ifeq ($(MAKE_WITH_ITK),1)
     LOCAL_INCDIR += -I$(TOP)/../Code/Source/Segmentation/ITK/Include
endif

# Link flags, which also need to be dealt with conditionally depending
# on which concrete classes derived from SolidModel are being
# included.
# -----

ifeq ($(CLUSTER),x64_cygwin)
	LFLAGS 	 = $(GLOBAL_LFLAGS) \
	           $(TCLTK_LIBS) \
                   $(LIBPATH_COMPILER_FLAG)$(TOP)/Lib
endif

ifeq ($(CLUSTER),x64_linux)
	LFLAGS 	 = $(GLOBAL_LFLAGS) \
	           $(TCLTK_LIBS) \
	           -L $(TOP)/Lib
endif

ifeq ($(CLUSTER),x64_cygwin)
LFLAGS     += $(LIBFLAG)lib_lib_simvascular_lset$(LIBLINKEXT) \
              $(LIBFLAG)lib_lib_simvascular_image$(LIBLINKEXT) \
              $(LIBFLAG)lib_lib_simvascular_mesh$(LIBLINKEXT) \
              $(LIBFLAG)lib_lib_simvascular_solid$(LIBLINKEXT) \
              $(LIBFLAG)lib_lib_simvascular_sysgeom$(LIBLINKEXT) \
              $(LIBFLAG)lib_lib_simvascular_repository$(LIBLINKEXT) \
              $(LIBFLAG)lib_lib_simvascular_utils$(LIBLINKEXT) \
              $(LIBFLAG)lib_lib_simvascular_post$(LIBLINKEXT) \
              $(LIBFLAG)lib_lib_simvascular_polydatasolid$(LIBLINKEXT) \
              $(LIBFLAG)lib_lib_simvascular_globals$(LIBLINKEXT)
endif

ifeq ($(CLUSTER),x64_linux)
LFLAGS     += -l_lib_simvascular_lset \
              -l_lib_simvascular_image \
              -l_lib_simvascular_solid \
              -l_lib_simvascular_sysgeom \
              -l_lib_simvascular_repository \
              -l_lib_simvascular_utils \
              -l_lib_simvascular_post \
              -l_lib_simvascular_solid \
              -l_lib_simvascular_globals \
              -l_lib_simvascular_polydatasolid \
              -l_lib_simvascular_utils
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

# ------
# Sparse
# ------

ifeq ($(MAKE_WITH_SPARSE),1)
  THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/sparse
  SPARSE_TOP = $(TOP)/../Code/ThirdParty/sparse
  SPARSE_INCDIR  = -I $(SPARSE_TOP)
  ifeq ($(CLUSTER),x64_cygwin)
    SPARSE_LIBS    = $(LIBPATH_COMPILER_FLAG)$(TOP)/Lib $(LIBFLAG)lib_lib_simvascular_sparse$(LIBLINKEXT)
  endif
  ifeq ($(CLUSTER),x64_linux)
    SPARSE_LIBS    = -L $(TOP)/Lib -l_lib_simvascular_sparse
  endif
endif

# ----
# zlib
# ----

ifeq ($(MAKE_WITH_ZLIB),1)
  THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/zlib
  ZLIB_TOP = $(TOP)/../Code/ThirdParty/zlib
  ZLIB_INCDIR  = -I $(ZLIB_TOP)
  ifeq ($(CLUSTER),x64_cygwin)
    ZLIB_LIBS    = $(LIBPATH_COMPILER_FLAG)$(TOP)/Lib $(LIBFLAG)lib_lib_simvascular_zlib$(LIBLINKEXT)
  endif
  ifeq ($(CLUSTER),x64_linux)
    ZLIB_LIBS    = -L $(TOP)/Lib -l_lib_simvascular_zlib
  endif
endif

# -----------------------------------------
# ***  Optional Open Source Packages    ***
# ***           (LGPL code)             ***
# -----------------------------------------

# ------
# NSPCG
# ------

ifeq ($(MAKE_WITH_NSPCG),1)
  THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/nspcg
  NSPCG_TOP = $(TOP)/../Code/ThirdParty/nspcg
  NSPCG_INCDIR  = -I $(NSPCG_TOP)
  ifeq ($(CLUSTER),x64_cygwin)
    NSPCG_LIBS    = $(LIBPATH_COMPILER_FLAG)$(TOP)/Lib $(LIBFLAG)lib_lib_simvascular_nspcg$(LIBLINKEXT)
  endif
  ifeq ($(CLUSTER),x64_linux)
    NSPCG_LIBS    = -L $(TOP)/Lib -l_lib_simvascular_nspcg
  endif
endif

# -----------------------------------------
# ***  Optional Open Source Packages    ***
# ***  (not free for commercial use)    ***
# -----------------------------------------

ifeq ($(MAKE_WITH_TETGEN),1)
  TETGEN150       = 1
  GLOBAL_DEFINES += -DTETLIBRARY
  THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/tetgen
  TETGEN_TOP = $(TOP)/../Code/ThirdParty/tetgen
  TETGEN_INCDIR  = -I $(TETGEN_TOP)
  ifeq ($(CLUSTER),x64_cygwin)
    TETGEN_LIBS    = $(LIBPATH_COMPILER_FLAG)$(TOP)/Lib $(LIBFLAG)lib_lib_simvascular_tetgen$(LIBLINKEXT)
  endif
  ifeq ($(CLUSTER),x64_linux)
    TETGEN_LIBS    = -L $(TOP)/Lib -l_lib_simvascular_tetgen
  endif
endif

# ----
# svLS
# ----

ifeq ($(MAKE_WITH_DUMMY_SVLS),1)
    SVLS_DEFS   = 
    SVLS_INCDIR = -I ../svLS
    SVLS_LIBS   = $(TOP)/Lib/lib_lib_simvascular_dummy_svLS.$(STATICEXT)
endif

ifeq ($(MAKE_WITH_SOURCE_CODE_SVLS),1)
    SVLS_DEFS   = 
    SVLS_INCDIR = -I ../svLS
    SVLS_LIBS   = $(TOP)/Lib/lib_lib_simvascular_svLS.$(STATICEXT)
endif

# -----
# Metis
# -----

ifeq ($(MAKE_WITH_METIS),1)
  THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/metis
  METIS_TOP = $(TOP)/../Code/ThirdParty/metis
  METIS_INCDIR  = -I $(METIS_TOP)
  ifeq ($(CLUSTER),x64_cygwin)
    METIS_LIBS    = $(LIBPATH_COMPILER_FLAG)$(TOP)/Lib $(LIBFLAG)lib_lib_simvascular_metis$(LIBLINKEXT)
  endif
  ifeq ($(CLUSTER),x64_linux)
    METIS_LIBS    = -L $(TOP)/Lib -l_lib_simvascular_metis
  endif
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
	include $(TOP)/MakeHelpers/tcltk-8.5.18.x64_cygwin.mk
endif

ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/tcltk-8.5.18.x64_linux.mk
endif

# ---------------------
# Visualization toolkit
# ---------------------

ifeq ($(MAKE_WITH_VTK),1)

ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/vtk-6.2.0.x64_cygwin.mk
endif

ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/vtk-6.2.0.x64_linux.mk
endif

endif

# -----------------------------------------
# ***  Optional Open Source Packages    ***
# ***   (less restrictive licenses)     ***
# *** (e.g. MIT or BSD or Apache 2.0)   ***
# -----------------------------------------

# --------------
# Insight ToolKit
# ---------------

ifeq ($(MAKE_WITH_ITK),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/itk-4.8.0.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/itk-4.8.0.x64_linux.mk
  endif

endif

# -----
# MPI
# -----

ifeq ($(MAKE_WITH_MPICH2),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/msmpi.x64_cygwin.mk
  endif

  # on linux, use the OS installed version of mpich2
  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/mpich2.usr.x64_linux.mk
  endif

endif

# -----------------------------------------
# ***  Optional Open Source Packages    ***
# ***           (GPL code)              ***
# -----------------------------------------

# ----
# glib
# ----

ifeq ($(MAKE_WITH_GLIB),1)

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

ifeq ($(MAKE_WITH_GTS),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/gts-2010.03.21.x64_cygwin.mk
  endif

###  ifeq ($(CLUSTER), x64_linux)
###	include $(TOP)/MakeHelpers/gts-2010.03.21.x64_linux.mk
###  endif

endif

# -----------------------------------------
# ***  Optional Open Source Packages    ***
# ***  (not free for commercial use)    ***
# -----------------------------------------

# --------------------------------------
# ***  Optional Commercial Packages  ***
# --------------------------------------

# ---------
# Parasolid
# ---------

ifeq ($(MAKE_WITH_PARASOLID),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/parasolid-26.1.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/parasolid-26.1.x64_linux.mk
  endif

endif

# -------
# MeshSim
# -------

ifeq ($(MAKE_WITH_MESHSIM),1)

  SIM_LICENSE_FILE = Licenses/MeshSim/license.dat

  ifeq ($(MAKE_WITH_PARASOLID),1)
    MESHSIM_MODELER=parasolid
  endif

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/meshsim-9.0-150704-vs12.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/meshsim-9.0-150704.x64_linux.mk
  endif

endif

# ------
# LesLib
# ------

ifeq ($(MAKE_WITH_BINARY_LESLIB),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/leslib-1.5.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/leslib-1.5.x64_linux.mk
  endif

endif

ifeq ($(MAKE_WITH_DUMMY_LESLIB),1)

  LESLIB_INCDIR = 
  LESLIB_LIBS   = 

  ifeq ($(CLUSTER), x86_cygwin)
    LESLIB_DEFS   = -DACUSIM_NT -DACUSIM_WIN
    LESLIB_LIBS   = 
  endif

  ifeq ($(CLUSTER), x64_cygwin)
    LESLIB_DEFS   = -DACUSIM_NT -DACUSIM_WIN -DACUSIM_WIN64
    LESLIB_LIBS   = 
  endif

  ifeq ($(CLUSTER), x64_linux)
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
