# Hey emacs, this is a -*- makefile -*-

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
# CLUSTER = { x64_cygwin, x64_linux, x64_macosx }
# -----------------------------------------------------------

CLUSTER = x64_cygwin
#CLUSTER = x64_linux
#CLUSTER = x64_macosx

# ---------------------------------------------------------------------
# CXX_COMPILER_VERSION = { icpc, msvc-19.0, msvc-19.16, clang,
#                          mingw-gcc, gcc}
# FORTRAN_COMPILER_VERSION = { ifort, mingw-gfortran, gfortran }
# ---------------------------------------------------------------------

# NOTE: CXX_COMPILER_VERSION and FORTRAN_COMPILER_VERSION
#       should be replaced with additional variables

SV_COMPILER = msvc
#SV_COMPILER_VERSION = 19.0
#CXX_COMPILER_VERSION = msvc-19.0
SV_COMPILER_VERSION = 19.16
CXX_COMPILER_VERSION = msvc-19.16

FORTRAN_COMPILER_VERSION = ifort

# optionally override with cluster options
# -----------------------------------------------------------------------

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
# SimVascular allows for multiple kernels to be built into a
# single executable.  In order to include a kernel, uncomment its line
# in the following short section.  In order to exclude a kernel,
# set the value to something other than 1 in global_overrides.mk or
# via the make command line.

# ------------
# Open Cascade
# ------------

SV_USE_OPENCASCADE = 1
SV_USE_OPENCASCADE_SHARED = 1

# --------
# SolverIO
# --------

SV_USE_SOLVERIO = 1

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
# Compile with tcl
# -----------------------------------------------------

SV_USE_TCL = 1
SV_USE_TCL_SHARED = 1

# -----------------------------------------------------
# Compile with python interpreter
# -----------------------------------------------------

SV_USE_PYTHON = 1
SV_USE_PYTHON2 = 0
SV_USE_PYTHON3 = 1
SV_USE_PYTHON_SHARED = 1
SV_USE_SYSTEM_PYTHON = 1

# -----------------------------------------------------
# Compile with Qt
# -----------------------------------------------------

SV_USE_QT = 1
SV_EXTERNALS_PREBUILT_QT_SYSTEM_INSTALL = 0
SV_USE_SYSTEM_QT = 1
SV_USE_SV4_GUI = 1
SV_USE_SV4_GUI_SHARED = 1

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
SV_USE_VTK_SHARED = 1

# -----------------------------------------------------
# Compile with ITK (ITK uses HDF5)
# -----------------------------------------------------

SV_USE_ITK = 1
SV_USE_HDF5 = 1

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
SV_USE_MITK_SEGMENTATION = 1
SV_IGNORE_PROVISIONING_FILE = 1

# -----------------------------------------------------
# Compile with tinyxml and/or tinyxml2
# -----------------------------------------------------

SV_USE_TINYXML  = 1
SV_USE_TINYXML2 = 0

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
  SVEXTERN_COMPILER_VERSION = gnu-5.4
endif
ifeq ($(CLUSTER), x64_macosx)
  SVEXTERN_COMPILER_VERSION = clang-8.1
endif

#SV_EXTERNALS_VERSION_NUMBER = 2019.02
SV_EXTERNALS_VERSION_NUMBER = 2019.06
SV_VTK_OPENGL_VERSION=gl2

ifeq ($(CLUSTER), x64_cygwin)
    SV_LOWERCASE_CMAKE_BUILD_TYPE=release
    SV_CMAKE_BUILD_TYPE=Release
    OPEN_SOFTWARE_BINARIES_TOPLEVEL = C:/cygwin64/usr/local/sv/ext/$(SV_EXTERNALS_VERSION_NUMBER)/$(SV_LOWERCASE_CMAKE_BUILD_TYPE)/$(SV_VTK_OPENGL_VERSION)/bin/$(SV_COMPILER)/$(SV_COMPILER_VERSION)/x64
endif

ifeq ($(CLUSTER), x64_linux)
    SV_LOWERCASE_CMAKE_BUILD_TYPE=release
    SV_CMAKE_BUILD_TYPE=Release
    OPEN_SOFTWARE_BINARIES_TOPLEVEL = /usr/local/sv/ext/$(SV_EXTERNALS_VERSION_NUMBER)/$(SV_LOWERCASE_CMAKE_BUILD_TYPE)/$(SV_VTK_OPENGL_VERSION)/bin/$(SV_COMPILER)/$(SV_COMPILER_VERSION)/x64
endif

ifeq ($(CLUSTER), x64_macosx)
    SV_LOWERCASE_CMAKE_BUILD_TYPE=release
    SV_CMAKE_BUILD_TYPE=Release
    OPEN_SOFTWARE_BINARIES_TOPLEVEL = /usr/local/sv/ext/$(SV_EXTERNALS_VERSION_NUMBER)/$(SV_LOWERCASE_CMAKE_BUILD_TYPE)/$(SV_VTK_OPENGL_VERSION)/bin/$(SV_COMPILER)/$(SV_COMPILER_VERSION)/x64
endif

# -------------------------------------------
#   Release version numbers for SimVascular
# -------------------------------------------

SV_USE_WIN32_REGISTRY=1
SV_REGISTRY_TOPLEVEL=SIMVASCULAR

# if you need to override anything above, stuff it in global_overrides.mk
# -----------------------------------------------------------------------

ifeq ($(LOCAL_DIR_GLOBAL_OVERRIDES),1)
-include global_overrides.mk
else
-include $(TOP)/global_overrides.mk
endif

SV_MAJOR_VERSION ?= $(shell date +"%Y")
SV_MINOR_VERSION ?= $(shell date +"%m")
SV_PATCH_VERSION ?= $(shell date +"%d")
SV_MAJOR_VERSION_TWO_DIGIT ?= $(shell date +"%y")
SV_MAJOR_VER_NO = "$(SV_MAJOR_VERSION_TWO_DIGIT).$(SV_MINOR_VERSION)"
SV_FULL_VER_NO = "$(SV_MAJOR_VERSION_TWO_DIGIT).$(SV_MINOR_VERSION).$(SV_PATCH_VERSION)"

ifeq ($(CLUSTER),x64_cygwin)
  SV_VERSION  = SimVascular
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
ifeq ($(CLUSTER),x64_macosx)
  SV_VERSION  = simvascular
  SV_PLATFORM = x64
  SV_POSTFIX=
  SV_OS=macosx
endif

# --------------
# Global defines
# --------------

GLOBAL_DEFINES = -DSV_VERSION=\"$(SV_VERSION)\" \
                 -DSV_MAJOR_VERSION=\"$(SV_MAJOR_VERSION)\" \
                 -DSV_MINOR_VERSION=\"$(SV_MINOR_VERSION)\" \
                 -DSV_PATCH_VERSION=\"$(SV_PATCH_VERSION)\" \
                 -DSV_MAJOR_VER_NO=\"$(SV_MAJOR_VER_NO)\" \
                 -DSV_FULL_VER_NO=\"$(SV_FULL_VER_NO)\" \
                 -DSV_REGISTRY_TOPLEVEL=\"$(SV_REGISTRY_TOPLEVEL)\"

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

ifeq ($(SV_USE_OPENCASCADE),1)
    GLOBAL_DEFINES += -DSV_USE_OpenCASCADE
endif
ifeq ($(SV_USE_OPENCASCADE_SHARED),1)
    GLOBAL_DEFINES += -DSV_USE_OpenCASCADE_SHARED
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

ifeq ($(SV_USE_TCL),1)
    GLOBAL_DEFINES += -DSV_USE_TCL
endif

ifeq ($(SV_USE_PYTHON),1)
    GLOBAL_DEFINES += -DSV_USE_PYTHON
endif

ifeq ($(SV_USE_QT),1)
    GLOBAL_DEFINES += -DSV_USE_QT
  ifeq ($(SV_USE_SV4_GUI),1)
    GLOBAL_DEFINES += -DSV_USE_SV4_GUI
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
  ifeq ($(SV_USE_MITK_SEGMENTATION),1)
    GLOBAL_DEFINES += -DSV_USE_MITK_SEGMENTATION
  endif
  ifeq ($(SV_IGNORE_PROVISIONING_FILE),1)
    GLOBAL_DEFINES += -DSV_IGNORE_PROVISIONING_FILE
  endif
endif

ifeq ($(SV_USE_GDCM),1)
  GLOBAL_DEFINES += -DSV_USE_GDCM
endif

ifeq ($(SV_USE_TINYXML),1)
  GLOBAL_DEFINES += -DSV_USE_TINYXML
endif

ifeq ($(SV_USE_TINYXML2),1)
  GLOBAL_DEFINES += -DSV_USE_TINYXML2
endif

ifeq ($(SV_USE_VMTK),1)
  GLOBAL_DEFINES += -DSV_USE_VMTK
endif

ifeq ($(SV_USE_GTS),1)
  GLOBAL_DEFINES += -DSV_USE_GTS -DNATIVE_WIN32
endif

ifeq ($(SV_USE_VTK),1)
  GLOBAL_DEFINES += -DSV_USE_VTK -DSV_USE_VTK_SHARED
endif

# ----------------------------------
# Platform-specific compiler options
# ----------------------------------

ifeq ($(CLUSTER), x64_cygwin)
  ifeq ($(CXX_COMPILER_VERSION), msvc-19.0)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/compiler.vs19.0.x64_cygwin.mk
  endif
  ifeq ($(CXX_COMPILER_VERSION), msvc-19.16)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/compiler.vs19.16.x64_cygwin.mk
  endif
  ifeq ($(FORTRAN_COMPILER_VERSION), ifort)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/compiler.ifort.x64_cygwin.mk
        GLOBAL_DEFINES += -DSV_WRAP_FORTRAN_IN_CAPS_NO_UNDERSCORE
  endif
  ifeq ($(CXX_COMPILER_VERSION), mingw-gcc)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/compiler.mingw-gcc.x64_cygwin.mk
  endif
  ifeq ($(FORTRAN_COMPILER_VERSION), mingw-gfortran)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/compiler.mingw-gfortran.x64_cygwin.mk
        GLOBAL_DEFINES += -DSV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE
  endif
endif

ifeq ($(CLUSTER), x64_linux)
  ifeq ($(CXX_COMPILER_VERSION), icpc)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/compiler.icpc.x64_linux.mk
  endif
  ifeq ($(FORTRAN_COMPILER_VERSION), ifort)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/compiler.ifort.x64_linux.mk
        GLOBAL_DEFINES += -DSV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE
  endif
  ifeq ($(CXX_COMPILER_VERSION), gcc)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/compiler.gcc.x64_linux.mk
  endif
  ifeq ($(FORTRAN_COMPILER_VERSION), gfortran)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/compiler.gfortran.x64_linux.mk
        GLOBAL_DEFINES += -DSV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE
  endif
endif

ifeq ($(CLUSTER), x64_macosx)
  ifeq ($(CXX_COMPILER_VERSION), clang)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/compiler.clang.x64_macosx.mk
  endif
  ifeq ($(FORTRAN_COMPILER_VERSION), ifort)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/compiler.ifort.x64_macosx.mk
        GLOBAL_DEFINES += -DSV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE
  endif
  ifeq ($(CXX_COMPILER_VERSION), gcc)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/compiler.gcc.x64_macosx.mk
  endif
  ifeq ($(FORTRAN_COMPILER_VERSION), gfortran)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/compiler.gfortran.x64_macosx.mk
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
  SHARED_LIBDIRS = ../Code/Source/sv2/Globals
else
  LIBDIRS = ../Code/Source/sv2/Globals
endif

ifeq ($(SV_USE_SHARED),1)
  SHARED_LIBDIRS += \
		../Code/Source/sv/Utils \
	  ../Code/Source/sv/Repository \
		../Code/Source/vtkSV/Common \
		../Code/Source/vtkSV/IO \
		../Code/Source/vtkSV/Modules/Misc \
		../Code/Source/vtkSV/Modules/Boolean \
		../Code/Source/vtkSV/Modules/Geometry \
		../Code/Source/vtkSV/Modules/NURBS \
		../Code/Source/vtkSV/Modules/Parameterization \
		../Code/Source/vtkSV/Modules/Segmentation \
	  ../Code/Source/sv/Model/SolidModel \
	  ../Code/Source/sv/Mesh/MeshObject \
	  ../Code/Source/sv/Geometry \
	  ../Code/Source/sv2/ImageProcessing \
	  ../Code/Source/sv2/PostProcessing \
	  ../Code/Source/sv2/Segmentation \
	  ../Code/Source/sv3/Common \
	  ../Code/Source/sv3/Path
else
  LIBDIRS += \
		../Code/Source/sv/Utils \
	  ../Code/Source/sv/Repository \
		../Code/Source/vtkSV/Common \
		../Code/Source/vtkSV/IO \
		../Code/Source/vtkSV/Modules/Misc \
		../Code/Source/vtkSV/Modules/Boolean \
		../Code/Source/vtkSV/Modules/Geometry \
		../Code/Source/vtkSV/Modules/NURBS \
		../Code/Source/vtkSV/Modules/Parameterization \
		../Code/Source/vtkSV/Modules/Segmentation \
	  ../Code/Source/sv/Model/SolidModel \
	  ../Code/Source/sv/Mesh/MeshObject \
	  ../Code/Source/sv/Geometry \
	  ../Code/Source/sv2/ImageProcessing \
	  ../Code/Source/sv2/PostProcessing \
	  ../Code/Source/sv2/Segmentation \
	  ../Code/Source/sv3/Common \
	  ../Code/Source/sv3/Path
endif

ifeq ($(SV_USE_VMTK),1)
  ifeq ($(SV_USE_SHARED),1)
     SHARED_LIBDIRS += ../Code/Source/sv/Mesh/VMTKUtils
  else
     LIBDIRS += ../Code/Source/sv/Mesh/VMTKUtils
  endif
endif

# polydata model can depend on vmtk
ifeq ($(SV_USE_SHARED),1)
     SHARED_LIBDIRS += ../Code/Source/sv/Model/PolyDataSolidModel
else
     LIBDIRS += ../Code/Source/sv/Model/PolyDataSolidModel
endif

ifeq ($(SV_USE_ITK),1)
  ifeq ($(SV_USE_SHARED),1)
     SHARED_LIBDIRS += ../Code/Source/sv3/ITKSegmentation
  else
     LIBDIRS += ../Code/Source/sv3/ITKSegmentation
  endif
endif

# segmentation module depends on itk segmentation code
ifeq ($(SV_USE_SHARED),1)
  SHARED_LIBDIRS += ../Code/Source/sv3/Segmentation
else
  LIBDIRS += ../Code/Source/sv3/Segmentation
endif

ifeq ($(SV_USE_MMG),1)
  ifeq ($(SV_USE_MMG_SHARED),1)
     SHARED_LIBDIRS += ../Code/Source/sv/Mesh/MMGMeshUtils
  else
     LIBDIRS += ../Code/Source/sv/Mesh/MMGMeshUtils
  endif
endif

ifeq ($(SV_USE_TETGEN),1)
  ifeq ($(SV_USE_SHARED),1)
     SHARED_LIBDIRS += ../Code/Source/sv/Mesh/TetGenMeshObject
  else
     LIBDIRS += ../Code/Source/sv/Mesh/TetGenMeshObject
  endif
endif

#  solid modeling

ifeq ($(SV_USE_OPENCASCADE),1)
  ifeq ($(SV_USE_OPENCASCADE_SHARED),1)
    SHARED_LIBDIRS += ../Code/Source/sv/Model/OCCTSolidModel
  else
    LIBDIRS += ../Code/Source/sv/Model/OCCTSolidModel
  endif
endif

EXECDIRS = ../Code/Source/Application

# need solverio for adaptor classes so add them after adding solverio
ifeq ($(SV_USE_SHARED),1)
  SHARED_LIBDIRS += ../Code/Source/sv/Mesh/AdaptObject
else
  LIBDIRS += ../Code/Source/sv/Mesh/AdaptObject
endif

ifeq ($(SV_USE_TETGEN_ADAPTOR),1)
  ifeq ($(SV_USE_SHARED),1)
    SHARED_LIBDIRS += ../Code/Source/sv/Mesh/TetGenAdapt
  else
    LIBDIRS += ../Code/Source/sv/Mesh/TetGenAdapt
  endif
endif

# -------------------------
# MITK & sv4gui
# -------------------------

# for now, combine mitk code qt gui code
ifeq ($(SV_USE_MITK),1)
  ifeq ($(SV_USE_SV4_GUI),1)
    ifeq ($(SV_USE_SV4_GUI_SHARED),1)
       SHARED_LIBDIRS += ../Code/Source/sv4gui/Modules
       SHARED_LIBDIRS += ../Code/Source/sv4gui/Plugins
    else
       LIBDIRS += ../Code/Source/sv4gui/Modules
       LIBDIRS += ../Code/Source/sv4gui/Plugins
    endif
  endif
endif

ifeq ($(SV_USE_PYTHON),1)
  ifeq ($(SV_USE_SHARED),1)
     SHARED_LIBDIRS += ../Code/Source/PythonAPI
  else
     LIBDIRS += ../Code/Source/PythonAPI
  endif
endif

SUBDIRS         = $(LIBDIRS) $(EXECDIRS)

# -------------------------
# Local include directories
# -------------------------

LOCAL_SUBDIRS   = $(LIBDIRS) $(SHARED_LIBDIRS) ../Code/Source/Include ../Code/Source/Include/Make
LOCAL_INCDIR    := $(foreach i, ${LOCAL_SUBDIRS}, -I$(TOP)/$(i))

# for now, combine the mitk and qt gui include dirs
ifeq ($(SV_USE_MITK),1)
  ifeq ($(SV_USE_SV4_GUI),1)
     LOCAL_INCDIR += -I$(TOP)/../Code/Source/sv4gui/Plugins/org.sv.projectdatanodes \
                     -I$(TOP)/../Code/Source/sv4gui/Plugins/org.sv.gui.qt.projectmanager \
                     -I$(TOP)/../Code/Source/sv4gui/Plugins/org.sv.gui.qt.datamanager \
                     -I$(TOP)/../Code/Source/sv4gui/Plugins/org.sv.gui.qt.meshing \
                     -I$(TOP)/../Code/Source/sv4gui/Plugins/org.sv.gui.qt.modeling \
                     -I$(TOP)/../Code/Source/sv4gui/Plugins/org.sv.gui.qt.pathplanning \
                     -I$(TOP)/../Code/Source/sv4gui/Plugins/org.sv.gui.qt.segmentation \
                     -I$(TOP)/../Code/Source/sv4gui/Plugins/org.sv.gui.qt.simulation \
                     -I$(TOP)/../Code/Source/sv4gui/Plugins/org.sv.gui.qt.romsimulation \
                     -I$(TOP)/../Code/Source/sv4gui/Plugins/org.sv.gui.qt.application \
                     -I$(TOP)/../Code/Source/sv4gui/Plugins/org.sv.gui.qt.imageprocessing \
                     -I$(TOP)/../Code/Source/sv4gui/Plugins/org.sv.gui.qt.svfsi \
                     -I$(TOP)/../Code/Source/sv4gui/Modules/Common \
                     -I$(TOP)/../Code/Source/sv4gui/Modules/MachineLearning \
                     -I$(TOP)/../Code/Source/sv4gui/Modules/Model/Common \
                     -I$(TOP)/../Code/Source/sv4gui/Modules/Mesh/Common \
                     -I$(TOP)/../Code/Source/sv4gui/Modules/Path \
                     -I$(TOP)/../Code/Source/sv4gui/Modules/ProjectManagement \
                     -I$(TOP)/../Code/Source/sv4gui/Modules/QtWidgets \
                     -I$(TOP)/../Code/Source/sv4gui/Modules/Segmentation \
                     -I$(TOP)/../Code/Source/sv4gui/Modules/Simulation \
                     -I$(TOP)/../Code/Source/sv4gui/Modules/ROMSimulation \
                     -I$(TOP)/../Code/Source/sv4gui/Modules/ImageProcessing \
                     -I$(TOP)/../Code/Source/sv4gui/Modules/svFSI
    ifeq ($(SV_USE_MITK_SEGMENTATION),1)
       LOCAL_INCDIR += -I$(TOP)/../Code/Source/sv4gui/Plugins/org.sv.gui.qt.mitksegmentation
    endif
    ifeq ($(SV_USE_OPENCASCADE),1)
       LOCAL_INCDIR += -I$(TOP)/../Code/Source/sv4gui/Modules/Model/OCCT
    endif
    ifeq ($(SV_USE_PYTHON),1)
       LOCAL_INCDIR += -I$(TOP)/../Code/Source/sv4gui/Plugins/org.sv.pythondatanodes
    endif
  endif
endif

#
# SV library naming
#

SV_LIB_ADAPTOR_NAME=_simvascular_adaptor
SV_LIB_COMMON_NAME=_simvascular_common
SV_LIB_GEOM_NAME=_simvascular_geom
SV_LIB_GLOBALS_NAME=_simvascular_globals
SV_LIB_IMAGE_NAME=_simvascular_image
SV_LIB_ITK_LSET_NAME=_simvascular_itk_lset
SV_LIB_LSET_NAME=_simvascular_lset
SV_LIB_MESH_NAME=_simvascular_mesh
SV_LIB_MMG_MESH_NAME=_simvascular_mmg_mesh
SV_LIB_OpenCASCADE_SOLID_NAME=_simvascular_opencascade_solid
SV_LIB_PATH_NAME=_simvascular_path
SV_LIB_SEGMENTATION_NAME=_simvascular_segmentation
SV_LIB_POLYDATA_SOLID_NAME=_simvascular_polydata_solid
SV_LIB_POST_NAME=_simvascular_post
SV_LIB_PYTHON_INTERP_NAME=_simvascular_python_interp
SV_LIB_PYTHON_API_NAME=_simvascular_python_api
SV_LIB_REPOSITORY_NAME=_simvascular_repository
SV_LIB_SOLID_NAME=_simvascular_solid
SV_LIB_TETGEN_ADAPTOR_NAME=_simvascular_tetgen_adaptor
SV_LIB_TETGEN_MESH_NAME=_simvascular_tetgen_mesh
SV_LIB_UTILS_NAME=_simvascular_utils
SV_LIB_VMTK_UTILS_NAME=_simvascular_vmtk_utils
SV_LIB_VTKSVCOMMON_NAME=_simvascular_vtksvcommon
SV_LIB_VTKSVIO_NAME=_simvascular_vtksvio
SV_LIB_VTKSVMISC_NAME=_simvascular_vtksvmisc
SV_LIB_VTKSVBOOLEAN_NAME=_simvascular_vtksvboolean
SV_LIB_VTKSVGEOMETRY_NAME=_simvascular_vtksvgeometry
SV_LIB_VTKSVNURBS_NAME=_simvascular_vtksvnurbs
SV_LIB_VTKSVPARAMETERIZATION_NAME=_simvascular_vtksvparameterization
SV_LIB_VTKSVSEGMENTATION_NAME=_simvascular_vtksvsegmentation

# sv4gui module and plugin names
SV_LIB_MODULE_COMMON_NAME=_simvascular_module_common
SV_LIB_MODULE_IMAGEPROCESSING_NAME=_simvascular_module_imageprocessing
SV_LIB_MODULE_MACHINELEARNING_NAME=_simvascular_module_machinelearning
SV_LIB_MODULE_MESH_NAME=_simvascular_module_mesh
SV_LIB_MODULE_MODEL_NAME=_simvascular_module_model
SV_LIB_MODULE_MODEL_OCCT_NAME=_simvascular_module_model_occt
SV_LIB_MODULE_PATH_NAME=_simvascular_module_path
SV_LIB_MODULE_PROJECTMANAGEMENT_NAME=_simvascular_module_projectmanagement
SV_LIB_MODULE_QTWIDGETS_NAME=_simvascular_module_qtwidgets
SV_LIB_MODULE_SEGMENTATION_NAME=_simvascular_module_segmentation
SV_LIB_MODULE_SIMULATION_NAME=_simvascular_module_simulation
SV_LIB_MODULE_ROM_SIMULATION_NAME=_simvascular_module_romsimulation
SV_LIB_MODULE_SVFSI_NAME=_simvascular_module_svfsi
SV_PLUGIN_APPLICATION_NAME=org_sv_gui_qt_application
SV_PLUGIN_IMAGEPROCESSING_NAME=org_sv_gui_qt_imageprocessing
SV_PLUGIN_DATAMANAGER_NAME=org_sv_gui_qt_datamanager
SV_PLUGIN_MESHING_NAME=org_sv_gui_qt_meshing
SV_PLUGIN_MITKSEGMENTATION_NAME=org_sv_gui_qt_mitksegmentation
SV_PLUGIN_MODELING_NAME=org_sv_gui_qt_modeling
SV_PLUGIN_PATHPLANNING_NAME=org_sv_gui_qt_pathplanning
SV_PLUGIN_PROJECTMANAGER_NAME=org_sv_gui_qt_projectmanager
SV_PLUGIN_SEGMENTATION_NAME=org_sv_gui_qt_segmentation
SV_PLUGIN_SEGMENTATION1D_NAME=org_sv_gui_qt_segmentation1d
SV_PLUGIN_SVFSI_NAME=org_sv_gui_qt_svfsi
SV_PLUGIN_SIMULATION_NAME=org_sv_gui_qt_simulation
SV_PLUGIN_ROM_SIMULATION_NAME=org_sv_gui_qt_romsimulation
SV_PLUGIN_PROJECTDATANODES_NAME=org_sv_projectdatanodes
SV_PLUGIN_PYTHONDATANODES_NAME=org_sv_pythondatanodes

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

LFLAGS 	 = $(GLOBAL_LFLAGS)

ifneq ($(SV_USE_SHARED),1)
  LFLAGS     += $(SVLIBFLAG)$(SV_LIB_LSET_NAME)$(LIBLINKEXT) \
              $(SVLIBFLAG)$(SV_LIB_IMAGE_NAME)$(LIBLINKEXT) \
              $(SVLIBFLAG)$(SV_LIB_MESH_NAME)$(LIBLINKEXT) \
              $(SVLIBFLAG)$(SV_LIB_SOLID_NAME)$(LIBLINKEXT) \
              $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
              $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
              $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
              $(SVLIBFLAG)$(SV_LIB_POST_NAME)$(LIBLINKEXT) \
              $(SVLIBFLAG)$(SV_LIB_POLYDATA_SOLID_NAME)$(LIBLINKEXT)
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
     SV_LIB_THIRDPARTY_VMTK_NAME=_simvascular_thirdparty_vmtk
     THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/vmtk
     VMTK_TOP = $(TOP)/../Code/ThirdParty/vmtk
     VMTK_INCDIR  = -I $(VMTK_TOP) -I $(VMTK_TOP)/simvascular_vmtk
     VMTK_LIBS    = $(SVLIBFLAG)$(SV_LIB_THIRDPARTY_VMTK_NAME)$(LIBLINKEXT)
endif

# ----
# zlib
# ----

ifeq ($(SV_USE_ZLIB),1)
  SV_LIB_THIRDPARTY_ZLIB_NAME=_simvascular_thirdparty_zlib
  THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/zlib
  ZLIB_TOP = $(TOP)/../Code/ThirdParty/zlib
  ZLIB_INCDIR  = -I $(ZLIB_TOP)
  ZLIB_LIBS    = $(SVLIBFLAG)$(SV_LIB_THIRDPARTY_ZLIB_NAME)$(LIBLINKEXT)
endif

# -------
# tinyxml
# -------

ifeq ($(SV_USE_TINYXML),1)
  SV_LIB_THIRDPARTY_TINYXML_NAME=_simvascular_thirdparty_tinyxml
  THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/tinyxml
  SV_TINYXML_TOP = $(TOP)/../Code/ThirdParty/tinyxml
  SV_TINYXML_INCDIR  = -I $(SV_TINYXML_TOP)
  SV_TINYXML_LIBS    = $(SVLIBFLAG)$(SV_LIB_THIRDPARTY_TINYXML_NAME)$(LIBLINKEXT)
endif

# --------
# SolverIO
# --------

ifeq ($(SV_USE_SOLVERIO),1)
  SV_LIB_THIRDPARTY_SOLVERIO_NAME=_simvascular_thirdparty_solverio
  THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/SolverIO
  SOLVERIO_TOP = $(TOP)/../Code/ThirdParty/SolverIO
  SOLVERIO_INCDIR  = -I $(SOLVERIO_TOP)
  SOLVERIO_LIB     = $(SVLIBFLAG)$(SV_LIB_THIRDPARTY_SOLVERIO_NAME)$(LIBLINKEXT)
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
  SV_LIB_THIRDPARTY_TETGEN_NAME=_simvascular_thirdparty_tetgen
  TETGEN150       = 1
  GLOBAL_DEFINES += -DTETLIBRARY
  THIRD_PARTY_LIBDIRS += ../Code/ThirdParty/tetgen
  TETGEN_TOP = $(TOP)/../Code/ThirdParty/tetgen
  TETGEN_INCDIR  = -I $(TETGEN_TOP)
  TETGEN_LIBS    = $(SVLIBFLAG)$(SV_LIB_THIRDPARTY_TETGEN_NAME)$(LIBLINKEXT)
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
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/tcltk.x64_cygwin.mk
endif

ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/tcltk.x64_linux.mk
endif

ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/tcltk.x64_macosx.mk
endif

# ---------------------
# Visualization toolkit
# ---------------------

ifeq ($(SV_USE_VTK),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/vtk.$(SV_VTK_OPENGL_VERSION).x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/vtk.$(SV_VTK_OPENGL_VERSION).x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/vtk.$(SV_VTK_OPENGL_VERSION).x64_macosx.mk
  endif

endif

# -----------------------------------------
# ***  Optional Open Source Packages    ***
# ***   (less restrictive licenses)     ***
# *** (e.g. MIT or BSD or Apache 2.0)   ***
# -----------------------------------------

# --------
# TINYXML2
# --------

ifeq ($(SV_USE_TINYXML2),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/tinyxml2.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/tinyxml2.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/tinyxml2.x64_macosx.mk
  endif

endif

# ----
# GDCM
# ----

ifeq ($(SV_USE_GDCM),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/gdcm.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/gdcm.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/gdcm.x64_macosx.mk
  endif

endif

# ----
# HDF5
# ----

ifeq ($(SV_USE_HDF5),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/hdf5.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/hdf5.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/hdf5.x64_macosx.mk
  endif

endif

# ---------------
# Insight ToolKit
# ---------------

ifeq ($(SV_USE_ITK),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/itk.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/itk.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/itk.x64_macosx.mk
  endif

endif

# ----
# MITK
# ----

ifeq ($(SV_USE_MITK),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/mitk.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/mitk.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/mitk.x64_macosx.mk
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
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/opencascade.x64_cygwin.mk
         OPENCASCADE_DEFS = -DWNT
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/opencascade.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/opencascade.x64_macosx.mk
  endif

endif

# ------------------
# Python
# ------------------

ifeq ($(SV_USE_PYTHON),1)
  ifeq ($(CLUSTER), x64_cygwin)
	  include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/python.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	  include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/python.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/python.x64_macosx.mk
  endif
endif

# --------
# Freetype
# --------

ifeq ($(SV_USE_FREETYPE),1)
  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/freetype.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/freetype.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/freetype.x64_macosx.mk
  endif
endif

# ---
# MMG
# ---

ifeq ($(SV_USE_MMG),1)

  ifeq ($(CLUSTER), x64_cygwin)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/mmg.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/mmg.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/mmg.x64_macosx.mk
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
	  include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/qt.x64_cygwin.mk
  endif

  ifeq ($(CLUSTER), x64_linux)
	  include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/qt.x64_linux.mk
  endif

  ifeq ($(CLUSTER), x64_macosx)
	  include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/qt.x64_macosx.mk
  endif
endif

# --------------------------------------
# ***  Optional Commercial Packages  ***
# --------------------------------------

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
	  include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/rules.x64_cygwin.mk
endif

ifeq ($(CLUSTER), x64_linux)
	  include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/rules.x64_linux.mk
endif

ifeq ($(CLUSTER), x64_macosx)
	  include $(TOP)/MakeHelpers/$(SV_EXTERNALS_VERSION_NUMBER)/rules.x64_macosx.mk
endif
