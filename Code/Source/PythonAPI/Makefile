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

## clean trailing spaces from source files
#find . -type f | grep cxx | xargs sed -i 's/[ \t]*$//'
#find . -type f | grep cxx | xargs sed -i 's/.tp_name = //g'
#find . -type f | grep cxx | xargs sed -i 's/.tp_basicsize = //g'
#find . -type f | grep h | xargs sed -i 's/[ \t]*$//'
#find . -type f | grep h | xargs sed -i 's/.tp_name = //g'
#find . -type f | grep h | xargs sed -i 's/.tp_basicsize = //g'


TOP=../../../BuildWithMake

include $(TOP)/include.mk

CXXFLAGS = $(GLOBAL_CXXFLAGS) \
	   $(LOCAL_INCDIR) \
	   $(VTK_INCDIRS) \
	   $(TCLTK_INCDIR) \
           $(PYTHON_INCDIR) \
           $(QT_INCDIRS) $(QT_DEFS) \
	   $(ITK_INCDIRS) \
           $(SV_TINYXML_INCDIR) $(SV_TINYXML2_INCDIR) \
           $(MITK_INCDIRS) $(MITK_DEFS) \
           $(ZLIB_INCDIR) \
	   $(TETGEN_INCDIR) \
           $(SV_MMG_INCDIR) \
           $(OPENCASCADE_INCDIR) $(OPENCASCADE_DEFS)

# hack until we don't use tinyxml from mitk
ifeq ($(SV_USE_MITK),0)
  MITK_MAJOR_VERSION=2018
  MITK_MINOR_VERSION=04
  MITK_PATCH_VERSION=0
  MITK_VERSION=$(MITK_MAJOR_VERSION).$(MITK_MINOR_VERSION).$(MITK_PATCH_VERSION)
  MITK_BINDIR = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/mitk-$(MITK_VERSION)
endif
CXXFLAGS += -I$(MITK_BINDIR)/include/tinyxml

CREATE_EXPORTS_CV_ALL_CAPS=SV_PYTHON_API
CREATE_EXPORTS_CV_EXPORT_NAME=SV_EXPORT_PYTHON_API
CREATE_EXPORTS_CV_FILE_H=svPythonAPIExports.h

CXXFLAGS += -DSV_EXPORT_PYTHON_API_COMPILE

MODULE_NAME = sv4guiModulePythonAPI
MODULE_NAME_ALL_CAPS = SV4GUIMODULEPYTHONAPI
CXXFLAGS += -D$(MODULE_NAME)_EXPORTS
CXXFLAGS += -DUS_MODULE_NAME="$(MODULE_NAME)"

HDRS	=     Imaging_PyModule.h \
              Segmentation_PyModule.h \
              Geometry_PyModule.h \
              Math_PyModule.h \
              Meshing_PyModule.h \
              MeshUtils_PyModule.h \
              PathPlanning_PyModule.h \
              PyUtils.h \
              Modeling_PyModule.h \
              Simulation_PyModule.h \
              Vmtk_PyModule.h

CXXSRCS	=     Imaging_PyModule.cxx \
              Segmentation_PyModule.cxx \
              Geometry_PyModule.cxx \
              Math_PyModule.cxx \
              Meshing_PyModule.cxx \
              MeshUtils_PyModule.cxx \
              PathPlanning_PyModule.cxx \
              PyUtils.cxx \
              Modeling_PyModule.cxx \
              Simulation_PyModule.cxx \
              Vmtk_PyModule.cxx

SVPYHDRS = 
SVPYSRCS = 

DLLLIBS = $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_COMMON_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_SOLID_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MESH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_VTKSVCOMMON_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_VTKSVMISC_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_VTKSVNURBS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_POLYDATA_SOLID_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_PLUGIN_ROM_SIMULATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_MESH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_MODEL_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_MODEL_OCCT_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_SEGMENTATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PATH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PROJECTMANAGEMENT_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_COMMON_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_ITK_LSET_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_PATH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_SEGMENTATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_TETGEN_MESH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_TETGEN_ADAPTOR_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_ADAPTOR_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MMG_MESH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_VMTK_UTILS_NAME)$(LIBLINKEXT)

ifeq ($(SV_USE_MESHSIM_DISCRETE_MODEL),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_MESHSIM_DISCRETE_SOLID_NAME)$(LIBLINKEXT)
endif
ifeq ($(SV_USE_MESHSIM_SOLID_MODEL),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_MESHSIM_SOLID_NAME)$(LIBLINKEXT)
endif
ifeq ($(SV_USE_PARASOLID),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_PARASOLID_SOLID_NAME)$(LIBLINKEXT)
endif
ifeq ($(SV_USE_OPENCASCADE),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_OpenCASCADE_SOLID_NAME)$(LIBLINKEXT)
endif

DLLLIBS += $(PARASOLID_LIBS) $(MESHSIM_LIBS)

ifeq ($(SV_USE_OPENCASCADE),1)
    DLLLIBS += $(OPENCASCADE_LIBS)
endif

ifeq ($(SV_USE_TINYXML),1)
  DLLLIBS += $(SV_TINYXML_LIBS)
endif

ifeq ($(SV_USE_TINYXML2),1)
  DLLLIBS += $(SV_TINYXML2_LIBS)
endif

ifeq ($(SV_USE_ITK),1)
  DLLLIBS += $(ITK_LIBS)
endif

ifeq ($(SV_USE_MMG),1)
  DLLLIBS += $(MMG_LIBS)
endif

ifeq ($(SV_USE_MITK),1)
  DLLLIBS += $(MITK_LIBS)
endif

ifeq ($(SV_USE_QT),1)
  DLLLIBS += $(QT_LIBS)
endif

TARGET_LIB_NAME = $(SV_LIB_PYTHON_API_NAME)

SV_PYTHON_MODULE_NAME = pyPythonAPI

include $(TOP)/targetlib.mk
