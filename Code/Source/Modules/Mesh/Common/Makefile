# Hey emacs, this is a -*- makefile -*-

# Copyright (c) 2009-2011, Open Source Medical Software Corporation,
#              University of California, San Diego.
#
# All rights reserved.
#
# Portions copyright (c) 1999-2007 Stanford University,
# Nathan Wilson, Ken Wang, Charles Taylor.
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

TOP=../../../../../BuildWithMake

include $(TOP)/include.mk

CXXFLAGS = $(GLOBAL_CXXFLAGS) \
	   $(LOCAL_INCDIR) \
	   $(VTK_INCDIRS) \
	   $(TCLTK_INCDIR) \
           $(PYTHON_INCDIR) \
           $(ZLIB_INCDIR) \
           $(TETGEN_INCDIR) \
           $(MESHSIM_INCDIR) \
           $(PARASOLID_INCDIR) \
           $(QT_INCDIRS) $(QT_DEFS) \
	   $(ITK_INCDIRS) \
           $(MITK_INCDIRS) $(MITK_DEFS)

ifeq ($(SV_USE_OPENCASCADE),1)
    CXXFLAGS += $(OPENCASCADE_INCDIR)
endif

MODULE_NAME = svMesh
MODULE_NAME_ALL_CAPS = SVMESH
CXXFLAGS += -D$(MODULE_NAME)_EXPORTS

CXXFLAGS += -DUS_MODULE_NAME="$(MODULE_NAME)"

HDRS	= \
    svMesh.h \
    svMeshTetGen.h \
    svMitkMesh.h \
    svMeshFactory.h \
    svMitkMeshOperation.h \
    svMitkMeshMapper3D.h \
    svMitkMeshMapper2D.h \
    svMitkMeshIO.h \
    svMeshLegacyIO.h \
    svMeshAdaptor.h \
    svMeshTetGenAdaptor.h \
    svMitkMeshObjectFactory.h

CXXSRCS	= \
    svMesh.cxx \
    svMeshTetGen.cxx \
    svMeshFactory.cxx \
    svRegisterTetGenFunction.cxx \
    svMitkMesh.cxx \
    svMitkMeshOperation.cxx \
    svMitkMeshMapper3D.cxx \
    svMitkMeshMapper2D.cxx \
    svMitkMeshIO.cxx \
    svMeshLegacyIO.cxx \
    svMeshTetGenAdaptor.cxx \
    svMitkMeshObjectFactory.cxx

CXXSRCS += us_init.cxx

UIFILES =

RCFILES =

QRCFILES =

CXXSRCS += $(addprefix moc_,$(HDRS:.h=.cxx))

CXXSRCS += $(addprefix rcc_,$(notdir $(QRCFILES:.qrc=.cxx)))

DLLHDRS =

DLLSRCS =

DLLLIBS += $(PYTHON_LIB) $(VTK_LIBS) $(ITK_LIBS) $(QT_LIBS) $(MITK_LIBS)

DLLLIBS += \
          $(SVLIBFLAG)$(SV_LIB_MODULE_COMMON_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_MODEL_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_ITK_LSET_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_TETGEN_MESH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MESH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MMG_MESH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_POLYDATA_SOLID_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_SOLID_NAME)$(LIBLINKEXT) \
          $(TETGEN_LIBS) \
          $(ZLIB_LIBS) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GLOBALS_NAME)$(LIBLINKEXT)

ifeq ($(SV_USE_MESHSIM_DISCRETE_MODEL_QT_GUI),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_MESHSIM_DISCRETE_SOLID_NAME)$(LIBLINKEXT)
endif
ifeq ($(SV_USE_MESHSIM_SOLID_MODEL_QT_GUI),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_MESHSIM_SOLID_NAME)$(LIBLINKEXT)
endif
ifeq ($(SV_USE_PARASOLID_QT_GUI),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_PARASOLID_SOLID_NAME)$(LIBLINKEXT)
endif
ifeq ($(SV_USE_OPENCASCADE_QT_GUI),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_OpenCASCADE_SOLID_NAME)$(LIBLINKEXT)
endif
ifeq ($(SV_USE_TETGEN_ADAPTOR),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_TETGEN_ADAPTOR_NAME)$(LIBLINKEXT)
endif

DLLLIBS += $(PARASOLID_LIBS) $(MESHSIM_LIBS)

ifeq ($(SV_USE_OPENCASCADE_QT_GUI),1)
    DLLLIBS += $(OPENCASCADE_LIBS)
endif

TARGET_LIB_NAME = $(SV_LIB_MODULE_MESH_NAME)

include $(TOP)/targetlib.mk
