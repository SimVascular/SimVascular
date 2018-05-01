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

TOP=../../../../../../BuildWithMake

include $(TOP)/include.mk

CXXFLAGS = $(GLOBAL_CXXFLAGS) \
	   $(LOCAL_INCDIR) \
	   $(VTK_INCDIRS) \
	   $(TCLTK_INCDIR) \
           $(PYTHON_INCDIR) \
           $(ZLIB_INCDIR) \
           $(TETGEN_INCDIR) \
           $(QT_INCDIRS) $(QT_DEFS) \
	   $(ITK_INCDIRS) \
           $(MITK_INCDIRS) $(MITK_DEFS)

ifeq ($(SV_USE_OPENCASCADE),1)
    CXXFLAGS += $(OPENCASCADE_INCDIR)
endif

MODULE_NAME = sv4guiModuleMesh
MODULE_NAME_ALL_CAPS = SV4GUIMODULEMESH
CXXFLAGS += -D$(MODULE_NAME)_EXPORTS

CXXFLAGS += -DUS_MODULE_NAME="$(MODULE_NAME)"

HDRS	= \
    sv4gui_Mesh.h \
    sv4gui_MeshTetGen.h \
    sv4gui_MitkMesh.h \
    sv4gui_MeshFactory.h \
    sv4gui_RegisterTetGenFunction.h \
    sv4gui_MitkMeshOperation.h \
    sv4gui_MitkMeshMapper3D.h \
    sv4gui_MitkMeshMapper2D.h \
    sv4gui_MitkMeshIO.h \
    sv4gui_MeshLegacyIO.h \
    sv4gui_MeshAdaptor.h \
    sv4gui_MeshTetGenAdaptor.h \
    sv4gui_MitkMeshObjectFactory.h

CXXSRCS	= \
    sv4gui_Mesh.cxx \
    sv4gui_MeshTetGen.cxx \
    sv4gui_MeshFactory.cxx \
    sv4gui_RegisterTetGenFunction.cxx \
    sv4gui_MitkMesh.cxx \
    sv4gui_MitkMeshOperation.cxx \
    sv4gui_MitkMeshMapper3D.cxx \
    sv4gui_MitkMeshMapper2D.cxx \
    sv4gui_MitkMeshIO.cxx \
    sv4gui_MeshLegacyIO.cxx \
    sv4gui_MeshTetGenAdaptor.cxx \
    sv4gui_MitkMeshObjectFactory.cxx \
    sv4gui_MeshAdaptor.cxx

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

ifeq ($(SV_USE_OPENCASCADE),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_OpenCASCADE_SOLID_NAME)$(LIBLINKEXT)
endif
ifeq ($(SV_USE_TETGEN_ADAPTOR),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_TETGEN_ADAPTOR_NAME)$(LIBLINKEXT)
endif

ifeq ($(SV_USE_OPENCASCADE),1)
    DLLLIBS += $(OPENCASCADE_LIBS)
endif

TARGET_LIB_NAME = $(SV_LIB_MODULE_MESH_NAME)

include $(TOP)/targetlib.mk
