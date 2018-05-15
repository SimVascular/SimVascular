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

TOP=../../../../../BuildWithMake

include $(TOP)/include.mk

CXXFLAGS = $(GLOBAL_CXXFLAGS) \
	   $(LOCAL_INCDIR) \
	   $(VTK_INCDIRS) \
	   $(TCLTK_INCDIR) \
	   $(PYTHON_INCDIR)

CREATE_EXPORTS_CV_ALL_CAPS=SV_TETGEN_MESH
CREATE_EXPORTS_CV_EXPORT_NAME=SV_EXPORT_TETGEN_MESH
CREATE_EXPORTS_CV_FILE_H=svTetGenMeshExports.h

CXXFLAGS += -DSV_EXPORT_TETGEN_MESH_COMPILE

ifeq ($(SV_USE_ZLIB),1)
  CXXFLAGS += $(ZLIB_INCDIR)
endif

ifeq ($(SV_USE_TETGEN),1)
  CXXFLAGS += $(TETGEN_INCDIR)
endif

ifeq ($(SV_USE_MESHSIM_DISCRETE_MODEL),1)
  CXXFLAGS += $(MESHSIM_INCDIR)
endif

ifeq ($(SV_USE_MMG),1)
  CXXFLAGS += $(SV_MMG_INCDIR)
endif

ifeq ($(SV_USE_VMTK),1)
  CXXFLAGS += $(VMTK_INCDIR)
endif

HDRS	= sv_TetGenMeshObject.h sv_TetGenMeshSystem.h \
          sv_tetgenmesh_utils.h

CXXSRCS	= sv_TetGenMeshObject.cxx \
          sv_TetGenMeshSystem.cxx sv_tetgenmesh_utils.cxx

DLLHDRS = sv_tetgen_mesh_init.h
DLLSRCS = sv_tetgen_mesh_init.cxx

SVPYHDRS = sv_tetgen_mesh_init_py.h
SVPYSRCS = sv_tetgen_mesh_init_py.cxx

DLLLIBS = $(SVLIBFLAG)$(SV_LIB_MMG_MESH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_POLYDATA_SOLID_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MESH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_VMTK_UTILS_NAME)$(LIBLINKEXT) \
          $(TETGEN_LIBS) $(VMTK_LIBS) $(SV_MMG_LIBS) $(ZLIB_LIBS)

TARGET_LIB_NAME = $(SV_LIB_TETGEN_MESH_NAME)

SV_PYTHON_MODULE_NAME = pyMeshTetgen

include $(TOP)/targetlib.mk

