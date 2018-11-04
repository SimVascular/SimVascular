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

# --------------
# Compiler flags
# --------------

CXXFLAGS = $(GLOBAL_CXXFLAGS)

CREATE_EXPORTS_CV_ALL_CAPS=SV_MMG
CREATE_EXPORTS_CV_EXPORT_NAME=SV_EXPORT_MMG
CREATE_EXPORTS_CV_FILE_H=svMMGExports.h

CXXFLAGS += -DSV_EXPORT_MMG_COMPILE

CXXFLAGS += $(LOCAL_INCDIR) \
	    $(SV_MMG_INCDIR) $(VTK_INCDIRS) $(TCLTK_INCDIR) $(PYTHON_INCDIR)

HDRS	= sv_mmg_mesh_utils.h

CXXSRCS	= sv_mmg_mesh_utils.cxx

DLLHDRS = sv_mmg_mesh_init.h
DLLSRCS = sv_mmg_mesh_init.cxx

SVPYHDRS = sv_mmg_mesh_init_py.h
SVPYSRCS = sv_mmg_mesh_init_py.cxx

TARGET_LIB_NAME = $(SV_LIB_MMG_MESH_NAME)

SV_PYTHON_MODULE_NAME = pyMeshUtil

DLLLIBS = $(SVLIBFLAG)$(SV_LIB_POLYDATA_SOLID_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_VTKSVMISC_NAME)$(LIBLINKEXT) \
	  $(SV_MMG_LIBS)

include $(TOP)/targetlib.mk




