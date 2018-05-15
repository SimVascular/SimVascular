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

CXXFLAGS = $(GLOBAL_CXXFLAGS) \
	   $(LOCAL_INCDIR) \
	   $(VTK_INCDIRS) \
	   $(TCLTK_INCDIR) \
	   $(PYTHON_INCDIR)

# double hack for now, want to make code consistent with meshsim code
ifeq ($(SV_USE_MESHSIM_SHARED),1)
  CXXFLAGS += -DNOT_ADAMS_CREATEINTERNALMESHOBJECT_CODE
endif

CREATE_EXPORTS_CV_ALL_CAPS=SV_TETGEN_ADAPTOR
CREATE_EXPORTS_CV_EXPORT_NAME=SV_EXPORT_TETGEN_ADAPTOR
CREATE_EXPORTS_CV_FILE_H=svTetGenAdaptorExports.h

CXXFLAGS += -DSV_EXPORT_TETGEN_ADAPTOR_COMPILE

ifeq ($(SV_USE_OPENCASCADE),1)
  CXXFLAGS += $(OPENCASCADE_INCDIR)
endif

ifeq ($(SV_USE_ZLIB),1)
  CXXFLAGS += $(ZLIB_INCDIR)
endif

ifeq ($(SV_USE_TETGEN),1)
        CXXFLAGS    += $(TETGEN_INCDIR)
endif

HDRS	= sv_TetGenAdapt.h

CXXSRCS = sv_TetGenAdapt.cxx

DLLHDRS = sv_tetgen_adapt_init.h
DLLSRCS = sv_tetgen_adapt_init.cxx

SVPYHDRS = sv_tetgen_adapt_init_py.h
SVPYSRCS = sv_tetgen_adapt_init_py.cxx

DLLLIBS = $(SVLIBFLAG)$(SV_LIB_ADAPTOR_NAME)$(LIBLINKEXT) \
          $(SOLVERIO_LIB)

ifeq ($(SV_USE_TETGEN),1)
  DLLLIBS += $(SVLIBFLAG)$(SV_LIB_TETGEN_MESH_NAME)$(LIBLINKEXT)
  DLLLIBS += $(TETGEN_LIBS)
endif

DLLLIBS += $(SVLIBFLAG)$(SV_LIB_MESH_NAME)$(LIBLINKEXT)

ifeq ($(SV_USE_VMTK),1)
  DLLLIBS +=  $(VMTK_LIBS)
endif

ifeq ($(SV_USE_ZLIB),1)
  DLLLIBS +=  $(ZLIB_LIBS)
endif

ifeq ($(SV_USE_TETGEN),1)
  DLLLIBS += $(TETGEN_LIBS)
endif

DLLLIBS += $(VTK_LIBS)

DLLLIBS += $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
           $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT)

TARGET_LIB_NAME = $(SV_LIB_TETGEN_ADAPTOR_NAME)

SV_PYTHON_MODULE_NAME = pyTetGenAdapt

include $(TOP)/targetlib.mk


