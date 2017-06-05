# Hey emacs, this is a -*- makefile -*-

# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Portions of the code Copyright (c) 2009-2011 Open Source Medical
# Software Corporation, University of California, San Diego.
#
# Portions Copyright (c) 2000-2007, Stanford University, 
#    Rensselaer Polytechnic Institute, Kenneth E. Jansen, 
#    Charles A. Taylor (see SimVascular Acknowledgements file 
#    for additional contributors to the source code).
#
# Redistribution and use in source and binary forms, with or without 
# modification, are permitted provided that the following conditions 
# are met:
#
# Redistributions of source code must retain the above copyright notice,
# this list of conditions and the following disclaimer. 
# Redistributions in binary form must reproduce the above copyright 
# notice, this list of conditions and the following disclaimer in the 
# documentation and/or other materials provided with the distribution. 
# Neither the name of the Stanford University or Rensselaer Polytechnic
# Institute nor the names of its contributors may be used to endorse or
# promote products derived from this software without specific prior 
# written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
# COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
# OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
# AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
# THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

TOP=../../../../BuildWithMake

include $(TOP)/include.mk

# --------------
# Compiler flags
# --------------

CXXFLAGS = $(GLOBAL_CXXFLAGS) \
	   $(LOCAL_INCDIR) \
	   $(VTK_INCDIRS) \
	   $(TCLTK_INCDIR)

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

HDRS	= cvTetGenAdapt.h

CXXSRCS = cvTetGenAdapt.cxx

DLLHDRS = cv_tetgen_adapt_init.h

DLLSRCS = cv_tetgen_adapt_init.cxx

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

include $(TOP)/targetlib.mk


