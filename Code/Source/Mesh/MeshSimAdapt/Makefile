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

# --------------
# Compiler flags
# --------------

CXXFLAGS = $(GLOBAL_CXXFLAGS) \
	   $(LOCAL_INCDIR) \
	   $(VTK_INCDIRS) \
	   $(TCLTK_INCDIR)

ifeq ($(MAKE_WITH_ZLIB),1)
  CXXFLAGS += $(ZLIB_INCDIR)
endif

ifeq ($(MAKE_WITH_PARASOLID),1)
    CXXFLAGS    += $(PARASOLID_INCDIR) \
                   -I$(TOP)/../Code/Licensed/ParasolidSolidModel
endif

ifeq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL),1)
    CXXFLAGS    += $(MESHSIM_INCDIR) \
		  -I$(TOP)/../Source/Model/MeshSimDiscreteSolidModel
endif

ifeq ($(MAKE_WITH_MESHSIM),1)
        CXXFLAGS    += $(MESHSIM_INCDIR)
endif

ifeq ($(MAKE_WITH_TETGEN),1)
        CXXFLAGS    += $(TETGEN_INCDIR)
endif

HDRS	= cvMeshSimAdapt.h MeshSimInternal_phAdapt.h cvAdaptHelp.h

CXXSRCS = cvMeshSimAdapt.cxx

DLLHDRS = cv_meshsim_adapt_init.h

DLLSRCS = cv_meshsim_adapt_init.cxx

DLLLIBS = $(SVLIBFLAG)_lib_simvascular_mesh$(LIBLINKEXT) \
          $(SVLIBFLAG)_lib_simvascular_adaptor$(LIBLINKEXT) \
          $(SVLIBFLAG)_lib_simvascular_solverio$(LIBLINKEXT) \
          $(SVLIBFLAG)_lib_simvascular_repository$(LIBLINKEXT)

ifeq ($(MAKE_WITH_PARASOLID),1)
  DLLLIBS += $(SVLIBFLAG)_lib_simvascular_parasolid$(LIBLINKEXT)
endif
ifeq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL),1)
  DLLLIBS += $(SVLIBFLAG)_lib_simvascular_meshsim_discrete$(LIBLINKEXT)
endif

DLLLIBS += $(SVLIBFLAG)_lib_simvascular_mesh$(LIBLINKEXT)

ifeq ($(MAKE_WITH_VMTK),1)
  DLLLIBS +=  $(SVLIBFLAG)_lib_simvascular_vmtk$(LIBLINKEXT)
endif

ifeq ($(MAKE_WITH_ZLIB),1)
  DLLLIBS +=  $(SVLIBFLAG)_lib_simvascular_zlib$(LIBLINKEXT)
endif

ifeq ($(MAKE_WITH_MESHSIM),1)
  DLLLIBS += $(MESHSIM_LIBS)
endif

ifeq ($(MAKE_WITH_PARASOLID),1)
  DLLLIBS += $(PARASOLID_LIBS)
endif

DLLLIBS += $(VTK_LIBS)

TARGET_LIB_NAME = simvascular_meshsim_adaptor

include $(TOP)/targetlib.mk


