# Hey emacs, this is a -*- makefile -*-

# =========================================================================
#
# Copyright (c) 2014 The Regents of the University of California.
# All Rights Reserved. 
#
# Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                         University of California, San Diego.
#
# Portions of the code Copyright (c) 1998-2007 Stanford University,
# Charles Taylor, Nathan Wilson, Ken Wang.
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
#
# =========================================================================*/

TOP=../../../../BuildWithMake

include $(TOP)/include.mk

# --------------
# Compiler flags
# --------------

CXXFLAGS = $(GLOBAL_CXXFLAGS) \
	   $(LOCAL_INCDIR) \
	   $(VTK_INCDIRS) \
	   $(TCLTK_INCDIR) \
           $(OPENCASCADE_INCDIR)

CXXFLAGS += -DSV_EXPORT_MESHSIM_MESH_COMPILE

ifeq ($(SV_USE_ZLIB),1)
  CXXFLAGS += $(ZLIB_INCDIR)
endif

ifeq ($(SV_USE_PARASOLID),1)
    CXXFLAGS    += $(PARASOLID_INCDIR) \
                   -I$(TOP)/../Code/Licensed/ParasolidSolidModel
endif

ifeq ($(SV_USE_MESHSIM),1)
        CXXFLAGS    += $(MESHSIM_INCDIR)
endif

ifeq ($(SV_USE_MESHSIM_DISCRETE_MODEL),1)
    CXXFLAGS    += $(MESHSIM_INCDIR) \
		  -I$(TOP)/../Code/Source/Model/MeshSimDiscreteSolidModel
endif

ifeq ($(SV_USE_MESHSIM_SOLID_MODEL),1)
    CXXFLAGS    += $(MESHSIM_INCDIR) \
		  -I$(TOP)/../Code/Source/Model/MeshSimSolidModel
endif

HDRS	= cvMeshSimMeshObject.h cvMeshSimMeshSystem.h

CXXSRCS	= cvMeshSimMeshObject.cxx \
          cvMeshSimMeshSystem.cxx stats.cxx scorecXYZ.cxx

DLLHDRS = cv_meshsim_mesh_init.h

DLLSRCS = cv_meshsim_mesh_init.cxx

DLLLIBS = $(SVLIBFLAG)_simvascular_solid$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_mesh$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_repository$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_utils$(LIBLINKEXT)

ifeq ($(SV_USE_PARASOLID),1)
  ifeq ($(SV_USE_PARASOLID_SHARED),1)
    DLLLIBS += $(SVLIBFLAG)_simvascular_parasolid_solid$(LIBLINKEXT)
  else
    DLLLIBS += $(SVLIBFLAG)_simvascular_parasolid_solid$(LIBLINKEXT)
  endif
endif
ifeq ($(SV_USE_MESHSIM_DISCRETE_MODEL),1)
  ifeq ($(SV_USE_MESHSIM_DISCRETE_MODEL_SHARED),1)
    DLLLIBS += $(SVLIBFLAG)_simvascular_meshsim_discrete_solid$(LIBLINKEXT)
  else
    DLLLIBS += $(SVLIBFLAG)_simvascular_meshsim_discrete_solid$(LIBLINKEXT)
  endif
endif
ifeq ($(SV_USE_MESHSIM_SOLID_MODEL),1)
  ifeq ($(SV_USE_MESHSIM_SOLID_MODEL_SHARED),1)
    DLLLIBS += $(SVLIBFLAG)_simvascular_meshsim_solid$(LIBLINKEXT)
  else
    DLLLIBS += $(SVLIBFLAG)_simvascular_meshsim_solid$(LIBLINKEXT)
  endif
endif

ifeq ($(SV_USE_VMTK),1)
  DLLLIBS +=  $(SVLIBFLAG)_simvascular_thirdparty_vmtk$(LIBLINKEXT)
endif

DLLLIBS += $(SVLIBFLAG)_simvascular_mesh$(LIBLINKEXT)

ifeq ($(SV_USE_VMTK),1)
  DLLLIBS +=  $(SVLIBFLAG)_simvascular_thirdparty_vmtk$(LIBLINKEXT)
endif

ifeq ($(SV_USE_ZLIB),1)
  DLLLIBS +=  $(SVLIBFLAG)_simvascular_thirdparty_zlib$(LIBLINKEXT)
endif

DLLLIBS += $(SVLIBFLAG)_simvascular_polydata_solid$(LIBLINKEXT)
DLLLIBS += $(SVLIBFLAG)_simvascular_geom$(LIBLINKEXT)
DLLLIBS += $(SVLIBFLAG)_simvascular_utils$(LIBLINKEXT)

DLLLIBS += $(SVLIBFLAG)_simvascular_polydata_solid$(LIBLINKEXT)
DLLLIBS += $(SVLIBFLAG)_simvascular_geom$(LIBLINKEXT)
DLLLIBS += $(SVLIBFLAG)_simvascular_utils$(LIBLINKEXT)

ifeq ($(SV_USE_MESHSIM),1)
  DLLLIBS += $(MESHSIM_LIBS)
endif

ifeq ($(SV_USE_PARASOLID),1)
  DLLLIBS += $(PARASOLID_LIBS)
endif

ifeq ($(SV_USE_OPENCASCADE),1)
  DLLLIBS += $(OPENCASCADE_LIBS)
endif

DLLLIBS +=  $(SVLIBFLAG)_simvascular_thirdparty_vmtk$(LIBLINKEXT)

DLLLIBS += $(VTK_LIBS)

TARGET_LIB_NAME = simvascular_meshsim_mesh

include $(TOP)/targetlib.mk

