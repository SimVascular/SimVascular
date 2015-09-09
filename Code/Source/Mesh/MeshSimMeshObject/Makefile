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
	   $(TCLTK_INCDIR)

ifeq ($(MAKE_WITH_ZLIB),1)
  CXXFLAGS += $(ZLIB_INCDIR)
endif


# Conditional inclusion of headers for concrete SolidModel classes
# ----------------------------------------------------------------

ifeq ($(EXCLUDE_SOLID_MODEL),0)

    ifeq ($(MAKE_WITH_PARASOLID),1)
        CXXFLAGS    += $(PARASOLID_INCDIR) \
		       -I$(TOP)/ParasolidSolidModel
    endif

    ifeq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL),1)
        CXXFLAGS    += $(MESHSIM_INCDIR) \
		       -I$(TOP)/MeshSimDiscreteSolidModel
    endif

endif

ifeq ($(MAKE_WITH_MESHSIM),1)
        CXXFLAGS    += $(MESHSIM_INCDIR)
endif


HDRS	= cvMeshSimMeshObject.h cvMeshSimMeshSystem.h

CXXSRCS	= cvMeshSimMeshObject.cxx \
          cvMeshSimMeshSystem.cxx stats.cxx scorecXYZ.cxx

SRCS	= $(CXXSRCS)

DEPS	= $(CXXSRCS:.cxx=.d)

OBJS	= $(CXXSRCS:.cxx=.$(OBJECTEXT))

DEPLIBS = $(TOP)/lib/lib_simvascular_utils.$(STATICEXT) $(TOP)/lib/lib_simvascular_mesh.$(STATICEXT)

TARGET_LIB = lib_lib_simvascular_meshsim_mesh.$(STATICEXT)

DLLHDRS = $(HDRS) cv_meshsim_mesh_init.h
DLLSRCS = $(CXXSRCS) cv_meshsim_mesh_init.cxx
DLLOBJS = $(DLLSRCS:.cxx=.$(OBJECTEXT))
ifeq ($(CLUSTER),x64_linux)
  DLLLIBS = -l_lib_simvascular_mesh \
            -l_simvascular_parasolid
  ifeq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL),1)
    DLLLIBS += -l_simvascular_meshsim_discrete
  endif
  DLLLIBS += -l_lib_simvascular_mesh
  DLLLIBS += $(MESHSIM_LIBS) $(PARASOLID_LIBS)
else
  DLLLIBS = lib_lib_simvascular_mesh.$(STATICEXT) \
            lib_simvascular_meshsim_discrete.$(STATICEXT) \
            lib_simvascular_parasolid.$(STATICEXT) \
            $(MESHSIM_LIBS) $(PARASOLID_LIBS)
endif

ifeq ($(MAKE_WITH_VMTK),1)
	DLLLIBS += ../../../../BuildWithMake/Lib/lib_lib_simvascular_vmtk.$(STATICEXT)
endif
	DLLLIBS += ../../../../BuildWithMake/Lib/lib_lib_simvascular_zlib.$(STATICEXT)

DLLLIBS += $(VTK_LIBS) $(PARASOLID_LIBS)

TARGET_SHARED = lib_simvascular_meshsim_mesh.$(SOEXT)

LFLAGS +=  
# -------
# Targets
# -------

include $(TOP)/targetlib.mk

