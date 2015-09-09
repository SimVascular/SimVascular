# Hey emacs, this is a -*- makefile -*-

# =========================================================================
#
# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
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

ifeq ($(MAKE_WITH_TETGEN),1)
  CXXFLAGS += $(TETGEN_INCDIR)
endif

ifeq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL),1)
  CXXFLAGS += $(MESHSIM_INCDIR)
endif

HDRS	= cvTetGenMeshObject.h cvTetGenMeshSystem.h cv_tetgenmesh_utils.h

CXXSRCS	= cvTetGenMeshObject.cxx \
          cvTetGenMeshSystem.cxx cv_tetgenmesh_utils.cxx

SRCS	= $(CXXSRCS)

DEPS	= $(CXXSRCS:.cxx=.d)

OBJS	= $(CXXSRCS:.cxx=.$(OBJECTEXT))

DEPLIBS = $(TOP)/lib/lib_simvascular_utils.$(STATICEXT) $(TOP)/lib/lib_simvascular_mesh.$(STATICEXT)

TARGET_LIB = lib_lib_simvascular_tetgen_mesh.$(STATICEXT)

DLLHDRS = $(HDRS) cv_tetgen_mesh_init.h
DLLSRCS = $(CXXSRCS) cv_tetgen_mesh_init.cxx
DLLOBJS = $(DLLSRCS:.cxx=.$(OBJECTEXT))
ifeq ($(CLUSTER),x64_linux)
DLLLIBS = -l_simvascular_globals \
          -l_lib_simvascular_mesh \
          $(TETGEN_LIBS) \
 $(VTK_LIBS) $(TETGEN_LIBS) $(ZLIB_LIBS) -l_lib_simvascular_sysgeom -l_lib_simvascular_solid -l_lib_simvascular_repository -l_lib_simvascular_utils -l_simvascular_globals
else
DLLLIBS = lib_simvascular_globals.$(STATICEXT) \
          lib_lib_simvascular_mesh.$(STATICEXT) \
          $(TETGEN_LIBS) $(PARASOLID_LIBS)
endif
TARGET_SHARED = lib_simvascular_tetgen_mesh.$(SOEXT)

LFLAGS +=  
# -------
# Targets
# -------

include $(TOP)/targetlib.mk

