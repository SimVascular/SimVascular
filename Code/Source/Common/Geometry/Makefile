# Hey emacs, this is a -*- makefile -*-

# =========================================================================
#
# Copyright (c) 2014-2015 The Regents of the University of California.
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

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(LOCAL_INCDIR) $(TCLTK_INCDIR) $(VTK_INCDIRS) \
	   $(SHAPES_INCDIR) $(GLIB_INCDIR) $(GTS_INCDIR)


HDRS	= cv_sys_geom.h cv_ggems.h \
	cv_spline.h cv_integrate_surface.h \
	vtkIntegrateAttributes.h vtkIntegrateFlowThroughSurface.h \
	vtkSurfaceVectors.h vtkIntersectionPolyDataFilter2.h \
	vtkBooleanOperationPolyDataFilter2.h \
	vtkMultiplePolyDataIntersectionFilter.h \
	vtkFillHolesFilterWithIds.h \
	vtkLoftPolyDataSolid.h \
	vtkLocalQuadricDecimation.h \
	vtkLocalInterpolatingSubdivisionFilter.h \
	vtkLocalLinearSubdivisionFilter.h \
	vtkLocalSmoothPolyDataFilter.h \
	vtkCGSmooth.h svMath.h sparse_matrix.h \
	vtkFindSeparateRegions.h vtkGetSphereRegions.h \
        vtkDelaunay2D_60.h

ifeq ($(MAKE_WITH_GTS),1)
  HDRS +=  vtkSurfaceBooleanOperations.h
endif

CXXSRCS	= cv_sys_geom.cxx cv_ggems.cxx \
	cv_spline.cxx cv_integrate_surface.cxx \
	vtkIntegrateAttributes.cxx vtkIntegrateFlowThroughSurface.cxx \
	vtkSurfaceVectors.cxx vtkIntersectionPolyDataFilter2.cxx \
	vtkBooleanOperationPolyDataFilter2.cxx \
	vtkMultiplePolyDataIntersectionFilter.cxx \
	vtkFillHolesFilterWithIds.cxx \
	vtkLoftPolyDataSolid.cxx \
	vtkLocalQuadricDecimation.cxx \
	vtkLocalInterpolatingSubdivisionFilter.cxx \
	vtkLocalLinearSubdivisionFilter.cxx \
	vtkLocalSmoothPolyDataFilter.cxx \
	vtkCGSmooth.cxx svMath.cxx sparse_matrix.cxx \
	vtkFindSeparateRegions.cxx vtkGetSphereRegions.cxx \
        vtkDelaunay2D_60.cxx

ifeq ($(MAKE_WITH_GTS),1)
  CXXSRCS +=  vtkSurfaceBooleanOperations.cxx
endif

SRCS	= $(CXXSRCS)

DEPS	= $(CXXSRCS:.cxx=.d)

OBJS	= $(CXXSRCS:.cxx=.$(OBJECTEXT))

TARGET_LIB = lib_lib_simvascular_sysgeom.$(STATICEXT)

DLLHDRS = $(HDRS) cv_geom_init.h
DLLSRCS = $(CXXSRCS) cv_geom_init.cxx
DLLOBJS = $(DLLSRCS:.cxx=.$(OBJECTEXT))
ifeq ($(CLUSTER),x64_linux)
DLLLIBS = -l_simvascular_globals
else
DLLLIBS = lib_simvascular_globals.$(STATICEXT)
endif

TARGET_SHARED = lib_simvascular_sysgeom.$(SOEXT)

# -------
# Targets
# -------

include $(TOP)/targetlib.mk
