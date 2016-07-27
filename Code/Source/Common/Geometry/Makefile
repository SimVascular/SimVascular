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

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(LOCAL_INCDIR) $(VMTK_INCDIR) $(TCLTK_INCDIR) $(VTK_INCDIRS) \
	   $(PYTHON_INCDIR)

ifeq ($(SV_USE_OPENCASCADE),1)
   CXXFLAGS += $(OPENCASCADE_INCDIR)
endif

CXXFLAGS += -DSV_EXPORT_SYSGEOM_COMPILE

HDRS	= cv_sys_geom.h cv_ggems.h \
	cv_spline.h cv_integrate_surface.h \
	vtkIntegrateAttributes.h vtkIntegrateFlowThroughSurface.h \
	vtkSurfaceVectors.h vtkIntersectionPolyDataFilter2.h \
	vtkBooleanOperationPolyDataFilter2.h \
	vtkMultiplePolyDataIntersectionFilter.h \
	vtkFillHolesFilterWithIds.h \
	vtkLoftPolyDataSolid.h \
	vtkLocalApproximatingSubdivisionFilter.h \
	vtkLocalButterflySubdivisionFilter.h \
	vtkLocalInterpolatingSubdivisionFilter.h \
	vtkLocalLinearSubdivisionFilter.h \
	vtkLocalLoopSubdivisionFilter.h \
	vtkLocalQuadricDecimation.h \
	vtkLocalSmoothPolyDataFilter.h \
	vtkCGSmooth.h svMath.h sparse_matrix.h \
	vtkFindSeparateRegions.h vtkGetSphereRegions.h \
        vtkDelaunay2D_60.h vtkConstrainedBlend.h

CXXSRCS	= cv_sys_geom.cxx cv_ggems.cxx \
	cv_spline.cxx cv_integrate_surface.cxx \
	vtkIntegrateAttributes.cxx vtkIntegrateFlowThroughSurface.cxx \
	vtkSurfaceVectors.cxx vtkIntersectionPolyDataFilter2.cxx \
	vtkBooleanOperationPolyDataFilter2.cxx \
	vtkMultiplePolyDataIntersectionFilter.cxx \
	vtkFillHolesFilterWithIds.cxx \
	vtkLoftPolyDataSolid.cxx \
	vtkLocalApproximatingSubdivisionFilter.cxx \
	vtkLocalButterflySubdivisionFilter.cxx \
	vtkLocalInterpolatingSubdivisionFilter.cxx \
	vtkLocalLinearSubdivisionFilter.cxx \
	vtkLocalLoopSubdivisionFilter.cxx \
	vtkLocalQuadricDecimation.cxx \
	vtkLocalSmoothPolyDataFilter.cxx \
	vtkCGSmooth.cxx svMath.cxx sparse_matrix.cxx \
	vtkFindSeparateRegions.cxx vtkGetSphereRegions.cxx \
        vtkDelaunay2D_60.cxx vtkConstrainedBlend.cxx

DLLHDRS = cv_geom_init.h

DLLSRCS = cv_geom_init.cxx

DLLLIBS = $(SVLIBFLAG)_simvascular_solid$(LIBLINKEXT) \
	  $(SVLIBFLAG)_simvascular_repository$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_utils$(LIBLINKEXT) \
          $(VMTK_LIBS)

TARGET_LIB_NAME = simvascular_geom

include $(TOP)/targetlib.mk
