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

MODULE_NAME = vtkSVMisc
MODULE_NAME_ALL_CAPS = VTKSVMISC
CXXFLAGS += -D$(MODULE_NAME)_EXPORTS
MODULE_EXPORT_FILE_H = vtkSVMiscModule.h

HDRS	= \
  vtkSVCleanUnstructuredGrid.h \
  vtkSVFindGeodesicPath.h \
  vtkSVFindSeparateRegions.h \
  vtkSVGetBoundaryFaces.h \
  vtkSVHausdorffDistance.h \
  vtkSVSurfaceMapper.h \
  vtkSVPassDataArray.h \
  vtkSVPlacePointsOnS2.h \
  vtkSVGetSphereRegions.h \
  vtkSVFillHolesWithIdsFilter.h \
  vtkSVIntegrateFlowThroughSurface.h \
  vtkSVLoftSplineSurface.h \
  vtkSVIntegrateAttributes.h \
  vtkSVSurfaceVectors.h \
  vtkSVPolyDataSurfaceInspector.h \
  vtkSVCellComplexThinner.h \
  vtkSVPolyDataEdgeSplitter.h \
  vtkSVSeedSelector.h \
  vtkSVPickPointSeedSelector.h \
  vtkSVOpenProfilesSeedSelector.h \
  vtkSVIdListSeedSelector.h

CXXSRCS	= \
  vtkSVCleanUnstructuredGrid.cxx \
  vtkSVFindGeodesicPath.cxx \
  vtkSVFindSeparateRegions.cxx \
  vtkSVGetBoundaryFaces.cxx \
  vtkSVHausdorffDistance.cxx \
  vtkSVSurfaceMapper.cxx \
  vtkSVPassDataArray.cxx \
  vtkSVPlacePointsOnS2.cxx \
  vtkSVGetSphereRegions.cxx \
  vtkSVFillHolesWithIdsFilter.cxx \
  vtkSVIntegrateFlowThroughSurface.cxx \
  vtkSVLoftSplineSurface.cxx \
  vtkSVIntegrateAttributes.cxx \
  vtkSVSurfaceVectors.cxx \
  vtkSVPolyDataSurfaceInspector.cxx \
  vtkSVCellComplexThinner.cxx \
  vtkSVPolyDataEdgeSplitter.cxx \
  vtkSVSeedSelector.cxx \
  vtkSVPickPointSeedSelector.cxx \
  vtkSVOpenProfilesSeedSelector.cxx \
  vtkSVIdListSeedSelector.cxx

DLLHDRS =

DLLSRCS =

DLLLIBS += $(PYTHON_LIB) $(VTK_LIBS) $(ITK_LIBS)

DLLLIBS += $(SVLIBFLAG)$(SV_LIB_VTKSVCOMMON_NAME)$(LIBLINKEXT)

TARGET_LIB_NAME = $(SV_LIB_VTKSVMISC_NAME)

include $(TOP)/targetlib.mk
