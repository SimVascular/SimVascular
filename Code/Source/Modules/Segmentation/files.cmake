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

set(H_FILES
    sv3gui_SegmentationUtils.h
    sv3gui_Contour.h
    sv3gui_ContourCircle.h
    sv3gui_ContourEllipse.h
    sv3gui_ContourPolygon.h
    sv3gui_ContourTensionPolygon.h
    sv3gui_ContourSplinePolygon.h
    sv3gui_ContourOperation.h
    sv3gui_ContourModel.h
    sv3gui_ContourModelVtkMapper2D.h
    sv3gui_ContourModelThresholdInteractor.h
    sv3gui_ContourGroup.h
    sv3gui_ContourGroupVtkMapper2D.h
    sv3gui_ContourGroupVtkMapper3D.h
    sv3gui_ContourGroupDataInteractor.h
    sv3gui_ContourGroupIO.h
    sv3gui_SegmentationLegacyIO.h
    sv3gui_Surface.h
    sv3gui_SurfaceVtkMapper3D.h
    sv3gui_Seg3D.h
    sv3gui_MitkSeg3D.h
    sv3gui_MitkSeg3DOperation.h
    sv3gui_MitkSeg3DIO.h
    sv3gui_MitkSeg3DVtkMapper3D.h
    sv3gui_MitkSeg3DDataInteractor.h
    sv3gui_SegmentationObjectFactory.h
    sv3gui_Seg3DUtils.h
)

set(CPP_FILES
    sv3gui_SegmentationUtils.cxx
    sv3gui_Contour.cxx
    sv3gui_ContourCircle.cxx
    sv3gui_ContourEllipse.cxx
    sv3gui_ContourPolygon.cxx
    sv3gui_ContourTensionPolygon.cxx
    sv3gui_ContourSplinePolygon.cxx
    sv3gui_ContourOperation.cxx
    sv3gui_ContourModel.cxx
    sv3gui_ContourModelVtkMapper2D.cxx
    sv3gui_ContourModelThresholdInteractor.cxx
    sv3gui_ContourGroup.cxx
    sv3gui_ContourGroupVtkMapper2D.cxx
    sv3gui_ContourGroupVtkMapper3D.cxx
    sv3gui_ContourGroupDataInteractor.cxx
    sv3gui_ContourGroupIO.cxx
    sv3gui_SegmentationLegacyIO.cxx
    sv3gui_Surface.cxx
    sv3gui_SurfaceVtkMapper3D.cxx
    sv3gui_Seg3D.cxx
    sv3gui_MitkSeg3D.cxx
    sv3gui_MitkSeg3DOperation.cxx
    sv3gui_MitkSeg3DIO.cxx
    sv3gui_MitkSeg3DVtkMapper3D.cxx
    sv3gui_MitkSeg3DDataInteractor.cxx
    sv3gui_SegmentationObjectFactory.cxx
    sv3gui_Seg3DUtils.cxx
)

set(RESOURCE_FILES
    Interactions/svContourGroupInteraction.xml
    Interactions/svContourModelThresholdInteraction.xml
    Interactions/svMitkSeg3DInteraction.xml
    Interactions/svSegmentationConfig.xml
)
