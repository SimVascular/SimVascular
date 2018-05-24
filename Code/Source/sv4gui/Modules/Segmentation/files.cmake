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
    sv4gui_SegmentationUtils.h
    sv4gui_Contour.h
    sv4gui_ContourCircle.h
    sv4gui_ContourEllipse.h
    sv4gui_ContourPolygon.h
    sv4gui_ContourTensionPolygon.h
    sv4gui_ContourSplinePolygon.h
    sv4gui_ContourOperation.h
    sv4gui_ContourModel.h
    sv4gui_ContourModelVtkMapper2D.h
    sv4gui_ContourModelThresholdInteractor.h
    sv4gui_ContourGroup.h
    sv4gui_ContourGroupVtkMapper2D.h
    sv4gui_ContourGroupVtkMapper3D.h
    sv4gui_ContourGroupDataInteractor.h
    sv4gui_ContourGroupIO.h
    sv4gui_SegmentationLegacyIO.h
    sv4gui_Surface.h
    sv4gui_SurfaceVtkMapper3D.h
    sv4gui_Seg3D.h
    sv4gui_MitkSeg3D.h
    sv4gui_MitkSeg3DOperation.h
    sv4gui_MitkSeg3DIO.h
    sv4gui_MitkSeg3DVtkMapper3D.h
    sv4gui_MitkSeg3DDataInteractor.h
    sv4gui_SegmentationObjectFactory.h
    sv4gui_Seg3DUtils.h
)

set(CPP_FILES
    sv4gui_SegmentationUtils.cxx
    sv4gui_Contour.cxx
    sv4gui_ContourCircle.cxx
    sv4gui_ContourEllipse.cxx
    sv4gui_ContourPolygon.cxx
    sv4gui_ContourTensionPolygon.cxx
    sv4gui_ContourSplinePolygon.cxx
    sv4gui_ContourOperation.cxx
    sv4gui_ContourModel.cxx
    sv4gui_ContourModelVtkMapper2D.cxx
    sv4gui_ContourModelThresholdInteractor.cxx
    sv4gui_ContourGroup.cxx
    sv4gui_ContourGroupVtkMapper2D.cxx
    sv4gui_ContourGroupVtkMapper3D.cxx
    sv4gui_ContourGroupDataInteractor.cxx
    sv4gui_ContourGroupIO.cxx
    sv4gui_SegmentationLegacyIO.cxx
    sv4gui_Surface.cxx
    sv4gui_SurfaceVtkMapper3D.cxx
    sv4gui_Seg3D.cxx
    sv4gui_MitkSeg3D.cxx
    sv4gui_MitkSeg3DOperation.cxx
    sv4gui_MitkSeg3DIO.cxx
    sv4gui_MitkSeg3DVtkMapper3D.cxx
    sv4gui_MitkSeg3DDataInteractor.cxx
    sv4gui_SegmentationObjectFactory.cxx
    sv4gui_Seg3DUtils.cxx
)

set(RESOURCE_FILES
    Interactions/sv4gui_ContourGroupInteraction.xml
    Interactions/sv4gui_ContourModelThresholdInteraction.xml
    Interactions/sv4gui_MitkSeg3DInteraction.xml
    Interactions/sv4gui_SegmentationConfig.xml
)
