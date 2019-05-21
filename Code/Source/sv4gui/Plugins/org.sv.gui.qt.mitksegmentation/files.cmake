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

set(CPP_FILES
  QmitkSegmentationPreferencePage.cpp
  mitkPluginActivator.cpp
  QmitkSegmentationView.cpp
  QmitkThresholdAction.cpp
  QmitkCreatePolygonModelAction.cpp
  #QmitkStatisticsAction.cpp
  QmitkAutocropAction.cpp
  QmitkDeformableClippingPlaneView.cpp
  QmitkDataSelectionWidget.cpp
  QmitkSegmentationUtilitiesView.cpp
  QmitkSegmentationUtilityWidget.cpp
  QmitkBooleanOperationsWidget.cpp
  QmitkImageMaskingWidget.cpp
  QmitkContourModelToImageWidget.cpp
  QmitkMorphologicalOperationsWidget.cpp
  QmitkSurfaceToImageWidget.cpp
)

set(UI_FILES
  QmitkSegmentationControls.ui
  QmitkDeformableClippingPlaneViewControls.ui
  QmitkDataSelectionWidgetControls.ui
  QmitkSegmentationUtilitiesViewControls.ui
  QmitkBooleanOperationsWidgetControls.ui
  QmitkImageMaskingWidgetControls.ui
  QmitkContourModelToImageWidgetControls.ui
  QmitkMorphologicalOperationsWidgetControls.ui
  QmitkSurfaceToImageWidgetControls.ui
)

set(MOC_H_FILES
  QmitkSegmentationPreferencePage.h
  mitkPluginActivator.h
  QmitkSegmentationView.h
  QmitkThresholdAction.h
  QmitkCreatePolygonModelAction.h
  #QmitkStatisticsAction.h
  QmitkAutocropAction.h
  QmitkDeformableClippingPlaneView.h
  QmitkDataSelectionWidget.h
  QmitkSegmentationUtilitiesView.h
  QmitkSegmentationUtilityWidget.h
  QmitkBooleanOperationsWidget.h
  QmitkImageMaskingWidget.h
  QmitkContourModelToImageWidget.h
  QmitkMorphologicalOperationsWidget.h
  QmitkSurfaceToImageWidget.h
)

set(CACHED_RESOURCE_FILES
  resources/segmentation.svg
  resources/deformablePlane.png
  resources/clipping_plane_translate_48x48.png
  resources/clipping_plane_rotate48x48.png
  resources/clipping_plane_deform48x48.png
  resources/segmentation_utilities.svg
  plugin.xml
)

set(QRC_FILES
  resources/segmentation.qrc
  resources/SegmentationUtilities.qrc
  resources/BooleanOperationsWidget.qrc
  resources/MorphologicalOperationsWidget.qrc
)
