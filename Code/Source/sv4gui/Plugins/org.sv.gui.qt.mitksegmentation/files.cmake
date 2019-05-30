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
  sv4gui_QmitkSegmentationPreferencePage.cxx
  sv4gui_mitkPluginActivator.cxx
  sv4gui_QmitkSegmentationView.cxx
  sv4gui_QmitkThresholdAction.cxx
  sv4gui_QmitkCreatePolygonModelAction.cxx
  #sv4gui_QmitkStatisticsAction.cxx
  sv4gui_QmitkAutocropAction.cxx
  sv4gui_QmitkDeformableClippingPlaneView.cxx
  sv4gui_QmitkDataSelectionWidget.cxx
  sv4gui_QmitkSegmentationUtilitiesView.cxx
  sv4gui_QmitkSegmentationUtilityWidget.cxx
  sv4gui_QmitkBooleanOperationsWidget.cxx
  sv4gui_QmitkImageMaskingWidget.cxx
  sv4gui_QmitkContourModelToImageWidget.cxx
  sv4gui_QmitkMorphologicalOperationsWidget.cxx
  sv4gui_QmitkSurfaceToImageWidget.cxx
)

set(UI_FILES
  sv4gui_QmitkSegmentationControls.ui
  sv4gui_QmitkDeformableClippingPlaneViewControls.ui
  sv4gui_QmitkDataSelectionWidgetControls.ui
  sv4gui_QmitkSegmentationUtilitiesViewControls.ui
  sv4gui_QmitkBooleanOperationsWidgetControls.ui
  sv4gui_QmitkImageMaskingWidgetControls.ui
  sv4gui_QmitkContourModelToImageWidgetControls.ui
  sv4gui_QmitkMorphologicalOperationsWidgetControls.ui
  sv4gui_QmitkSurfaceToImageWidgetControls.ui
)

set(MOC_H_FILES
  sv4gui_QmitkSegmentationPreferencePage.h
  sv4gui_mitkPluginActivator.h
  sv4gui_QmitkSegmentationView.h
  sv4gui_QmitkThresholdAction.h
  sv4gui_QmitkCreatePolygonModelAction.h
  #sv4gui_QmitkStatisticsAction.h
  sv4gui_QmitkAutocropAction.h
  sv4gui_QmitkDeformableClippingPlaneView.h
  sv4gui_QmitkDataSelectionWidget.h
  sv4gui_QmitkSegmentationUtilitiesView.h
  sv4gui_QmitkSegmentationUtilityWidget.h
  sv4gui_QmitkBooleanOperationsWidget.h
  sv4gui_QmitkImageMaskingWidget.h
  sv4gui_QmitkContourModelToImageWidget.h
  sv4gui_QmitkMorphologicalOperationsWidget.h
  sv4gui_QmitkSurfaceToImageWidget.h
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
