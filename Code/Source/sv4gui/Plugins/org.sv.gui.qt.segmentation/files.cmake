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
    sv4gui_LoftingUtils.cxx
    sv4gui_ContourGroupCreate.cxx
    sv4gui_ContourGroupCreateAction.cxx
    sv4gui_SegmentationLegacyLoadAction.cxx
    sv4gui_SegmentationLegacySaveAction.cxx
    sv4gui_SegmentationLoadAction.cxx
    sv4gui_LevelSet2DWidget.cxx
    sv4gui_LoftParamWidget.cxx
    sv4gui_Seg2DEdit.cxx
    sv4gui_ContourGroupPoint2DSizeAction.cxx
    sv4gui_ContourGroupPoint3DSizeAction.cxx
    sv4gui_Seg3DCreateAction.cxx
    sv4gui_Seg3DEdit.cxx
    sv4gui_SegmentationPluginActivator.cxx
)

set(MOC_H_FILES
    sv4gui_LoftingUtils.h
    sv4gui_ContourGroupCreate.h
    sv4gui_ContourGroupCreateAction.h
    sv4gui_SegmentationLegacyLoadAction.h
    sv4gui_SegmentationLegacySaveAction.h
    sv4gui_SegmentationLoadAction.h
    sv4gui_LevelSet2DWidget.h
    sv4gui_LoftParamWidget.h
    sv4gui_Seg2DEdit.h
    sv4gui_ContourGroupPoint2DSizeAction.h
    sv4gui_ContourGroupPoint3DSizeAction.h
    sv4gui_Seg3DCreateAction.h
    sv4gui_Seg3DEdit.h
    sv4gui_SegmentationPluginActivator.h
)

set(UI_FILES
    sv4gui_ContourGroupCreate.ui
    sv4gui_LevelSet2DWidget.ui
    sv4gui_LoftParamWidget.ui
    sv4gui_Seg2DEdit.ui
    sv4gui_Seg3DEdit.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/contourgroup.png
  resources/svseg3d.png
)

set(QRC_FILES

)
