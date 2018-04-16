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

set(SRC_CPP_FILES

)

set(INTERNAL_CPP_FILES
    sv3gui_LoftingUtils.cxx
    sv3gui_ContourGroupCreate.cxx
    sv3gui_ContourGroupCreateAction.cxx
    sv3gui_SegmentationLegacyLoadAction.cxx
    sv3gui_SegmentationLegacySaveAction.cxx
    sv3gui_SegmentationLoadAction.cxx
    sv3gui_LevelSet2DWidget.cxx
    sv3gui_LoftParamWidget.cxx
    sv3gui_Seg2DEdit.cxx
    sv3gui_ContourGroupPoint2DSizeAction.cxx
    sv3gui_ContourGroupPoint3DSizeAction.cxx
    sv3gui_Seg3DCreateAction.cxx
    sv3gui_Seg3DEdit.cxx
    sv3gui_SegmentationPluginActivator.cxx
)

set(MOC_H_FILES
    src/internal/sv3gui_LoftingUtils.h
    src/internal/sv3gui_ContourGroupCreate.h
    src/internal/sv3gui_ContourGroupCreateAction.h
    src/internal/sv3gui_SegmentationLegacyLoadAction.h
    src/internal/sv3gui_SegmentationLegacySaveAction.h
    src/internal/sv3gui_SegmentationLoadAction.h
    src/internal/sv3gui_LevelSet2DWidget.h
    src/internal/sv3gui_LoftParamWidget.h
    src/internal/sv3gui_Seg2DEdit.h
    src/internal/sv3gui_ContourGroupPoint2DSizeAction.h
    src/internal/sv3gui_ContourGroupPoint3DSizeAction.h
    src/internal/sv3gui_Seg3DCreateAction.h
    src/internal/sv3gui_Seg3DEdit.h
    src/internal/sv3gui_SegmentationPluginActivator.h
)

set(UI_FILES
    src/internal/sv3gui_ContourGroupCreate.ui
    src/internal/sv3gui_LevelSet2DWidget.ui
    src/internal/sv3gui_LoftParamWidget.ui
    src/internal/sv3gui_Seg2DEdit.ui
    src/internal/sv3gui_Seg3DEdit.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/contourgroup.png
  resources/svseg3d.png
)

set(QRC_FILES

)

set(CPP_FILES )

foreach(file ${SRC_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/${file})
endforeach(file ${SRC_CPP_FILES})

foreach(file ${INTERNAL_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/internal/${file})
endforeach(file ${INTERNAL_CPP_FILES})

