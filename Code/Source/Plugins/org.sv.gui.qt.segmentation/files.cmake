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
    svLoftingUtils.cxx
    svContourGroupCreate.cxx
    svContourGroupCreateAction.cxx
    svSegmentationLegacyLoadAction.cxx
    svSegmentationLegacySaveAction.cxx
    svSegmentationLoadAction.cxx
    svLevelSet2DWidget.cxx
    svLoftParamWidget.cxx
    svSeg2DEdit.cxx
    svContourGroupPoint2DSizeAction.cxx
    svContourGroupPoint3DSizeAction.cxx
    svSeg3DCreateAction.cxx
    svSeg3DEdit.cxx
    svSegmentationPluginActivator.cxx
)

set(MOC_H_FILES
    src/internal/svLoftingUtils.h
    src/internal/svContourGroupCreate.h
    src/internal/svContourGroupCreateAction.h
    src/internal/svSegmentationLegacyLoadAction.h
    src/internal/svSegmentationLegacySaveAction.h
    src/internal/svSegmentationLoadAction.h
    src/internal/svLevelSet2DWidget.h
    src/internal/svLoftParamWidget.h
    src/internal/svSeg2DEdit.h
    src/internal/svContourGroupPoint2DSizeAction.h
    src/internal/svContourGroupPoint3DSizeAction.h
    src/internal/svSeg3DCreateAction.h
    src/internal/svSeg3DEdit.h
    src/internal/svSegmentationPluginActivator.h
)

set(UI_FILES
    src/internal/svContourGroupCreate.ui
    src/internal/svLevelSet2DWidget.ui
    src/internal/svLoftParamWidget.ui
    src/internal/svSeg2DEdit.ui
    src/internal/svSeg3DEdit.ui
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

