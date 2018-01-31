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
    svModelCreate.cxx
    svModelCreateAction.cxx
    svModelLoadAction.cxx
    svModelLegacySaveAction.cxx
    svModelExtractPathsAction.cxx
    svSegSelectionWidget.cxx
    svModelEdit.cxx
    svFaceListDelegate.cxx
    svModelFaceInfoExportAction.cxx
    svModelingPluginActivator.cxx
    svLoftingPreferencePage.cxx
    svCapSelectionWidget.cxx
)

set(MOC_H_FILES
    src/internal/svModelCreate.h
    src/internal/svModelCreateAction.h
    src/internal/svModelLoadAction.h
    src/internal/svModelLegacySaveAction.h
    src/internal/svModelExtractPathsAction.h
    src/internal/svSegSelectionWidget.h
    src/internal/svModelEdit.h
    src/internal/svFaceListDelegate.h
    src/internal/svModelFaceInfoExportAction.h
    src/internal/svModelingPluginActivator.h
    src/internal/svLoftingPreferencePage.h
    src/internal/svCapSelectionWidget.h
)

set(UI_FILES
    src/internal/svModelCreate.ui
    src/internal/svSegSelectionWidget.ui
    src/internal/svModelEdit.ui
    src/internal/svCapSelectionWidget.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/model.png
)

set(QRC_FILES
  resources/modeling.qrc
)

set(CPP_FILES )

foreach(file ${SRC_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/${file})
endforeach(file ${SRC_CPP_FILES})

foreach(file ${INTERNAL_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/internal/${file})
endforeach(file ${INTERNAL_CPP_FILES})

