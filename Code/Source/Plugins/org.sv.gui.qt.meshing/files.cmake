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
    svMeshCreate.cxx
    svMeshCreateAction.cxx
    svMeshEdit.cxx
    svVtkMeshSphereWidget.cxx
    svLocalTableDelegate.cxx
    svMeshLegacySaveAction.cxx
    svMeshLoadSurfaceAction.cxx
    svMeshLoadVolumeAction.cxx
    svMeshingPluginActivator.cxx
)

set(MOC_H_FILES
    src/internal/svMeshCreate.h
    src/internal/svMeshCreateAction.h
    src/internal/svMeshEdit.h
    src/internal/svLocalTableDelegate.h
    src/internal/svMeshLegacySaveAction.h
    src/internal/svMeshLoadSurfaceAction.h
    src/internal/svMeshLoadVolumeAction.h
    src/internal/svMeshingPluginActivator.h
)

set(UI_FILES
    src/internal/svMeshCreate.ui
    src/internal/svMeshEdit.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/mesh.png
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

