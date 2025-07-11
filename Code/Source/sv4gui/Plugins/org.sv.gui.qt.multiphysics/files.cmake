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
    sv4gui_MultiPhysicsJobCreate.cxx
    sv4gui_MultiPhysicsJobCreateAction.cxx
    sv4gui_MultiPhysicsView.cxx
    sv4gui_MultiPhysicsBCWidget.cxx
    sv4gui_MultiPhysicsPreferencePage.cxx
    sv4gui_MultiPhysicsPreferences.cxx
    sv4gui_MultiPhysicsPluginActivator.cxx
)

set(MOC_H_FILES
    sv4gui_MultiPhysicsJobCreate.h
    sv4gui_MultiPhysicsJobCreateAction.h
    sv4gui_MultiPhysicsUtil.h
    sv4gui_MultiPhysicsView.h
    sv4gui_MultiPhysicsBCWidget.h
    sv4gui_MultiPhysicsPreferencePage.h
    sv4gui_MultiPhysicsPreferences.h
    sv4gui_MultiPhysicsPluginActivator.h
)

set(UI_FILES
    sv4gui_MultiPhysicsJobCreate.ui
    sv4gui_MultiPhysicsView.ui
    sv4gui_MultiPhysicsBCWidget.ui
    sv4gui_MultiPhysicsPreferencePage.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/MultiPhysics.png
)

set(QRC_FILES
  resources/datanode.qrc
)

set(CPP_FILES )

foreach(file ${SRC_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} ${file})
endforeach(file ${SRC_CPP_FILES})

foreach(file ${INTERNAL_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} ${file})
endforeach(file ${INTERNAL_CPP_FILES})
