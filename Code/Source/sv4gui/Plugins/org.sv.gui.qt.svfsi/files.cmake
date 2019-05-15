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
    sv4gui_svFSIJobCreate.cxx
    sv4gui_svFSIJobCreateAction.cxx
    sv4gui_svFSIView.cxx
    sv4gui_svFSIBCWidget.cxx
    sv4gui_svFSIPreferencePage.cxx
    sv4gui_svFSIPluginActivator.cxx
)

set(MOC_H_FILES
    sv4gui_svFSIJobCreate.h
    sv4gui_svFSIJobCreateAction.h
    sv4gui_svFSIUtil.h
    sv4gui_svFSIView.h
    sv4gui_svFSIBCWidget.h
    sv4gui_svFSIPreferencePage.h
    sv4gui_svFSIPluginActivator.h
)

set(UI_FILES
    sv4gui_svFSIJobCreate.ui
    sv4gui_svFSIView.ui
    sv4gui_svFSIBCWidget.ui
    sv4gui_svFSIPreferencePage.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/svFSI.png
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
