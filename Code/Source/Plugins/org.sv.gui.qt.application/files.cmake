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
  sv3gui_Application.cxx
  sv3gui_ApplicationPluginActivator.cxx
  sv3gui_FileCreateProjectAction.cxx
  sv3gui_FileOpenProjectAction.cxx
  sv3gui_FileSaveProjectAction.cxx
  sv3gui_FileSaveProjectAsAction.cxx
  sv3gui_CloseProjectAction.cxx
  sv3gui_ProjectCreate.cxx
  sv3gui_WorkbenchWindowAdvisor.cxx
  sv3gui_AppWorkbenchAdvisor.cxx
  sv3gui_AboutDialog.cxx
  sv3gui_WorkbenchIntroPart.cxx
  sv3gui_DefaultPerspective.cxx
  sv3gui_WorkbenchWindowAdvisorHack.cxx
)

set(MOC_H_FILES
  src/internal/sv3gui_Application.h
  src/internal/sv3gui_ApplicationPluginActivator.h
  src/internal/sv3gui_FileCreateProjectAction.h
  src/internal/sv3gui_FileOpenProjectAction.h
  src/internal/sv3gui_FileSaveProjectAction.h
  src/internal/sv3gui_FileSaveProjectAsAction.h
  src/internal/sv3gui_CloseProjectAction.h
  src/internal/sv3gui_ProjectCreate.h
  src/internal/sv3gui_WorkbenchWindowAdvisor.h
  src/internal/sv3gui_WorkbenchWindowAdvisorHack.h
  src/internal/sv3gui_AboutDialog.h
  src/internal/sv3gui_WorkbenchIntroPart.h
  src/internal/sv3gui_DefaultPerspective.h
)

set(UI_FILES
  src/internal/sv3gui_ProjectCreate.ui
  src/internal/sv3gui_AboutDialog.ui
  src/internal/sv3gui_WelcomeScreenViewControls.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/icon.png
)

set(QRC_FILES
resources/svApplication.qrc
resources/welcome/svWelcomeScreenView.qrc
)

set(CPP_FILES )

foreach(file ${SRC_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/${file})
endforeach(file ${SRC_CPP_FILES})

foreach(file ${INTERNAL_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/internal/${file})
endforeach(file ${INTERNAL_CPP_FILES})

