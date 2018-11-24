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
  sv4gui_Application.cxx
  sv4gui_ApplicationPluginActivator.cxx
  sv4gui_FileCreateProjectAction.cxx
  sv4gui_FileOpenProjectAction.cxx
  sv4gui_FileSaveProjectAction.cxx
  sv4gui_FileSaveProjectAsAction.cxx
  sv4gui_CloseProjectAction.cxx
  sv4gui_ProjectCreate.cxx
  sv4gui_WorkbenchWindowAdvisor.cxx
  sv4gui_AppWorkbenchAdvisor.cxx
  sv4gui_AboutDialog.cxx
  sv4gui_WorkbenchIntroPart.cxx
  sv4gui_DefaultPerspective.cxx
  sv4gui_WorkbenchWindowAdvisorHack.cxx
  sv4gui_Main.cxx
  sv4gui_MitkApp.cxx
)

set(MOC_H_FILES
  sv4gui_Application.h
  sv4gui_ApplicationPluginActivator.h
  sv4gui_FileCreateProjectAction.h
  sv4gui_FileOpenProjectAction.h
  sv4gui_FileSaveProjectAction.h
  sv4gui_FileSaveProjectAsAction.h
  sv4gui_CloseProjectAction.h
  sv4gui_ProjectCreate.h
  sv4gui_WorkbenchWindowAdvisor.h
  sv4gui_WorkbenchWindowAdvisorHack.h
  sv4gui_AboutDialog.h
  sv4gui_WorkbenchIntroPart.h
  sv4gui_DefaultPerspective.h
  sv4gui_Main.h
  sv4gui_MitkApp.h
)

set(UI_FILES
  sv4gui_ProjectCreate.ui
  sv4gui_AboutDialog.ui
  sv4gui_WelcomeScreenViewControls.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/icon.png
)

set(QRC_FILES
resources/svApplication.qrc
resources/welcome/svWelcomeScreenView.qrc
)
