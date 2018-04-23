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
  sv4gui_berrySingleNodeSelection.cxx
  sv4gui_QmitkDataManagerView.cxx
  sv4gui_QmitkDataManagerPreferencePage.cxx
  sv4gui_QmitkDataManagerHotkeysPrefPage.cxx
  sv4gui_DataManagerPluginActivator.cxx
  sv4gui_QmitkNodeTableViewKeyFilter.cxx
  sv4gui_QmitkInfoDialog.cxx
  sv4gui_QmitkDataManagerItemDelegate.cxx
  sv4gui_QmitkFunctionality.cxx
)

set(MOC_H_FILES
  sv4gui_QmitkDataManagerView.h
  sv4gui_QmitkDataManagerPreferencePage.h
  sv4gui_QmitkDataManagerHotkeysPrefPage.h
  sv4gui_QmitkNodeTableViewKeyFilter.h
  sv4gui_QmitkInfoDialog.h
  sv4gui_QmitkDataManagerItemDelegate.h
  sv4gui_DataManagerPluginActivator.h
  sv4gui_mitkIContextMenuAction.h
  sv4gui_QmitkFunctionality.h
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/DataManager_48.png
)

set(QRC_FILES
  resources/svdatamanager.qrc
)
