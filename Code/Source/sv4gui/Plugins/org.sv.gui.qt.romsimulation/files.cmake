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
    sv4gui_ConvertProcessHandlerROM.cxx
    sv4gui_ConvertWorkerROM.cxx
    sv4gui_ROMSimJobCreate.cxx
    sv4gui_ROMSimJobCreateAction.cxx
    sv4gui_CapBCWidgetROM.cxx
    sv4gui_SplitBCWidgetROM.cxx
    sv4gui_TableCapDelegateROM.cxx
    sv4gui_TableSolverDelegateROM.cxx
    sv4gui_ROMSimulationExtractCenterlines.cxx
    sv4gui_ROMSimulationLinesContainer.cxx
    sv4gui_ROMSimulationLinesMapper.cxx
    sv4gui_ROMSimulationView.cxx
    sv4gui_ROMSimulationPreferencePage.cxx
    sv4gui_ROMSimulationPython.cxx
    sv4gui_ROMSimulationPythonConvert.cxx
    sv4gui_ROMSimJobStopAction.cxx
    sv4gui_ROMSimJobExportAction.cxx
    sv4gui_ROMSimulationPluginActivator.cxx
    sv4gui_ProcessHandlerROM.cxx
    sv4gui_SolverProcessHandlerROM.cxx
)

set(MOC_H_FILES
    sv4gui_ConvertProcessHandlerROM.h
    sv4gui_ConvertWorkerROM.h
    sv4gui_ROMSimJobCreate.h
    sv4gui_ROMSimJobCreateAction.h
    sv4gui_CapBCWidgetROM.h
    sv4gui_SplitBCWidgetROM.h
    sv4gui_TableCapDelegateROM.h
    sv4gui_TableSolverDelegateROM.h
    sv4gui_ROMSimulationExtractCenterlines.h
    sv4gui_ROMSimulationLinesContainer.h
    sv4gui_ROMSimulationLinesMapper.h
    sv4gui_ROMSimulationView.h
    sv4gui_ROMSimulationPreferencePage.h
    sv4gui_ROMSimulationPython.h
    sv4gui_ROMSimulationPythonConvert.h
    sv4gui_ROMSimJobStopAction.h
    sv4gui_ROMSimJobExportAction.h
    sv4gui_ROMSimulationPluginActivator.h
    sv4gui_ProcessHandlerROM.h
    sv4gui_SolverProcessHandlerROM.h
)

set(UI_FILES
    sv4gui_ROMSimJobCreate.ui
    sv4gui_CapBCWidgetROM.ui
    sv4gui_SplitBCWidgetROM.ui
    sv4gui_ROMSimulationPreferencePage.ui
    sv4gui_ROMSimulationView.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/romsimulation.png
)

set(QRC_FILES
  resources/romsimulation.qrc
)
