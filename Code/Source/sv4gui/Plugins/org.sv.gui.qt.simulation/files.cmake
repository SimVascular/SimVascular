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
    sv4gui_SimJobCreate.cxx
    sv4gui_SimJobCreateAction.cxx
    sv4gui_CapBCWidget.cxx
    sv4gui_SplitBCWidget.cxx
    sv4gui_TableCapDelegate.cxx
    sv4gui_TableSolverDelegate.cxx
    sv4gui_SimulationView.cxx
    sv4gui_SimulationPreferencePage.cxx
    sv4gui_MPIPreferencePage.cxx
    sv4gui_SimJobStopAction.cxx
    sv4gui_SimJobExportAction.cxx
    sv4gui_SimulationPluginActivator.cxx
    sv4gui_ProcessHandler.cxx
    sv4gui_SolverProcessHandler.cxx
    sv4gui_SimulationPreferences.cxx
    sv4gui_MPIPreferences.cxx
)

if(SV_USE_WIN32_REGISTRY)
  LIST(APPEND CPP_FILES sv4gui_win32_use_registry.cxx)
endif()

set(MOC_H_FILES
    sv4gui_SimJobCreate.h
    sv4gui_SimJobCreateAction.h
    sv4gui_CapBCWidget.h
    sv4gui_SplitBCWidget.h
    sv4gui_TableCapDelegate.h
    sv4gui_TableSolverDelegate.h
    sv4gui_SimulationView.h
    sv4gui_SimulationPreferencePage.h
    sv4gui_MPIPreferencePage.h
    sv4gui_SimJobStopAction.h
    sv4gui_SimJobExportAction.h
    sv4gui_SimulationPluginActivator.h
    sv4gui_ProcessHandler.h
    sv4gui_SolverProcessHandler.h
    sv4gui_SimulationPreferences.h
    sv4gui_MPIPreferences.h
)

set(UI_FILES
    sv4gui_SimJobCreate.ui
    sv4gui_CapBCWidget.ui
    sv4gui_SplitBCWidget.ui
    sv4gui_SimulationPreferencePage.ui
    sv4gui_SimulationView.ui
    sv4gui_MPIPreferencePage.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/simulation.png
)

set(QRC_FILES
  resources/simulation.qrc
)
