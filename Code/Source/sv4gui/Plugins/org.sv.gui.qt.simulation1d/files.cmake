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
    sv4gui_SimJobCreate1d.cxx
    sv4gui_SimJobCreateAction1d.cxx
    sv4gui_CapBCWidget1d.cxx
    sv4gui_SplitBCWidget1d.cxx
    sv4gui_TableCapDelegate1d.cxx
    sv4gui_TableSolverDelegate1d.cxx
    sv4gui_SimulationExtractCenterlines1d.cxx
    sv4gui_SimulationLinesContainer.cxx
    sv4gui_SimulationLinesMapper.cxx
    sv4gui_SimulationView1d.cxx
    sv4gui_SimulationPreferencePage1d.cxx
    sv4gui_SimulationPython1d.cxx
    sv4gui_SimulationPythonConvert1d.cxx
    sv4gui_SimJobStopAction1d.cxx
    sv4gui_SimJobExportAction1d.cxx
    sv4gui_SimulationPluginActivator1d.cxx
    sv4gui_ProcessHandler1d.cxx
    sv4gui_SolverProcessHandler1d.cxx
)

set(MOC_H_FILES
    sv4gui_SimJobCreate1d.h
    sv4gui_SimJobCreateAction1d.h
    sv4gui_CapBCWidget1d.h
    sv4gui_SplitBCWidget1d.h
    sv4gui_TableCapDelegate1d.h
    sv4gui_TableSolverDelegate1d.h
    sv4gui_SimulationExtractCenterlines1d.h
    sv4gui_SimulationLinesContainer.h
    sv4gui_SimulationLinesMapper.h
    sv4gui_SimulationView1d.h
    sv4gui_SimulationPreferencePage1d.h
    sv4gui_SimulationPython1d.h
    sv4gui_SimulationPythonConvert1d.h
    sv4gui_SimJobStopAction1d.h
    sv4gui_SimJobExportAction1d.h
    sv4gui_SimulationPluginActivator1d.h
    sv4gui_ProcessHandler1d.h
    sv4gui_SolverProcessHandler1d.h
)

set(UI_FILES
    sv4gui_SimJobCreate1d.ui
    sv4gui_CapBCWidget1d.ui
    sv4gui_SplitBCWidget1d.ui
    sv4gui_SimulationPreferencePage1d.ui
    sv4gui_SimulationView1d.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/simulation1d.png
)

set(QRC_FILES
  resources/simulation1d.qrc
)
