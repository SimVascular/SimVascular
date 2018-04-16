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
    sv3gui_SimJobCreate.cxx
    sv3gui_SimJobCreateAction.cxx
    sv3gui_CapBCWidget.cxx
    sv3gui_SplitBCWidget.cxx
    sv3gui_TableCapDelegate.cxx
    sv3gui_TableSolverDelegate.cxx
    sv3gui_SimulationView.cxx
    sv3gui_SimulationPreferencePage.cxx
    sv3gui_SimJobStopAction.cxx
    sv3gui_SimJobExportAction.cxx
    sv3gui_SimulationPluginActivator.cxx
    sv3gui_ProcessHandler.cxx
    sv3gui_SolverProcessHandler.cxx
)

set(MOC_H_FILES
    src/internal/sv3gui_SimJobCreate.h
    src/internal/sv3gui_SimJobCreateAction.h
    src/internal/sv3gui_CapBCWidget.h
    src/internal/sv3gui_SplitBCWidget.h
    src/internal/sv3gui_TableCapDelegate.h
    src/internal/sv3gui_TableSolverDelegate.h
    src/internal/sv3gui_SimulationView.h
    src/internal/sv3gui_SimulationPreferencePage.h
    src/internal/sv3gui_SimJobStopAction.h
    src/internal/sv3gui_SimJobExportAction.h
    src/internal/sv3gui_SimulationPluginActivator.h
    src/internal/sv3gui_ProcessHandler.h
    src/internal/sv3gui_SolverProcessHandler.h
)

set(UI_FILES
    src/internal/sv3gui_SimJobCreate.ui
    src/internal/sv3gui_CapBCWidget.ui
    src/internal/sv3gui_SplitBCWidget.ui
    src/internal/sv3gui_SimulationPreferencePage.ui
    src/internal/sv3gui_SimulationView.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/simulation.png
)

set(QRC_FILES
  resources/simulation.qrc
)

set(CPP_FILES )

foreach(file ${SRC_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/${file})
endforeach(file ${SRC_CPP_FILES})

foreach(file ${INTERNAL_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/internal/${file})
endforeach(file ${INTERNAL_CPP_FILES})

