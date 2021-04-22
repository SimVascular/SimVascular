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

# Plug-ins must be ordered according to their dependencies

set(SV_PLUGINS

  org.sv.gui.qt.application:ON

  org.sv.projectdatanodes:ON

  org.sv.pythondatanodes:ON

  org.sv.gui.qt.projectmanager:ON
  org.sv.gui.qt.datamanager:ON
  org.sv.gui.qt.pathplanning:ON
  org.sv.gui.qt.segmentation:ON
  org.sv.gui.qt.modeling:ON
  org.sv.gui.qt.meshing:ON
  org.sv.gui.qt.simulation:ON
  org.sv.gui.qt.romsimulation:ON
  org.sv.gui.qt.svfsi:ON
  org.sv.gui.qt.imageprocessing:ON
)
if(SV_USE_MITK_SEGMENTATION)
    LIST(APPEND SV_PLUGINS org.sv.gui.qt.mitksegmentation:ON)
endif()
