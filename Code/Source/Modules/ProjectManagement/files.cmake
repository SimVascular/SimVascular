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

set(H_FILES
    sv3gui_DataFolder.h
    sv3gui_ProjectFolder.h
    sv3gui_ImageFolder.h
    sv3gui_PathFolder.h
    sv3gui_SegmentationFolder.h
    sv3gui_ModelFolder.h
    sv3gui_MeshFolder.h
    sv3gui_SimulationFolder.h
    sv3gui_ProjectManager.h
    sv3gui_DataNodeOperation.h
    sv3gui_DataNodeOperationInterface.h
)

set(CPP_FILES
    sv3gui_DataFolder.cxx
    sv3gui_ProjectManager.cxx
    sv3gui_DataNodeOperation.cxx
    sv3gui_DataNodeOperationInterface.cxx
    sv3gui_ImageFolder.cxx
    sv3gui_MeshFolder.cxx
    sv3gui_ModelFolder.cxx
    sv3gui_PathFolder.cxx
    sv3gui_ProjectFolder.cxx
    sv3gui_SegmentationFolder.cxx
    sv3gui_SimulationFolder.cxx
)

