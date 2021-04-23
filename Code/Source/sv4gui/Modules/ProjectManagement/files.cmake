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
    sv4gui_DataFolder.h
    sv4gui_ProjectFolder.h
    sv4gui_ImageFolder.h
    sv4gui_PathFolder.h
    sv4gui_SegmentationFolder.h
    sv4gui_svFSIFolder.h
    sv4gui_ModelFolder.h
    sv4gui_MeshFolder.h
    sv4gui_SimulationFolder.h
    sv4gui_ROMSimulationFolder.h
    sv4gui_RepositoryFolder.h
    sv4gui_ProjectManager.h
    sv4gui_DataNodeOperation.h
    sv4gui_DataNodeOperationInterface.h
    
)

set(CPP_FILES
    sv4gui_DataFolder.cxx
    sv4gui_ProjectManager.cxx
    sv4gui_DataNodeOperation.cxx
    sv4gui_DataNodeOperationInterface.cxx
    sv4gui_ImageFolder.cxx
    sv4gui_MeshFolder.cxx
    sv4gui_ModelFolder.cxx
    sv4gui_PathFolder.cxx
    sv4gui_RepositoryFolder.cxx
    sv4gui_ProjectFolder.cxx
    sv4gui_SegmentationFolder.cxx
    sv4gui_SimulationFolder.cxx
    sv4gui_ROMSimulationFolder.cxx
    sv4gui_svFSIFolder.cxx
)

