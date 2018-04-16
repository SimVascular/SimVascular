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
    sv3gui_Mesh.h
    sv3gui_MeshTetGen.h
    sv3gui_MitkMesh.h
    sv3gui_MeshFactory.h
    sv3gui_RegisterTetGenFunction.h
    sv3gui_MitkMeshOperation.h
    sv3gui_MitkMeshMapper3D.h
    sv3gui_MitkMeshMapper2D.h
    sv3gui_MitkMeshIO.h
    sv3gui_MeshLegacyIO.h
    sv3gui_MeshAdaptor.h
    sv3gui_MeshTetGenAdaptor.h
    sv3gui_MitkMeshObjectFactory.h
)

set(CPP_FILES
    sv3gui_Mesh.cxx
    sv3gui_MeshTetGen.cxx
    sv3gui_MeshFactory.cxx
    sv3gui_RegisterTetGenFunction.cxx
    sv3gui_MitkMesh.cxx
    sv3gui_MitkMeshOperation.cxx
    sv3gui_MitkMeshMapper3D.cxx
    sv3gui_MitkMeshMapper2D.cxx
    sv3gui_MitkMeshIO.cxx
    sv3gui_MeshLegacyIO.cxx
    sv3gui_MeshTetGenAdaptor.cxx
    sv3gui_MitkMeshObjectFactory.cxx
    sv3gui_MeshAdaptor.cxx
)

set(RESOURCE_FILES

)
