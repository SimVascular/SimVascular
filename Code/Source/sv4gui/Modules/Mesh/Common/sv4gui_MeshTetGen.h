/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef SV4GUI_MESHTETGEN_H
#define SV4GUI_MESHTETGEN_H

#include <sv4guiModuleMeshExports.h>

#include <sv4gui_Mesh.h>

#include <sv_TetGenMeshObject.h>

class SV4GUIMODULEMESH_EXPORT sv4guiMeshTetGen : public sv4guiMesh
{
public:

    sv4guiMeshTetGen();

    sv4guiMeshTetGen(const sv4guiMeshTetGen &other);

    virtual ~sv4guiMeshTetGen();

    sv4guiMeshTetGen* Clone() override;

    void InitNewMesher() override;

    bool SetModelElement(sv4guiModelElement* modelElement) override;

    bool Execute(std::string flag, double values[20], std::string strValues[5], bool option, std::string& msg) override;

    bool ParseCommand(std::string cmd, std::string& flag, double values[20], std::string strValues[5], bool& option, std::string& msg) override;

    cvTetGenMeshObject* GetMesher();

    static sv4guiMesh* CreateMesh();

//    bool WriteMeshComplete(vtkSmartPointer<vtkPolyData> surfaceMesh, vtkSmartPointer<vtkUnstructuredGrid> volumeMesh, sv4guiModelElement* modelElement, std::string meshDir) override;

//    bool WriteMeshComplete(std::string meshDir) override;

  protected:

    cvTetGenMeshObject* m_cvTetGenMesh;

  };


#endif // sv4guiMeshTetGen_H
