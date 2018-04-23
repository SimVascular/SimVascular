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

#include "sv4gui_MeshLoadSurfaceAction.h"

#include "sv4gui_MitkMesh.h"
#include "sv4gui_MitkMeshIO.h"

#include <QMessageBox>

sv4guiMeshLoadSurfaceAction::sv4guiMeshLoadSurfaceAction()
{
}

sv4guiMeshLoadSurfaceAction::~sv4guiMeshLoadSurfaceAction()
{
}

void sv4guiMeshLoadSurfaceAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer meshNode = selectedNodes[0];

    sv4guiMitkMesh* mitkMesh=dynamic_cast<sv4guiMitkMesh*>(meshNode->GetData());
    if(!mitkMesh) return;

    sv4guiMesh* mesh=mitkMesh->GetMesh();
    if(!mesh) return;

    if(mesh->GetSurfaceMesh()==NULL)
    {
        if (QMessageBox::question(NULL, "Load Surface Mesh from File", "Do you want to load surface mesh from file if applicable?",
                                  QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
        {
          return;
        }

        std::string path="";
        meshNode->GetStringProperty("path",path);
        if(path=="")
            return;

        std::string surfaceFileName = path+"/"+meshNode->GetName()+".vtp";
        mesh->ReadSurfaceFile(surfaceFileName);
    }
    else
    {
        if (QMessageBox::question(NULL, "Unload Surface/Volume Mesh", "Do you want to unload surface and volume mesh?",
                                  QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
        {
          return;
        }

        mesh->SetSurfaceMesh(NULL);
        mesh->SetVolumeMesh(NULL);
    }

    mitkMesh->SetMesh(mesh);
}

void sv4guiMeshLoadSurfaceAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}
