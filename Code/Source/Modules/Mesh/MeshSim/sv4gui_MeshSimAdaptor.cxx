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

#include "sv4gui_MeshSimAdaptor.h"


sv4guiMeshSimAdaptor::sv4guiMeshSimAdaptor()
    : m_cvMeshSimMesh(new cvMeshSimMeshObject(NULL))
    , m_cvMeshSimAdaptor(new cvMeshSimAdapt())
{
    m_Type="MeshSim";
    m_cvMeshSimAdaptor->SetMeshObject(m_cvMeshSimMesh);
}

sv4guiMeshSimAdaptor::~sv4guiMeshSimAdaptor()
{
    if(m_cvMeshSimMesh!=NULL)
        delete m_cvMeshSimMesh;

    if(m_cvMeshSimAdaptor!=NULL)
    {
        m_cvMeshSimAdaptor->SetMeshObject(NULL);
        delete m_cvMeshSimAdaptor;
    }
}

bool sv4guiMeshSimAdaptor::SetModelElement(sv4guiModelElement *modelElement)
{
    m_ModelElement=modelElement;
    if(m_cvMeshSimMesh && modelElement && modelElement->GetInnerSolid())
        if(m_cvMeshSimMesh->LoadModel(modelElement->GetInnerSolid())==SV_OK)
            return true;

    return false;
}

bool sv4guiMeshSimAdaptor::LoadMesh(std::string filePath)
{
    return (m_cvMeshSimAdaptor && m_cvMeshSimAdaptor->LoadMesh(const_cast<char*>(filePath.c_str()))==SV_OK);
}

bool sv4guiMeshSimAdaptor::SetAdaptOptions(std::string flag, double value)
{
    return m_cvMeshSimAdaptor && m_cvMeshSimAdaptor->SetAdaptOptions(const_cast<char*>(flag.c_str()),value)==SV_OK;
}

bool sv4guiMeshSimAdaptor::Adapt()
{
    return m_cvMeshSimAdaptor
            && m_cvMeshSimAdaptor->SetMetric(NULL,-1,-1)==SV_OK
            && m_cvMeshSimAdaptor->SetupMesh()==SV_OK
            && m_cvMeshSimAdaptor->RunAdaptor()==SV_OK
            && m_cvMeshSimAdaptor->GetAdaptedMesh()==SV_OK
            && m_cvMeshSimAdaptor->TransferSolution()==SV_OK;
}

bool sv4guiMeshSimAdaptor::WriteAdaptedSolution(std::string filePath)
{
    return m_cvMeshSimAdaptor && m_cvMeshSimAdaptor->WriteAdaptedSolution(const_cast<char*>(filePath.c_str()))==SV_OK;
}

sv4guiMeshSim* sv4guiMeshSimAdaptor::GetAdaptedMesh()
{
    if(m_cvMeshSimMesh)
    {
        cvUnstructuredGrid* cvug=m_cvMeshSimMesh->GetUnstructuredGrid();
        vtkUnstructuredGrid* volumeMesh=NULL;
        if(cvug)
            volumeMesh=cvug->GetVtkUnstructuredGrid();

        if(volumeMesh)
        {
            vtkSmartPointer<vtkPolyData> surf=sv4guiMeshSim::CreateSurfaceMeshContainingModelFaceIDs(m_ModelElement,m_cvMeshSimMesh);
            vtkSmartPointer<vtkUnstructuredGrid> vol=vtkSmartPointer<vtkUnstructuredGrid>::New();
            vol->DeepCopy(volumeMesh);

            sv4guiMeshSim* adaptedMesh=new sv4guiMeshSim();
            adaptedMesh->SetSurfaceMesh(surf);
            adaptedMesh->SetVolumeMesh(vol);

            delete cvug;

            return adaptedMesh;
        }
    }

    return NULL;
}

bool sv4guiMeshSimAdaptor::WriteAdaptedMesh(std::string filePath)
{
    return m_cvMeshSimAdaptor && m_cvMeshSimAdaptor->WriteAdaptedMesh(const_cast<char*>(filePath.c_str()))==SV_OK;
}

sv4guiMeshAdaptor* sv4guiMeshSimAdaptor::CreateAdaptor()
{
    return new sv4guiMeshSimAdaptor();
}
