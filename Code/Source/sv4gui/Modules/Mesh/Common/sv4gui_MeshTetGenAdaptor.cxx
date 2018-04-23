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

#include "sv4gui_MeshTetGenAdaptor.h"


sv4guiMeshTetGenAdaptor::sv4guiMeshTetGenAdaptor()
    : m_cvTetGetMesh(new cvTetGenMeshObject(NULL))
    , m_cvTetGenAdaptor(new cvTetGenAdapt())
{
    m_Type="TetGen";
    m_cvTetGenAdaptor->SetMeshObject(m_cvTetGetMesh);
}

sv4guiMeshTetGenAdaptor::~sv4guiMeshTetGenAdaptor()
{
    if(m_cvTetGetMesh!=NULL)
        delete m_cvTetGetMesh;

    if(m_cvTetGenAdaptor!=NULL)
    {
        m_cvTetGenAdaptor->SetMeshObject(NULL);
        delete m_cvTetGenAdaptor;
    }
}

bool sv4guiMeshTetGenAdaptor::SetModelElement(sv4guiModelElement *modelElement)
{
    if(m_cvTetGenAdaptor && modelElement && modelElement->GetWholeVtkPolyData())
        if(m_cvTetGenAdaptor->LoadModel(modelElement->GetWholeVtkPolyData())==SV_OK)
            return true;

    return false;
}

bool sv4guiMeshTetGenAdaptor::LoadMesh(std::string filePath)
{
    return (m_cvTetGenAdaptor && m_cvTetGenAdaptor->LoadMesh(const_cast<char*>(filePath.c_str()))==SV_OK);
}

bool sv4guiMeshTetGenAdaptor::SetAdaptOptions(std::string flag, double value)
{
    return m_cvTetGenAdaptor && m_cvTetGenAdaptor->SetAdaptOptions(const_cast<char*>(flag.c_str()),value)==SV_OK;
}

bool sv4guiMeshTetGenAdaptor::Adapt()
{
    return m_cvTetGenAdaptor
            && m_cvTetGenAdaptor->SetMetric(NULL,-1,-1)==SV_OK
            && m_cvTetGenAdaptor->SetupMesh()==SV_OK
            && m_cvTetGenAdaptor->RunAdaptor()==SV_OK
            && m_cvTetGenAdaptor->GetAdaptedMesh()==SV_OK
            && m_cvTetGenAdaptor->TransferSolution()==SV_OK;
}

bool sv4guiMeshTetGenAdaptor::WriteAdaptedSolution(std::string filePath)
{
    return m_cvTetGenAdaptor && m_cvTetGenAdaptor->WriteAdaptedSolution(const_cast<char*>(filePath.c_str()))==SV_OK;
}

sv4guiMeshTetGen* sv4guiMeshTetGenAdaptor::GetAdaptedMesh()
{
    if(m_cvTetGetMesh)
    {
        vtkPolyData* surfaceMesh=m_cvTetGetMesh->GetPolyData()->GetVtkPolyData();
        vtkUnstructuredGrid* volumeMesh=m_cvTetGetMesh->GetUnstructuredGrid()->GetVtkUnstructuredGrid();
        if(surfaceMesh && volumeMesh)
        {
            vtkSmartPointer<vtkPolyData> surf=vtkSmartPointer<vtkPolyData>::New();
            vtkSmartPointer<vtkUnstructuredGrid> vol=vtkSmartPointer<vtkUnstructuredGrid>::New();
            surf->DeepCopy(surfaceMesh);
            vol->DeepCopy(volumeMesh);

            sv4guiMeshTetGen* adaptedMesh=new sv4guiMeshTetGen();
            adaptedMesh->SetSurfaceMesh(surf);
            adaptedMesh->SetVolumeMesh(vol);

            return adaptedMesh;
        }
    }

    return NULL;
}

bool sv4guiMeshTetGenAdaptor::WriteAdaptedMesh(std::string filePath)
{
    return m_cvTetGenAdaptor && m_cvTetGenAdaptor->WriteAdaptedMesh(const_cast<char*>(filePath.c_str()))==SV_OK;
}

sv4guiMeshAdaptor* sv4guiMeshTetGenAdaptor::CreateAdaptor()
{
    return new sv4guiMeshTetGenAdaptor();
}
