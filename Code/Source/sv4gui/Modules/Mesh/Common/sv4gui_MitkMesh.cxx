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

#include "sv4gui_MitkMesh.h"

sv4guiMitkMesh::sv4guiMitkMesh()
    : m_CalculateBoundingBox(true)
    , m_Type("")
    , m_ModelName("")
    , m_DataModified(false)
{
    this->InitializeEmpty();
}

sv4guiMitkMesh::sv4guiMitkMesh(const sv4guiMitkMesh &other)
    : mitk::BaseData(other)
    , m_Type(other.m_Type)
    , m_ModelName(other.m_ModelName)
    , m_MeshSet(other.GetTimeSize())
    , m_DataModified(true)
    , m_CalculateBoundingBox(true)
{
    for (std::size_t t = 0; t < other.m_MeshSet.size(); ++t)
    {
        if(other.m_MeshSet[t])
            m_MeshSet[t]=other.m_MeshSet[t]->Clone();
//        else
//            m_MeshSet.push_back(NULL);
    }
}

sv4guiMitkMesh::~sv4guiMitkMesh()
{
    this->ClearData();
}

void sv4guiMitkMesh::ClearData()
{
    for(int t=0;t<m_MeshSet.size();t++)
    {
        if(m_MeshSet[t])
            delete m_MeshSet[t];
    }
    m_MeshSet.clear();
    Superclass::ClearData();
}

void sv4guiMitkMesh::InitializeEmpty()
{
    if (!m_MeshSet.empty())
        this->ClearData();

    m_MeshSet.resize( 1 );
    Superclass::InitializeTimeGeometry(1);
    m_Initialized = true;
}

bool sv4guiMitkMesh::IsEmptyTimeStep(unsigned int t) const
{
//    return IsInitialized() && (GetMesh(t) == NULL);

//    if(!IsInitialized())
//        return false;

//    return GetMesh(t) == NULL || GetMesh(t)->GetSurfaceMesh() == NULL || (
//                GetMesh(t)->GetSurfaceMesh()->GetNumberOfLines() == 0 &&
//                GetMesh(t)->GetSurfaceMesh()->GetNumberOfPolys() == 0 &&
//                GetMesh(t)->GetSurfaceMesh()->GetNumberOfStrips() == 0 &&
//                GetMesh(t)->GetSurfaceMesh()->GetNumberOfVerts() == 0
//                );

    return false;
}

void sv4guiMitkMesh::Expand(unsigned int timeSteps)
{
    unsigned int oldSize = m_MeshSet.size();

    if ( timeSteps > oldSize )
    {
        Superclass::Expand( timeSteps );

        m_MeshSet.resize( timeSteps );

        m_CalculateBoundingBox = true;

        this->InvokeEvent( sv4guiMitkMeshExtendTimeRangeEvent() );
    }

}

unsigned int sv4guiMitkMesh::GetTimeSize() const
{
    return m_MeshSet.size();
}

sv4guiMesh* sv4guiMitkMesh::GetMesh(unsigned int t) const
{
    if ( t < m_MeshSet.size() )
    {
        return m_MeshSet[t];
    }
    else
    {
        return NULL;
    }
}

void sv4guiMitkMesh::SetMesh(sv4guiMesh* mesh, unsigned int t)
{
    if(t<m_MeshSet.size())
    {
        m_MeshSet[t]=mesh;

        m_CalculateBoundingBox = true;

        this->Modified();
        this->UpdateOutputInformation();
        this->InvokeEvent( sv4guiMitkMeshSetEvent() );
    }
}

void sv4guiMitkMesh::ExecuteOperation( mitk::Operation* operation )
{
    int timeStep = -1;

    sv4guiMitkMeshOperation* meshOperation = dynamic_cast<sv4guiMitkMeshOperation*>(operation);

    if ( meshOperation )
    {
        timeStep = meshOperation->GetTimeStep();

    }else{
        MITK_ERROR << "No valid Mesh Operation for sv4guiMitkMesh" << std::endl;
        return;
    }

    if ( timeStep < 0 )
    {
        MITK_ERROR << "Time step (" << timeStep << ") outside of sv4guiMitkMesh time bounds" << std::endl;
        return;
    }

    //sv4guiMesh* originalMesh=m_MeshSet[timeStep];
    sv4guiMesh* newMesh=meshOperation->GetMesh();

    switch (operation->GetOperationType())
    {

    case sv4guiMitkMeshOperation::OpSETMESH:
    {
        SetMesh(newMesh,timeStep);
        m_DataModified=true;
    }
        break;

    default:
        itkWarningMacro("sv4guiMitkMesh could not understrand the operation. Please check!");
        break;
    }

    mitk::OperationEndEvent endevent(operation);
    ((const itk::Object*)this)->InvokeEvent(endevent);
}

void sv4guiMitkMesh::CalculateBoundingBox(double *bounds,unsigned int t)
{
    sv4guiMesh* mesh=GetMesh(t);
    if(mesh)
    {
        mesh->CalculateBoundingBox(bounds);
    }
}

void sv4guiMitkMesh::SetType(std::string type)
{
    m_Type=type;
}

std::string sv4guiMitkMesh::GetType() const
{
    return m_Type;
}

std::string sv4guiMitkMesh::GetModelName() const
{
    return m_ModelName;
}

void sv4guiMitkMesh::SetModelName(std::string name)
{
    m_ModelName=name;
}

void sv4guiMitkMesh::UpdateOutputInformation()
{
    if ( this->GetSource( ) )
    {
        this->GetSource( )->UpdateOutputInformation( );
    }

    mitk::TimeGeometry* timeGeometry = GetTimeGeometry();
    if ( timeGeometry->CountTimeSteps() != m_MeshSet.size() )
    {
        itkExceptionMacro(<<"timeGeometry->CountTimeSteps() != m_MeshSet.size() -- use Initialize(timeSteps) with correct number of timeSteps!");
    }

    if (m_CalculateBoundingBox)
    {
        for ( unsigned int t = 0 ; t < m_MeshSet.size() ; ++t )
        {
            double bounds[6] = {0};
            CalculateBoundingBox(bounds,t);
            this->GetGeometry(t)->SetFloatBounds(bounds);
        }

        m_CalculateBoundingBox = false;
    }

    this->GetTimeGeometry()->Update();
}

void sv4guiMitkMesh::SetRequestedRegionToLargestPossibleRegion()
{
}

bool sv4guiMitkMesh::RequestedRegionIsOutsideOfTheBufferedRegion()
{
    return false;
}

bool sv4guiMitkMesh::VerifyRequestedRegion()
{
    return true;
}

void sv4guiMitkMesh::SetRequestedRegion(const DataObject * )
{
}

void sv4guiMitkMesh::PrintSelf( std::ostream& os, itk::Indent indent ) const
{
    Superclass::PrintSelf(os, indent);

    os << indent << "Number timesteps: " << m_MeshSet.size() << "\n";

    for ( unsigned int t = 0 ; t < m_MeshSet.size() ; ++t )
    {
        os << indent << "Timestep " << t << ": \n";
        itk::Indent nextIndent = indent.GetNextIndent();

        if(m_MeshSet[t] && m_MeshSet[t]->GetVolumeMesh())
        {
            os << nextIndent << "Mesh Type: " << m_MeshSet[t]->GetType() << "\n";
                os << nextIndent << "Number of cells: " << m_MeshSet[t]->GetVolumeMesh()->GetNumberOfCells() << "\n";
                os << nextIndent << "Number of points: " << m_MeshSet[t]->GetVolumeMesh()->GetNumberOfPoints() << "\n";
        }else
        {
            os << nextIndent << "No Mesh! \n";
        }
    }
}

