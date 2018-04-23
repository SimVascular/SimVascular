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

#include "sv4gui_Model.h"

sv4guiModel::sv4guiModel()
    : m_CalculateBoundingBox(true)
    , m_Type("")
    , m_DataModified(false)
{
    this->InitializeEmpty();
}

sv4guiModel::sv4guiModel(const sv4guiModel &other)
    : mitk::BaseData(other)
    , m_Type(other.m_Type)
    , m_ModelElementSet(other.GetTimeSize())
    , m_DataModified(true)
    , m_CalculateBoundingBox(true)
{
    for (std::size_t t = 0; t < other.m_ModelElementSet.size(); ++t)
    {
        if(other.m_ModelElementSet[t])
            m_ModelElementSet[t]=other.m_ModelElementSet[t]->Clone();
//        else
//            m_ModelElementSet.push_back(NULL);
    }
}

sv4guiModel::~sv4guiModel()
{
    this->ClearData();
}

void sv4guiModel::ClearData()
{
    for(int t=0;t<m_ModelElementSet.size();t++)
        delete m_ModelElementSet[t];

    m_ModelElementSet.clear();
    Superclass::ClearData();
}

void sv4guiModel::InitializeEmpty()
{
    if (!m_ModelElementSet.empty())
      this->ClearData();

    m_ModelElementSet.resize( 1 );
    Superclass::InitializeTimeGeometry(1);
    m_Initialized = true;
}

bool sv4guiModel::IsEmptyTimeStep(unsigned int t) const
{
//    return IsInitialized() && (GetModelElement(t) == NULL);

//    if(!IsInitialized())
//        return false;

//    return GetModelElement(t) == NULL || GetModelElement(t)->GetWholeVtkPolyData() == NULL || (
//                GetModelElement(t)->GetWholeVtkPolyData()->GetNumberOfLines() == 0 &&
//                GetModelElement(t)->GetWholeVtkPolyData()->GetNumberOfPolys() == 0 &&
//                GetModelElement(t)->GetWholeVtkPolyData()->GetNumberOfStrips() == 0 &&
//                GetModelElement(t)->GetWholeVtkPolyData()->GetNumberOfVerts() == 0
//                );
    return false;
}

void sv4guiModel::Expand(unsigned int timeSteps)
{
    unsigned int oldSize = m_ModelElementSet.size();

    if ( timeSteps > oldSize )
    {
        Superclass::Expand( timeSteps );

        m_ModelElementSet.resize( timeSteps );

        m_CalculateBoundingBox = true;

        this->InvokeEvent( sv4guiModelExtendTimeRangeEvent() );
    }

}

unsigned int sv4guiModel::GetTimeSize() const
{
    return m_ModelElementSet.size();
}

sv4guiModelElement* sv4guiModel::GetModelElement(unsigned int t) const
{
    if ( t < m_ModelElementSet.size() )
    {
        return m_ModelElementSet[t];
    }
    else
    {
        return NULL;
    }
}

void sv4guiModel::SetModelElement(sv4guiModelElement* modelElement, unsigned int t)
{
    if(t<m_ModelElementSet.size())
    {
        m_ModelElementSet[t]=modelElement;

        m_CalculateBoundingBox = true;

        this->Modified();
        this->UpdateOutputInformation();
        this->InvokeEvent( sv4guiModelSetEvent() );
    }
}

void sv4guiModel::ExecuteOperation( mitk::Operation* operation )
{
    int timeStep = -1;

    sv4guiModelOperation* modelOperation = dynamic_cast<sv4guiModelOperation*>(operation);

    if ( modelOperation )
    {
        timeStep = modelOperation->GetTimeStep();

    }else{
        MITK_ERROR << "No valid Model Operation for sv4guiModel" << std::endl;
        return;
    }

    if ( timeStep < 0 )
    {
        MITK_ERROR << "Time step (" << timeStep << ") outside of sv4guiModel time bounds" << std::endl;
        return;
    }

    //sv4guiModelElement* originalModelElement=m_ModelElementSet[timeStep];

    sv4guiModelElement* newModelElement=modelOperation->GetModelElement();
    vtkSmartPointer<vtkPolyData> newVpd=modelOperation->GetVtkPolyData();

    switch (operation->GetOperationType())
    {

    case sv4guiModelOperation::OpSETMODELELEMENT:
    {
        SetModelElement(newModelElement,timeStep);
        m_DataModified=true;
    }
        break;

    case sv4guiModelOperation::OpSETVTKPOLYDATA:
    {
        if(GetModelElement(timeStep)==NULL)
            return;

        GetModelElement(timeStep)->SetWholeVtkPolyData(newVpd);

        m_CalculateBoundingBox = true;
        m_DataModified=true;
        this->Modified();
        this->UpdateOutputInformation();
        this->InvokeEvent( sv4guiModelSetVtkPolyDataEvent() );
    }
        break;

    default:
        itkWarningMacro("sv4guiModel could not understrand the operation. Please check!");
        break;
    }

    mitk::OperationEndEvent endevent(operation);
    ((const itk::Object*)this)->InvokeEvent(endevent);

}


void sv4guiModel::CalculateBoundingBox(double *bounds,unsigned int t)
{
    sv4guiModelElement* modelElement=GetModelElement(t);
    if(modelElement)
    {
        modelElement->CalculateBoundingBox(bounds);
    }
}

void sv4guiModel::SetType(std::string type)
{
    m_Type=type;
}

std::string sv4guiModel::GetType() const
{
    return m_Type;
}

void sv4guiModel::UpdateOutputInformation()
{
    if ( this->GetSource( ) )
    {
        this->GetSource( )->UpdateOutputInformation( );
    }

    mitk::TimeGeometry* timeGeometry = GetTimeGeometry();
    if ( timeGeometry->CountTimeSteps() != m_ModelElementSet.size() )
    {
        itkExceptionMacro(<<"timeGeometry->CountTimeSteps() != m_ModelElementSet.size() -- use Initialize(timeSteps) with correct number of timeSteps!");
    }

    if (m_CalculateBoundingBox)
    {
        for ( unsigned int t = 0 ; t < m_ModelElementSet.size() ; ++t )
        {
            double bounds[6] = {0};
            CalculateBoundingBox(bounds,t);
            this->GetGeometry(t)->SetFloatBounds(bounds);
        }

        m_CalculateBoundingBox = false;
    }

    this->GetTimeGeometry()->Update();
}

void sv4guiModel::SetRequestedRegionToLargestPossibleRegion()
{
}

bool sv4guiModel::RequestedRegionIsOutsideOfTheBufferedRegion()
{
    return false;
}

bool sv4guiModel::VerifyRequestedRegion()
{
    return true;
}

void sv4guiModel::SetRequestedRegion(const DataObject * )
{
}

void sv4guiModel::PrintSelf( std::ostream& os, itk::Indent indent ) const
{
    Superclass::PrintSelf(os, indent);

    os << indent << "Number timesteps: " << m_ModelElementSet.size() << "\n";

    for ( unsigned int t = 0 ; t < m_ModelElementSet.size() ; ++t )
    {
        os << indent << "Timestep " << t << ": \n";
        itk::Indent nextIndent = indent.GetNextIndent();

        if(m_ModelElementSet[t])
        {
            os << nextIndent << "Model Type: " << m_ModelElementSet[t]->GetType() << "\n";
            if(m_ModelElementSet[t]->GetType()=="PolyData" && m_ModelElementSet[t]->GetWholeVtkPolyData())
            {
                os << nextIndent << "Number of cells: " << m_ModelElementSet[t]->GetWholeVtkPolyData()->GetNumberOfCells() << "\n";
                os << nextIndent << "Number of points: " << m_ModelElementSet[t]->GetWholeVtkPolyData()->GetNumberOfPoints() << "\n";
            }
        }else
        {
            os << nextIndent << "No Model! \n";
        }
    }
}

