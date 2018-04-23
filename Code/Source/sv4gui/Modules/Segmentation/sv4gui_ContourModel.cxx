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

#include "sv4gui_ContourModel.h"
#include "sv4gui_ContourOperation.h"

sv4guiContourModel::sv4guiContourModel()
    : m_CalculateBoundingBox(true)
{
    this->InitializeEmpty();
}

sv4guiContourModel::sv4guiContourModel(const sv4guiContourModel &other)
    : BaseData(other)
    , m_ContourSet(other.GetTimeSize())
    , m_CalculateBoundingBox(true)
{
    for (std::size_t t = 0; t < other.GetTimeSize(); ++t)
    {
        m_ContourSet[t]=other.GetContour(t)->Clone();
    }
}

sv4guiContourModel::~sv4guiContourModel()
{
    this->ClearData();
}

void sv4guiContourModel::ClearData()
{
    //may need delele each arrays inside first.
    m_ContourSet.clear();
    Superclass::ClearData();
}

void sv4guiContourModel::InitializeEmpty()
{
    m_ContourSet.resize(1);

    Superclass::InitializeTimeGeometry(1);
    m_Initialized = true;
}

bool sv4guiContourModel::IsEmptyTimeStep(unsigned int t) const
{
    return IsInitialized() && (GetContour(t) == NULL);
}

void sv4guiContourModel::Expand( unsigned int timeSteps )
{
    unsigned int oldSize = m_ContourSet.size();

    if ( timeSteps > oldSize )
    {
        Superclass::Expand( timeSteps );

        m_ContourSet.resize( timeSteps );

        m_CalculateBoundingBox = true;

        this->InvokeEvent( sv4guiContourModelExtendTimeRangeEvent() );
    }
}

unsigned int sv4guiContourModel::GetTimeSize() const
{
    return m_ContourSet.size();
}

sv4guiContour* sv4guiContourModel::GetContour(unsigned int t) const
{
    if ( t < m_ContourSet.size())
    {
        return m_ContourSet[t];
    }
    else
    {
        return NULL;
    }
}

void sv4guiContourModel::SetContour(sv4guiContour* contour, unsigned int t)
{
    if(t<m_ContourSet.size())
    {
        m_ContourSet[t]=contour;

        ContoursChanged(t);
        this->InvokeEvent( sv4guiContourModelSetEvent() );
    }
}

void sv4guiContourModel::ControlPointsChanged(unsigned int t)
{
    this->Modified();
}

void sv4guiContourModel::ContoursChanged(unsigned int t)
{
    this->Modified();
}

void sv4guiContourModel::InsertControlPoint(int index, mitk::Point3D point, unsigned int t)
{
    sv4guiContour* contour=GetContour(t);
    if(contour)
    {
        contour->InsertControlPoint(index,point);

        ControlPointsChanged(t);
        this->InvokeEvent( sv4guiContourModelPointInsertEvent() );
    }
}

void sv4guiContourModel::RemoveControlPoint(int index, unsigned int t)
{
    sv4guiContour* contour=GetContour(t);
    if(contour)
    {
        contour->RemoveControlPoint(index);

        ControlPointsChanged(t);
        this->InvokeEvent( sv4guiContourModelPointRemoveEvent() );
    }
}

void sv4guiContourModel::SetControlPoint(int index, mitk::Point3D point, unsigned int t)
{
    sv4guiContour* contour=GetContour(t);
    if(contour)
    {
        contour->SetControlPoint(index,point);

        ControlPointsChanged(t);
        this->InvokeEvent( sv4guiContourModelPointMoveEvent() );
    }
}

void sv4guiContourModel::SetControlPointSelectedIndex(int index, unsigned int t)
{
    sv4guiContour* contour=GetContour(t);
    if(contour)
    {
        contour->SetControlPointSelectedIndex(index);

        ControlPointsChanged(t);
        this->InvokeEvent( sv4guiContourModelPointEvent() );
    }
}

void sv4guiContourModel::DeselectControlPoint(unsigned int t)
{
    sv4guiContour* contour=GetContour(t);
    if(contour)
    {
        contour->DeselectControlPoint();

        ControlPointsChanged(t);
        this->InvokeEvent( sv4guiContourModelPointEvent() );
    }
}

int sv4guiContourModel::GetControlPointSelectedIndex(unsigned int t)
{
    sv4guiContour* contour=GetContour(t);
    if(contour)
    {
        return contour->GetControlPointSelectedIndex();
    }else{
        return -2;
    }
}

void sv4guiContourModel::SetContourSelected(bool selected, unsigned int t)
{
    sv4guiContour* contour=GetContour(t);
    if(contour)
    {
        if(contour->IsSelected()!=selected)
        {
            contour->SetSelected(selected);
            ContoursChanged(t);
            this->InvokeEvent( sv4guiContourModelEvent() );
        }
    }
}

void sv4guiContourModel::ExecuteOperation( mitk::Operation* operation )
{
    int timeStep = -1;

    sv4guiContourOperation* contourOperation = dynamic_cast<sv4guiContourOperation*>(operation);

    if ( contourOperation )
    {
        timeStep = contourOperation->GetTimeStep();

    }else{
        MITK_ERROR << "No valid Contour Operation for sv4guiContourModel" << std::endl;
        return;
    }

    if ( timeStep < 0 )
    {
        MITK_ERROR << "Time step (" << timeStep << ") outside of sv4guiContourModel time bounds" << std::endl;
        return;
    }

    sv4guiContour* contour=contourOperation->GetContour();
    int index = contourOperation->GetIndex();
    mitk::Point3D point=contourOperation->GetPoint();

    switch (contourOperation->GetOperationType())
    {
    case sv4guiContourOperation::OpINSERTCONTROLPOINT:
    {
        InsertControlPoint(index,point,timeStep);
        m_CalculateBoundingBox = true;
    }
        break;

    case sv4guiContourOperation::OpMOVECONTROLPOINT:
    {
        SetControlPoint(index, point, timeStep);
        m_CalculateBoundingBox = true;
    }
        break;

    case sv4guiContourOperation::OpREMOVECONTROLPOINT:
    {
        RemoveControlPoint(index,timeStep);
        m_CalculateBoundingBox = true;
    }
        break;

    case sv4guiContourOperation::OpSETCONTOUR:
    {
        SetContour(contour,timeStep);
        m_CalculateBoundingBox = true;
    }
        break;

    default:
        itkWarningMacro("sv4guiContourModel could not understrand the operation. Please check!");
        break;
    }

    mitk::OperationEndEvent endevent(operation);
    ((const itk::Object*)this)->InvokeEvent(endevent);
}

void sv4guiContourModel::CalculateBoundingBox(double *bounds,unsigned int t)
{
    sv4guiContour* contour=GetContour(t);
    if(contour)
    {
        contour->CalculateBoundingBox(bounds);
    }
}

void sv4guiContourModel::UpdateOutputInformation()
{
    if ( this->GetSource( ) )
    {
        this->GetSource( )->UpdateOutputInformation( );
    }

    mitk::TimeGeometry* timeGeometry = GetTimeGeometry();
    if ( timeGeometry->CountTimeSteps() != m_ContourSet.size() )
    {
        itkExceptionMacro(<<"timeGeometry->CountTimeSteps() != m_ContourSet.size() -- use Initialize(timeSteps) with correct number of timeSteps!");
    }

    if (m_CalculateBoundingBox)
    {
        for ( unsigned int t = 0 ; t < m_ContourSet.size() ; ++t )
        {
            double bounds[6] = {0};
            CalculateBoundingBox(bounds,t);
            this->GetGeometry(t)->SetFloatBounds(bounds);
        }

        m_CalculateBoundingBox = false;
    }

    this->GetTimeGeometry()->Update();
}

void sv4guiContourModel::SetRequestedRegionToLargestPossibleRegion()
{
}

bool sv4guiContourModel::RequestedRegionIsOutsideOfTheBufferedRegion()
{
    return false;
}

bool sv4guiContourModel::VerifyRequestedRegion()
{
    return true;
}

void sv4guiContourModel::SetRequestedRegion(const DataObject * )
{
}

void sv4guiContourModel::PrintSelf( std::ostream& os, itk::Indent indent ) const
{
    Superclass::PrintSelf(os, indent);

    os << indent << "Number timesteps: " << m_ContourSet.size() << "\n";

    for ( unsigned int t = 0 ; t < m_ContourSet.size() ; ++t )
    {
        os << indent << "Timestep " << t << ": \n";
        itk::Indent nextIndent = indent.GetNextIndent();

        if(m_ContourSet[t])
        {
            os << nextIndent << "Contour " << ": ";
            os << "selected: " << m_ContourSet[t]->IsSelected() << "\n";
        }else{
            os << nextIndent << "Contour doesn't exist \n";
        }
    }
}
