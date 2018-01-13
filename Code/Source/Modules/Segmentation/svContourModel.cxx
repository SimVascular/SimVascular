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

#include "svContourModel.h"
#include "svContourOperation.h"

svContourModel::svContourModel()
    : m_CalculateBoundingBox(true)
{
    this->InitializeEmpty();
}

svContourModel::svContourModel(const svContourModel &other)
    : BaseData(other)
    , m_ContourSet(other.GetTimeSize())
    , m_CalculateBoundingBox(true)
{
    for (std::size_t t = 0; t < other.GetTimeSize(); ++t)
    {
        m_ContourSet[t]=other.GetContour(t)->Clone();
    }
}

svContourModel::~svContourModel()
{
    this->ClearData();
}

void svContourModel::ClearData()
{
    //may need delele each arrays inside first.
    m_ContourSet.clear();
    Superclass::ClearData();
}

void svContourModel::InitializeEmpty()
{
    m_ContourSet.resize(1);

    Superclass::InitializeTimeGeometry(1);
    m_Initialized = true;
}

bool svContourModel::IsEmptyTimeStep(unsigned int t) const
{
    return IsInitialized() && (GetContour(t) == NULL);
}

void svContourModel::Expand( unsigned int timeSteps )
{
    unsigned int oldSize = m_ContourSet.size();

    if ( timeSteps > oldSize )
    {
        Superclass::Expand( timeSteps );

        m_ContourSet.resize( timeSteps );

        m_CalculateBoundingBox = true;

        this->InvokeEvent( svContourModelExtendTimeRangeEvent() );
    }
}

unsigned int svContourModel::GetTimeSize() const
{
    return m_ContourSet.size();
}

svContour* svContourModel::GetContour(unsigned int t) const
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

void svContourModel::SetContour(svContour* contour, unsigned int t)
{
    if(t<m_ContourSet.size())
    {
        m_ContourSet[t]=contour;

        ContoursChanged(t);
        this->InvokeEvent( svContourModelSetEvent() );
    }
}

void svContourModel::ControlPointsChanged(unsigned int t)
{
    this->Modified();
}

void svContourModel::ContoursChanged(unsigned int t)
{
    this->Modified();
}

void svContourModel::InsertControlPoint(int index, mitk::Point3D point, unsigned int t)
{
    svContour* contour=GetContour(t);
    if(contour)
    {
        contour->InsertControlPoint(index,point);

        ControlPointsChanged(t);
        this->InvokeEvent( svContourModelPointInsertEvent() );
    }
}

void svContourModel::RemoveControlPoint(int index, unsigned int t)
{
    svContour* contour=GetContour(t);
    if(contour)
    {
        contour->RemoveControlPoint(index);

        ControlPointsChanged(t);
        this->InvokeEvent( svContourModelPointRemoveEvent() );
    }
}

void svContourModel::SetControlPoint(int index, mitk::Point3D point, unsigned int t)
{
    svContour* contour=GetContour(t);
    if(contour)
    {
        contour->SetControlPoint(index,point);

        ControlPointsChanged(t);
        this->InvokeEvent( svContourModelPointMoveEvent() );
    }
}

void svContourModel::SetControlPointSelectedIndex(int index, unsigned int t)
{
    svContour* contour=GetContour(t);
    if(contour)
    {
        contour->SetControlPointSelectedIndex(index);

        ControlPointsChanged(t);
        this->InvokeEvent( svContourModelPointEvent() );
    }
}

void svContourModel::DeselectControlPoint(unsigned int t)
{
    svContour* contour=GetContour(t);
    if(contour)
    {
        contour->DeselectControlPoint();

        ControlPointsChanged(t);
        this->InvokeEvent( svContourModelPointEvent() );
    }
}

int svContourModel::GetControlPointSelectedIndex(unsigned int t)
{
    svContour* contour=GetContour(t);
    if(contour)
    {
        return contour->GetControlPointSelectedIndex();
    }else{
        return -2;
    }
}

void svContourModel::SetContourSelected(bool selected, unsigned int t)
{
    svContour* contour=GetContour(t);
    if(contour)
    {
        if(contour->IsSelected()!=selected)
        {
            contour->SetSelected(selected);
            ContoursChanged(t);
            this->InvokeEvent( svContourModelEvent() );
        }
    }
}

void svContourModel::ExecuteOperation( mitk::Operation* operation )
{
    int timeStep = -1;

    svContourOperation* contourOperation = dynamic_cast<svContourOperation*>(operation);

    if ( contourOperation )
    {
        timeStep = contourOperation->GetTimeStep();

    }else{
        MITK_ERROR << "No valid Contour Operation for svContourModel" << std::endl;
        return;
    }

    if ( timeStep < 0 )
    {
        MITK_ERROR << "Time step (" << timeStep << ") outside of svContourModel time bounds" << std::endl;
        return;
    }

    svContour* contour=contourOperation->GetContour();
    int index = contourOperation->GetIndex();
    mitk::Point3D point=contourOperation->GetPoint();

    switch (contourOperation->GetOperationType())
    {
    case svContourOperation::OpINSERTCONTROLPOINT:
    {
        InsertControlPoint(index,point,timeStep);
        m_CalculateBoundingBox = true;
    }
        break;

    case svContourOperation::OpMOVECONTROLPOINT:
    {
        SetControlPoint(index, point, timeStep);
        m_CalculateBoundingBox = true;
    }
        break;

    case svContourOperation::OpREMOVECONTROLPOINT:
    {
        RemoveControlPoint(index,timeStep);
        m_CalculateBoundingBox = true;
    }
        break;

    case svContourOperation::OpSETCONTOUR:
    {
        SetContour(contour,timeStep);
        m_CalculateBoundingBox = true;
    }
        break;

    default:
        itkWarningMacro("svContourModel could not understrand the operation. Please check!");
        break;
    }

    mitk::OperationEndEvent endevent(operation);
    ((const itk::Object*)this)->InvokeEvent(endevent);
}

void svContourModel::CalculateBoundingBox(double *bounds,unsigned int t)
{
    svContour* contour=GetContour(t);
    if(contour)
    {
        contour->CalculateBoundingBox(bounds);
    }
}

void svContourModel::UpdateOutputInformation()
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

void svContourModel::SetRequestedRegionToLargestPossibleRegion()
{
}

bool svContourModel::RequestedRegionIsOutsideOfTheBufferedRegion()
{
    return false;
}

bool svContourModel::VerifyRequestedRegion()
{
    return true;
}

void svContourModel::SetRequestedRegion(const DataObject * )
{
}

void svContourModel::PrintSelf( std::ostream& os, itk::Indent indent ) const
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
