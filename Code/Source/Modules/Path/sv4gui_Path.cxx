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

#include "sv4gui_Path.h"
#include "sv4gui_Math3.h"

sv4guiPath::sv4guiPath()
    : m_CalculateBoundingBox(true)
    , m_PathID(-1)
    , m_Method(sv4guiPathElement::CONSTANT_TOTAL_NUMBER)
    , m_CalculationNumber(100)
    , m_Spacing(0)
    , m_DataModified(false)
    , m_ResliceSize(5.0)
    , m_AddingMode(SMART)
{
    this->InitializeEmpty();
}

sv4guiPath::sv4guiPath(const sv4guiPath &other)
    : BaseData(other)
    , m_PathID(other.m_PathID)
    , m_Method(other.m_Method)
    , m_CalculationNumber(other.m_CalculationNumber)
    , m_Spacing(other.m_Spacing)
    , m_PathElementSet(other.GetTimeSize())
    , m_DataModified(true)
    , m_CalculateBoundingBox(true)
    , m_ResliceSize(other.m_ResliceSize)
    , m_AddingMode(other.m_AddingMode)
    , m_Props(other.m_Props)
{
    for (std::size_t t = 0; t < other.GetTimeSize(); ++t)
    {
        m_PathElementSet[t]=other.GetPathElement(t)->Clone();
    }
}

sv4guiPath::~sv4guiPath()
{
    this->ClearData();
}

void sv4guiPath::ClearData()
{
    //may need delele each arrays inside first.
    m_PathElementSet.clear();
    Superclass::ClearData();
}

void sv4guiPath::InitializeEmpty()
{
    m_PathElementSet.resize( 1 );

    Superclass::InitializeTimeGeometry(1);
    m_Initialized = true;
}

bool sv4guiPath::IsEmptyTimeStep(unsigned int t) const
{
//    return IsInitialized() && (GetPathElement(t) == NULL);
    return false;
}

void sv4guiPath::Expand( unsigned int timeSteps )
{
    unsigned int oldSize = m_PathElementSet.size();

    if ( timeSteps > oldSize )
    {
        Superclass::Expand( timeSteps );

        m_PathElementSet.resize( timeSteps );

        m_CalculateBoundingBox = true;

        this->InvokeEvent( sv4guiPathExtendTimeRangeEvent() );
    }
}

unsigned int sv4guiPath::GetTimeSize() const
{
    return m_PathElementSet.size();
}

int sv4guiPath::GetSize( unsigned int t ) const
{
    if(GetPathElement(t))
        return GetPathElement(t)->GetControlPointNumber();
    else
        return 0;
}

sv4guiPathElement* sv4guiPath::GetPathElement(unsigned int t ) const
{
    if ( t < m_PathElementSet.size() )
    {
        return m_PathElementSet[t];
    }
    else
    {
        return NULL;
    }
}

void sv4guiPath::SetPathElement(sv4guiPathElement* pathElement, unsigned int t)
{
    if(t<m_PathElementSet.size())
    {
        m_PathElementSet[t]=pathElement;

        Modified();
        this->InvokeEvent( sv4guiPathSetEvent() );
    }
}

int sv4guiPath::GetPathID() const
{
    return m_PathID;
}

void sv4guiPath::SetPathID(int pathID)
{
    m_PathID=pathID;
}

int sv4guiPath::GetMaxPathID(mitk::DataStorage::SetOfObjects::ConstPointer rs)
{
    int maxID=0;

    if(rs){

        for(int i=0;i<rs->size();i++){

            sv4guiPath* path=dynamic_cast<sv4guiPath*>(rs->GetElement(i)->GetData());
            if(path){
                if(maxID<path->GetPathID())
                {
                    maxID=path->GetPathID();
                }
            }
        }

    }

    return maxID;
}

void sv4guiPath::ExecuteOperation( mitk::Operation* operation )
{
    int timeStep = -1;

    sv4guiPathOperation* pathOperation = dynamic_cast<sv4guiPathOperation*>(operation);

    if ( pathOperation )
    {
        timeStep = pathOperation->GetTimeStep();

    }else{
        MITK_ERROR << "No valid Path Operation for sv4guiPath" << std::endl;
        return;
    }

    if ( timeStep < 0 )
    {
        MITK_ERROR << "Time step (" << timeStep << ") outside of sv4guiPath time bounds" << std::endl;
        return;
    }

    sv4guiPathElement* originalPathElement=m_PathElementSet[timeStep];

    sv4guiPathElement* newPathElement=pathOperation->GetPathElement();
    int index = pathOperation->GetIndex();
    mitk::Point3D point=pathOperation->GetPoint();
    bool selected=pathOperation->GetSelected();

    m_OperationType= (sv4guiPathOperation::PathOperationType) operation->GetOperationType();

    switch (operation->GetOperationType())
    {

    case sv4guiPathOperation::OpINSERTCONTROLPOINT:
    {
        if(originalPathElement)
        {
            originalPathElement->InsertControlPoint(index,point);
            originalPathElement->DeselectControlPoint();
            originalPathElement->SetControlPointSelected(index,true);
            m_CalculateBoundingBox = true;
            m_NewControlPoint=point;
            m_DataModified=true;
            this->Modified();
            this->InvokeEvent( sv4guiPathPointInsertEvent() );
        }
    }
        break;

    case sv4guiPathOperation::OpMOVECONTROLPOINT:
    {
        if(originalPathElement)
        {
            originalPathElement->SetControlPoint(index,point);
            m_CalculateBoundingBox = true;
            m_NewControlPoint=point;
            m_DataModified=true;
            this->Modified();
            this->InvokeEvent( sv4guiPathPointMoveEvent() );
        }
    }
        break;

    case sv4guiPathOperation::OpREMOVECONTROLPOINT:
    {
        if(originalPathElement)
        {
            originalPathElement->RemoveControlPoint(index);
            m_CalculateBoundingBox = true;
            m_DataModified=true;
            this->Modified();
            this->InvokeEvent( sv4guiPathPointRemoveEvent() );
        }
    }
        break;

    case sv4guiPathOperation::OpSELECTCONTROLPOINT:
    {
        if(originalPathElement)
        {
            originalPathElement->SetControlPointSelected(index,selected);
            this->Modified();
            this->InvokeEvent( sv4guiPathPointSelectEvent() );
        }
    }
        break;

    case sv4guiPathOperation::OpDESELECTALL:
    {
        if(originalPathElement)
        {
            originalPathElement->DeselectControlPoint();
            this->Modified();
            this->InvokeEvent( sv4guiPathPointSelectEvent() );
        }
    }
        break;

    case sv4guiPathOperation::OpSETPATHELEMENT:
    {
        m_PathElementSet[timeStep]=newPathElement;
        m_CalculateBoundingBox = true;
        m_DataModified=true;
        this->Modified();
        this->InvokeEvent( sv4guiPathSetEvent() );
    }
        break;

    default:
        itkWarningMacro("sv4guiPath could not understrand the operation. Please check!");
        break;
    }

    mitk::OperationEndEvent endevent(operation);
    ((const itk::Object*)this)->InvokeEvent(endevent);

}

void sv4guiPath::CalculateBoundingBox(double *bounds,unsigned int t)
{
    sv4guiPathElement* pathElement=GetPathElement(t);
    if(pathElement)
    {
        pathElement->CalculateBoundingBox(bounds);
    }
}

void sv4guiPath::SetSpacing(double spacing)
{
    m_Spacing=spacing;
}

double sv4guiPath::GetSpacing() const
{
    return m_Spacing;
}

void sv4guiPath::SetMethod(sv4guiPathElement::CalculationMethod method)\
{
    m_Method=method;
}

sv4guiPathElement::CalculationMethod sv4guiPath::GetMethod() const
{
    return m_Method;
}

void sv4guiPath::SetCalculationNumber(int number)
{
    m_CalculationNumber=number;
}

int sv4guiPath::GetCalculationNumber() const
{
    return m_CalculationNumber;
}

mitk::Point3D sv4guiPath::GetNewControlPoint()
{
    return m_NewControlPoint;
}

sv4guiPathOperation::PathOperationType sv4guiPath::GetOperationType()
{
    return m_OperationType;
}

void sv4guiPath::UpdateOutputInformation()
{
    if ( this->GetSource( ) )
    {
        this->GetSource( )->UpdateOutputInformation( );
    }

    //
    // first make sure, that the associated time sliced geometry has
    // the same number of geometry 3d's as sv4guiPaths are present
    //
    mitk::TimeGeometry* timeGeometry = GetTimeGeometry();
    if ( timeGeometry->CountTimeSteps() != m_PathElementSet.size() )
    {
        itkExceptionMacro(<<"timeGeometry->CountTimeSteps() != m_PathElementSet.size() -- use Initialize(timeSteps) with correct number of timeSteps!");
    }

    if (m_CalculateBoundingBox)
    {
        for ( unsigned int t = 0 ; t < m_PathElementSet.size() ; ++t )
        {
            double bounds[6] = {0};
            CalculateBoundingBox(bounds,t);
            this->GetGeometry(t)->SetFloatBounds(bounds);
        }

        m_CalculateBoundingBox = false;
    }

    this->GetTimeGeometry()->Update();
}

void sv4guiPath::SetRequestedRegionToLargestPossibleRegion()
{
}

bool sv4guiPath::RequestedRegionIsOutsideOfTheBufferedRegion()
{
    return false;
}

bool sv4guiPath::VerifyRequestedRegion()
{
    return true;
}

void sv4guiPath::SetRequestedRegion(const DataObject * )
{
}

void sv4guiPath::PrintSelf( std::ostream& os, itk::Indent indent ) const
{
    Superclass::PrintSelf(os, indent);

    os << indent << "Number timesteps: " << m_PathElementSet.size() << "\n";

    for ( unsigned int t = 0 ; t < m_PathElementSet.size() ; ++t )
    {
        if(m_PathElementSet[t])
        {
            os << indent << "Timestep " << t << ": \n";
            itk::Indent nextIndent = indent.GetNextIndent();

            for(int i=0;i<m_PathElementSet[t]->GetControlPointNumber();i++)
            {
                os << nextIndent << "Point " << i << ": [";
                os << m_PathElementSet[t]->GetControlPoint(i)[0];
                os << ", " << m_PathElementSet[t]->GetControlPoint(i)[1];
                os << ", " << m_PathElementSet[t]->GetControlPoint(i)[2];
                os << "]";
                os << ", selected: " <<m_PathElementSet[t]->IsControlPointSelected(i) << "\n";

            }
        }
    }
}

bool sv4guiPath::IsDataModified()
{
    return m_DataModified;
}

void sv4guiPath::SetDataModified(bool modified)
{
    m_DataModified=modified;
}

void sv4guiPath::SetProp(const std::string& key, std::string value)
{
    m_Props[key]=value;
}

std::string sv4guiPath::GetProp(const std::string& key) const
{
    std::map<std::string,std::string>* p=const_cast<std::map<std::string,std::string>*>(&m_Props);
    return (*p)[key];
}

bool Equal( const sv4guiPath* leftHandSide, const sv4guiPath* rightHandSide, mitk::ScalarType eps, bool verbose )
{
    if((leftHandSide == nullptr) || (rightHandSide == nullptr))
    {
        MITK_ERROR << "Equal( const sv4guiPath* leftHandSide, const sv4guiPath* rightHandSide, mitk::ScalarType eps, bool verbose ) does not work with NULL pointer input.";
        return false;
    }
    return Equal( *leftHandSide, *rightHandSide, eps, verbose);
}

bool Equal( const sv4guiPath& leftHandSide, const sv4guiPath& rightHandSide, mitk::ScalarType eps, bool verbose )
{
    bool result = true;

    if( !mitk::Equal( *leftHandSide.GetGeometry(), *rightHandSide.GetGeometry(), eps, verbose) )
    {
        if(verbose)
            MITK_INFO << "[( sv4guiPath )] Geometries differ.";
        result = false;
    }

    if ( leftHandSide.GetSize() != rightHandSide.GetSize())
    {
        if(verbose)
            MITK_INFO << "[( sv4guiPath )] Number of control points differ.";
        result = false;
    }else if (leftHandSide.GetPathElement()->GetPathPointNumber()!=rightHandSide.GetPathElement()->GetPathPointNumber())
    {
        if(verbose)
            MITK_INFO << "[( sv4guiPath )] Number of path points differ.";
        result = false;
    }
    else
    {
        int numberOfIncorrectPoints = 0;
        int num=leftHandSide.GetSize();

        for(int i=0;i<num;i++)
        {
            if( !mitk::Equal( leftHandSide.GetPathElement()->GetControlPoint(i), rightHandSide.GetPathElement()->GetControlPoint(i), eps, verbose ) )
            {
                if(verbose)
                    MITK_INFO << "[( sv4guiPath )] control point values are different.";
                result = false;
                numberOfIncorrectPoints++;
            }
        }

        if((numberOfIncorrectPoints > 0) && verbose)
        {
            MITK_INFO << numberOfIncorrectPoints <<" of a total of " << leftHandSide.GetSize() << " control points are different.";
        }
    }
    return result;
}
