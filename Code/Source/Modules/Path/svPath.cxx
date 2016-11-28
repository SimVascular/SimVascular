#include "svPath.h"
#include "svMath3.h"

svPath::svPath()
    : m_CalculateBoundingBox(true)
    , m_PathID(-1)
    , m_Method(svPathElement::CONSTANT_TOTAL_NUMBER)
    , m_CalculationNumber(100)
    , m_Spacing(0)
    , m_DataModified(false)
{
    this->InitializeEmpty();
}

svPath::svPath(const svPath &other)
    : BaseData(other)
    , m_PathID(other.m_PathID)
    , m_Method(other.m_Method)
    , m_CalculationNumber(other.m_CalculationNumber)
    , m_Spacing(other.m_Spacing)
    , m_PathElementSet(other.GetTimeSize())
    , m_DataModified(true)
{
    for (std::size_t t = 0; t < other.GetTimeSize(); ++t)
    {
        m_PathElementSet.push_back(other.GetPathElement(t)->Clone());
    }
}

svPath::~svPath()
{
    this->ClearData();
}

void svPath::ClearData()
{
    //may need delele each arrays inside first.
    m_PathElementSet.clear();
    Superclass::ClearData();
}

void svPath::InitializeEmpty()
{
    m_PathElementSet.resize( 1 );

    Superclass::InitializeTimeGeometry(1);
    m_Initialized = true;
}

bool svPath::IsEmptyTimeStep(unsigned int t) const
{
//    return IsInitialized() && (GetPathElement(t) == NULL);
    return false;
}

void svPath::Expand( unsigned int timeSteps )
{
    unsigned int oldSize = m_PathElementSet.size();

    if ( timeSteps > oldSize )
    {
        Superclass::Expand( timeSteps );

        m_PathElementSet.resize( timeSteps );

        m_CalculateBoundingBox = true;

        this->InvokeEvent( svPathExtendTimeRangeEvent() );
    }
}

unsigned int svPath::GetTimeSize() const
{
    return m_PathElementSet.size();
}

int svPath::GetSize( unsigned int t ) const
{
    if(GetPathElement(t))
        return GetPathElement(t)->GetControlPointNumber();
    else
        return 0;
}

svPathElement* svPath::GetPathElement(unsigned int t ) const
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

void svPath::SetPathElement(svPathElement* pathElement, unsigned int t)
{
    if(t<m_PathElementSet.size())
    {
        m_PathElementSet[t]=pathElement;

        Modified();
        this->InvokeEvent( svPathSetEvent() );
    }
}

int svPath::GetPathID() const
{
    return m_PathID;
}

void svPath::SetPathID(int pathID)
{
    m_PathID=pathID;
}

int svPath::GetMaxPathID(mitk::DataStorage::SetOfObjects::ConstPointer rs)
{
    int maxID=0;

    if(rs){

        for(int i=0;i<rs->size();i++){

            svPath* path=dynamic_cast<svPath*>(rs->GetElement(i)->GetData());
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

void svPath::ExecuteOperation( mitk::Operation* operation )
{
    int timeStep = -1;

    svPathOperation* pathOperation = dynamic_cast<svPathOperation*>(operation);

    if ( pathOperation )
    {
        timeStep = pathOperation->GetTimeStep();

    }else{
        MITK_ERROR << "No valid Path Operation for svPath" << std::endl;
        return;
    }

    if ( timeStep < 0 )
    {
        MITK_ERROR << "Time step (" << timeStep << ") outside of svPath time bounds" << std::endl;
        return;
    }

    svPathElement* originalPathElement=m_PathElementSet[timeStep];

    svPathElement* newPathElement=pathOperation->GetPathElement();
    int index = pathOperation->GetIndex();
    mitk::Point3D point=pathOperation->GetPoint();
    bool selected=pathOperation->GetSelected();

    m_OperationType= (svPathOperation::PathOperationType) operation->GetOperationType();

    switch (operation->GetOperationType())
    {

    case svPathOperation::OpINSERTCONTROLPOINT:
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
            this->InvokeEvent( svPathPointInsertEvent() );
        }
    }
        break;

    case svPathOperation::OpMOVECONTROLPOINT:
    {
        if(originalPathElement)
        {
            originalPathElement->SetControlPoint(index,point);
            m_CalculateBoundingBox = true;
            m_NewControlPoint=point;
            m_DataModified=true;
            this->Modified();
            this->InvokeEvent( svPathPointMoveEvent() );
        }
    }
        break;

    case svPathOperation::OpREMOVECONTROLPOINT:
    {
        if(originalPathElement)
        {
            originalPathElement->RemoveControlPoint(index);
            m_CalculateBoundingBox = true;
            m_DataModified=true;
            this->Modified();
            this->InvokeEvent( svPathPointRemoveEvent() );
        }
    }
        break;

    case svPathOperation::OpSELECTCONTROLPOINT:
    {
        if(originalPathElement)
        {
            originalPathElement->SetControlPointSelected(index,selected);
            this->Modified();
            this->InvokeEvent( svPathPointSelectEvent() );
        }
    }
        break;

    case svPathOperation::OpDESELECTALL:
    {
        if(originalPathElement)
        {
            originalPathElement->DeselectControlPoint();
            this->Modified();
            this->InvokeEvent( svPathPointSelectEvent() );
        }
    }
        break;

    case svPathOperation::OpSETPATHELEMENT:
    {
        m_PathElementSet[timeStep]=newPathElement;
        m_CalculateBoundingBox = true;
        m_DataModified=true;
        this->Modified();
        this->InvokeEvent( svPathSetEvent() );
    }
        break;

    default:
        itkWarningMacro("svPath could not understrand the operation. Please check!");
        break;
    }

    mitk::OperationEndEvent endevent(operation);
    ((const itk::Object*)this)->InvokeEvent(endevent);

}

void svPath::CalculateBoundingBox(double *bounds,unsigned int t)
{
    svPathElement* pathElement=GetPathElement(t);
    if(pathElement)
    {
        pathElement->CalculateBoundingBox(bounds);
    }
}

void svPath::SetSpacing(double spacing)
{
    m_Spacing=spacing;
}

double svPath::GetSpacing() const
{
    return m_Spacing;
}

void svPath::SetMethod(svPathElement::CalculationMethod method)\
{
    m_Method=method;
}

svPathElement::CalculationMethod svPath::GetMethod() const
{
    return m_Method;
}

void svPath::SetCalculationNumber(int number)
{
    m_CalculationNumber=number;
}

int svPath::GetCalculationNumber() const
{
    return m_CalculationNumber;
}

mitk::Point3D svPath::GetNewControlPoint()
{
    return m_NewControlPoint;
}

svPathOperation::PathOperationType svPath::GetOperationType()
{
    return m_OperationType;
}

void svPath::UpdateOutputInformation()
{
    if ( this->GetSource( ) )
    {
        this->GetSource( )->UpdateOutputInformation( );
    }

    //
    // first make sure, that the associated time sliced geometry has
    // the same number of geometry 3d's as svPaths are present
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

void svPath::SetRequestedRegionToLargestPossibleRegion()
{
}

bool svPath::RequestedRegionIsOutsideOfTheBufferedRegion()
{
    return false;
}

bool svPath::VerifyRequestedRegion()
{
    return true;
}

void svPath::SetRequestedRegion(const DataObject * )
{
}

void svPath::PrintSelf( std::ostream& os, itk::Indent indent ) const
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

bool svPath::IsDataModified()
{
    return m_DataModified;
}

void svPath::SetDataModified(bool modified)
{
    m_DataModified=modified;
}

bool Equal( const svPath* leftHandSide, const svPath* rightHandSide, mitk::ScalarType eps, bool verbose )
{
    if((leftHandSide == nullptr) || (rightHandSide == nullptr))
    {
        MITK_ERROR << "Equal( const svPath* leftHandSide, const svPath* rightHandSide, mitk::ScalarType eps, bool verbose ) does not work with NULL pointer input.";
        return false;
    }
    return Equal( *leftHandSide, *rightHandSide, eps, verbose);
}

bool Equal( const svPath& leftHandSide, const svPath& rightHandSide, mitk::ScalarType eps, bool verbose )
{
    bool result = true;

    if( !mitk::Equal( *leftHandSide.GetGeometry(), *rightHandSide.GetGeometry(), eps, verbose) )
    {
        if(verbose)
            MITK_INFO << "[( svPath )] Geometries differ.";
        result = false;
    }

    if ( leftHandSide.GetSize() != rightHandSide.GetSize())
    {
        if(verbose)
            MITK_INFO << "[( svPath )] Number of control points differ.";
        result = false;
    }else if (leftHandSide.GetPathElement()->GetPathPointNumber()!=rightHandSide.GetPathElement()->GetPathPointNumber())
    {
        if(verbose)
            MITK_INFO << "[( svPath )] Number of path points differ.";
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
                    MITK_INFO << "[( svPath )] control point values are different.";
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
