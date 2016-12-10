#include "svContourGroup.h"
#include "svContourOperation.h"
#include "svMath3.h"

svContourGroup::svContourGroup()
    : m_CalculateBoundingBox(true)
    , m_GroupID(-1)
    , m_CurrentIndexOn2DView(-2)
    , m_DataModified(false)
{
    this->InitializeEmpty();
    m_LoftingParam=new svContourGroup::svLoftingParam();
}

svContourGroup::svContourGroup(const svContourGroup &other)
    : BaseData(other)
    , m_PathID(other.m_PathID)
    , m_PathName(other.m_PathName)
    , m_ContourSets(other.GetTimeSize())
    , m_DataModified(true)
{
    for (std::size_t t = 0; t < other.GetTimeSize(); ++t)
    {
        for(std::size_t i=0; i< other.GetSize(t); ++i)
        {
            m_ContourSets[t].push_back(other.GetContour(i,t)->Clone());
        }
    }

    //     m_GroupID=other.GetGroupID();

    m_LoftingParam=new svContourGroup::svLoftingParam(*(other.m_LoftingParam));

}

svContourGroup::~svContourGroup()
{
    this->ClearData();
}

void svContourGroup::ClearData()
{
    //may need delele each arrays inside first.
    m_ContourSets.clear();
    Superclass::ClearData();
}

void svContourGroup::InitializeEmpty()
{
    m_ContourSets.resize(1);
    //    m_CalculateBoundingBox = false;

    Superclass::InitializeTimeGeometry(1);
    m_Initialized = true;
}

bool svContourGroup::IsEmptyTimeStep(unsigned int t) const
{
//    return IsInitialized() && (GetSize(t) == 0);
    return false;
}

void svContourGroup::Expand( unsigned int timeSteps )
{
    unsigned int oldSize = m_ContourSets.size();

    if ( timeSteps > oldSize )
    {
        Superclass::Expand( timeSteps );

        m_ContourSets.resize( timeSteps );

        //if the size changes, then compute the bounding box
        m_CalculateBoundingBox = true;

        this->InvokeEvent( svContourGroupExtendTimeRangeEvent() );
    }
}

unsigned int svContourGroup::GetTimeSize() const
{
    return m_ContourSets.size();
}

int svContourGroup::GetSize( unsigned int t ) const
{
    if ( t < m_ContourSets.size() )
    {
        return m_ContourSets[t].size();
    }
    else
    {
        return 0;
    }
}

svContour* svContourGroup::GetContour(int contourIndex, unsigned int t) const
{
    if ( t < m_ContourSets.size())
    {
        if(contourIndex==-1) contourIndex=m_ContourSets[t].size()-1;

        if (contourIndex>-1 && contourIndex<m_ContourSets[t].size())
        {
            return m_ContourSets[t][contourIndex];
        }else{
            return NULL;
        }
    }
    else
    {
        return NULL;
    }
}

void svContourGroup::ContourControlPointsChanged(unsigned int t)
{
    this->Modified();
}

void svContourGroup::ContoursChanged(unsigned int t)
{
    this->Modified();
}

void svContourGroup::InsertControlPoint(int contourIndex, int index, mitk::Point3D point, unsigned int t)
{
    svContour* contour=GetContour(contourIndex,t);
    if(contour)
    {
        contour->InsertControlPoint(index,point);

        ContourControlPointsChanged(t);
        this->InvokeEvent( svContourPointInsertEvent() );
    }
}

void svContourGroup::RemoveControlPoint(int contourIndex, int index, unsigned int t)
{
    svContour* contour=GetContour(contourIndex,t);
    if(contour)
    {
        contour->RemoveControlPoint(index);

        ContourControlPointsChanged(t);
        this->InvokeEvent( svContourPointRemoveEvent() );

    }
}

void svContourGroup::SetControlPoint(int contourIndex, int index, mitk::Point3D point, unsigned int t)
{
    svContour* contour=GetContour(contourIndex,t);
    if(contour)
    {
        contour->SetControlPoint(index,point);

        ContourControlPointsChanged(t);
        this->InvokeEvent( svContourPointMoveEvent() );
    }
}

void svContourGroup::SetControlPointSelectedIndex(int contourIndex, int index, unsigned int t)
{
    svContour* contour=GetContour(contourIndex,t);
    if(contour)
    {
        contour->SetControlPointSelectedIndex(index);

        ContourControlPointsChanged(t);
        this->InvokeEvent( svContourPointSelectEvent() );
    }
}

void svContourGroup::DeselectControlPoint(unsigned int t)
{
    for(int i=0;i<GetSize(t);i++){
        svContour* contour=GetContour(i,t);
        if(contour) contour->DeselectControlPoint();
    }
    ContourControlPointsChanged(t);
    this->InvokeEvent( svContourPointSelectEvent() );
}

int svContourGroup::GetControlPointSelectedIndex(int contourIndex, unsigned int t)
{
    svContour* contour=GetContour(contourIndex,t);
    if(contour)
    {
        return contour->GetControlPointSelectedIndex();
    }else{
        return -2;
    }
}

void svContourGroup::InsertContour(int contourIndex, svContour* contour, unsigned int t)
{
    this->Expand(t+1);
    if(t<m_ContourSets.size())
    {
        if(contourIndex==-1) contourIndex=m_ContourSets[t].size();

        if(contourIndex>-1 && contourIndex<=m_ContourSets[t].size())
        {
            m_ContourSets[t].insert(m_ContourSets[t].begin()+contourIndex,contour);
            ContoursChanged(t);
            this->InvokeEvent( svContourInsertEvent() );
        }
    }
}

void svContourGroup::RemoveContour(int contourIndex, unsigned int t)
{
    if(t<m_ContourSets.size() )
    {
        if(contourIndex==-1) contourIndex=m_ContourSets[t].size()-1;

        if(contourIndex>-1 && contourIndex<m_ContourSets[t].size())
        {
            m_ContourSets[t].erase(m_ContourSets[t].begin()+contourIndex);
            ContoursChanged(t);
            this->InvokeEvent( svContourRemoveEvent() );
        }
    }
}

void svContourGroup::RemoveInvalidContours(unsigned int t)
{
    if(t<m_ContourSets.size() )
    {
        for(int i=m_ContourSets[t].size()-1;i>-1;i--)
        {
            svContour* contour=m_ContourSets[t][i];
            if(contour==NULL || contour->GetContourPointNumber()<3)
                RemoveContour(i,t);
        }
    }
}

void svContourGroup::SetContour(int contourIndex, svContour* contour, unsigned int t)
{
    if(t<m_ContourSets.size())
    {
        if(contourIndex==-1) contourIndex=m_ContourSets[t].size()-1;

        if(contourIndex>-1 && contourIndex<m_ContourSets[t].size())
        {
            m_ContourSets[t][contourIndex]=contour;
            ContoursChanged(t);
            this->InvokeEvent( svContourSetEvent() );
        }
    }
}

bool svContourGroup::IsContourSelected(int contourIndex, unsigned int t)
{
    svContour* contour=GetContour(contourIndex,t);
    if(contour)
    {
        return contour->IsSelected();
    }else{
        return false;
    }
}

void svContourGroup::SetContourSelected(int contourIndex, bool selected, unsigned int t)
{
    svContour* contour=GetContour(contourIndex,t);
    if(contour)
    {
        if(contour->IsSelected()!=selected)
        {
            contour->SetSelected(selected);
            ContoursChanged(t);
            this->InvokeEvent( svContourEvent() );

        }
    }
}

void svContourGroup::DeselectContours(unsigned int t)
{
    for(int i=0;i<GetSize(t);i++){
        svContour* contour=GetContour(i,t);
        if(contour) contour->SetSelected(false);
    }
    ContoursChanged(t);
    this->InvokeEvent( svContourEvent() );
}

int svContourGroup::GetSelectedContourIndex(unsigned int t)
{
    for(int i=0;i<GetSize(t);i++){
        svContour* contour=GetContour(i,t);
        if(contour&&contour->IsSelected())
            return i;
    }
    return -2;
}

int svContourGroup::GetUnplacedContourIndex(unsigned int t)
{
    for(int i=0;i<GetSize(t);i++){
        svContour* contour=GetContour(i,t);
        if(contour&&!contour->IsPlaced())
            return i;
    }
    return -2;
}

svContour* svContourGroup::GetUnplacedContour(unsigned int t)
{
    int contourIndex=GetUnplacedContourIndex(t);
    return GetContour(contourIndex,t);
}

int svContourGroup::SearchContourByPlane(const mitk::PlaneGeometry *planeGeometry, double precisionFactor, unsigned int t)
{
    if(planeGeometry!=NULL)
    {
        for(int i=0;i<GetSize(t);i++){

            svContour* contour=GetContour(i,t);

            if(contour&&contour->IsOnPlane(planeGeometry,precisionFactor)){

                return i;

            }
        }
    }

    return -2;
}

svContour* svContourGroup::GetContourOnPlane(const mitk::PlaneGeometry *planeGeometry, double precisionFactor, unsigned int t)
{
    int contourIndex=SearchContourByPlane(planeGeometry,precisionFactor,t);
    return GetContour(contourIndex,t);
}

void svContourGroup::ExecuteOperation( mitk::Operation* operation )
{
    int timeStep = -1;

    svContourOperation* contourOperation = dynamic_cast<svContourOperation*>(operation);

    if ( contourOperation )
    {
        timeStep = contourOperation->GetTimeStep();

    }else{
        MITK_ERROR << "No valid Contour Operation for svContourGroup" << std::endl;
        return;
    }

    if ( timeStep < 0 )
    {
        MITK_ERROR << "Time step (" << timeStep << ") outside of svContourGroup time bounds" << std::endl;
        return;
    }

    int contourIndex=contourOperation->GetContourIndex();
    svContour* contour=contourOperation->GetContour();
    int index = contourOperation->GetIndex();
    mitk::Point3D point=contourOperation->GetPoint();

    switch (contourOperation->GetOperationType())
    {
    case svContourOperation::OpINSERTCONTROLPOINT:
    {
        InsertControlPoint(contourIndex,index,point,timeStep);
        m_CalculateBoundingBox = true;
        m_DataModified=true;
    }
        break;

    case svContourOperation::OpMOVECONTROLPOINT:
    {
        SetControlPoint(contourIndex,index, point, timeStep);
        m_CalculateBoundingBox = true;
        m_DataModified=true;
    }
        break;

    case svContourOperation::OpREMOVECONTROLPOINT:
    {
        RemoveControlPoint(contourIndex,index,timeStep);
        m_CalculateBoundingBox = true;
        m_DataModified=true;
    }
        break;

    case svContourOperation::OpINSERTCONTOUR:
    {
        InsertContour(contourIndex,contour,timeStep);
        m_CalculateBoundingBox = true;
        m_DataModified=true;
    }
        break;

    case svContourOperation::OpREMOVECONTOUR:
    {
        RemoveContour(contourIndex,timeStep);
        m_CalculateBoundingBox = true;
        m_DataModified=true;
    }
        break;

    case svContourOperation::OpSETCONTOUR:
    {
        SetContour(contourIndex,contour,timeStep);
        m_CalculateBoundingBox = true;
        m_DataModified=true;
    }
        break;

    default:
        itkWarningMacro("svContourGroup could not understrand the operation. Please check!");
        break;
    }

    mitk::OperationEndEvent endevent(operation);
    ((const itk::Object*)this)->InvokeEvent(endevent);
}

void svContourGroup::CalculateBoundingBox(double *bounds,unsigned int t)
{
    double contourBounds[6]={0};
    bool firstTime=true;
    for(int i=0;i<GetSize(t);i++){
        svContour* contour=GetContour(i,t);
        if(contour)
        {
            contour->CalculateBoundingBox(contourBounds);
            if(firstTime)
            {
                bounds[0]=contourBounds[0];
                bounds[1]=contourBounds[1];
                bounds[2]=contourBounds[2];
                bounds[3]=contourBounds[3];
                bounds[4]=contourBounds[4];
                bounds[5]=contourBounds[5];
                firstTime=false;
            }else{
                if(contourBounds[0]<bounds[0]) bounds[0]=contourBounds[0];
                if(contourBounds[1]>bounds[1]) bounds[1]=contourBounds[1];
                if(contourBounds[2]<bounds[2]) bounds[2]=contourBounds[2];
                if(contourBounds[3]>bounds[3]) bounds[3]=contourBounds[3];
                if(contourBounds[4]<bounds[4]) bounds[4]=contourBounds[4];
                if(contourBounds[5]>bounds[5]) bounds[5]=contourBounds[5];
            }
        }
    }

}

void svContourGroup::UpdateOutputInformation()
{
    if ( this->GetSource( ) )
    {
        this->GetSource( )->UpdateOutputInformation( );
    }

    mitk::TimeGeometry* timeGeometry = GetTimeGeometry();
    if ( timeGeometry->CountTimeSteps() != m_ContourSets.size() )
    {
        itkExceptionMacro(<<"timeGeometry->CountTimeSteps() != m_ContourSets.size() -- use Initialize(timeSteps) with correct number of timeSteps!");
    }

    if (m_CalculateBoundingBox)
    {
        for ( unsigned int t = 0 ; t < m_ContourSets.size() ; ++t )
        {
            double bounds[6] = {0};
            CalculateBoundingBox(bounds,t);
            this->GetGeometry(t)->SetFloatBounds(bounds);
        }

        m_CalculateBoundingBox = false;
    }

    this->GetTimeGeometry()->Update();
}

void svContourGroup::SetRequestedRegionToLargestPossibleRegion()
{
}

bool svContourGroup::RequestedRegionIsOutsideOfTheBufferedRegion()
{
    return false;
}

bool svContourGroup::VerifyRequestedRegion()
{
    return true;
}

void svContourGroup::SetRequestedRegion(const DataObject * )
{
}

void svContourGroup::PrintSelf( std::ostream& os, itk::Indent indent ) const
{
    Superclass::PrintSelf(os, indent);

    os << indent << "Number timesteps: " << m_ContourSets.size() << "\n";

    for ( unsigned int t = 0 ; t < m_ContourSets.size() ; ++t )
    {

            os << indent << "Timestep " << t << ": \n";
            itk::Indent nextIndent = indent.GetNextIndent();

            for(int i=0;i<m_ContourSets[t].size();i++)
            {
                if(m_ContourSets[t][i])
                {
                    os << nextIndent << "Contour " << i << ": ";
                    os << "selected: " << m_ContourSets[t][i]->IsSelected() << "\n";
                }else{
                    os << nextIndent << "Contour " << i << ": doesn't exist \n";
                }

            }

    }
}

std::string svContourGroup::GetPathName() const
{
    return m_PathName;
}

void svContourGroup::SetPathName(std::string name)
{
    m_PathName=name;
}

int svContourGroup::GetPathID() const
{
    return m_PathID;
}

void svContourGroup::SetPathID(int id)
{
    m_PathID=id;
}

std::vector<svPathElement::svPathPoint>  svContourGroup::GetContourPathPoints(unsigned int t)
{
    std::vector<svPathElement::svPathPoint> pathPoints;
    for(int i=0;i<m_ContourSets[t].size();i++)
        pathPoints.push_back(m_ContourSets[t][i]->GetPathPoint());

    return pathPoints;
}

std::vector<mitk::Point3D> svContourGroup::GetContourPathPosPoints(unsigned int t)
{
    std::vector<mitk::Point3D> points;
    for(int i=0;i<m_ContourSets[t].size();i++)
        points.push_back(m_ContourSets[t][i]->GetPathPoint().pos);

    return points;
}

int svContourGroup::GetInsertingContourIndexByPathPosPoint(mitk::Point3D posPoint, unsigned int t)
{
    if(GetSize(t)==0)
        return 0;

    return svMath3::GetInsertintIndexByDistance(GetContourPathPosPoints(t),posPoint);
}

int svContourGroup::GetContourIndexByPathPosPoint(mitk::Point3D posPoint, unsigned int t)
{
    for(int i=0;i<GetSize(t);i++)
    {
      svContour* contour=GetContour(i,t);
      if(contour&&contour->GetPathPosPoint()==posPoint)
          return i;
    }

    return -2;
}

int svContourGroup::GetCurrentIndexOn2DView()
{
    return m_CurrentIndexOn2DView;
}

void svContourGroup::SetCurrentIndexOn2DView(int index)
{
    m_CurrentIndexOn2DView=index;
}

std::vector<svContour*> svContourGroup::GetContourSet(unsigned int t)
{
    return m_ContourSets[t];
}

std::vector<svContour*> svContourGroup::GetValidContourSet(unsigned int t)
{
    std::vector<svContour*> contourSet;
    for(int i=0;i<m_ContourSets[t].size();i++)
    {
        svContour* contour=m_ContourSets[t][i];
        if(contour && contour->GetContourPointNumber()>2)
            contourSet.push_back(contour);
    }

    return contourSet;
}

//int svContourGroup::GetInsertingContourIndexByPathPosID(int posID, unsigned int t)
//{
//    if(GetSize(t)==0)
//        return 0;

//    svContour* contour=GetContour(0,t);
//    if(contour)
//    {
//        if(posID<contour->GetPathPosID())
//            return 0;
//    }

//    contour=GetContour(-1,t);
//    if(contour)
//    {
//        if(posID>contour->GetPathPosID())
//            return -1;
//    }

//    for(int i=0;i<GetSize(t)-1;i++)
//    {
//      int posID1=GetContour(i,t)->GetPathPosID();
//      int posID2=GetContour(i+1,t)->GetPathPosID();
//      if(posID==posID1)
//          return i;
//      else if(posID>posID1&&posID<=posID2)
//          return i+1;
//    }

//    return -2;

//}

//int svContourGroup::GetContourIndexByPathPosID(int posID, unsigned int t)
//{
//    for(int i=0;i<GetSize(t);i++)
//    {
//      svContour* contour=GetContour(i,t);
//      if(contour&&contour->GetPathPosID()==posID)
//          return i;
//    }

//    return -2;
//}

