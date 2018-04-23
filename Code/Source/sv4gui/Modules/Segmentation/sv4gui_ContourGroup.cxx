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

#include "sv4gui_ContourGroup.h"
#include "sv4gui_ContourOperation.h"
#include "sv4gui_Math3.h"

sv4guiContourGroup::sv4guiContourGroup()
    : m_CalculateBoundingBox(true)
    , m_PathID(-1)
    , m_PathName("")
    , m_GroupID(-1)
    , m_CurrentIndexOn2DView(-2)
    , m_DataModified(false)
    , m_ResliceSize(5.0)
{
    this->InitializeEmpty();
    m_LoftingParam=new svLoftingParam();
}

sv4guiContourGroup::sv4guiContourGroup(const sv4guiContourGroup &other)
    : BaseData(other)
    , m_PathID(other.m_PathID)
    , m_PathName(other.m_PathName)
    , m_ContourSets(other.GetTimeSize())
    , m_DataModified(true)
    , m_CalculateBoundingBox(true)
    , m_ResliceSize(other.m_ResliceSize)
    , m_Props(other.m_Props)
{
    for (std::size_t t = 0; t < other.GetTimeSize(); ++t)
    {
        for(std::size_t i=0; i< other.GetSize(t); ++i)
        {
            m_ContourSets[t].push_back(other.GetContour(i,t)->Clone());
        }
    }

    //     m_GroupID=other.GetGroupID();

    m_LoftingParam=new svLoftingParam(*(other.m_LoftingParam));

}

sv4guiContourGroup::~sv4guiContourGroup()
{
    this->ClearData();
    if(m_LoftingParam)
        delete m_LoftingParam;
}

void sv4guiContourGroup::ClearData()
{
    //may need delele each arrays inside first.
    m_ContourSets.clear();
    Superclass::ClearData();
}

void sv4guiContourGroup::InitializeEmpty()
{
    m_ContourSets.resize(1);
    //    m_CalculateBoundingBox = false;

    Superclass::InitializeTimeGeometry(1);
    m_Initialized = true;
}

bool sv4guiContourGroup::IsEmptyTimeStep(unsigned int t) const
{
//    return IsInitialized() && (GetSize(t) == 0);
    return false;
}

void sv4guiContourGroup::Expand( unsigned int timeSteps )
{
    unsigned int oldSize = m_ContourSets.size();

    if ( timeSteps > oldSize )
    {
        Superclass::Expand( timeSteps );

        m_ContourSets.resize( timeSteps );

        //if the size changes, then compute the bounding box
        m_CalculateBoundingBox = true;

        this->InvokeEvent( sv4guiContourGroupExtendTimeRangeEvent() );
    }
}

unsigned int sv4guiContourGroup::GetTimeSize() const
{
    return m_ContourSets.size();
}

int sv4guiContourGroup::GetSize( unsigned int t ) const
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

sv4guiContour* sv4guiContourGroup::GetContour(int contourIndex, unsigned int t) const
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

void sv4guiContourGroup::ContourControlPointsChanged(unsigned int t)
{
    this->Modified();
}

void sv4guiContourGroup::ContoursChanged(unsigned int t)
{
    this->Modified();
}

void sv4guiContourGroup::InsertControlPoint(int contourIndex, int index, mitk::Point3D point, unsigned int t)
{
    sv4guiContour* contour=GetContour(contourIndex,t);
    if(contour)
    {
        contour->InsertControlPoint(index,point);

        ContourControlPointsChanged(t);
        this->InvokeEvent( sv4guiContourPointInsertEvent() );
    }
}

void sv4guiContourGroup::RemoveControlPoint(int contourIndex, int index, unsigned int t)
{
    sv4guiContour* contour=GetContour(contourIndex,t);
    if(contour)
    {
        contour->RemoveControlPoint(index);

        ContourControlPointsChanged(t);
        this->InvokeEvent( sv4guiContourPointRemoveEvent() );

    }
}

void sv4guiContourGroup::SetControlPoint(int contourIndex, int index, mitk::Point3D point, unsigned int t)
{
    sv4guiContour* contour=GetContour(contourIndex,t);
    if(contour)
    {
        contour->SetControlPoint(index,point);

        ContourControlPointsChanged(t);
        this->InvokeEvent( sv4guiContourPointMoveEvent() );
    }
}

void sv4guiContourGroup::SetControlPointSelectedIndex(int contourIndex, int index, unsigned int t)
{
    sv4guiContour* contour=GetContour(contourIndex,t);
    if(contour)
    {
        contour->SetControlPointSelectedIndex(index);

        ContourControlPointsChanged(t);
        this->InvokeEvent( sv4guiContourPointSelectEvent() );
    }
}

void sv4guiContourGroup::DeselectControlPoint(unsigned int t)
{
    for(int i=0;i<GetSize(t);i++){
        sv4guiContour* contour=GetContour(i,t);
        if(contour) contour->DeselectControlPoint();
    }
    ContourControlPointsChanged(t);
    this->InvokeEvent( sv4guiContourPointSelectEvent() );
}

int sv4guiContourGroup::GetControlPointSelectedIndex(int contourIndex, unsigned int t)
{
    sv4guiContour* contour=GetContour(contourIndex,t);
    if(contour)
    {
        return contour->GetControlPointSelectedIndex();
    }else{
        return -2;
    }
}

void sv4guiContourGroup::InsertContour(int contourIndex, sv4guiContour* contour, unsigned int t)
{
    this->Expand(t+1);
    if(t<m_ContourSets.size())
    {
        if(contourIndex==-1) contourIndex=m_ContourSets[t].size();

        if(contourIndex>-1 && contourIndex<=m_ContourSets[t].size())
        {
            m_ContourSets[t].insert(m_ContourSets[t].begin()+contourIndex,contour);
            ContoursChanged(t);
            this->InvokeEvent( sv4guiContourInsertEvent() );
        }
    }
}

void sv4guiContourGroup::RemoveContour(int contourIndex, unsigned int t)
{
    if(t<m_ContourSets.size() )
    {
        if(contourIndex==-1) contourIndex=m_ContourSets[t].size()-1;

        if(contourIndex>-1 && contourIndex<m_ContourSets[t].size())
        {
            m_ContourSets[t].erase(m_ContourSets[t].begin()+contourIndex);
            ContoursChanged(t);
            this->InvokeEvent( sv4guiContourRemoveEvent() );
        }
    }
}

void sv4guiContourGroup::RemoveInvalidContours(unsigned int t)
{
    if(t<m_ContourSets.size() )
    {
        for(int i=m_ContourSets[t].size()-1;i>-1;i--)
        {
            sv4guiContour* contour=m_ContourSets[t][i];
            if(contour==NULL || contour->GetContourPointNumber()<3)
                RemoveContour(i,t);
        }
    }
}

void sv4guiContourGroup::SetContour(int contourIndex, sv4guiContour* contour, unsigned int t)
{
    if(t<m_ContourSets.size())
    {
        if(contourIndex==-1) contourIndex=m_ContourSets[t].size()-1;

        if(contourIndex>-1 && contourIndex<m_ContourSets[t].size())
        {
            m_ContourSets[t][contourIndex]=contour;
            ContoursChanged(t);
            this->InvokeEvent( sv4guiContourSetEvent() );
        }
    }
}

bool sv4guiContourGroup::IsContourSelected(int contourIndex, unsigned int t)
{
    sv4guiContour* contour=GetContour(contourIndex,t);
    if(contour)
    {
        return contour->IsSelected();
    }else{
        return false;
    }
}

void sv4guiContourGroup::SetContourSelected(int contourIndex, bool selected, unsigned int t)
{
    sv4guiContour* contour=GetContour(contourIndex,t);
    if(contour)
    {
        if(contour->IsSelected()!=selected)
        {
            contour->SetSelected(selected);
            ContoursChanged(t);
//            this->InvokeEvent( sv4guiContourEvent() );

        }
    }
}

void sv4guiContourGroup::DeselectContours(unsigned int t)
{
    for(int i=0;i<GetSize(t);i++){
        sv4guiContour* contour=GetContour(i,t);
        if(contour) contour->SetSelected(false);
    }
    ContoursChanged(t);
//    this->InvokeEvent( sv4guiContourEvent() );
}

int sv4guiContourGroup::GetSelectedContourIndex(unsigned int t)
{
    for(int i=0;i<GetSize(t);i++){
        sv4guiContour* contour=GetContour(i,t);
        if(contour&&contour->IsSelected())
            return i;
    }
    return -2;
}

int sv4guiContourGroup::GetUnplacedContourIndex(unsigned int t)
{
    for(int i=0;i<GetSize(t);i++){
        sv4guiContour* contour=GetContour(i,t);
        if(contour&&!contour->IsPlaced())
            return i;
    }
    return -2;
}

sv4guiContour* sv4guiContourGroup::GetUnplacedContour(unsigned int t)
{
    int contourIndex=GetUnplacedContourIndex(t);
    return GetContour(contourIndex,t);
}

int sv4guiContourGroup::SearchContourByPlane(const mitk::PlaneGeometry *planeGeometry, double precisionFactor, unsigned int t)
{
    if(planeGeometry!=NULL)
    {
        mitk::Point3D center=planeGeometry->GetCenter();
        mitk::Vector3D spacing=planeGeometry->GetSpacing();
        double minDist=sqrt(spacing[0]*spacing[0]+spacing[1]*spacing[1]+spacing[2]*spacing[2]);

        for(int i=0;i<GetSize(t);i++){

            sv4guiContour* contour=GetContour(i,t);
            if(contour==NULL) continue;

            if(contour->IsOnPlane(planeGeometry,precisionFactor)){
                double dist=center.EuclideanDistanceTo(contour->GetPathPosPoint());
                if(dist<2*minDist)
                    return i;

            }
        }
    }

    return -2;
}

sv4guiContour* sv4guiContourGroup::GetContourOnPlane(const mitk::PlaneGeometry *planeGeometry, double precisionFactor, unsigned int t)
{
    int contourIndex=SearchContourByPlane(planeGeometry,precisionFactor,t);
    return GetContour(contourIndex,t);
}

void sv4guiContourGroup::ExecuteOperation( mitk::Operation* operation )
{
    int timeStep = -1;

    sv4guiContourOperation* contourOperation = dynamic_cast<sv4guiContourOperation*>(operation);

    if ( contourOperation )
    {
        timeStep = contourOperation->GetTimeStep();

    }else{
        MITK_ERROR << "No valid Contour Operation for sv4guiContourGroup" << std::endl;
        return;
    }

    if ( timeStep < 0 )
    {
        MITK_ERROR << "Time step (" << timeStep << ") outside of sv4guiContourGroup time bounds" << std::endl;
        return;
    }

    int contourIndex=contourOperation->GetContourIndex();
    sv4guiContour* contour=contourOperation->GetContour();
    int index = contourOperation->GetIndex();
    mitk::Point3D point=contourOperation->GetPoint();

    switch (contourOperation->GetOperationType())
    {
    case sv4guiContourOperation::OpINSERTCONTROLPOINT:
    {
        InsertControlPoint(contourIndex,index,point,timeStep);
        m_CalculateBoundingBox = true;
        m_DataModified=true;
    }
        break;

    case sv4guiContourOperation::OpMOVECONTROLPOINT:
    {
        SetControlPoint(contourIndex,index, point, timeStep);
        m_CalculateBoundingBox = true;
        m_DataModified=true;
    }
        break;

    case sv4guiContourOperation::OpREMOVECONTROLPOINT:
    {
        RemoveControlPoint(contourIndex,index,timeStep);
        m_CalculateBoundingBox = true;
        m_DataModified=true;
    }
        break;

    case sv4guiContourOperation::OpINSERTCONTOUR:
    {
        InsertContour(contourIndex,contour,timeStep);
        m_CalculateBoundingBox = true;
        m_DataModified=true;
    }
        break;

    case sv4guiContourOperation::OpREMOVECONTOUR:
    {
        RemoveContour(contourIndex,timeStep);
        m_CalculateBoundingBox = true;
        m_DataModified=true;
    }
        break;

    case sv4guiContourOperation::OpSETCONTOUR:
    {
        SetContour(contourIndex,contour,timeStep);
        m_CalculateBoundingBox = true;
        m_DataModified=true;
    }
        break;

    default:
        itkWarningMacro("sv4guiContourGroup could not understrand the operation. Please check!");
        break;
    }

    mitk::OperationEndEvent endevent(operation);
    ((const itk::Object*)this)->InvokeEvent(endevent);
}

void sv4guiContourGroup::CalculateBoundingBox(double *bounds,unsigned int t)
{
    double contourBounds[6]={0};
    bool firstTime=true;
    for(int i=0;i<GetSize(t);i++){
        sv4guiContour* contour=GetContour(i,t);
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

void sv4guiContourGroup::UpdateOutputInformation()
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

void sv4guiContourGroup::SetRequestedRegionToLargestPossibleRegion()
{
}

bool sv4guiContourGroup::RequestedRegionIsOutsideOfTheBufferedRegion()
{
    return false;
}

bool sv4guiContourGroup::VerifyRequestedRegion()
{
    return true;
}

void sv4guiContourGroup::SetRequestedRegion(const DataObject * )
{
}

void sv4guiContourGroup::PrintSelf( std::ostream& os, itk::Indent indent ) const
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

std::string sv4guiContourGroup::GetPathName() const
{
    return m_PathName;
}

void sv4guiContourGroup::SetPathName(std::string name)
{
    m_PathName=name;
}

int sv4guiContourGroup::GetPathID() const
{
    return m_PathID;
}

void sv4guiContourGroup::SetPathID(int id)
{
    m_PathID=id;
}

std::vector<sv4guiPathElement::sv4guiPathPoint>  sv4guiContourGroup::GetContourPathPoints(unsigned int t)
{
    std::vector<sv4guiPathElement::sv4guiPathPoint> pathPoints;
    for(int i=0;i<m_ContourSets[t].size();i++)
        pathPoints.push_back(m_ContourSets[t][i]->GetPathPoint());

    return pathPoints;
}

std::vector<mitk::Point3D> sv4guiContourGroup::GetContourPathPosPoints(unsigned int t)
{
    std::vector<mitk::Point3D> points;
    for(int i=0;i<m_ContourSets[t].size();i++)
        points.push_back(m_ContourSets[t][i]->GetPathPoint().pos);

    return points;
}

int sv4guiContourGroup::GetInsertingContourIndexByPathPosPoint(mitk::Point3D posPoint, unsigned int t)
{
    if(GetSize(t)==0)
        return 0;

    return sv4guiMath3::GetInsertintIndexByDistance(GetContourPathPosPoints(t),posPoint);
}

int sv4guiContourGroup::GetInsertingContourIndexByTagIndex(int tagIndex, unsigned int t)
{
    for(int i=0;i<GetSize(t);i++)
    {
      sv4guiContour* contour=GetContour(i,t);
      if(!contour)
          continue;

      if(tagIndex<=contour->GetTagIndex())
          return i;
    }

    if(GetSize(t)==0)
        return 0;
    else
        return GetSize(t);
}

int sv4guiContourGroup::GetContourIndexByPathPosPoint(mitk::Point3D posPoint, unsigned int t)
{
    for(int i=0;i<GetSize(t);i++)
    {
      sv4guiContour* contour=GetContour(i,t);
      if(contour&&contour->GetPathPosPoint()==posPoint)
          return i;
    }

    return -2;
}

int sv4guiContourGroup::GetCurrentIndexOn2DView()
{
    return m_CurrentIndexOn2DView;
}

void sv4guiContourGroup::SetCurrentIndexOn2DView(int index)
{
    m_CurrentIndexOn2DView=index;
}

std::vector<sv4guiContour*> sv4guiContourGroup::GetContourSet(unsigned int t)
{
    return m_ContourSets[t];
}

std::vector<sv4guiContour*> sv4guiContourGroup::GetValidContourSet(unsigned int t)
{
    std::vector<sv4guiContour*> contourSet;
    for(int i=0;i<m_ContourSets[t].size();i++)
    {
        sv4guiContour* contour=m_ContourSets[t][i];
        if(contour && contour->GetContourPointNumber()>2)
            contourSet.push_back(contour);
    }

    return contourSet;
}

void sv4guiContourGroup::SetProp(const std::string& key, std::string value)
{
    m_Props[key]=value;
}

std::string sv4guiContourGroup::GetProp(const std::string& key) const
{
    std::map<std::string,std::string>* p=const_cast<std::map<std::string,std::string>*>(&m_Props);
    return (*p)[key];
}

//int sv4guiContourGroup::GetInsertingContourIndexByPathPosID(int posID, unsigned int t)
//{
//    if(GetSize(t)==0)
//        return 0;

//    sv4guiContour* contour=GetContour(0,t);
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

//int sv4guiContourGroup::GetContourIndexByPathPosID(int posID, unsigned int t)
//{
//    for(int i=0;i<GetSize(t);i++)
//    {
//      sv4guiContour* contour=GetContour(i,t);
//      if(contour&&contour->GetPathPosID()==posID)
//          return i;
//    }

//    return -2;
//}

