#include "svContourPolygon.h"

svContourPolygon::svContourPolygon()
{
    m_Method="Manual";
    m_Type="Polygon";

    m_MinControlPointNumber=4;
    m_MaxControlPointNumber=200;

    m_ControlPointNonRemovableIndices[0]=0;
    m_ControlPointNonRemovableIndices[1]=1;
    m_Extendable=true;
}

svContourPolygon::svContourPolygon(const svContourPolygon &other)
    : svContour(other)
{
}

svContourPolygon::~svContourPolygon()
{
}

svContourPolygon* svContourPolygon::Clone()
{
    return new svContourPolygon(*this);
}

std::string svContourPolygon::GetClassName()
{
    return "svContourPolygon";
}

std::vector<mitk::Point3D> CreateInterpolationPoints(mitk::Point3D pt1, mitk::Point3D pt2, int interNumber)
{
    std::vector<mitk::Point3D> points;

    double dx,dy,dz;
    dx=(pt2[0]-pt1[0])/interNumber;
    dy=(pt2[1]-pt1[1])/interNumber;
    dz=(pt2[2]-pt1[2])/interNumber;

    mitk::Point3D pt;
    for(int i=1;i<interNumber;i++)
    {
        pt[0]=pt1[0]+i*dx;
        pt[1]=pt1[1]+i*dy;
        pt[2]=pt1[2]+i*dz;

        points.push_back(pt);
    }

    return points;
}

void svContourPolygon::SetControlPoint(int index, mitk::Point3D point)
{
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index<0||index>m_ControlPoints.size()-1) return;

    if(index==0)
    {
        mitk::Vector3D dirVec=point-m_ControlPoints[index];
        Shift(dirVec);
    }
    else if(index==1)
    {
        Scale(m_ControlPoints[0], m_ControlPoints[index], point);
    }
    else
    {
        m_ControlPoints[index]=point;
        ControlPointsChanged();
    }
}

void svContourPolygon::CreateContourPoints()
{
    //exclude the first two points

    int controlNumber=GetControlPointNumber();

    if(controlNumber<=2)
    {
        return;
    }
    else if(controlNumber==3)
    {
        m_ContourPoints.push_back(GetControlPoint(2));
        return;
    }

    std::vector<mitk::Point3D> tempControlPoints=m_ControlPoints;
    tempControlPoints.push_back(m_ControlPoints[2]);

    int interNumber;

    switch(m_SubdivisionType)
    {
    case CONSTANT_TOTAL_NUMBER:
        if(m_Closed)
            interNumber=std::ceil(m_SubdivisionNumber*1.0/(controlNumber-2));
        else
            interNumber=std::ceil((m_SubdivisionNumber-1.0)/(controlNumber-3));
        break;
    case CONSTANT_SUBDIVISION_NUMBER:
            interNumber=m_SubdivisionNumber;
        break;
    default:
        break;
    }

    int controlBeginIndex=2;
    for(int i=controlBeginIndex;i<controlNumber;i++)
    {
        mitk::Point3D pt1,pt2;
        pt1=tempControlPoints[i];
        pt2=tempControlPoints[i+1];

        m_ContourPoints.push_back(pt1);

        if(i==controlNumber-1 &&!m_Closed) break;

        if(m_SubdivisionType==CONSTANT_SPACING)
        {
            interNumber=std::ceil(pt2.EuclideanDistanceTo(pt1)/m_SubdivisionSpacing);
        }

        std::vector<mitk::Point3D> interPoints=CreateInterpolationPoints(pt1,pt2,interNumber);

         m_ContourPoints.insert(m_ContourPoints.end(),interPoints.begin(),interPoints.end());
    }

}

int svContourPolygon::SearchControlPointByContourPoint( int contourPointIndex )
{
    if(contourPointIndex<-1 || contourPointIndex>=m_ContourPoints.size()) return -2;

    if(contourPointIndex==-1) return m_ControlPoints.size();

    int controlBeginIndex=2;//exclude the first two points

    for(int i=contourPointIndex;i<m_ContourPoints.size();i++)
    {
        for(int j=controlBeginIndex;j<m_ControlPoints.size();j++)
        {
            if(m_ContourPoints[i][0]==m_ControlPoints[j][0]
                    &&m_ContourPoints[i][1]==m_ControlPoints[j][1]
                    &&m_ContourPoints[i][2]==m_ControlPoints[j][2])
            {
                return j;
            }
        }
    }

    return m_ControlPoints.size();
}

void svContourPolygon::AssignCenterScalingPoints()
{
    if(m_ControlPoints.size()>1)
    {
        m_ControlPoints[0]=m_CenterPoint;
        m_ControlPoints[1]=m_ScalingPoint;
    }
}

void svContourPolygon::PlaceControlPoints(mitk::Point3D point)
{
    svContour::PlaceControlPoints(point);
    m_ControlPointSelectedIndex = 3;
}
