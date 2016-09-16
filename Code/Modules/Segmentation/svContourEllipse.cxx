#include "svContourEllipse.h"

svContourEllipse::svContourEllipse()
{
    m_Method="Manual";
    m_Type="Ellipse";

    m_MinControlPointNumber=4;
    m_MaxControlPointNumber=4;
    m_ControlPointNonRemovableIndices[0]=0;
    m_ControlPointNonRemovableIndices[1]=1;
    m_ControlPointNonRemovableIndices[2]=2;
    m_ControlPointNonRemovableIndices[3]=3;

    m_TreatAsCircle=true;

    m_SubdivisionType=CONSTANT_TOTAL_NUMBER;
    m_SubdivisionNumber=36;
}

svContourEllipse::svContourEllipse(const svContourEllipse &other)
    : svContour(other)
    , m_TreatAsCircle(other.m_TreatAsCircle)
{
}

svContourEllipse::~svContourEllipse()
{
}

svContourEllipse* svContourEllipse::Clone()
{
    return new svContourEllipse(*this);
}

std::string svContourEllipse::GetClassName()
{
    return "svContourEllipse";
}

bool svContourEllipse::AsCircle()
{
    return m_TreatAsCircle;
}

void svContourEllipse::SetAsCircle(bool asCircle)
{
    m_TreatAsCircle=asCircle;
}

void svContourEllipse::SetControlPoint(int index, mitk::Point3D point3d)
{
    if(index == 0)
    {
        mitk::Vector3D dirVec=point3d-GetControlPoint(index);
        Shift(dirVec);
    }
    else if(index==1)
    {
        Scale(m_ControlPoints[0], m_ControlPoints[index], point3d);
    }
    else if ( index < 4 )
    {
        m_ControlPoints[index]=point3d;
        int otherIndex = index+1;
        if (otherIndex > 3)
            otherIndex = 2;

        mitk::Point2D centerPoint, point, otherPoint;
        mitk::Point3D otherPt3d;

        m_PlaneGeometry->Map(m_ControlPoints[0], centerPoint );
        m_PlaneGeometry->Map(point3d, point );
        m_PlaneGeometry->Map(m_ControlPoints[otherIndex], otherPoint );

        mitk::Vector2D vec1 = point - centerPoint;
        mitk::Vector2D vec2;

        if (index == 2 && m_TreatAsCircle )
        {
            vec2[0] = vec1[1];
            vec2[1] = vec1[0];

            if (index==2)
                vec2[0] *= -1;
            else
                vec2[1] *= -1;

            otherPoint = centerPoint+vec2;

            m_PlaneGeometry->Map(otherPoint,otherPt3d);

            m_ControlPoints[otherIndex]=otherPt3d;
        }
        else if ( vec1.GetNorm() > 0 )
        {
            float r = centerPoint.EuclideanDistanceTo(otherPoint);

            vec2[0] = vec1[1];
            vec2[1] = vec1[0];

            if (index==2)
                vec2[0] *= -1;
            else
                vec2[1] *= -1;

            vec2.Normalize();
            vec2 *= r;

            if ( vec2.GetNorm() > 0 )
            {
                otherPoint = centerPoint+vec2;
                m_PlaneGeometry->Map(otherPoint,otherPt3d);
                m_ControlPoints[otherIndex]=otherPt3d;
            }

            m_TreatAsCircle = false;
        }

        ControlPointsChanged();
    }

}

void svContourEllipse::CreateContourPoints()
{
    mitk::Point2D centerPoint, boundaryPoint1,boundaryPoint2;

    m_PlaneGeometry->Map(m_ControlPoints[0], centerPoint );
    m_PlaneGeometry->Map(m_ControlPoints[2], boundaryPoint1 );
    m_PlaneGeometry->Map(m_ControlPoints[3], boundaryPoint2 );

    double radius1 = centerPoint.EuclideanDistanceTo( boundaryPoint1 );
    double radius2 = centerPoint.EuclideanDistanceTo( boundaryPoint2 );

    int interNumber;

    switch(m_SubdivisionType)
    {
    case CONSTANT_TOTAL_NUMBER:
        interNumber=m_SubdivisionNumber;
        break;
    case CONSTANT_SPACING:
        interNumber=std::max(radius1,radius2)*vnl_math::pi/m_SubdivisionSpacing;
        interNumber*=2;
        if(interNumber<m_SubdivisionNumber)
        {
            interNumber=m_SubdivisionNumber;
        }
        break;
    default:
        break;
    }

    //
    mitk::Vector2D dir = boundaryPoint1 - centerPoint;
    dir.Normalize();
    vnl_matrix_fixed<float, 2, 2> rot;
//    vnl_matrix_fixed<double, 2, 2> rot;

    // differentiate between clockwise and counterclockwise rotation
    int start = 0;
    int end = interNumber;
    if (dir[1]<0)
    {
        dir[0] = -dir[0];
        start = -interNumber/2;
        end = interNumber/2;
    }
    // construct rotation matrix to align ellipse with control point vector
    rot[0][0] = dir[0];
    rot[1][1] = rot[0][0];
    rot[1][0] = sin(acos(rot[0][0]));
    rot[0][1] = -rot[1][0];

    for ( int i = start; i < end; ++i )
    {
        double alpha = (double) i * vnl_math::pi * 2.0 / interNumber;

        vnl_vector_fixed< float, 2 > vec;
//        vnl_vector_fixed< double, 2 > vec;
        vec[0] = radius1 * cos( alpha );
        vec[1] = radius2 * sin( alpha );
        vec = rot*vec;

        mitk::Point2D point;
        mitk::Point3D pt3d;
        point[0] = centerPoint[0] + vec[0];
        point[1] = centerPoint[1] + vec[1];

        m_PlaneGeometry->Map(point,pt3d);

        m_ContourPoints.push_back(pt3d);
    }

}

void svContourEllipse::AssignCenterScalingPoints()
{
    if(m_ControlPoints.size()>1)
        m_ControlPoints[1]=m_ScalingPoint;
}

void svContourEllipse::PlaceControlPoints(mitk::Point3D point)
{
    svContour::PlaceControlPoints(point);
    m_ControlPointSelectedIndex = 2;
}
