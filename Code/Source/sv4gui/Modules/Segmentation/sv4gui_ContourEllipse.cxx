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

#include "sv4gui_ContourEllipse.h"

sv4guiContourEllipse::sv4guiContourEllipse()
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

   SetKernel(cKernelType::cKERNEL_ELLIPSE);
}

sv4guiContourEllipse::sv4guiContourEllipse(const sv4guiContourEllipse &other)
    : sv4guiContour(other)
    , m_TreatAsCircle(other.m_TreatAsCircle)
{
}

sv4guiContourEllipse::~sv4guiContourEllipse()
{
}

sv4guiContourEllipse* sv4guiContourEllipse::Clone()
{
    return new sv4guiContourEllipse(*this);
}

std::string sv4guiContourEllipse::GetClassName()
{
    return "sv4guiContourEllipse";
}

bool sv4guiContourEllipse::AsCircle()
{
    return m_TreatAsCircle;
}

void sv4guiContourEllipse::SetAsCircle(bool asCircle)
{
    m_TreatAsCircle=asCircle;
}

void sv4guiContourEllipse::SetControlPoint(int index, mitk::Point3D point3d)
{
          
    if(index == 0)
    {
        mitk::Vector3D dirVec=point3d-GetControlPoint(index);
        Shift(dirVec);
    }
    else if(index==1)
    {
        Scale(GetControlPoint(0), GetControlPoint(index), point3d);
    }
    else if ( index < 4 )
    {
        std::array<double,3> stdPt;
        for(int i=0; i<3; i++)
            stdPt[i] = point3d[i];
        m_ControlPoints[index]=stdPt;
        int otherIndex = index+1;
        if (otherIndex > 3)
            otherIndex = 2;

        mitk::Point2D centerPoint, point, otherPoint;
        mitk::Point3D otherPt3d;

        m_PlaneGeometry->Map(GetControlPoint(0), centerPoint );
        m_PlaneGeometry->Map(point3d, point );
        m_PlaneGeometry->Map(GetControlPoint(otherIndex), otherPoint );

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

            std::array<double,3> stdPt2;
            for(int i=0; i<3; i++)
                stdPt2[i] = otherPt3d[i];
            m_ControlPoints[otherIndex]=stdPt2;
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
                std::array<double,3> stdPt2;
                for(int i=0; i<3; i++)
                    stdPt2[i] = otherPt3d[i];
                m_ControlPoints[otherIndex]=stdPt2;
            }

            m_TreatAsCircle = false;
        }

        ControlPointsChanged();
    }

}

void sv4guiContourEllipse::CreateContourPoints()
{
    mitk::Point2D centerPoint, boundaryPoint1,boundaryPoint2;
    
    m_PlaneGeometry->Map(GetControlPoint(0), centerPoint );
    m_PlaneGeometry->Map(GetControlPoint(2), boundaryPoint1 );
    m_PlaneGeometry->Map(GetControlPoint(3), boundaryPoint2 );

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
        
        std::array<double,3> stdPt;
        for (int j=0; j<3; j++)
            stdPt[j] = pt3d[j];
        m_ContourPoints.push_back(stdPt);
    }

}

void sv4guiContourEllipse::AssignCenterScalingPoints()
{
    if(m_ControlPoints.size()>1)
        m_ControlPoints[1]=m_ScalingPoint;
}

void sv4guiContourEllipse::PlaceControlPoints(mitk::Point3D point)
{
    sv4guiContour::PlaceControlPoints(point);
    m_ControlPointSelectedIndex = 2;
}

sv4guiContour* sv4guiContourEllipse::CreateByFitting(sv4guiContour* contour)
{
    double maxDist=0.0;
    double minDist=0.0;
    int minIndex=0;
    int maxIndex=0;
    mitk::Point2D centerPoint,point;
    contour->GetPlaneGeometry()->Map(contour->GetControlPoint(0),centerPoint);
    mitk::Point2D pt1,pt2;

    for(int i=0;i<contour->GetContourPointNumber();i++)
    {
        contour->GetPlaneGeometry()->Map(contour->GetContourPoint(i), point);
        double dist=centerPoint.EuclideanDistanceTo(point);
        if(i==0)
        {
            minDist=dist;
            pt2=point;
        }
        if(dist>maxDist)
        {
            maxDist=dist;
            maxIndex=i;
            pt1=point;
        }
        if(dist<minDist)
        {
            minDist=dist;
            minIndex=i;
            pt2=point;
        }
    }

    double area=contour->GetArea();
    double a=centerPoint.EuclideanDistanceTo(pt1);
    if(a>0)
    {
        mitk::Vector2D vec1 = pt1 - centerPoint;
        mitk::Vector2D vec2;
        vec2[0]=vec1[1];
        vec2[1]=vec1[0];
        vec2[0]*=-1;
        vec2.Normalize();
        vec2 *=(area/vnl_math::pi/a);

        pt2=centerPoint+vec2;
    }

    std::vector<mitk::Point3D> controlPoints;
    mitk::Point3D pttemp;
    contour->GetPlaneGeometry()->Map(centerPoint,pttemp);
    controlPoints.push_back(pttemp);
    controlPoints.push_back(pttemp);
    contour->GetPlaneGeometry()->Map(pt1,pttemp);
    controlPoints.push_back(pttemp);
    contour->GetPlaneGeometry()->Map(pt2,pttemp);
    controlPoints.push_back(pttemp);

    sv4guiContourEllipse* newContour=new sv4guiContourEllipse();
    newContour->SetAsCircle(false);
    newContour->SetPathPoint(contour->GetPathPoint());
    newContour->SetPlaced(true);
    newContour->SetMethod(contour->GetMethod());
    newContour->SetControlPoints(controlPoints);

    newContour->SetSubdivisionType(contour->GetSubdivisionType());
    newContour->SetSubdivisionSpacing(contour->GetSubdivisionSpacing());
    newContour->SetSubdivisionNumber(contour->GetSubdivisionNumber());

    return newContour;
}

