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

#include "sv3_EllipseContour.h"
using sv3::ContourEllipse;

ContourEllipse::ContourEllipse() : Contour( KERNEL_ELLIPSE )
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

ContourEllipse::ContourEllipse(const ContourEllipse &other)
    : Contour(other)
    , m_TreatAsCircle(other.m_TreatAsCircle)
{
}

ContourEllipse::~ContourEllipse()
{
}

ContourEllipse* ContourEllipse::Clone()
{
    return new ContourEllipse(*this);
}

std::string ContourEllipse::GetClassName()
{
    return "ContourEllipse";
}

bool ContourEllipse::AsCircle()
{
    return m_TreatAsCircle;
}

void ContourEllipse::SetAsCircle(bool asCircle)
{
    m_TreatAsCircle=asCircle;
}

ContourEllipse* ContourEllipse::CreateSmoothedContour(int fourierNumber)
{
    if(m_ContourPoints.size()<3)
        return this->Clone();

    ContourEllipse* contour=new ContourEllipse();
    contour->SetPathPoint(m_PathPoint);
//    contour->SetPlaneGeometry(m_vtkPlaneGeometry);
    std::string method=m_Method;
    int idx=method.find("Smoothed");
    if(idx<0)
        method=method+" + Smoothed";

    contour->SetMethod(method);
    //contour->SetPlaced(true);
    contour->SetClosed(m_Closed);

    int pointNumber=m_ContourPoints.size();

    int smoothedPointNumber;

    if((2*pointNumber)<fourierNumber)
        smoothedPointNumber=3*fourierNumber;
    else
        smoothedPointNumber=pointNumber;

    cvMath *cMath = new cvMath();
    std::vector<std::array<double, 3> > smoothedContourPoints=cMath->CreateSmoothedCurve(m_ContourPoints,m_Closed,fourierNumber,0,smoothedPointNumber);
    delete cMath;
    contour->SetContourPoints(smoothedContourPoints);

    return contour;
}

void ContourEllipse::SetControlPoint(int index, std::array<double,3>  point3d)
{
    if(index == 0)
    {
        if(m_ControlPoints.size()==0)
            m_ControlPoints.push_back(std::array<double,3>{projPt[0],projPt[1],projPt[2]});
        else
        {
            std::array<double,3> dirVec;
            for (int i=0; i<3; i++)
                dirVec[i]=projPt[i]-GetControlPoint(0)[i];
            Shift(dirVec);
        }
    }
    else if(index==1)
    {
        if (m_ControlPoints.size()<2)
            m_ControlPoints.push_back({0.,0.,0.});
        Scale(m_ControlPoints[0], m_ControlPoints[index], point3d);
    }
    else if ( index < 4 )
    {
        if (m_ControlPoints.size()<index+1)
            m_ControlPoints.insert(m_ControlPoints.begin()+index,point3d);
        else
            m_ControlPoints[index]=point3d;
        int otherIndex = index+1;
        if m_ControlPoints.size()<otherIndex+1)
        {
            fprintf(stderr, "Missing a control point on the contour\n");
            return;
        }
        if (otherIndex > 3)
            otherIndex = 2;

        double centerPoint[3], point[3], otherPoint[3];
        double otherPt3d[3];
        double tmpPt1[3], tmpPt2[3], tmpPt3[3];
        for (int i = 0; i<3; i++)
        {
            tmpPt1[i] = m_ControlPoints[0][i];
            tmpPt2[i] = point3d[i];
            tmpPt3[i] = m_ControlPoints[otherIndex][i];
        }
        
        m_vtkPlaneGeometry->ProjectPoint(tmpPt1, centerPoint );
        m_vtkPlaneGeometry->ProjectPoint(tmpPt2, point );
        m_vtkPlaneGeometry->ProjectPoint(tmpPt3, otherPoint );

        double vec1[3], vec2[3];
        for (int i = 0; i<3; i++)
            vec1[i] = point[i] - centerPoint[i];
            
        if (index == 2 && m_TreatAsCircle )
        {
            vec2[0] = vec1[1];
            vec2[1] = vec1[0];

            if (index==2)
                vec2[0] *= -1;
            else
                vec2[1] *= -1;
            
            double* normal = m_vtkPlaneGeometry->GetNormal();
            vec2[2] = (-normal[0]*vec2[0]-normal[1]*vec2[1])/normal[2];
            for (int i = 0; i<3; i++)
                otherPoint[i] = centerPoint[i]+vec2[i];

            m_vtkPlaneGeometry->ProjectPoint(otherPoint,otherPt3d);

            m_ControlPoints[otherIndex]={otherPoint[0], otherPoint[1], otherPoint[2]};
        }
        else if ( vec1.GetNorm() > 0 )
        {
            float r = sqrt(pow(centerPoint[0]-otherPoint[0],2) +
                            pow(centerPoint[1]-otherPoint[1],2) +
                            pow(centerPoint[2]-otherPoint[2],2));
            vec2[0] = vec1[1];
            vec2[1] = vec1[0];

            if (index==2)
                vec2[0] *= -1;
            else
                vec2[1] *= -1;
                
            double* normal = m_vtkPlaneGeometry->GetNormal();
            vec2[2] = (-normal[0]*vec2[0]-normal[1]*vec2[1])/normal[2];
            double lth = sqrt(pow(vec2[0],2)+pow(vec2[1],2)+pow(vec2[2],2));
            for (int i = 0; i<3; i++)
                vec2[i] = vec2[i]/lth*r;
            
            if ( sqrt(pow(vec2[0],2)+pow(vec2[1],2)+pow(vec2[2],2)) > 0 )
            {
                for(int i = 0; i<3; i++)
                    otherPoint[i] = centerPoint[i]+vec2[i];
                m_vtkPlaneGeometry->ProjectPoint(otherPoint,otherPt3d);
                m_ControlPoints[otherIndex]=otherPt3d;
            }
            m_TreatAsCircle = false;
        }

        ControlPointsChanged();
    }

}

void ContourEllipse::CreateContourPoints()
{
    std::array<double, 3> centerPoint, boundaryPoint1,boundaryPoint2;

    double tmpPt1[3], tmpPt2[3], tmpPt3[3];
    for (int i = 0; i<3; i++)
    {
        tmpPt1[i] = m_ControlPoints[0][i];
        tmpPt2[i] = m_ControlPoints[2][i];
        tmpPt3[i] = m_ControlPoints[3][i];
    }
    m_vtkPlaneGeometry->ProjectPoint(tmpPt1[0], centerPoint );
    m_vtkPlaneGeometry->ProjectPoint(tmpPt2[2], boundaryPoint1 );
    m_vtkPlaneGeometry->ProjectPoint(tmpPt3[3], boundaryPoint2 );

    double radius1 = sqrt(pow(centerPoint[0]-boundaryPoint1[0],2) +
                            pow(centerPoint[1]-boundaryPoint1[1],2) +
                            pow(centerPoint[2]-boundaryPoint1[2],2));
    
    double radius2 = sqrt(pow(centerPoint[0]-boundaryPoint2[0],2) +
                            pow(centerPoint[1]-boundaryPoint2[1],2) +
                            pow(centerPoint[2]-boundaryPoint2[2],2));
    
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
    std::array<double, 3> dir;
    for (int i = 0; i<3; i++)
        dir[i] = boundaryPoint1[i] - centerPoint[i];
        
    double lth = sqrt(pow(dir[0],2)+pow(dir[1],2)+pow(dir[2],2));
    for (int i = 0; i<3; i++)
        dir[i] = dir[i]/lth;
        
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
    //=====================================================================
    // construct rotation matrix to align ellipse with control point vector
    rot[0][0] = dir[0];
    rot[1][1] = rot[0][0];
    rot[1][0] = sin(acos(rot[0][0]));
    rot[0][1] = -rot[1][0];
    double* normal = m_vtkPlaneGeometry->GetNormal();

    for ( int i = start; i < end; ++i )
    {
        double alpha = (double) i * vnl_math::pi * 2.0 / interNumber;


        double vec[3];
        SegmentationUtils::getOrthogonalVector(normal,vec);
        vnl_vector_fixed< float, 2 > vec;
//        vnl_vector_fixed< double, 2 > vec;
        vec[0] = radius1 * cos( alpha );
        vec[1] = radius2 * sin( alpha );
        vec = rot*vec;

        mitk::Point2D point;
        mitk::Point3D pt3d;
        point[0] = centerPoint[0] + vec[0];
        point[1] = centerPoint[1] + vec[1];

        m_vtkPlaneGeometry->Map(point,pt3d);

        m_ContourPoints.push_back(pt3d);
    }

}

void ContourEllipse::AssignCenterScalingPoints()
{
    if(m_ControlPoints.size()>1)
        m_ControlPoints[1]=m_ScalingPoint;
}

void ContourEllipse::PlaceControlPoints(mitk::Point3D point)
{
    sv4guiContour::PlaceControlPoints(point);
    m_ControlPointSelectedIndex = 2;
}

sv4guiContour* ContourEllipse::CreateByFitting(sv4guiContour* contour)
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

    ContourEllipse* newContour=new ContourEllipse();
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

