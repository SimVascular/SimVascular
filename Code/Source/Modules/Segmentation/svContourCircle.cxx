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

#include "svContourCircle.h"

svContourCircle::svContourCircle()
{
    m_Method="Manual";
    m_Type="Circle";

    m_MinControlPointNumber=2;
    m_MaxControlPointNumber=2;
    m_ControlPointNonRemovableIndices[0]=0;
    m_ControlPointNonRemovableIndices[1]=1;

    m_SubdivisionType=CONSTANT_TOTAL_NUMBER;
    m_SubdivisionNumber=36;
}

svContourCircle::svContourCircle(const svContourCircle &other)
    :svContour(other)
{
}

svContourCircle::~svContourCircle()
{
}

svContourCircle* svContourCircle::Clone()
{
    return new svContourCircle(*this);
}

std::string svContourCircle::GetClassName()
{
    return "svContourCircle";
}

void svContourCircle::SetControlPoint(int index, mitk::Point3D point)
{
    if(index == 0)
    {
        mitk::Vector3D dirVec=point-GetControlPoint(index);
        Shift(dirVec);
    }
    else if ( index == 1 )
    {
        m_ControlPoints[index]=point;
        ControlPointsChanged();
    }

}

void svContourCircle::CreateContourPoints()
{
    mitk::Point2D centerPoint, boundaryPoint;

    m_PlaneGeometry->Map(m_ControlPoints[0], centerPoint );

    m_PlaneGeometry->Map(m_ControlPoints[1], boundaryPoint );

    double radius = centerPoint.EuclideanDistanceTo( boundaryPoint );

    int interNumber;

    switch(m_SubdivisionType)
    {
    case CONSTANT_TOTAL_NUMBER:
        interNumber=m_SubdivisionNumber;
        break;
    case CONSTANT_SPACING:
        interNumber=2.0*vnl_math::pi*radius/m_SubdivisionSpacing;
        if(interNumber<m_SubdivisionNumber)
        {
            interNumber=m_SubdivisionNumber;
        }
        break;
    default:
        break;
    }

    for ( int i = 0; i < interNumber; ++i )
    {
        double alpha = (double) i * vnl_math::pi * 2.0 / interNumber;

        mitk::Point2D point;
        mitk::Point3D pt3d;
        point[0] = centerPoint[0] + radius * cos( alpha );
        point[1] = centerPoint[1] + radius * sin( alpha );

        m_PlaneGeometry->Map(point,pt3d);

        m_ContourPoints.push_back(pt3d);

    }
}

void svContourCircle::AssignCenterScalingPoints()
{
}

svContour* svContourCircle::CreateByFitting(svContour* contour)
{
    double area=contour->GetArea();
    double radius=sqrt(area/vnl_math::pi);
    mitk::Point2D centerPoint, boundaryPoint;

    contour->GetPlaneGeometry()->Map(contour->GetControlPoint(0), centerPoint );

    boundaryPoint[0]=centerPoint[0]+radius;
    boundaryPoint[1]=centerPoint[1];

    std::vector<mitk::Point3D> controlPoints;

    mitk::Point3D pt1,pt2;

    contour->GetPlaneGeometry()->Map(centerPoint,pt1);
    contour->GetPlaneGeometry()->Map(boundaryPoint,pt2);

    controlPoints.push_back(pt1);
    controlPoints.push_back(pt2);

    svContourCircle* newContour=new svContourCircle();
    newContour->SetPathPoint(contour->GetPathPoint());
    newContour->SetPlaced(true);
    newContour->SetMethod(contour->GetMethod());
//    newContour->SetClosed(contour->IsClosed());
    newContour->SetControlPoints(controlPoints);

    newContour->SetSubdivisionType(contour->GetSubdivisionType());
    newContour->SetSubdivisionSpacing(contour->GetSubdivisionSpacing());
    newContour->SetSubdivisionNumber(contour->GetSubdivisionNumber());

    return newContour;
}
