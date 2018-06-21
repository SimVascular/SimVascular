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

#include "sv4gui_ContourCircle.h"
#include "sv3_CircleContour.h"

using sv3::circleContour;

sv4guiContourCircle::sv4guiContourCircle():circleContour()
{
}

sv4guiContourCircle::sv4guiContourCircle(const sv4guiContourCircle &other)
    :sv4guiContour(other)
{
}

sv4guiContourCircle::~sv4guiContourCircle()
{
}

sv4guiContourCircle* sv4guiContourCircle::Clone()
{
    return new sv4guiContourCircle(*this);
}

void sv4guiContourCircle::SetControlPoint(int index, mitk::Point3D point)
{
    std::array<double,3> stdPt;
    for (int i=0; i<3; i++)
        stdPt[i] = point[i];
        
    this->sv3::circleContour::SetControlPoint(index,stdPt);
}

sv4guiContour* sv4guiContourCircle::CreateByFitting(sv4guiContour* contour)
{
    std::cout<<"sv4guiContourCircle::CreateByFitting()"<<std::endl;
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

    sv4guiContourCircle* newContour=new sv4guiContourCircle();
    newContour->sv4guiContour::SetPathPoint(contour->sv4guiContour::GetPathPoint());
    newContour->SetPlaced(true);
    newContour->sv4guiContour::SetMethod(contour->sv4guiContour::GetMethod());
//    newContour->SetClosed(contour->IsClosed());
    newContour->sv4guiContour::SetControlPoints(controlPoints);

    newContour->sv4guiContour::SetSubdivisionType(contour->sv4guiContour::GetSubdivisionType());
    newContour->sv4guiContour::SetSubdivisionSpacing(contour->sv4guiContour::GetSubdivisionSpacing());
    newContour->sv4guiContour::SetSubdivisionNumber(contour->sv4guiContour::GetSubdivisionNumber());

    return newContour;
}
