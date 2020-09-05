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

#include "sv4gui_ContourSplinePolygon.h"
#include "sv3_Spline.h"
#include "sv4gui_Spline.h"
#include "sv4gui_SegmentationUtils.h"
#include "sv3_VtkParametricSpline.h"

#include "vtkSplineFilter.h"


#include <iostream>
using namespace std;
sv4guiContourSplinePolygon::sv4guiContourSplinePolygon()
{
    m_Method="Manual";
    m_Type="SplinePolygon";
    SetKernel(cKernelType::cKERNEL_SPLINEPOLYGON);
}

sv4guiContourSplinePolygon::sv4guiContourSplinePolygon(const sv4guiContourSplinePolygon &other)
    : sv4guiContourPolygon(other)
{
}

sv4guiContourSplinePolygon::~sv4guiContourSplinePolygon()
{
}

sv4guiContourSplinePolygon* sv4guiContourSplinePolygon::Clone()
{
    return new sv4guiContourSplinePolygon(*this);
}

std::string sv4guiContourSplinePolygon::GetClassName()
{
    return "sv4guiContourSplinePolygon";
}

void sv4guiContourSplinePolygon::CreateContourPoints()
{
    int controlNumber=GetControlPointNumber();

    if(controlNumber<=2)
    {
        return;
    }
    else if(controlNumber==3)
    {
        m_ContourPoints.push_back(sv3::Contour::GetControlPoint(2));
        return;
    }

    sv3::Spline* spline=new sv3::Spline();
    spline->SetClosed(m_Closed);

    switch(m_SubdivisionType)
    {
    case CONSTANT_TOTAL_NUMBER:
        spline->SetMethod(sv4guiSpline::CONSTANT_TOTAL_NUMBER);
        spline->SetCalculationNumber(m_SubdivisionNumber);
        break;
    case CONSTANT_SUBDIVISION_NUMBER:
        spline->SetMethod(sv4guiSpline::CONSTANT_SUBDIVISION_NUMBER);
        spline->SetCalculationNumber(m_SubdivisionNumber);
        break;
    case CONSTANT_SPACING:
        spline->SetMethod(sv4guiSpline::CONSTANT_SPACING);
        spline->SetSpacing(m_SubdivisionSpacing);
        break;
    default:
        break;
    }

    std::vector<std::array<double,3> > controlPoints;
    controlPoints.insert(controlPoints.begin(),m_ControlPoints.begin()+2,m_ControlPoints.end());

    spline->SetInputPoints(controlPoints);
    spline->Update();//remember Update() before fetching spline points
    m_ContourPoints=spline->GetSplinePosPoints();
}

sv4guiContour* sv4guiContourSplinePolygon::CreateByFitting(sv4guiContour* contour, int divisionNumber)
{
    int inputPointNumber=contour->GetContourPointNumber();

    if(inputPointNumber<3)
        return contour->Clone();

    sv3::VtkParametricSpline* svpp= new sv3::VtkParametricSpline();
    svpp->ParameterizeByLengthOn();

    if(contour->IsClosed())
        svpp->ClosedOn();
    else
        svpp->ClosedOff();

    svpp->SetNumberOfPoints(inputPointNumber);

    for(int i=0;i<inputPointNumber;i++)
    {
        mitk::Point3D point=contour->GetContourPoint(i);
        svpp->SetPoint(i,point[0],point[1],point[2]);
    }

    double pt[3];
    mitk::Point3D point;
    std::vector<mitk::Point3D> controlPoints;
    for(int i=0;i<=divisionNumber;i++)
    {
        if(i==divisionNumber&&contour->IsClosed())
            break;

        svpp->EvaluateByLengthFactor(i*1.0/divisionNumber, pt);
        point[0]=pt[0];
        point[1]=pt[1];
        point[2]=pt[2];
        controlPoints.push_back(point);
    }

    //just add the first two points using the last point
    controlPoints.insert(controlPoints.begin(),point);
    controlPoints.insert(controlPoints.begin(),point);

    sv4guiContourSplinePolygon* newContour=new sv4guiContourSplinePolygon();
    newContour->SetPathPoint(contour->GetPathPoint());
//    newContour->SetPlaneGeometry(contour->GetPlaneGeometry());
    newContour->SetPlaced(true);
    newContour->SetMethod(contour->GetMethod());
    newContour->SetClosed(contour->IsClosed());
    newContour->SetControlPoints(controlPoints);

    newContour->SetSubdivisionType(contour->GetSubdivisionType());
    newContour->SetSubdivisionSpacing(contour->GetSubdivisionSpacing());
    newContour->SetSubdivisionNumber(contour->GetSubdivisionNumber());

    return newContour;
}
