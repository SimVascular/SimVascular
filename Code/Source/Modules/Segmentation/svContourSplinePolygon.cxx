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

#include "svContourSplinePolygon.h"
#include "svSpline.h"
#include "svSegmentationUtils.h"
#include "svVtkParametricSpline.h"

#include "vtkSplineFilter.h"


#include <iostream>
using namespace std;

svContourSplinePolygon::svContourSplinePolygon()
{
    m_Method="Manual";
    m_Type="SplinePolygon";
}

svContourSplinePolygon::svContourSplinePolygon(const svContourSplinePolygon &other)
    : svContourPolygon(other)
{
}

svContourSplinePolygon::~svContourSplinePolygon()
{
}

svContourSplinePolygon* svContourSplinePolygon::Clone()
{
    return new svContourSplinePolygon(*this);
}

std::string svContourSplinePolygon::GetClassName()
{
    return "svContourSplinePolygon";
}

void svContourSplinePolygon::CreateContourPoints()
{
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

    svSpline* spline=new svSpline();
    spline->SetClosed(m_Closed);

    switch(m_SubdivisionType)
    {
    case CONSTANT_TOTAL_NUMBER:
        spline->SetMethod(svSpline::CONSTANT_TOTAL_NUMBER);
        spline->SetCalculationNumber(m_SubdivisionNumber);
        break;
    case CONSTANT_SUBDIVISION_NUMBER:
        spline->SetMethod(svSpline::CONSTANT_SUBDIVISION_NUMBER);
        spline->SetCalculationNumber(m_SubdivisionNumber);
        break;
    case CONSTANT_SPACING:
        spline->SetMethod(svSpline::CONSTANT_SPACING);
        spline->SetSpacing(m_SubdivisionSpacing);
        break;
    default:
        break;
    }

    std::vector<mitk::Point3D> controlPoints;
    controlPoints.insert(controlPoints.begin(),m_ControlPoints.begin()+2,m_ControlPoints.end());

    spline->SetInputPoints(controlPoints);
    spline->Update();//remember Update() before fetching spline points
    m_ContourPoints=spline->GetSplinePosPoints();
}

svContour* svContourSplinePolygon::CreateByFitting(svContour* contour, int divisionNumber)
{
    int inputPointNumber=contour->GetContourPointNumber();

    if(inputPointNumber<3)
        return contour->Clone();

    svVtkParametricSpline* svpp= new svVtkParametricSpline();
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

    svContourSplinePolygon* newContour=new svContourSplinePolygon();
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
