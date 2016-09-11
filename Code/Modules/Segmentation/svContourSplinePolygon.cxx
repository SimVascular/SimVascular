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

    //just add the first two points as the last point
    controlPoints.insert(controlPoints.begin(),point);
    controlPoints.insert(controlPoints.begin(),point);

    svContourSplinePolygon* newContour=new svContourSplinePolygon();
    newContour->SetPathPoint(contour->GetPathPoint());
//    newContour->SetPlaneGeometry(contour->GetPlaneGeometry());
    newContour->SetPlaced(true);
    newContour->SetMethod(contour->GetMethod());
    newContour->SetClosed(contour->IsClosed());
    newContour->SetControlPoints(controlPoints);

    return newContour;
}
