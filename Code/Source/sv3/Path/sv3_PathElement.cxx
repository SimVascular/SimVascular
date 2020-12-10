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
#include "SimVascular.h"

#include "sv3_PathElement.h"
#include "sv_RepositoryData.h"
#include "sv_Math.h"
#include <array>
#include <vector>

using sv3::PathElement;
using sv3::Spline;

PathElement::PathElement() : cvRepositoryData( PATH_T )
{
    m_Method = CONSTANT_TOTAL_NUMBER;
    m_CalculationNumber=100;
    m_Spacing = 0;
}

PathElement::PathElement(const PathElement &other) : cvRepositoryData( PATH_T )
{
    m_Method = other.m_Method;
    m_CalculationNumber=other.m_CalculationNumber;
    m_Spacing=other.m_Spacing;
    m_ControlPoints=other.m_ControlPoints;
    m_PathPoints=other.m_PathPoints;
}

PathElement::~PathElement()
{
}

PathElement* PathElement::Clone()
{
    return new PathElement(*this);
}

int PathElement::GetControlPointNumber()
{
    return m_ControlPoints.size();
}

std::vector<std::array<double,3> > PathElement::GetControlPoints() const
{
    std::vector<std::array<double,3> > controlPoints;
    for(int i=0;i<m_ControlPoints.size();i++)
        controlPoints.push_back(m_ControlPoints[i].point);

    return controlPoints;
}

PathElement::svControlPoint PathElement::GetsvControlPoint(int index)
{
    svControlPoint controlPoint;
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        controlPoint=m_ControlPoints[index];
    }
    return controlPoint;
}

std::array<double,3>  PathElement::GetControlPoint(int index)
{
    return GetsvControlPoint(index).point;
}

void PathElement::InsertControlPoint(int index, std::array<double,3>  point)
{
    if(index==-1) index=m_ControlPoints.size();

    if(index>-1 && index<=m_ControlPoints.size())
    {
        svControlPoint controlPoint;
        controlPoint.point=point;
        m_ControlPoints.insert(m_ControlPoints.begin()+index,controlPoint);
        ControlPointsChanged();
    }
}


int PathElement::GetInsertintIndexByDistance(std::array<double,3>  point)
{
    cvMath *cMath = new cvMath();
    int result = cMath->GetInsertintIndexByDistance(GetControlPoints(),point);
    delete cMath;
    return result;
}


void PathElement::RemoveControlPoint(int index)
{
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        m_ControlPoints.erase(m_ControlPoints.begin()+index);
        ControlPointsChanged();
    }
}

void PathElement::SetControlPoint(int index, std::array<double,3>  point)
{
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        m_ControlPoints[index].point=point;
        ControlPointsChanged();
    }
}

void PathElement::SetControlPoints(std::vector<std::array<double,3> > points, bool update)
{
    std::vector<svControlPoint> controlPoints;
    for(int i=0;i<points.size();i++)
    {
        svControlPoint controlPoint;
        controlPoint.id=i;
        controlPoint.point=points[i];
        controlPoints.push_back(controlPoint);
    }
    m_ControlPoints=controlPoints;
    if(update)
        ControlPointsChanged();
}

bool PathElement::IsControlPointSelected(int index)
{
    bool selected=false;

    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        selected=m_ControlPoints[index].selected;
    }

    return selected;
}

void PathElement::SetControlPointSelected( int index, bool selected)
{
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        if(m_ControlPoints[index].selected!=selected)
        {
            m_ControlPoints[index].selected=selected;
            ControlPointsChanged();
        }

    }
}

void PathElement::DeselectControlPoint()
{
    for(int i=0;i<m_ControlPoints.size();i++)
    {
        m_ControlPoints[i].selected=false;
    }
    ControlPointsChanged();
}

int PathElement::GetControlPointSelectedIndex()
{
    for(int i=0;i<m_ControlPoints.size();i++)
    {
        if(m_ControlPoints[i].selected) return i;
    }

    return -2;
}

void PathElement::ControlPointsChanged()
{
    CreatePathPoints();
}

int PathElement::SearchControlPoint( std::array<double,3>  point, double distance)
{
    int bestIndex = -2;
    double bestDist = distance;
    double dist;

    for (int i = 0; i < m_ControlPoints.size(); ++i)
    {
        std::array<double,3>  pt=m_ControlPoints[i].point;

        if(point==pt)
        {
            return i;
        }

        dist=sqrt(pow(point[0]-pt[0],2)+pow(point[1]-pt[1],2)+pow(point[2]-pt[2],2));
        if ( dist < bestDist )
        {
            bestIndex = i;
            bestDist  = dist;
        }
    }

    return bestIndex;
}

PathElement* PathElement::CreateSmoothedPathElement(int sampleRate, int numModes, bool controlPointsBased )
{
    int numPts;
    std::vector<std::array<double,3> > originalPoints;

    if(controlPointsBased)
    {
        numPts=m_ControlPoints.size();

        if(sampleRate==0){
            if(numPts>250){
                sampleRate=10;
            }else if(numPts>100){
                sampleRate=5;
            }else{
                sampleRate=3;
            }
        }

        for(int i=0;i<m_ControlPoints.size();i++)
            originalPoints.push_back(m_ControlPoints[i].point);

    }
    else
    {
        numPts=m_PathPoints.size();

        if(sampleRate==0){
            if(numPts>250){
                sampleRate=10;
            }else if(numPts>100){
                sampleRate=5;
            }else{
                sampleRate=3;
            }
        }

        for(int i=0;i<m_PathPoints.size();i++)
            originalPoints.push_back(m_PathPoints[i].pos);

    }

    std::vector<std::array<double,3> > smoothedPoints;
    int outputNumPts=0;
    numPts=originalPoints.size();

    std::vector<std::array<double,3> > actualPoints;
    
    if(sampleRate>0)
    {
        for(int i=0;i<numPts;i+=sampleRate){
            actualPoints.push_back(originalPoints[i]);
        }
        if(sampleRate>1){
            actualPoints.push_back(originalPoints[numPts-1]);
        }
        outputNumPts=actualPoints.size();
    }
    
    for(int i=outputNumPts-1;i>=0;i--){
        actualPoints.push_back(actualPoints[i]);
    }
    
    int actualNumPts=actualPoints.size();

    cvMath *cMath = new cvMath();

    double **pts = cMath->createArray(actualNumPts,3);
    for(int i=0;i<actualNumPts;i++)
    {
        pts[i][0] = actualPoints[i][0];
        pts[i][1] = actualPoints[i][1];
        pts[i][2] = actualPoints[i][2];
    }
    double **outPts = NULL;
    int isClosed=0;
    int rslt;
    rslt=cMath->smoothCurve(pts, actualNumPts, 0, numModes, 2*outputNumPts, &outPts);

    if (rslt!=SV_ERROR)
    {
        for(int i=0;i<outputNumPts;i++){
        std::array<double,3> point;
        point[0]=outPts[i][0];
        point[1]=outPts[i][1];
        point[2]=outPts[i][2];

        smoothedPoints.push_back(point);
        }
    }
    

    PathElement* newPathElement=new PathElement();
    newPathElement->SetMethod(m_Method);
    newPathElement->SetCalculationNumber(m_CalculationNumber);
    newPathElement->SetSpacing(m_Spacing);
    newPathElement->SetControlPoints(smoothedPoints);

    return newPathElement;
}

int PathElement::GetPathPointNumber()
{
    return m_PathPoints.size();
}

void PathElement::SetSpacing(double spacing)
{
    m_Spacing=spacing;
}

double PathElement::GetSpacing()
{
    return m_Spacing;
}

void PathElement::SetMethod(PathElement::CalculationMethod method)
{
    m_Method=method;
}

PathElement::CalculationMethod PathElement::GetMethod()
{
    return m_Method;
}

void PathElement::SetCalculationNumber(int number)
{
    m_CalculationNumber=number;
}

int PathElement::GetCalculationNumber()
{
    return m_CalculationNumber;
}

std::vector<PathElement::PathPoint> PathElement::GetPathPoints() const
{
    return m_PathPoints;
}

std::vector<std::array<double,3> > PathElement::GetPathPosPoints()
{
    std::vector<std::array<double,3> > posPoints;
    for(int i=0;i<m_PathPoints.size();i++)
        posPoints.push_back(m_PathPoints[i].pos);

    return posPoints;
}

PathElement::PathPoint PathElement::GetPathPoint(int index)
{
    PathElement::PathPoint pathPoint;
    if(index==-1) index=m_PathPoints.size()-1;

    if(index>-1 && index<m_PathPoints.size())
    {
        pathPoint=m_PathPoints[index];
    }

    return pathPoint;
}

std::array<double,3> PathElement::GetPathPosPoint(int index)
{
    return GetPathPoint(index).pos;
}

void PathElement::SetPathPoints(std::vector<PathElement::PathPoint> pathPoints)
{
    m_PathPoints=pathPoints;
}

void PathElement::CreatePathPoints()
{

    m_PathPoints.clear();

    int controlNumber=m_ControlPoints.size();

    if(controlNumber<2)
    {
        return;
    }

    Spline* spline=new Spline();
    spline->SetClosed(false);

    switch(m_Method)
    {
    case CONSTANT_TOTAL_NUMBER:
        spline->SetMethod(Spline::CONSTANT_TOTAL_NUMBER);
        spline->SetCalculationNumber(m_CalculationNumber);
        break;
    case CONSTANT_SUBDIVISION_NUMBER:
        spline->SetMethod(Spline::CONSTANT_SUBDIVISION_NUMBER);
        spline->SetCalculationNumber(m_CalculationNumber);
        break;
    case CONSTANT_SPACING:
        spline->SetMethod(Spline::CONSTANT_SPACING);
        spline->SetSpacing(m_Spacing);
        break;
    default:
        break;
    }
    spline->SetInputPoints(GetControlPoints());
    spline->Update();//remember Update() before fetching spline points
    m_PathPoints=spline->GetSplinePoints();
}

void PathElement::CalculateBoundingBox(double *bounds)
{
    for(int i=0;i<m_ControlPoints.size();i++)
    {
        double x=m_ControlPoints[i].point[0];
        double y=m_ControlPoints[i].point[1];
        double z=m_ControlPoints[i].point[2];

        if(i==0){
            bounds[0]=x;
            bounds[1]=x;
            bounds[2]=y;
            bounds[3]=y;
            bounds[4]=z;
            bounds[5]=z;
        }else{
            if(x<bounds[0]) bounds[0]=x;
            if(x>bounds[1]) bounds[1]=x;
            if(y<bounds[2]) bounds[2]=y;
            if(y>bounds[3]) bounds[3]=y;
            if(z<bounds[4]) bounds[4]=z;
            if(z>bounds[5]) bounds[5]=z;
        }
    }

    for (int i = 0; i < m_PathPoints.size(); i++) {
        double x=m_PathPoints[i].pos[0];
        double y=m_PathPoints[i].pos[1];
        double z=m_PathPoints[i].pos[2];
        if(x<bounds[0]) bounds[0]=x;
        if(x>bounds[1]) bounds[1]=x;
        if(y<bounds[2]) bounds[2]=y;
        if(y>bounds[3]) bounds[3]=y;
        if(z<bounds[4]) bounds[4]=z;
        if(z>bounds[5]) bounds[5]=z;
    }
}

//---------------------------
// CreateVtkPolyDataFromPath
//---------------------------
// Get the geometry for the path control or path curve
// points as vtkPolyData.
//
vtkSmartPointer<vtkPolyData>
PathElement::CreateVtkPolyDataFromPath(bool fromControlPoints)
{
  vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
  vtkSmartPointer<vtkCellArray> lines = vtkSmartPointer<vtkCellArray>::New();
  auto numCurvePoints = m_PathPoints.size();

  for (int i = 0; i <= numCurvePoints; i++) {
     std::array<double, 3> point = m_PathPoints[i].pos;
     points->InsertPoint(i, point[0], point[1], point[2]);

     if ((i > 0) && (i < numCurvePoints)) {
         vtkIdType cell[2] = {i-1,i};
         lines->InsertNextCell(2,cell);
      }
  }

  vtkSmartPointer<vtkPolyData> polyData = vtkSmartPointer<vtkPolyData>::New();
  polyData->SetPoints(points);
  polyData->SetLines(lines);

  return polyData;
}

