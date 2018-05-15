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

#include "sv4_PathElement.h"
#include "cvRepositoryData.h"
#include "cvMath.h"
#include <array>
#include <vector>

sv4PathElement::sv4PathElement() : cvRepositoryData( PATH_T )
{
    m_Method = CONSTANT_TOTAL_NUMBER;
    m_CalculationNumber=100;
    m_Spacing = 0;
}

sv4PathElement::sv4PathElement(const sv4PathElement &other) : cvRepositoryData( PATH_T )
{
    m_Method = other.m_Method;
    m_CalculationNumber=other.m_CalculationNumber;
    m_Spacing=other.m_Spacing;
    m_ControlPoints=other.m_ControlPoints;
    m_PathPoints=other.m_PathPoints;
}

sv4PathElement::~sv4PathElement()
{
}

sv4PathElement* sv4PathElement::Clone()
{
    return new sv4PathElement(*this);
}

int sv4PathElement::GetControlPointNumber()
{
    return m_ControlPoints.size();
}

std::vector<std::array<double,3> > sv4PathElement::GetControlPoints()
{
    std::vector<std::array<double,3> > controlPoints;
    for(int i=0;i<m_ControlPoints.size();i++)
        controlPoints.push_back(m_ControlPoints[i].point);

    return controlPoints;
}

sv4PathElement::svControlPoint sv4PathElement::GetsvControlPoint(int index)
{
    svControlPoint controlPoint;
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        controlPoint=m_ControlPoints[index];
    }
    return controlPoint;
}

std::array<double,3>  sv4PathElement::GetControlPoint(int index)
{
    return GetsvControlPoint(index).point;
}

void sv4PathElement::InsertControlPoint(int index, std::array<double,3>  point)
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

//int sv4PathElement::GetInsertintIndexByDistance( std::array<double,3>  point)
//{
//    int idx=-2;

//    if(m_ControlPoints.size()<2){
//        idx=m_ControlPoints.size();
//    }else{
//        double dis1,dis2,minDisSum;
//        int index=0;
//        for (int i = 0; i < m_ControlPoints.size()-1; ++i)
//        {
//            dis1=point.EuclideanDistanceTo(m_ControlPoints[i].point);
//            dis2=point.EuclideanDistanceTo(m_ControlPoints[i+1].point);
//            if(i==0)
//            {
//                minDisSum=dis1+dis2;
//                index=i;
//            }else{
//                if(minDisSum>(dis1+dis2)){
//                    minDisSum=dis1+dis2;
//                    index=i;
//                }
//            }
//        }

//        double* p0=m_ControlPoints[index].point;
//        double* p1=m_ControlPoints[index+1].point;

//        double* pa,pb;
//        pa[0]=p1[0]-p0[0];
//        pa[1]=p1[1]-p0[1];
//        pa[2]=p1[2]-p0[2];
//        pb[0]=point[0]-p0[0];
//        pb[1]=point[1]-p0[1];
//        pb[2]=point[2]-p0[2];

//        double distance=(pa[0]*pb[0]+pa[1]*pb[1]+pa[2]*pb[2])/p1.EuclideanDistanceTo(p0);

//        if(distance<=0)
//        {
//            idx=index;
//        }else if(distance<p1.EuclideanDistanceTo(p0)){
//            idx=index+1;
//        }else{
//            idx=index+2;
//        }

//    }
//    return idx;
//}

int sv4PathElement::GetInsertintIndexByDistance(std::array<double,3>  point)
{
    cvMath *cMath = new cvMath();
    int result = cMath->GetInsertintIndexByDistance(GetControlPoints(),point);
    delete cMath;
    return result;
}


void sv4PathElement::RemoveControlPoint(int index)
{
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        m_ControlPoints.erase(m_ControlPoints.begin()+index);
        ControlPointsChanged();
    }
}

void sv4PathElement::SetControlPoint(int index, std::array<double,3>  point)
{
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        m_ControlPoints[index].point=point;
        ControlPointsChanged();
    }
}

void sv4PathElement::SetControlPoints(std::vector<std::array<double,3> > points, bool update)
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

bool sv4PathElement::IsControlPointSelected(int index)
{
    bool selected=false;

    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        selected=m_ControlPoints[index].selected;
    }

    return selected;
}

void sv4PathElement::SetControlPointSelected( int index, bool selected)
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

void sv4PathElement::DeselectControlPoint()
{
    for(int i=0;i<m_ControlPoints.size();i++)
    {
        m_ControlPoints[i].selected=false;
    }
    ControlPointsChanged();
}

int sv4PathElement::GetControlPointSelectedIndex()
{
    for(int i=0;i<m_ControlPoints.size();i++)
    {
        if(m_ControlPoints[i].selected) return i;
    }

    return -2;
}

void sv4PathElement::ControlPointsChanged()
{
    CreatePathPoints();
}

int sv4PathElement::SearchControlPoint( std::array<double,3>  point, double distance)
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

sv4PathElement* sv4PathElement::CreateSmoothedPathElement(int sampleRate, int numModes, bool controlPointsBased )
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
    

    sv4PathElement* newPathElement=new sv4PathElement();
    newPathElement->SetMethod(m_Method);
    newPathElement->SetCalculationNumber(m_CalculationNumber);
    newPathElement->SetSpacing(m_Spacing);
    newPathElement->SetControlPoints(smoothedPoints);

    return newPathElement;
}

int sv4PathElement::GetPathPointNumber()
{
    return m_PathPoints.size();
}

void sv4PathElement::SetSpacing(double spacing)
{
    m_Spacing=spacing;
}

double sv4PathElement::GetSpacing()
{
    return m_Spacing;
}

void sv4PathElement::SetMethod(sv4PathElement::CalculationMethod method)\
{
    m_Method=method;
}

sv4PathElement::CalculationMethod sv4PathElement::GetMethod()
{
    return m_Method;
}

void sv4PathElement::SetCalculationNumber(int number)
{
    m_CalculationNumber=number;
}

int sv4PathElement::GetCalculationNumber()
{
    return m_CalculationNumber;
}

std::vector<sv4PathElement::sv4PathPoint> sv4PathElement::GetPathPoints()
{
    return m_PathPoints;
}

std::vector<std::array<double,3> > sv4PathElement::GetPathPosPoints()
{
    std::vector<std::array<double,3> > posPoints;
    for(int i=0;i<m_PathPoints.size();i++)
        posPoints.push_back(m_PathPoints[i].pos);

    return posPoints;
}

sv4PathElement::sv4PathPoint sv4PathElement::GetPathPoint(int index)
{
    sv4PathPoint pathPoint;
    if(index==-1) index=m_PathPoints.size()-1;

    if(index>-1 && index<m_PathPoints.size())
    {
        pathPoint=m_PathPoints[index];
    }

    return pathPoint;
}

std::array<double,3> sv4PathElement::GetPathPosPoint(int index)
{
    return GetPathPoint(index).pos;
}

void sv4PathElement::SetPathPoints(std::vector<sv4PathElement::sv4PathPoint> pathPoints)
{
    m_PathPoints=pathPoints;
}

void sv4PathElement::CreatePathPoints()
{
    m_PathPoints.clear();

    int controlNumber=m_ControlPoints.size();

    if(controlNumber<2)
    {
        return;
    }

    sv4Spline* spline=new sv4Spline();
    spline->SetClosed(false);

    switch(m_Method)
    {
    case CONSTANT_TOTAL_NUMBER:
        spline->SetMethod(sv4Spline::CONSTANT_TOTAL_NUMBER);
        spline->SetCalculationNumber(m_CalculationNumber);
        break;
    case CONSTANT_SUBDIVISION_NUMBER:
        spline->SetMethod(sv4Spline::CONSTANT_SUBDIVISION_NUMBER);
        spline->SetCalculationNumber(m_CalculationNumber);
        break;
    case CONSTANT_SPACING:
        spline->SetMethod(sv4Spline::CONSTANT_SPACING);
        spline->SetSpacing(m_Spacing);
        break;
    default:
        break;
    }

    spline->SetInputPoints(GetControlPoints());
    spline->Update();//remember Update() before fetching spline points
    m_PathPoints=spline->GetSplinePoints();
}

void sv4PathElement::CalculateBoundingBox(double *bounds)
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

/*
std::vector<sv4PathElement::sv4PathPoint> sv4PathElement::GetExtendedPathPoints(double realBounds[6], double minSpacing, int& startingIndex)
{
    startingIndex=0;

    if(m_PathPoints.size()<2)
        return m_PathPoints;

    double* origin;
    mitk::Vector3D normal;
    mitk::FillVector3D(origin,realBounds[0],realBounds[2],realBounds[4]);
    mitk::FillVector3D(normal,1,0,0);
    mitk::PlaneGeometry::Pointer px1 = mitk::PlaneGeometry::New();
    px1->InitializePlane(origin,normal);

    mitk::FillVector3D(normal,0,1,0);
    mitk::PlaneGeometry::Pointer py1 = mitk::PlaneGeometry::New();
    py1->InitializePlane(origin,normal);

    mitk::FillVector3D(normal,0,0,1);
    mitk::PlaneGeometry::Pointer pz1 = mitk::PlaneGeometry::New();
    pz1->InitializePlane(origin,normal);

    mitk::FillVector3D(origin,realBounds[1],realBounds[3],realBounds[5]);
    mitk::FillVector3D(normal,1,0,0);
    mitk::PlaneGeometry::Pointer px2 = mitk::PlaneGeometry::New();
    px2->InitializePlane(origin,normal);

    mitk::FillVector3D(normal,0,1,0);
    mitk::PlaneGeometry::Pointer py2 = mitk::PlaneGeometry::New();
    py2->InitializePlane(origin,normal);

    mitk::FillVector3D(normal,0,0,1);
    mitk::PlaneGeometry::Pointer pz2 = mitk::PlaneGeometry::New();
    pz2->InitializePlane(origin,normal);

    mitk::PlaneGeometry::Pointer planes[6]={px1,px2,py1,py2,pz1,pz2};

    double* beginPathPosPoint=GetPathPoint(0).pos;
    mitk::Vector3D beginPathDirection=-GetPathPoint(0).tangent;

    double* endPathPosPoint=GetPathPoint(-1).pos;
    mitk::Vector3D endPathDirection=GetPathPoint(-1).tangent;

    double* interPoint;

    bool beginFound=false;
    for(int i=0;i<6;i++)
    {
        if(sv4guiMath3::GetIntersectionPoint(planes[i], beginPathPosPoint,beginPathDirection,interPoint))
        {
            if(sv4guiMath3::InsideBounds(interPoint,realBounds))
            {
                beginFound=true;
                break;
            }
        }
    }
    double* beginPoint=interPoint;

    bool endFound=false;
    for(int i=0;i<6;i++)
    {
        if(sv4guiMath3::GetIntersectionPoint(planes[i], endPathPosPoint,endPathDirection,interPoint))
        {
            if(sv4guiMath3::InsideBounds(interPoint,realBounds))
            {
                endFound=true;
                break;
            }
        }
    }

    double* endPoint=interPoint;

    std::vector<sv4PathElement::sv4PathPoint> beginPathPoints;
    std::vector<sv4PathElement::sv4PathPoint> endPathPoints;

    if(beginFound)
    {
        std::vector<double*> controlPoints={beginPoint,beginPathPosPoint};

        sv4PathElement* pathElement=new sv4PathElement();
        pathElement->SetMethod(sv4PathElement::CONSTANT_SPACING);
        pathElement->SetSpacing(minSpacing);
        pathElement->SetControlPoints(controlPoints);

        beginPathPoints=pathElement->GetPathPoints();
        startingIndex=beginPathPoints.size();
    }

    if(endFound)
    {
        std::vector<double*> controlPoints={endPathPosPoint,endPoint};

        sv4PathElement* pathElement=new sv4PathElement();
        pathElement->SetMethod(sv4PathElement::CONSTANT_SPACING);
        pathElement->SetSpacing(minSpacing);
        pathElement->SetControlPoints(controlPoints);

        endPathPoints=pathElement->GetPathPoints();
    }

    std::vector<sv4PathElement::sv4PathPoint> extendedPathPoints=GetPathPoints();
    if(beginFound)
        extendedPathPoints.insert(extendedPathPoints.begin(),beginPathPoints.begin(),beginPathPoints.end()-1);

    if(endFound)
        extendedPathPoints.insert(extendedPathPoints.end(),endPathPoints.begin()+1,endPathPoints.end());

    return extendedPathPoints;
}
*/
