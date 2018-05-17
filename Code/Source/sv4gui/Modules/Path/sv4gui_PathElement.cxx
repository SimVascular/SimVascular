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

#include "sv4gui_PathElement.h"
#include "sv4gui_Math3.h"

sv4guiPathElement::sv4guiPathElement()
    : m_Method(CONSTANT_TOTAL_NUMBER)
    , m_CalculationNumber(100)
    , m_Spacing(0)
{
}

sv4guiPathElement::sv4guiPathElement(const sv4guiPathElement &other)
    : m_Method(other.m_Method)
    , m_CalculationNumber(other.m_CalculationNumber)
    , m_Spacing(other.m_Spacing)
    , m_ControlPoints(other.m_ControlPoints)
    , m_PathPoints(other.m_PathPoints)
{
}

sv4guiPathElement::~sv4guiPathElement()
{
}

sv4guiPathElement* sv4guiPathElement::Clone()
{
    return new sv4guiPathElement(*this);
}

int sv4guiPathElement::GetControlPointNumber()
{
    return m_ControlPoints.size();
}

std::vector<mitk::Point3D> sv4guiPathElement::GetControlPoints()
{
    std::vector<mitk::Point3D> controlPoints;
    for(int i=0;i<m_ControlPoints.size();i++)
        controlPoints.push_back(m_ControlPoints[i].point);

    return controlPoints;
}

sv4guiPathElement::svControlPoint sv4guiPathElement::GetsvControlPoint(int index)
{
    svControlPoint controlPoint;
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        controlPoint=m_ControlPoints[index];
    }
    return controlPoint;
}

mitk::Point3D sv4guiPathElement::GetControlPoint(int index)
{
    return GetsvControlPoint(index).point;
}

void sv4guiPathElement::InsertControlPoint(int index, mitk::Point3D point)
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

//int sv4guiPathElement::GetInsertintIndexByDistance( mitk::Point3D point)
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

//        mitk::Point3D p0=m_ControlPoints[index].point;
//        mitk::Point3D p1=m_ControlPoints[index+1].point;

//        mitk::Point3D pa,pb;
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

int sv4guiPathElement::GetInsertintIndexByDistance(mitk::Point3D point)
{
    return sv4guiMath3::GetInsertintIndexByDistance(GetControlPoints(),point);
}


void sv4guiPathElement::RemoveControlPoint(int index)
{
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        m_ControlPoints.erase(m_ControlPoints.begin()+index);
        ControlPointsChanged();
    }
}

void sv4guiPathElement::SetControlPoint(int index, mitk::Point3D point)
{
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        m_ControlPoints[index].point=point;
        ControlPointsChanged();
    }
}

void sv4guiPathElement::SetControlPoints(std::vector<mitk::Point3D> points, bool update)
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

bool sv4guiPathElement::IsControlPointSelected(int index)
{
    bool selected=false;

    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        selected=m_ControlPoints[index].selected;
    }

    return selected;
}

void sv4guiPathElement::SetControlPointSelected( int index, bool selected)
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

void sv4guiPathElement::DeselectControlPoint()
{
    for(int i=0;i<m_ControlPoints.size();i++)
    {
        m_ControlPoints[i].selected=false;
    }
    ControlPointsChanged();
}

int sv4guiPathElement::GetControlPointSelectedIndex()
{
    for(int i=0;i<m_ControlPoints.size();i++)
    {
        if(m_ControlPoints[i].selected) return i;
    }

    return -2;
}

void sv4guiPathElement::ControlPointsChanged()
{
    CreatePathPoints();
}

int sv4guiPathElement::SearchControlPoint( mitk::Point3D point, mitk::ScalarType distance)
{
    int bestIndex = -2;
    mitk::ScalarType bestDist = distance;
    mitk::ScalarType dist;

    for (int i = 0; i < m_ControlPoints.size(); ++i)
    {
        mitk::Point3D pt=m_ControlPoints[i].point;

        if(point==pt)
        {
            return i;
        }

        dist=point.EuclideanDistanceTo(pt);
        if ( dist < bestDist )
        {
            bestIndex = i;
            bestDist  = dist;
        }
    }

    return bestIndex;
}

sv4guiPathElement* sv4guiPathElement::CreateSmoothedPathElement(int sampleRate, int numModes, bool controlPointsBased )
{
    int numPts;
    std::vector<mitk::Point3D> originalPoints;

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

    std::vector<mitk::Point3D> smoothedPoints=sv4guiMath3::CreateSmoothedCurve(originalPoints,false,numModes,sampleRate,0);

    sv4guiPathElement* newPathElement=new sv4guiPathElement();
    newPathElement->SetMethod(m_Method);
    newPathElement->SetCalculationNumber(m_CalculationNumber);
    newPathElement->SetSpacing(m_Spacing);
    newPathElement->SetControlPoints(smoothedPoints);

    return newPathElement;
}

int sv4guiPathElement::GetPathPointNumber()
{
    return m_PathPoints.size();
}

void sv4guiPathElement::SetSpacing(double spacing)
{
    m_Spacing=spacing;
}

double sv4guiPathElement::GetSpacing()
{
    return m_Spacing;
}

void sv4guiPathElement::SetMethod(sv4guiPathElement::CalculationMethod method)\
{
    m_Method=method;
}

sv4guiPathElement::CalculationMethod sv4guiPathElement::GetMethod()
{
    return m_Method;
}

void sv4guiPathElement::SetCalculationNumber(int number)
{
    m_CalculationNumber=number;
}

int sv4guiPathElement::GetCalculationNumber()
{
    return m_CalculationNumber;
}

std::vector<sv4guiPathElement::sv4guiPathPoint> sv4guiPathElement::GetPathPoints()
{
    return m_PathPoints;
}

std::vector<mitk::Point3D> sv4guiPathElement::GetPathPosPoints()
{
    std::vector<mitk::Point3D> posPoints;
    for(int i=0;i<m_PathPoints.size();i++)
        posPoints.push_back(m_PathPoints[i].pos);

    return posPoints;
}

sv4guiPathElement::sv4guiPathPoint sv4guiPathElement::GetPathPoint(int index)
{
    sv4guiPathPoint pathPoint;
    if(index==-1) index=m_PathPoints.size()-1;

    if(index>-1 && index<m_PathPoints.size())
    {
        pathPoint=m_PathPoints[index];
    }

    return pathPoint;
}

mitk::Point3D sv4guiPathElement::GetPathPosPoint(int index)
{
    return GetPathPoint(index).pos;
}

void sv4guiPathElement::SetPathPoints(std::vector<sv4guiPathElement::sv4guiPathPoint> pathPoints)
{
    m_PathPoints=pathPoints;
}

void sv4guiPathElement::CreatePathPoints()
{
    m_PathPoints.clear();

    int controlNumber=m_ControlPoints.size();

    if(controlNumber<2)
    {
        return;
    }

    sv4guiSpline* spline=new sv4guiSpline();
    spline->SetClosed(false);

    switch(m_Method)
    {
    case CONSTANT_TOTAL_NUMBER:
        spline->SetMethod(sv4guiSpline::CONSTANT_TOTAL_NUMBER);
        spline->SetCalculationNumber(m_CalculationNumber);
        break;
    case CONSTANT_SUBDIVISION_NUMBER:
        spline->SetMethod(sv4guiSpline::CONSTANT_SUBDIVISION_NUMBER);
        spline->SetCalculationNumber(m_CalculationNumber);
        break;
    case CONSTANT_SPACING:
        spline->SetMethod(sv4guiSpline::CONSTANT_SPACING);
        spline->SetSpacing(m_Spacing);
        break;
    default:
        break;
    }

    spline->SetInputPoints(GetControlPoints());
    spline->Update();//remember Update() before fetching spline points
    m_PathPoints=spline->GetSplinePoints();
}

void sv4guiPathElement::CalculateBoundingBox(double *bounds)
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

std::vector<sv4guiPathElement::sv4guiPathPoint> sv4guiPathElement::GetExtendedPathPoints(double realBounds[6], double minSpacing, int& startingIndex)
{
    startingIndex=0;

    if(m_PathPoints.size()<2)
        return m_PathPoints;

    mitk::Point3D origin;
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

    mitk::Point3D beginPathPosPoint=GetPathPoint(0).pos;
    mitk::Vector3D beginPathDirection=-GetPathPoint(0).tangent;

    mitk::Point3D endPathPosPoint=GetPathPoint(-1).pos;
    mitk::Vector3D endPathDirection=GetPathPoint(-1).tangent;

    mitk::Point3D interPoint;

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
    mitk::Point3D beginPoint=interPoint;

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

    mitk::Point3D endPoint=interPoint;

    std::vector<sv4guiPathElement::sv4guiPathPoint> beginPathPoints;
    std::vector<sv4guiPathElement::sv4guiPathPoint> endPathPoints;

    if(beginFound)
    {
        std::vector<mitk::Point3D> controlPoints={beginPoint,beginPathPosPoint};

        sv4guiPathElement* pathElement=new sv4guiPathElement();
        pathElement->SetMethod(sv4guiPathElement::CONSTANT_SPACING);
        pathElement->SetSpacing(minSpacing);
        pathElement->SetControlPoints(controlPoints);

        beginPathPoints=pathElement->GetPathPoints();
        startingIndex=beginPathPoints.size();
    }

    if(endFound)
    {
        std::vector<mitk::Point3D> controlPoints={endPathPosPoint,endPoint};

        sv4guiPathElement* pathElement=new sv4guiPathElement();
        pathElement->SetMethod(sv4guiPathElement::CONSTANT_SPACING);
        pathElement->SetSpacing(minSpacing);
        pathElement->SetControlPoints(controlPoints);

        endPathPoints=pathElement->GetPathPoints();
    }

    std::vector<sv4guiPathElement::sv4guiPathPoint> extendedPathPoints=GetPathPoints();
    if(beginFound)
        extendedPathPoints.insert(extendedPathPoints.begin(),beginPathPoints.begin(),beginPathPoints.end()-1);

    if(endFound)
        extendedPathPoints.insert(extendedPathPoints.end(),endPathPoints.begin()+1,endPathPoints.end());

    return extendedPathPoints;
}


    