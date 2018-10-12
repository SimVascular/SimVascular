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

sv4guiPathElement::sv4guiPathElement(): sv3::PathElement()
{
}

sv4guiPathElement::sv4guiPathElement(const sv4guiPathElement &other): sv3::PathElement(other)
{
}

sv4guiPathElement::~sv4guiPathElement()
{
}

sv4guiPathElement* sv4guiPathElement::Clone()
{
    return new sv4guiPathElement(*this);
}

std::vector<mitk::Point3D> sv4guiPathElement::GetControlPoints()
{
    std::vector<mitk::Point3D> controlPoints;
    for(int i=0;i<m_ControlPoints.size();i++)
    {
        mitk::Point3D pt;
        pt[0] = m_ControlPoints[i].point[0];
        pt[1] = m_ControlPoints[i].point[1];
        pt[2] = m_ControlPoints[i].point[2];
        controlPoints.push_back(pt);
    }
    return controlPoints;
}

sv4guiPathElement::svControlPoint sv4guiPathElement::GetsvControlPoint(int index)
{
    svControlPoint controlPoint;
    sv3::PathElement::svControlPoint stdctrlpt = this -> sv3::PathElement::GetsvControlPoint(index);
    
    controlPoint.id = stdctrlpt.id;
    controlPoint.selected = stdctrlpt.selected;
    controlPoint.point[0] = stdctrlpt.point[0];
    controlPoint.point[1] = stdctrlpt.point[1];
    controlPoint.point[2] = stdctrlpt.point[2];

    return controlPoint;
}

mitk::Point3D sv4guiPathElement::GetControlPoint(int index)
{
    return GetsvControlPoint(index).point;
}

void sv4guiPathElement::InsertControlPoint(int index, mitk::Point3D point)
{
    std::array<double,3> stdpt;
    stdpt[0] = point[0];
    stdpt[1] = point[1];
    stdpt[2] = point[2];
    
    this->sv3::PathElement::InsertControlPoint(index,stdpt);    
}

int sv4guiPathElement::GetInsertintIndexByDistance(mitk::Point3D point)
{
    std::array<double,3> stdpt;
    stdpt[0] = point[0];
    stdpt[1] = point[1];
    stdpt[2] = point[2];
    return this -> sv3::PathElement::GetInsertintIndexByDistance(stdpt);
}

void sv4guiPathElement::SetControlPoint(int index, mitk::Point3D point)
{
    std::array<double,3> stdpt;
    stdpt[0] = point[0];
    stdpt[1] = point[1];
    stdpt[2] = point[2];
    this -> sv3::PathElement::SetControlPoint(index,stdpt);
}


void sv4guiPathElement::SetControlPoints(std::vector<mitk::Point3D> points, bool update)
{
    std::vector<sv3::PathElement::svControlPoint> controlPoints;
    for(int i=0;i<points.size();i++)
    {
        sv3::PathElement::svControlPoint controlPoint;
        controlPoint.id=i;
        controlPoint.point[0]=points[i][0];
        controlPoint.point[1]=points[i][1];
        controlPoint.point[2]=points[i][2];
        controlPoints.push_back(controlPoint);
    }
    m_ControlPoints=controlPoints;
    if(update)
        ControlPointsChanged();
}


int sv4guiPathElement::SearchControlPoint( mitk::Point3D point, mitk::ScalarType distance)
{
    
    std::array<double,3> stdpt;
    stdpt[0] = point[0];
    stdpt[1] = point[1];
    stdpt[2] = point[2];
    
    double dist = distance;

    return this->sv3::PathElement::SearchControlPoint(stdpt,dist);
}

sv4guiPathElement* sv4guiPathElement::CreateSmoothedPathElement(int sampleRate, int numModes, bool controlPointsBased )
{
    sv3::PathElement *tmp = this->sv3::PathElement::CreateSmoothedPathElement(sampleRate,numModes,controlPointsBased);
    sv4guiPathElement* newPathElement = static_cast<sv4guiPathElement*>(tmp);
    return newPathElement;
}

std::vector<sv4guiPathElement::sv4guiPathPoint> sv4guiPathElement::GetPathPoints()
{
    std::vector<sv4guiPathElement::sv4guiPathPoint> pthPts(m_PathPoints.size());
    for (int j = 0; j<m_PathPoints.size(); j++)
    {
        for (int i = 0; i<3; i++)
        {
            pthPts[j].pos[i]=m_PathPoints[j].pos[i];
        }
        mitk::FillVector3D(pthPts[j].tangent, m_PathPoints[j].tangent[0], m_PathPoints[j].tangent[1], m_PathPoints[j].tangent[2]);
        mitk::FillVector3D(pthPts[j].rotation, m_PathPoints[j].rotation[0], m_PathPoints[j].rotation[1], m_PathPoints[j].rotation[2]);
        pthPts[j].id = m_PathPoints[j].id;
    }
    return pthPts;
}

std::vector<mitk::Point3D> sv4guiPathElement::GetPathPosPoints()
{
    std::vector<mitk::Point3D> posPoints;
    for(int i=0;i<m_PathPoints.size();i++)
    {
        mitk::Point3D mitkpos;
        mitkpos[0] = m_PathPoints[i].pos[0];
        mitkpos[1] = m_PathPoints[i].pos[1];
        mitkpos[2] = m_PathPoints[i].pos[2];
        posPoints.push_back(mitkpos);
    }
        
    return posPoints;
}

sv4guiPathElement::sv4guiPathPoint sv4guiPathElement::GetPathPoint(int index)
{
    sv4guiPathPoint pathPoint;
    sv3::PathElement::PathPoint pthPt = this->sv3::PathElement::GetPathPoint(index);
    for (int i = 0; i<3; i++)
    {
        pathPoint.pos[i] = pthPt.pos[i];
    }
    mitk::FillVector3D(pathPoint.tangent, pthPt.tangent[0], pthPt.tangent[1], pthPt.tangent[2]);
    mitk::FillVector3D(pathPoint.rotation, pthPt.rotation[0], pthPt.rotation[1], pthPt.rotation[2]);
    pathPoint.id = pthPt.id;
    return pathPoint;   
}

mitk::Point3D sv4guiPathElement::GetPathPosPoint(int index)
{
    return GetPathPoint(index).pos;
}

void sv4guiPathElement::SetPathPoints(std::vector<sv4guiPathElement::sv4guiPathPoint> pathPoints)
{
    std::vector<sv3::PathElement::PathPoint> pthPts(pathPoints.size());
    for (int j = 0; j<pathPoints.size(); j++)
    {
        for (int i = 0; i<3; i++)
        {
            pthPts[j].pos[i]=pathPoints[j].pos[i];
            pthPts[j].tangent[i] = pathPoints[j].tangent[i];
            pthPts[j].rotation[i] = pathPoints[j].rotation[i];
        }
        pthPts[j].id = pathPoints[j].id;
    }
    m_PathPoints=pthPts;
}

std::vector<sv4guiPathElement::sv4guiPathPoint> sv4guiPathElement::GetExtendedPathPoints(double realBounds[6], double minSpacing, int& startingIndex)
{
    startingIndex=0;

    if(m_PathPoints.size()<2)
        return GetPathPoints();
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
        pathElement->SetMethod(sv3::PathElement::CONSTANT_SPACING);
        pathElement->SetSpacing(minSpacing);
        pathElement->SetControlPoints(controlPoints);

        beginPathPoints=pathElement->GetPathPoints();
        startingIndex=beginPathPoints.size();
    }

    if(endFound)
    {
        std::vector<mitk::Point3D> controlPoints={endPathPosPoint,endPoint};

        sv4guiPathElement* pathElement=new sv4guiPathElement();
        pathElement->SetMethod(sv3::PathElement::CONSTANT_SPACING);
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
