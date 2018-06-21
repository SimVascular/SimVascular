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
#include "sv3_Contour.h"
#include "sv4gui_Contour.h"
#include "sv4gui_Math3.h"
#include "sv4gui_SegmentationUtils.h"

#include <vtkPoints.h>
#include <vtkCellArray.h>
using sv3::Contour;
sv4guiContour::sv4guiContour()
    : Contour(),
      m_Placed( false ),
      m_PlaneGeometry( nullptr ),
      m_PreviewControlPointVisible( false ),
      m_Extendable( false ),
      m_Selected(false)
{
}

sv4guiContour::sv4guiContour(const sv4guiContour &other)
    : Contour(other)
    , m_Placed(other.m_Placed)
    , m_Extendable(other.m_Extendable)
    , m_Selected(other.m_Selected)
{
}

sv4guiContour::~sv4guiContour()
{
}

sv4guiContour* sv4guiContour::Clone()
{
    return new sv4guiContour(*this);
}


bool sv4guiContour::IsSelected()
{
    return m_Selected;
}

void sv4guiContour::SetSelected(bool selected)
{
    m_Selected=selected;
}

bool sv4guiContour::IsHovering()
{
    return m_Hovering;
}

void sv4guiContour::SetHovering(bool hovering)
{
    m_Hovering=hovering;
}

bool sv4guiContour::IsPlaced()
{
    return m_Placed;
}

void sv4guiContour::SetPlaced(bool placed)
{
    m_Placed=placed;
}

bool sv4guiContour::IsExtendable()
{
    return m_Extendable;
}

void sv4guiContour::SetExtendable(bool extendable)
{
    m_Extendable=extendable;
}

void sv4guiContour::SetPlaneGeometry(mitk::PlaneGeometry* planeGeometry)
{

    if(planeGeometry!=NULL)
    {
        m_PlaneGeometry = planeGeometry->Clone();
    }else{
        m_PlaneGeometry = NULL;
    }

}

mitk::PlaneGeometry* sv4guiContour::GetPlaneGeometry()
{
    return m_PlaneGeometry;
}

mitk::Point3D sv4guiContour::GetControlPoint(int index){
    
    std::array<double,3> tmpPt = this->sv3::Contour::GetControlPoint(index);
    mitk::Point3D point;
    for (int i=0; i<3; i++)
        point[i] = tmpPt[i];
    return point;
}

void sv4guiContour::InsertControlPoint(int index, mitk::Point3D point)
{
    std::array<double,3> stdPt;
    for (int i=0; i<3; i++)
        stdPt[i] = point[i];
    this->sv3::Contour::InsertControlPoint(index,stdPt);
}

void sv4guiContour::SetControlPoint(int index, mitk::Point3D point)
{
    std::array<double,3> stdPt;
    for (int i=0; i<3; i++)
        stdPt[i] = point[i];
    this->sv3::Contour::SetControlPoint(index,stdPt);

}

void sv4guiContour::PlaceContour(mitk::Point3D point)
{
    std::array<double,3> stdPt;
    for (int i=0; i<3; i++)
        stdPt[i] = point[i];
    this->sv3::Contour::PlaceContour(stdPt);
}

void sv4guiContour::PlaceControlPoints(mitk::Point3D point)
{
    std::array<double,3> stdPt;
    for (int i=0; i<3; i++)
        stdPt[i] = point[i];
        
    this->sv3::Contour::PlaceControlPoints(stdPt);
}

void sv4guiContour::SetControlPoints(std::vector<mitk::Point3D> controlPoints, bool updateContour)
{
    std::vector<std::array<double,3> > stdPoints(controlPoints.size());
    for (int j=0; j<controlPoints.size(); j++)
    {
        for(int i=0; i<3; i++)
            stdPoints[j][i] = controlPoints[j][i];
    }
    
    this->sv3::Contour::SetControlPoints(stdPoints,updateContour);
            
}

void sv4guiContour::SetPreviewControlPoint(mitk::Point3D point )
{
    m_PreviewControlPoint = point;
    m_PreviewControlPointVisible = true;
}

void sv4guiContour::HidePreviewControlPoint()
{
    m_PreviewControlPointVisible = false;
}

bool sv4guiContour::IsPreviewControlPointVisible()
{
    return m_PreviewControlPointVisible;
}

mitk::Point3D sv4guiContour::GetPreviewControlPoint()
{
    return m_PreviewControlPoint;
}


void sv4guiContour::SetContourPoints(std::vector<mitk::Point3D> contourPoints, bool update)
{
    std::vector<std::array<double,3> > stdPoints(contourPoints.size());
    for (int j=0; j<contourPoints.size(); j++)
    {
        for(int i=0; i<3; i++)
            stdPoints[j][i] = contourPoints[j][i];
    }
    
    this->sv3::Contour::SetContourPoints(stdPoints,update);
}


mitk::Point3D sv4guiContour::GetContourPoint(int index)
{
    mitk::Point3D point;
    std::array<double,3> stdpt = this->sv3::Contour::GetContourPoint(index);
    for (int i=0; i<3; i++)
        point[i] = stdpt[i];
    return point;
}


//void sv4guiContour::CreateCenterScalingPoints()
//{
//    double Sx=0,Sy=0,A=0;
//
//    for(int i=0;i<m_ContourPoints.size();i++)
//    {
//        mitk::Point2D point1,point2;
//        m_PlaneGeometry->Map(m_ContourPoints[i], point1);
//        if(i==m_ContourPoints.size()-1)
//            m_PlaneGeometry->Map(m_ContourPoints[0], point2);
//        else
//            m_PlaneGeometry->Map(m_ContourPoints[i+1], point2);
//
//        Sx+=(point1[0]+point2[0])*(point1[0]*point2[1]-point2[0]*point1[1]);
//        Sy+=(point1[1]+point2[1])*(point1[0]*point2[1]-point2[0]*point1[1]);
//        A+=(point1[0]*point2[1]-point2[0]*point1[1]);
//    }
//
//    mitk::Point2D center;
//    if(A!=0)
//    {
//        center[0]=Sx/A/3;
//        center[1]=Sy/A/3;
//    }
//    else
//    {
//        Sx=0;
//        Sy=0;
//        for(int i=0;i<m_ContourPoints.size();i++)
//        {
//            mitk::Point2D point;
//            m_PlaneGeometry->Map(m_ContourPoints[i], point);
//            Sx+=point[0];
//            Sy+=point[1];
//        }
//        center[0]=Sx/m_ContourPoints.size();
//        center[1]=Sy/m_ContourPoints.size();
//    }
//
//    double minDis=0;
//    bool firstTime=true;
//
//    for(int i=0;i<m_ContourPoints.size();i++)
//    {
//        mitk::Point2D point;
//        m_PlaneGeometry->Map(m_ContourPoints[i], point);
//        double dis=point.EuclideanDistanceTo(center);
//        if(firstTime)
//        {
//            minDis=dis;
//            firstTime=false;
//        }
//        else if(dis<minDis)
//        {
//            minDis=dis;
//        }
//    }
//
//    mitk::Point2D scalingPoint;
//    scalingPoint[0]=center[0]+minDis/2;
//    scalingPoint[1]=center[1];
//
//    m_PlaneGeometry->Map(center,m_CenterPoint);
//    m_PlaneGeometry->Map(scalingPoint,m_ScalingPoint);
//
//}

mitk::Point3D sv4guiContour::GetCenterPoint()
{
    mitk::Point3D point;
    std::array<double,3> stdpt = this->sv3::Contour::GetCenterPoint();
    for (int i=0; i<3; i++)
        point[i] = stdpt[i];
    return point;
}

sv4guiContour* sv4guiContour::CreateSmoothedContour(int fourierNumber)
{
    if(m_ContourPoints.size()<3)
        return this->Clone();
    Contour* sv3contour = this->sv3::Contour::CreateSmoothedContour(fourierNumber);
    sv4guiContour* contour=new sv4guiContour();
    contour->sv3::Contour::SetPathPoint(sv3contour->GetPathPoint());
    contour->SetMethod(sv3contour->GetMethod());
    contour->SetPlaced(true);
    contour->SetClosed(sv3contour->IsClosed());
    contour->sv3::Contour::SetContourPoints(sv3contour->GetContourPoints());
    delete sv3contour;
    return contour;
}


void sv4guiContour::Shift(mitk::Vector3D dirVec){
    
    std::array<double,3> stdVec;
    
    for (int i=0; i<3; i++)
        stdVec[i] = dirVec[i];
    this->sv3::Contour::Shift(stdVec);
}

void sv4guiContour::Scale(double scalingFactor)
{
    mitk::Point3D referencePoint;
    for(int i=0; i<3; i++)
        referencePoint = m_ControlPoints[0][i];
    Scale(scalingFactor, referencePoint);
}

void sv4guiContour::Scale(double scalingFactor, mitk::Point3D referencePoint)
{    
    std::array<double,3> refPt;
    
    for (int i=0; i<3; i++)
        refPt[i] = referencePoint[i];
        
    this->sv3::Contour::Scale(scalingFactor,refPt);
}

void sv4guiContour::Scale(mitk::Point3D referencePoint, mitk::Point3D oldPoint, mitk::Point3D newPoint)
{
    std::array<double,3> refPt, stdOldPt,stdNewPt;
    
    for (int i=0; i<3; i++)
    {
        refPt[i] = referencePoint[i];
        stdOldPt[i] = oldPoint[i];
        stdNewPt[i] = newPoint[i];
    }
    this-> sv3::Contour::Scale(refPt,stdOldPt,stdNewPt);
}

bool sv4guiContour::IsOnPlane(const mitk::PlaneGeometry* planeGeometry, double precisionFactor)
{
    if(m_PlaneGeometry.IsNull() || planeGeometry==NULL) return false;

//    double contourThickness = m_PlaneGeometry->GetExtentInMM( 2 )*precisionFactor;
//    if(m_PlaneGeometry->IsParallel(planeGeometry)
//            && m_PlaneGeometry->DistanceFromPlane(planeGeometry)<contourThickness)
//        return true;
//    else
//        return false;

    double contourThickness = planeGeometry->GetExtentInMM( 2 )*precisionFactor;

    double ang=m_PlaneGeometry->Angle(planeGeometry);
    double dis=std::abs(m_PlaneGeometry->SignedDistance(planeGeometry->GetOrigin()));
    if( (ang<0.02||ang>3.12) && dis<contourThickness)
        return true;
    else
        return false;
}

sv4guiPathElement::sv4guiPathPoint sv4guiContour::GetPathPoint()
{
    sv4guiPathElement::sv4guiPathPoint pathPoint;
    sv3::PathElement::PathPoint pthPt = this->sv3::Contour::GetPathPoint();
    for (int i = 0; i<3; i++)
    {
        pathPoint.pos[i] = pthPt.pos[i];
    }
    mitk::FillVector3D(pathPoint.tangent, pthPt.tangent[0], pthPt.tangent[1], pthPt.tangent[2]);
    mitk::FillVector3D(pathPoint.rotation, pthPt.rotation[0], pthPt.rotation[1], pthPt.rotation[2]);
    pathPoint.id = pthPt.id;
    return pathPoint; 
}

void sv4guiContour::SetPathPoint(sv4guiPathElement::sv4guiPathPoint pathPoint)
{
    std::cout<<"sv4guiContour::SetPathPoint"<<std::endl;
    sv3::PathElement::PathPoint pthPt;
    for (int i = 0; i<3; i++)
    {
        pthPt.pos[i]=pathPoint.pos[i];
        pthPt.tangent[i] = pathPoint.tangent[i];
        pthPt.rotation[i] = pathPoint.rotation[i];
    }
        pthPt.id = pathPoint.id;
    
    this->sv3::Contour::SetPathPoint(pthPt);

    mitk::Vector3D spacing;
    spacing.Fill(0.1);

    m_PlaneGeometry=sv4guiSegmentationUtils::CreatePlaneGeometry(pathPoint,spacing, 1.0);
}


mitk::Point3D sv4guiContour::GetPathPosPoint()
{
    mitk::Point3D mitkPt;
    std::array<double,3> point = this->sv3::Contour::GetPathPosPoint();
    for(int i=0; i<3; i++)
        mitkPt[i] = point[i];
    return mitkPt;
}

