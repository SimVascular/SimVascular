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

#include "sv4gui_Contour.h"
#include "sv4gui_Math3.h"
#include "sv4gui_SegmentationUtils.h"

#include <vtkPoints.h>
#include <vtkCellArray.h>

sv4guiContour::sv4guiContour()
    : m_Type("Contour"),
      m_Method(""),
      m_Placed( false ),
      m_Closed( true ),
      m_Finished( true ),
      m_ControlPointSelectedIndex( -2 ),
      m_PlaneGeometry( nullptr ),
      m_PreviewControlPointVisible( false ),
      m_Extendable( false ),
      m_Selected(false),
      m_MinControlPointNumber(2),
      m_MaxControlPointNumber(2),
      m_TagIndex(0)
 {
    for (int i=0;i<5;i++)
    {
        m_ControlPointNonRemovableIndices[i]=-2;
    }
    m_ControlPointNonRemovableIndices[0]=0;
    m_ControlPointNonRemovableIndices[1]=1;


    m_SubdivisionType=CONSTANT_TOTAL_NUMBER;
    m_SubdivisionNumber=0;
    m_SubdivisionSpacing=0.0;

}

sv4guiContour::sv4guiContour(const sv4guiContour &other)
    : m_Type(other.m_Type)
    , m_Method(other.m_Method)
    , m_Placed(other.m_Placed)
    , m_Closed(other.m_Closed)
    , m_Finished(other.m_Finished)
    , m_MinControlPointNumber(other.m_MinControlPointNumber)
    , m_MaxControlPointNumber(other.m_MaxControlPointNumber)
    , m_SubdivisionType(other.m_SubdivisionType)
    , m_SubdivisionNumber(other.m_SubdivisionNumber)
    , m_SubdivisionSpacing(other.m_SubdivisionSpacing)
    , m_InitiallyPlaced(other.m_InitiallyPlaced)
    , m_ControlPoints(other.m_ControlPoints)
    , m_ContourPoints(other.m_ContourPoints)
    , m_Extendable(other.m_Extendable)
    , m_Selected(other.m_Selected)
{
//    SetPlaneGeometry(other.m_PlaneGeometry);
    SetPathPoint(other.m_PathPoint);

    for(int i=0;i<5;i++){
        m_ControlPointNonRemovableIndices[i]=other.m_ControlPointNonRemovableIndices[i];
    }

}

sv4guiContour::~sv4guiContour()
{
    //    if(m_PlaneGeometry)
    //    {
    //        delete m_PlaneGeometry;
    //    }

}

sv4guiContour* sv4guiContour::Clone()
{
    return new sv4guiContour(*this);
}

std::string sv4guiContour::GetClassName()
{
    return "sv4guiContour";
}

std::string sv4guiContour::GetType()
{
    return m_Type;
}

void sv4guiContour::SetType(std::string type)
{
    m_Type=type;
}

std::string sv4guiContour::GetMethod()
{
    return m_Method;
}

void sv4guiContour::SetMethod(std::string method)
{
    m_Method=method;
}

int sv4guiContour::GetContourID()
{
    return m_ContourID;
}

void sv4guiContour::SetContourID(int contourID)
{
    m_ContourID=contourID;
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

sv4guiContour::ShapeType sv4guiContour::GetShape()
{
    return m_Shape;
}

bool sv4guiContour::IsClosed()
{
    return m_Closed;
}

void sv4guiContour::SetClosed(bool closed)
{
    if(m_Closed!=closed)
    {
        m_Closed=closed;
        CreateContour();
    }
}

bool sv4guiContour::IsFinished()
{
    return m_Finished;
}

void sv4guiContour::SetFinished(bool finished)
{
    m_Finished=finished;
}

int sv4guiContour::GetSubdivisionNumber()
{
    return m_SubdivisionNumber;
}

void sv4guiContour::SetSubdivisionNumber(int number)
{
    if(m_SubdivisionNumber!=number)
    {
        m_SubdivisionNumber=number;
        CreateContour();
    }
}

sv4guiContour::SubdivisionType sv4guiContour::GetSubdivisionType()
{
    return m_SubdivisionType;
}

void sv4guiContour::SetSubdivisionType(SubdivisionType subdivType)
{
    if(m_SubdivisionType!=subdivType)
    {
        m_SubdivisionType=subdivType;
        CreateContour();
    }
}

double sv4guiContour::GetSubdivisionSpacing()
{
    return m_SubdivisionSpacing;
}

void sv4guiContour::SetSubdivisionSpacing(double spacing)
{
    if(m_SubdivisionSpacing!=spacing)
    {
        m_SubdivisionSpacing=spacing;
        CreateContour();
    }
}

void sv4guiContour::SetPlaneGeometry(mitk::PlaneGeometry* planeGeometry)
{
//    if(m_PlaneGeometry)
//    {
//        m_PlaneGeometry->Delete();
//    }

//    if(planeGeometry)
//    {
//        m_PlaneGeometry=dynamic_cast<mitk::PlaneGeometry*>(planeGeometry->Clone().GetPointer());
//    }
//    else
//    {
//        m_PlaneGeometry=NULL;
//    }

//    m_PlaneGeometry=planeGeometry;

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

int sv4guiContour::GetControlPointNumber()
{
    return m_ControlPoints.size();
}

int sv4guiContour::GetMinControlPointNumber()
{
    return m_MinControlPointNumber;
}

int sv4guiContour::GetMaxControlPointNumber()
{
    return m_MaxControlPointNumber;
}

void sv4guiContour::SetMinControlPointNumber(int number)
{
    m_MinControlPointNumber=number;
}

void sv4guiContour::SetMaxControlPointNumber(int number)
{
    m_MaxControlPointNumber=number;
}

mitk::Point3D sv4guiContour::GetControlPoint(int index){
    mitk::Point3D point;
    point.Fill(0);

    if(index==-1)
    {
        point=m_ControlPoints[m_ControlPoints.size()-1];
    }
    else if(index>-1 && index<m_ControlPoints.size())
    {
        point=m_ControlPoints[index];
    }

    return point;
}

void sv4guiContour::InsertControlPoint(int index, mitk::Point3D point)
{
    if(index==-1) index=m_ControlPoints.size();

    if(index>-1 && index<=m_ControlPoints.size())
    {
        m_ControlPoints.insert(m_ControlPoints.begin()+index,point);
        m_ControlPointSelectedIndex=index;
        ControlPointsChanged();
    }

}

void sv4guiContour::RemoveControlPoint(int index)
{
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        m_ControlPoints.erase(m_ControlPoints.begin()+index);
        ControlPointsChanged();
    }

}

void sv4guiContour::SetControlPoint(int index, mitk::Point3D point)
{
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        if(index==0)
        {
            mitk::Vector3D dirVec=point-m_ControlPoints[index];
            Shift(dirVec);
        }
        else if(index==1)
        {
            Scale(m_ControlPoints[0], m_ControlPoints[index], point);
        }

    }

}

//void sv4guiContour::SetActualControlPoint(int index, mitk::Point3D point)
//{
//    m_ControlPoints[index]=point;
//}

void sv4guiContour::SetControlPointSelectedIndex(int index)
{
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        m_ControlPointSelectedIndex=index;
    }else{
        m_ControlPointSelectedIndex=-2;
    }

}

void sv4guiContour::DeselectControlPoint()
{
    m_ControlPointSelectedIndex=-2;
}

int sv4guiContour::GetControlPointSelectedIndex()
{
    return m_ControlPointSelectedIndex;
}

void sv4guiContour::ClearControlPoints()
{
    m_ControlPoints.clear();
}

void sv4guiContour::PlaceContour(mitk::Point3D point)
{
    PlaceControlPoints(point);
    ControlPointsChanged();
}

void sv4guiContour::PlaceControlPoints(mitk::Point3D point)
{
    for ( unsigned int i = 0; i < GetMinControlPointNumber(); ++i )
    {
      m_ControlPoints.push_back( point );
    }

    m_Placed = true;
    m_ControlPointSelectedIndex = 1;
}

void sv4guiContour::SetControlPoints(std::vector<mitk::Point3D> controlPoints, bool updateContour)
{
    m_ControlPoints=controlPoints;
    if(updateContour)
        ControlPointsChanged();
}

bool sv4guiContour::IsControlPointRemovable(int index)
{
    for(int i=0;i<5;i++)
    {
        if(m_ControlPointNonRemovableIndices[i]==index)
            return false;
    }

    return true;
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

void sv4guiContour::ClearContourPoints()
{
    m_ContourPoints.clear();
}

void sv4guiContour::CreateContour()
{
    if(m_ControlPoints.size()<1)
    {
        return;
    }

    m_ContourPoints.clear();
    CreateContourPoints();
    ContourPointsChanged();
}

void sv4guiContour::ControlPointsChanged(){
    CreateContour();
    //    this->Modified();
}

void sv4guiContour::SetContourPoints(std::vector<mitk::Point3D> contourPoints, bool update)
{
    m_ContourPoints=contourPoints;
    if(update)
    ContourPointsChanged();
}

int sv4guiContour::GetContourPointNumber()
{
    return m_ContourPoints.size();
}

mitk::Point3D sv4guiContour::GetContourPoint(int index)
{
    mitk::Point3D point;
    point.Fill(0);

    if(index==-1)
        index=m_ContourPoints.size()-1;

    if(index>-1 && index<m_ContourPoints.size())
    {
        point=m_ContourPoints[index];
    }
    return point;
}

void sv4guiContour::ContourPointsChanged()
{
    CreateCenterScalingPoints();
    AssignCenterScalingPoints();
}

void sv4guiContour::AssignCenterScalingPoints()
{
    if(m_ControlPoints.size()==0)
    {
        m_ControlPoints.push_back(m_CenterPoint);
        m_ControlPoints.push_back(m_ScalingPoint);
    }
    else if(m_ControlPoints.size()==1)
    {
        m_ControlPoints[0]=m_CenterPoint;
        m_ControlPoints.push_back(m_ScalingPoint);
    }
    else
    {
        m_ControlPoints[0]=m_CenterPoint;
        m_ControlPoints[1]=m_ScalingPoint;
    }
}

void sv4guiContour::CreateCenterScalingPoints()
{
    double Sx=0,Sy=0,A=0;

    for(int i=0;i<m_ContourPoints.size();i++)
    {
        mitk::Point2D point1,point2;
        m_PlaneGeometry->Map(m_ContourPoints[i], point1);
        if(i==m_ContourPoints.size()-1)
            m_PlaneGeometry->Map(m_ContourPoints[0], point2);
        else
            m_PlaneGeometry->Map(m_ContourPoints[i+1], point2);

        Sx+=(point1[0]+point2[0])*(point1[0]*point2[1]-point2[0]*point1[1]);
        Sy+=(point1[1]+point2[1])*(point1[0]*point2[1]-point2[0]*point1[1]);
        A+=(point1[0]*point2[1]-point2[0]*point1[1]);
    }

    mitk::Point2D center;
    if(A!=0)
    {
        center[0]=Sx/A/3;
        center[1]=Sy/A/3;
    }
    else
    {
        Sx=0;
        Sy=0;
        for(int i=0;i<m_ContourPoints.size();i++)
        {
            mitk::Point2D point;
            m_PlaneGeometry->Map(m_ContourPoints[i], point);
            Sx+=point[0];
            Sy+=point[1];
        }
        center[0]=Sx/m_ContourPoints.size();
        center[1]=Sy/m_ContourPoints.size();
    }

    double minDis=0;
    bool firstTime=true;

    for(int i=0;i<m_ContourPoints.size();i++)
    {
        mitk::Point2D point;
        m_PlaneGeometry->Map(m_ContourPoints[i], point);
        double dis=point.EuclideanDistanceTo(center);
        if(firstTime)
        {
            minDis=dis;
            firstTime=false;
        }
        else if(dis<minDis)
        {
            minDis=dis;
        }
    }

    mitk::Point2D scalingPoint;
    scalingPoint[0]=center[0]+minDis/2;
    scalingPoint[1]=center[1];

    m_PlaneGeometry->Map(center,m_CenterPoint);
    m_PlaneGeometry->Map(scalingPoint,m_ScalingPoint);

}

//mitk::Point3D sv4guiContour::GetCenterPoint()
//{
//    return m_CenterPoint;
//}

sv4guiContour* sv4guiContour::CreateSmoothedContour(int fourierNumber)
{
    if(m_ContourPoints.size()<3)
        return this->Clone();

    sv4guiContour* contour=new sv4guiContour();
    contour->SetPathPoint(m_PathPoint);
//    contour->SetPlaneGeometry(m_PlaneGeometry);
    std::string method=m_Method;
    int idx=method.find("Smoothed");
    if(idx<0)
        method=method+" + Smoothed";

    contour->SetMethod(method);
    contour->SetPlaced(true);
    contour->SetClosed(m_Closed);

    int pointNumber=m_ContourPoints.size();

    int smoothedPointNumber;

    if((2*pointNumber)<fourierNumber)
        smoothedPointNumber=3*fourierNumber;
    else
        smoothedPointNumber=pointNumber;

    std::vector<mitk::Point3D> smoothedContourPoints=sv4guiMath3::CreateSmoothedCurve(m_ContourPoints,m_Closed,fourierNumber,0,smoothedPointNumber);

    contour->SetContourPoints(smoothedContourPoints);

    return contour;
}

vtkSmartPointer<vtkPolyData> sv4guiContour::CreateVtkPolyDataFromContour(bool includingAllLines)
{
    vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
    vtkSmartPointer<vtkCellArray> lines = vtkSmartPointer<vtkCellArray>::New();

    int pointNumber=m_ContourPoints.size();

    for (int i=0; i<=pointNumber; i++)
    {
        if(i<pointNumber)
        {
            mitk::Point3D point = GetContourPoint(i);
            points->InsertPoint(i,point[0],point[1],point[2]);
        }

        if(i>0&&i<pointNumber){
            vtkIdType cell[2] = {i-1,i};
            lines->InsertNextCell(2,cell);
        }else if(i==pointNumber&&m_Closed){
            vtkIdType cell[2] = {i-1,0};
            lines->InsertNextCell(2,cell);
        }

    }

    if(includingAllLines&&m_Closed&&m_Finished&&m_ControlPoints.size()>1)
    {
        mitk::Point3D point = GetControlPoint(0);
        points->InsertPoint(pointNumber,point[0],point[1],point[2]);
        point = GetControlPoint(1);
        points->InsertPoint(pointNumber+1,point[0],point[1],point[2]);
        vtkIdType cell[2] = {pointNumber,pointNumber+1};
        lines->InsertNextCell(2,cell);
    }

    vtkSmartPointer<vtkPolyData> polyData = vtkSmartPointer<vtkPolyData>::New();
    polyData->SetPoints(points);
    polyData->SetLines(lines);

    return polyData;
}

int sv4guiContour::SearchControlPointByContourPoint( int contourPointIndex )
{
    return contourPointIndex;
}

void sv4guiContour::Shift(mitk::Vector3D dirVec){

    for(int i=0;i<m_ControlPoints.size();i++)
    {
        m_ControlPoints[i]=m_ControlPoints[i]+dirVec;
    }

    for(int i=0;i<m_ContourPoints.size();i++)
    {
        m_ContourPoints[i]=m_ContourPoints[i]+dirVec;
    }
}

void sv4guiContour::Scale(double scalingFactor)
{
    Scale(scalingFactor, m_ControlPoints[0]);
}

void sv4guiContour::Scale(double scalingFactor, mitk::Point3D referencePoint)
{
    mitk::Vector3D dirVec;

    for(int i=0;i<m_ControlPoints.size();i++)
    {
        dirVec=m_ControlPoints[i]-referencePoint;
        m_ControlPoints[i]=referencePoint+scalingFactor*dirVec;
    }

    for(int i=0;i<m_ContourPoints.size();i++)
    {
        dirVec=m_ContourPoints[i]-referencePoint;
        m_ContourPoints[i]=referencePoint+scalingFactor*dirVec;
    }
}

void sv4guiContour::Scale(mitk::Point3D referencePoint, mitk::Point3D oldPoint, mitk::Point3D newPoint)
{
    double dis1=oldPoint.EuclideanDistanceTo(referencePoint);
    double dis2=newPoint.EuclideanDistanceTo(referencePoint);
    double scalingFactor;
    if(dis1==0||dis1==dis2){
        return;
    }else{
        scalingFactor=dis2/dis1;
        Scale(scalingFactor,referencePoint);
        m_ControlPoints[1]=newPoint;//make scaling point is exactly same as new point;

    }

}


void sv4guiContour::CalculateBoundingBox(double *bounds)
{
    for(int i=0;i<m_ControlPoints.size();i++)
    {
        double x=m_ControlPoints[i][0];
        double y=m_ControlPoints[i][1];
        double z=m_ControlPoints[i][2];

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

    for (int i = 0; i < m_ContourPoints.size(); i++) {
        double x=m_ContourPoints[i][0];
        double y=m_ContourPoints[i][1];
        double z=m_ContourPoints[i][2];
        if(x<bounds[0]) bounds[0]=x;
        if(x>bounds[1]) bounds[1]=x;
        if(y<bounds[2]) bounds[2]=y;
        if(y>bounds[3]) bounds[3]=y;
        if(z<bounds[4]) bounds[4]=z;
        if(z>bounds[5]) bounds[5]=z;
    }

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
    return m_PathPoint;
}

void sv4guiContour::SetPathPoint(sv4guiPathElement::sv4guiPathPoint pathPoint)
{
    m_PathPoint=pathPoint;

    mitk::Vector3D spacing;
    spacing.Fill(0.1);

    m_PlaneGeometry=sv4guiSegmentationUtils::CreatePlaneGeometry(pathPoint,spacing, 1.0);
}

int sv4guiContour::GetPathPosID(){
    return m_PathPoint.id;
}

mitk::Point3D sv4guiContour::GetPathPosPoint()
{
    return m_PathPoint.pos;
}

vtkImageData* sv4guiContour::GetVtkImageSlice()
{
    return m_VtkImageSlice;
}

void sv4guiContour::SetVtkImageSlice(vtkImageData* slice)
{
    m_VtkImageSlice=slice;
}

mitk::Point3D sv4guiContour::GetCenterPoint()
{
    return m_ControlPoints[0];
}

double sv4guiContour::GetArea()
{
    double A=0;

    for(int i=0;i<m_ContourPoints.size();i++)
    {
        mitk::Point2D point1,point2;
        m_PlaneGeometry->Map(m_ContourPoints[i], point1);
        if(i==m_ContourPoints.size()-1)
            m_PlaneGeometry->Map(m_ContourPoints[0], point2);
        else
            m_PlaneGeometry->Map(m_ContourPoints[i+1], point2);

        A+=(0.5*(point1[0]*point2[1]-point2[0]*point1[1]));
    }

    if(A<0) A=-A;

    return A;
}

double sv4guiContour::GetPerimeter()
{
    double L=0;

    for(int i=0;i<m_ContourPoints.size();i++)
    {
        mitk::Point2D point1,point2;
        m_PlaneGeometry->Map(m_ContourPoints[i], point1);
        if(i==m_ContourPoints.size()-1)
            m_PlaneGeometry->Map(m_ContourPoints[0], point2);
        else
            m_PlaneGeometry->Map(m_ContourPoints[i+1], point2);

        L+=point1.EuclideanDistanceTo(point2);
    }

    return L;
}
