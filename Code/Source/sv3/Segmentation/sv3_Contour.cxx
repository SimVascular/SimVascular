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
#include "Python.h"
#include "sv_StrPts.h"
#include "sv3_ITKLset_ITKUtils.h"
#include "sv_sys_geom.h"
#include "sv_vtk_utils.h"
#include "sv3_ITKLevelSet.h"
#include "sv_Math.h"

#include "sv3_Contour.h"
#include "sv4gui_Math3.h"
#include "sv3_SegmentationUtils.h"

#include <vtkPoints.h>
#include <vtkCellArray.h>
#include <vtkPolyData.h>
#include <vtkImageReslice.h>
#include <iostream>
using sv3::Contour;
using sv3::PathElement;
using sv3::SegmentationUtils;

cKernelType Contour::gCurrentKernel;
cvFactoryRegistrar Contour::gRegistrar;

Contour::Contour()
    : cvRepositoryData( CONTOUR_T ),
      m_Type("Contour"),
      m_Method(""),
      //m_Placed( false ),
      m_Closed( true ),
      m_Finished( true ),
      m_ControlPointSelectedIndex( -2 ),
      m_vtkPlaneGeometry( nullptr ),
      //m_PreviewControlPointVisible( false ),
      //m_Extendable( false ),
      //m_Selected(false),
      m_MinControlPointNumber(2),
      m_MaxControlPointNumber(2),
      m_TagIndex(0)
 {
    std::cout<<"Contour::Contour()"<<std::endl;
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

Contour::Contour(const Contour &other) 
    : cvRepositoryData( CONTOUR_T )
    , m_Type(other.m_Type)
    , m_Method(other.m_Method)
    //, m_Placed(other.m_Placed)
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
    , contour_kernel_(other.contour_kernel_)
    //, m_Extendable(other.m_Extendable)
    //, m_Selected(other.m_Selected)
{
//    SetPlaneGeometry(other.m_vtkPlaneGeometry);
    std::cout<<"Contour::Contour(const Contour &other) "<<std::endl;
    SetPathPoint(other.m_PathPoint);

    for(int i=0;i<5;i++){
        m_ControlPointNonRemovableIndices[i]=other.m_ControlPointNonRemovableIndices[i];
    }

}

Contour::~Contour()
{
}

// --------------------------------
// DefaultInstantiateContourObject
// --------------------------------
Contour* Contour::DefaultInstantiateContourObject(cKernelType t, PathElement::PathPoint pathPoint)
{
  // Get the adapt object factory registrar associated with the python interpreter
  cvFactoryRegistrar* contourObjectRegistrar;
  contourObjectRegistrar = (cvFactoryRegistrar *) PySys_GetObject("ContourObjectRegistrar");
  if (contourObjectRegistrar==NULL)
  {
    fprintf(stdout,"Cannot get contourObjectRegistrar from pySys");
  }
  Contour* contour = NULL;
  if (t == cKERNEL_LEVELSET || t==cKERNEL_THRESHOLD|| t == cKERNEL_CIRCLE || t == cKERNEL_POLYGON || t == cKERNEL_SPLINEPOLYGON ||t == cKERNEL_ELLIPSE)
  {
    contour = (Contour *) (contourObjectRegistrar->UseFactoryMethod( t ));
    if (contour == NULL) {
		  fprintf( stdout, "Unable to create contour object for kernel (%i)\n",Contour::gCurrentKernel);
    }
    contour->SetPathPoint(pathPoint);

  } else {
    fprintf( stdout, "current kernel is not valid (%i)\n",t);
  }

  return contour;
}

std::string Contour::GetClassName()
{
    std::cout<<"GetClassName "<<std::endl;
    return "Contour";
}

std::string Contour::GetType()
{
    std::cout<<"GetType() "<<std::endl;

    return m_Type;
}

void Contour::SetType(std::string type)
{
    std::cout<<"SetType() "<<std::endl;

    m_Type=type;
}

std::string Contour::GetMethod()
{
    std::cout<<"GetMethod() "<<std::endl;
    return m_Method;
}

void Contour::SetMethod(std::string method)
{
    std::cout<<"SetMethod() "<<std::endl;
    m_Method=method;
}

int Contour::GetContourID()
{
    std::cout<<"GetContourID() "<<std::endl;
    return m_ContourID;
}

void Contour::SetContourID(int contourID)
{
    std::cout<<"SetContourID() "<<std::endl;
    m_ContourID=contourID;
}

void Contour::SetPlaneGeometry(vtkSmartPointer<vtkPlane> planeGeometry)
{
//    if(m_vtkPlaneGeometry)
//    {
//        m_vtkPlaneGeometry->Delete();
//    }

//    if(planeGeometry)
//    {
//        m_vtkPlaneGeometry=dynamic_cast<mitk::PlaneGeometry*>(planeGeometry->Clone().GetPointer());
//    }
//    else
//    {
//        m_vtkPlaneGeometry=NULL;
//    }

//    m_vtkPlaneGeometry=planeGeometry;
    std::cout<<"SetPlaneGeometry() "<<std::endl;
    if(planeGeometry!=NULL)
    {
        m_vtkPlaneGeometry = vtkSmartPointer<vtkPlane>::New();
        m_vtkPlaneGeometry->SetOrigin(planeGeometry->GetOrigin());
        m_vtkPlaneGeometry->SetNormal(planeGeometry->GetNormal());
    }else{
        m_vtkPlaneGeometry = NULL;
    }

}

vtkSmartPointer<vtkPlane> Contour::GetPlaneGeometry()
{
    std::cout<<"GetPlaneGeometry() "<<std::endl;
    return m_vtkPlaneGeometry;
}


//bool Contour::IsSelected()
//{
//    return m_Selected;
//}
//
//void Contour::SetSelected(bool selected)
//{
//    m_Selected=selected;
//}

//bool Contour::IsHovering()
//{
//    return m_Hovering;
//}
//
//void Contour::SetHovering(bool hovering)
//{
//    m_Hovering=hovering;
//}
//
//bool Contour::IsPlaced()
//{
//    return m_Placed;
//}
//
//void Contour::SetPlaced(bool placed)
//{
//    m_Placed=placed;
//}
//
//bool Contour::IsExtendable()
//{
//    return m_Extendable;
//}
//
//void Contour::SetExtendable(bool extendable)
//{
//    m_Extendable=extendable;
//}
//
//Contour::ShapeType Contour::GetShape()
//{
//    return m_Shape;
//}

bool Contour::IsClosed()
{
    std::cout<<"IsClosed() "<<std::endl;
    return m_Closed;
}

void Contour::SetClosed(bool closed)
{
    std::cout<<"SetClosed() "<<std::endl;
    if(m_Closed!=closed)
    {
        m_Closed=closed;
        CreateContour();
    }
}

bool Contour::IsFinished()
{
    std::cout<<"IsFinished() "<<std::endl;
    return m_Finished;
}

void Contour::SetFinished(bool finished)
{
    std::cout<<"SetFinished() "<<std::endl;
    m_Finished=finished;
}

int Contour::GetSubdivisionNumber()
{
    std::cout<<"GetSubdivisionNumber() "<<std::endl;
    return m_SubdivisionNumber;
}

void Contour::SetSubdivisionNumber(int number)
{
    std::cout<<"SetSubdivisionNumber() "<<std::endl;
    if(m_SubdivisionNumber!=number)
    {
        m_SubdivisionNumber=number;
        CreateContour();
    }
}

Contour::SubdivisionType Contour::GetSubdivisionType()
{
    std::cout<<"GetSubdivisionType() "<<std::endl;
    return m_SubdivisionType;
}

void Contour::SetSubdivisionType(SubdivisionType subdivType)
{
    std::cout<<"SetSubdivisionType() "<<std::endl;
    if(m_SubdivisionType!=subdivType)
    {
        m_SubdivisionType=subdivType;
        CreateContour();
    }
}

double Contour::GetSubdivisionSpacing()
{
    std::cout<<"GetSubdivisionSpacing() "<<std::endl;
    return m_SubdivisionSpacing;
}

void Contour::SetSubdivisionSpacing(double spacing)
{
    std::cout<<"SetSubdivisionSpacing() "<<std::endl;
    if(m_SubdivisionSpacing!=spacing)
    {
        m_SubdivisionSpacing=spacing;
        CreateContour();
    }
}

int Contour::GetControlPointNumber()
{
    std::cout<<"GetControlPointNumber() "<<std::endl;
    return m_ControlPoints.size();
}

int Contour::GetMinControlPointNumber()
{
    std::cout<<"GetMinControlPointNumber() "<<std::endl;
    return m_MinControlPointNumber;
}

int Contour::GetMaxControlPointNumber()
{
    std::cout<<"GetMaxControlPointNumber() "<<std::endl;
    return m_MaxControlPointNumber;
}

void Contour::SetMinControlPointNumber(int number)
{
    std::cout<<"SetMinControlPointNumber() "<<std::endl;
    m_MinControlPointNumber=number;
}

void Contour::SetMaxControlPointNumber(int number)
{
    std::cout<<"SetMaxControlPointNumber() "<<std::endl;
    m_MaxControlPointNumber=number;
}

std::array<double, 3> Contour::GetControlPoint(int index){
    std::cout<<"GetControlPoint() "<<std::endl;
    std::array<double, 3> point;
    point.fill(0);

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

void Contour::InsertControlPoint(int index, std::array<double, 3> point)
{
    std::cout<<"InsertControlPoint() "<<std::endl;
    if(index==-1) index=m_ControlPoints.size();

    if(index>-1 && index<=m_ControlPoints.size())
    {
        m_ControlPoints.insert(m_ControlPoints.begin()+index,point);
        m_ControlPointSelectedIndex=index;
        ControlPointsChanged();
    }

}

void Contour::RemoveControlPoint(int index)
{
    std::cout<<"RemoveControlPoint() "<<std::endl;
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        m_ControlPoints.erase(m_ControlPoints.begin()+index);
        ControlPointsChanged();
    }

}

void Contour::SetControlPoint(int index, std::array<double, 3> point)
{
    std::cout<<"SetControlPoint() "<<std::endl;
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        if(index==0)
        {
            std::array<double, 3>  dirVec;
            dirVec[0]=point[0]-m_ControlPoints[index][0];
            dirVec[1]=point[1]-m_ControlPoints[index][1];
            dirVec[2]=point[2]-m_ControlPoints[index][2];
            Shift(dirVec);
        }
        else if(index==1)
        {
            Scale(m_ControlPoints[0], m_ControlPoints[index], point);
        }

    }

}

//void Contour::SetActualControlPoint(int index, std::array<double, 3> point)
//{
//    m_ControlPoints[index]=point;
//}

void Contour::SetControlPointSelectedIndex(int index)
{
    std::cout<<"SetControlPointSelectedIndex() "<<std::endl;
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        m_ControlPointSelectedIndex=index;
    }else{
        m_ControlPointSelectedIndex=-2;
    }

}

void Contour::DeselectControlPoint()
{
    std::cout<<"DeselectControlPoint() "<<std::endl;
    m_ControlPointSelectedIndex=-2;
}

int Contour::GetControlPointSelectedIndex()
{
    std::cout<<"GetControlPointSelectedIndex() "<<std::endl;
    return m_ControlPointSelectedIndex;
}

void Contour::ClearControlPoints()
{
    std::cout<<"ClearControlPoints() "<<std::endl;
    m_ControlPoints.clear();
}

void Contour::PlaceContour(std::array<double, 3> point)
{
    std::cout<<"PlaceContour() "<<std::endl;
    PlaceControlPoints(point);
    ControlPointsChanged();
}

void Contour::PlaceControlPoints(std::array<double, 3> point)
{
    std::cout<<"PlaceControlPoints() "<<std::endl;
    for ( unsigned int i = 0; i < GetMinControlPointNumber(); ++i )
    {
      m_ControlPoints.push_back( point );
    }

    //m_Placed = true;
    m_ControlPointSelectedIndex = 1;
}

void Contour::SetControlPoints(std::vector<std::array<double, 3> > controlPoints, bool updateContour)
{
    std::cout<<"SetControlPoints() "<<std::endl;
    m_ControlPoints=controlPoints;
    if(updateContour)
        ControlPointsChanged();
}

bool Contour::IsControlPointRemovable(int index)
{
    std::cout<<"IsControlPointRemovable() "<<std::endl;
    for(int i=0;i<5;i++)
    {
        if(m_ControlPointNonRemovableIndices[i]==index)
            return false;
    }

    return true;
}

//void Contour::SetPreviewControlPoint(std::array<double, 3>  point )
//{
//    m_PreviewControlPoint = point;
//    m_PreviewControlPointVisible = true;
//}
//
//void Contour::HidePreviewControlPoint()
//{
//    m_PreviewControlPointVisible = false;
//}
//
//bool Contour::IsPreviewControlPointVisible()
//{
//    return m_PreviewControlPointVisible;
//}
//
//std::array<double, 3>  Contour::GetPreviewControlPoint()
//{
//    return m_PreviewControlPoint;
//}

void Contour::ClearContourPoints()
{
    std::cout<<"ClearContourPoints() "<<std::endl;
    m_ContourPoints.clear();
}

void Contour::CreateContour()
{
    
    std::cout<<"CreateContour() "<<std::endl;
    if(m_ControlPoints.size()<1)
    {
        return;
    }

    m_ContourPoints.clear();
    CreateContourPoints();
    ContourPointsChanged();
}

void Contour::ControlPointsChanged(){
    std::cout<<"ControlPointsChanged() "<<std::endl;
    CreateContour();
    //    this->Modified();
}

void Contour::SetContourPoints(std::vector<std::array<double, 3> > contourPoints, bool update)
{
    std::cout<<"SetContourPoints() "<<std::endl;
    m_ContourPoints=contourPoints;
    if(update)
    ContourPointsChanged();
}

int Contour::GetContourPointNumber()
{
    std::cout<<"GetContourPointNumber() "<<std::endl;
    return m_ContourPoints.size();
}

std::array<double, 3>  Contour::GetContourPoint(int index)
{
    std::cout<<"GetContourPoint() "<<std::endl;
    std::array<double, 3>  point;
    point.fill(0);

    if(index==-1)
        index=m_ContourPoints.size()-1;

    if(index>-1 && index<m_ContourPoints.size())
    {
        point=m_ContourPoints[index];
    }
    return point;
}

std::vector<std::array<double,3> > Contour::GetContourPoints()
{
    std::cout<<"GetContourPoints() "<<std::endl;
    return m_ContourPoints;
}

void Contour::ContourPointsChanged()
{
     std::cout<<"ContourPointsChanged() "<<std::endl;
    CreateCenterScalingPoints();
    AssignCenterScalingPoints();
}

void Contour::AssignCenterScalingPoints()
{
     std::cout<<"AssignCenterScalingPoints() "<<std::endl;
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

void Contour::CreateCenterScalingPoints()
{
    std::cout<<"CreateCenterScalingPoints() "<<std::endl;
    double Sx=0,Sy=0, Sz=0, A=0;
    std::array<double, 3>  center;
    
    Sx=0;
    Sy=0;
    Sz = 0;
    for(int i=0;i<m_ContourPoints.size();i++)
    {
        double  point[3];
        double contourPoints[3];
        for (int j=0;j<3;j++)
            contourPoints[j] = m_ContourPoints[i][j];
        m_vtkPlaneGeometry->ProjectPoint(contourPoints, point);
        Sx+=point[0];
        Sy+=point[1];
        Sz+=point[2];
    }
    center[0]=Sx/m_ContourPoints.size();
    center[1]=Sy/m_ContourPoints.size();
    center[2]=Sz/m_ContourPoints.size();

    double minDis=0;
    bool firstTime=true;

    for(int i=0;i<m_ContourPoints.size();i++)
    {
        double point[3];
        double contourPoints[3];
        for (int j=0;j<3;j++)
            contourPoints[j] = m_ContourPoints[i][j];
        m_vtkPlaneGeometry->ProjectPoint(contourPoints, point);
        double dis=sqrt(pow(m_ContourPoints[i][0]-point[0],2)+pow(m_ContourPoints[i][1]-point[1],2)+pow(m_ContourPoints[i][2]-point[2],2));
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

    std::array<double, 3>  scalingPoint;
    scalingPoint[0]=center[0]+minDis/2;
    scalingPoint[1]=center[1];
    scalingPoint[2]=center[2];

    m_CenterPoint = center;
    m_ScalingPoint = scalingPoint;
}

std::array<double, 3>  Contour::GetCenterPoint()
{
    std::cout<<"GetCenterPoint() "<<std::endl;
    return m_CenterPoint;
}

vtkSmartPointer<vtkPolyData> Contour::CreateVtkPolyDataFromContour(bool includingAllLines)
{
    std::cout<<"CreateVtkPolyDataFromContour() "<<std::endl;
    vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
    vtkSmartPointer<vtkCellArray> lines = vtkSmartPointer<vtkCellArray>::New();

    int pointNumber=m_ContourPoints.size();

    for (int i=0; i<=pointNumber; i++)
    {
        if(i<pointNumber)
        {
            std::array<double, 3>  point = GetContourPoint(i);
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
        std::array<double, 3>  point = GetControlPoint(0);
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

int Contour::SearchControlPointByContourPoint( int contourPointIndex )
{
    std::cout<<"SearchControlPointByContourPoint() "<<std::endl;
    return contourPointIndex;
}

void Contour::Shift(std::array<double, 3>  dirVec){

std::cout<<"Shift() "<<std::endl;
    for(int i=0;i<m_ControlPoints.size();i++)
    {
        for (int j = 0; j<3;j++)
            m_ControlPoints[i][j]=m_ControlPoints[i][j]+dirVec[j];
    }

    for(int i=0;i<m_ContourPoints.size();i++)
    {
        for (int j = 0; j<3;j++)
            m_ContourPoints[i][j]=m_ContourPoints[i][j]+dirVec[j];
    }
}

void Contour::Scale(double scalingFactor)
{
    std::cout<<"Scale() "<<std::endl;
    Scale(scalingFactor, m_ControlPoints[0]);
}

void Contour::Scale(double scalingFactor, std::array<double, 3>  referencePoint)
{
    std::cout<<"Scale() "<<std::endl;
    std::array<double, 3>  dirVec;

    for(int i=0;i<m_ControlPoints.size();i++)
    {
        for (int j = 0; j<3;j++)
        {
            dirVec[j]=m_ControlPoints[i][j]-referencePoint[j];
            m_ControlPoints[i][j]=referencePoint[j]+scalingFactor*dirVec[j];
        }
    }

    for(int i=0;i<m_ContourPoints.size();i++)
    {
        for (int j = 0; j<3;j++)
        {
            dirVec[j]=m_ContourPoints[i][j]-referencePoint[j];
            m_ContourPoints[i][j]=referencePoint[j]+scalingFactor*dirVec[j];
        }
    }
}

void Contour::Scale(std::array<double, 3>  referencePoint, std::array<double, 3>  oldPoint, std::array<double, 3>  newPoint)
{
    std::cout<<"Scale() "<<std::endl;
    double dis1=sqrt(pow(oldPoint[0]-referencePoint[0],2)+pow(oldPoint[1]-referencePoint[1],2)+pow(oldPoint[2]-referencePoint[2],2));
    double dis2=sqrt(pow(newPoint[0]-referencePoint[0],2)+pow(newPoint[1]-referencePoint[1],2)+pow(newPoint[2]-referencePoint[2],2));
    double scalingFactor;
    if(dis1==0||dis1==dis2){
        return;
    }else{
        scalingFactor=dis2/dis1;
        Scale(scalingFactor,referencePoint);
        m_ControlPoints[1]=newPoint;//make scaling point is exactly same as new point;

    }

}


void Contour::CalculateBoundingBox(double *bounds)
{
    std::cout<<"CalculateBoundingBox() "<<std::endl;
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

//bool Contour::IsOnPlane(const mitk::PlaneGeometry* planeGeometry, double precisionFactor)
//{
//    if(m_vtkPlaneGeometry.IsNull() || planeGeometry==NULL) return false;
//
////    double contourThickness = m_vtkPlaneGeometry->GetExtentInMM( 2 )*precisionFactor;
////    if(m_vtkPlaneGeometry->IsParallel(planeGeometry)
////            && m_vtkPlaneGeometry->DistanceFromPlane(planeGeometry)<contourThickness)
////        return true;
////    else
////        return false;
//
//    double contourThickness = planeGeometry->GetExtentInMM( 2 )*precisionFactor;
//
//    double ang=m_vtkPlaneGeometry->Angle(planeGeometry);
//    double dis=std::abs(m_vtkPlaneGeometry->SignedDistance(planeGeometry->GetOrigin()));
//    if( (ang<0.02||ang>3.12) && dis<contourThickness)
//        return true;
//    else
//        return false;
//}

PathElement::PathPoint Contour::GetPathPoint()
{
    std::cout<<"GetPathPoint() "<<std::endl;
    return m_PathPoint;
}

void Contour::SetPathPoint(PathElement::PathPoint pathPoint)
{
    std::cout<<"SetPathPoint() "<<std::endl;
    m_PathPoint=pathPoint;

    std::array<double, 3>  spacing;
    spacing.fill(0.1);

    m_vtkPlaneGeometry=SegmentationUtils::CreatePlaneGeometry(pathPoint,spacing, 1.0);
}

int Contour::GetPathPosID(){
    std::cout<<"GetPathPosID() "<<std::endl;
    return m_PathPoint.id;
}

std::array<double, 3>  Contour::GetPathPosPoint()
{
    std::cout<<"GetPathPosPoint() "<<std::endl;
    return m_PathPoint.pos;
}

vtkImageData* Contour::GetVtkImageSlice()
{
    std::cout<<"GetVtkImageSlice() "<<std::endl;
    return m_VtkImageSlice;
}

void Contour::SetVtkImageSlice(vtkImageData* slice)
{
    std::cout<<"SetVtkImageSlice() "<<std::endl;
    m_VtkImageSlice=slice;
}

double Contour::GetArea()
{
    std::cout<<"GetArea() "<<std::endl;
    double A=0;

    for(int i=0;i<m_ContourPoints.size();i++)
    {
        double  point1[3],point2[3];
        double contourPoints[3];
        for (int j=0;j<3;j++)
            contourPoints[j] = m_ContourPoints[i][j];
        m_vtkPlaneGeometry->ProjectPoint(contourPoints, point1);
        if(i==m_ContourPoints.size()-1)
        {
            for (int j=0;j<3;j++)
                contourPoints[j] = m_ContourPoints[0][j];
            m_vtkPlaneGeometry->ProjectPoint(contourPoints, point2);
        }
        else
        {
            for (int j=0;j<3;j++)
                contourPoints[j] = m_ContourPoints[i+1][j];
            m_vtkPlaneGeometry->ProjectPoint(contourPoints, point2);
        }
        for (int j = 0; j<3; j++)
        {
            point1[j] = point1[j] - m_CenterPoint[j];
            point2[j] = point2[j] - m_CenterPoint[j];
        }
        
        A+=(0.5*sqrt(pow(point1[1]*point2[2]-point1[2]*point2[1],2)+
            pow(point1[2]*point2[0]-point1[0]*point2[2],2)+
            pow(point1[0]*point2[1]-point1[1]*point2[0],2)));
    }

    if(A<0) A=-A;

    return A;
}

double Contour::GetPerimeter()
{
    std::cout<<"GetPerimeter() "<<std::endl;
    double L=0;
    
    for(int i=0;i<m_ContourPoints.size();i++)
    {
        double point1[3],point2[3];
        double contourPoints[3];
        for (int j=0;j<3;j++)
            contourPoints[j] = m_ContourPoints[i][j];
        m_vtkPlaneGeometry->ProjectPoint(contourPoints, point1);
        if(i==m_ContourPoints.size()-1)
        {
            for (int j=0;j<3;j++)
                contourPoints[j] = m_ContourPoints[0][j];
            m_vtkPlaneGeometry->ProjectPoint(contourPoints, point2);
        }
        else
        {
            for (int j=0;j<3;j++)
                contourPoints[j] = m_ContourPoints[i+1][j];
            m_vtkPlaneGeometry->ProjectPoint(contourPoints, point2);
        }

        L+=sqrt(pow(point1[0]-point2[0],2)+pow(point1[1]-point2[1],2)+pow(point1[2]-point2[2],2));
    }

    return L;
}
