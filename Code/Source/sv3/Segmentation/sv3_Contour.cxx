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
#include "sv_StrPts.h"
#include "sv3_ITKLset_ITKUtils.h"
#include "sv_sys_geom.h"
#include "sv_vtk_utils.h"

#include "sv3_ITKLevelSet.h"
#include "sv_Math.h"

#include "sv3_Contour.h"
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
      m_TagIndex(0),
      m_CenterPoint{0.0,0.0,0.0}
 {
    for (int i=0;i<5;i++)
    {
        m_ControlPointNonRemovableIndices[i]=-2;
    }
    m_ControlPointNonRemovableIndices[0]=0;
    m_ControlPointNonRemovableIndices[1]=1;


    m_ContourID = 0;
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
    SetPathPoint(other.m_PathPoint);

    for(int i=0;i<5;i++){
        m_ControlPointNonRemovableIndices[i]=other.m_ControlPointNonRemovableIndices[i];
    }
}

Contour::~Contour()
{
}

std::string Contour::GetClassName()
{
    return "Contour";
}

std::string Contour::GetType()
{

    return m_Type;
}

void Contour::SetType(std::string type)
{

    m_Type=type;
}

std::string Contour::GetMethod()
{
    return m_Method;
}

void Contour::SetMethod(std::string method)
{
    m_Method=method;
}

int Contour::GetContourID()
{
    return m_ContourID;
}

void Contour::SetContourID(int contourID)
{
    m_ContourID=contourID;
}

void Contour::SetPlaneGeometry(vtkPlane * planeGeometry)
{

    if(planeGeometry!=NULL)
    {
        m_vtkPlaneGeometry = vtkPlane::New();
        m_vtkPlaneGeometry->SetOrigin(planeGeometry->GetOrigin());
        m_vtkPlaneGeometry->SetNormal(planeGeometry->GetNormal());
    }else{
        m_vtkPlaneGeometry = NULL;
    }

}

vtkPlane * Contour::GetPlaneGeometry()
{
    return m_vtkPlaneGeometry;
}

bool Contour::IsClosed()
{
    return m_Closed;
}

void Contour::SetClosed(bool closed)
{
    if(m_Closed!=closed)
    {
        m_Closed=closed;
        CreateContour();
    }
}

bool Contour::IsFinished()
{
    return m_Finished;
}

void Contour::SetFinished(bool finished)
{
    m_Finished=finished;
}

int Contour::GetSubdivisionNumber()
{
    return m_SubdivisionNumber;
}

void Contour::SetSubdivisionNumber(int number)
{
    if(m_SubdivisionNumber!=number)
    {
        m_SubdivisionNumber=number;
        CreateContour();
    }
}

Contour::SubdivisionType Contour::GetSubdivisionType()
{
    return m_SubdivisionType;
}

void Contour::SetSubdivisionType(SubdivisionType subdivType)
{
    if(m_SubdivisionType!=subdivType)
    {
        m_SubdivisionType=subdivType;
        CreateContour();
    }
}

double Contour::GetSubdivisionSpacing()
{
    return m_SubdivisionSpacing;
}

void Contour::SetSubdivisionSpacing(double spacing)
{
    if(m_SubdivisionSpacing!=spacing)
    {
        m_SubdivisionSpacing=spacing;
        CreateContour();
    }
}

int Contour::GetControlPointNumber()
{
    return m_ControlPoints.size();
}

int Contour::GetMinControlPointNumber()
{
    return m_MinControlPointNumber;
}

int Contour::GetMaxControlPointNumber()
{
    return m_MaxControlPointNumber;
}

void Contour::SetMinControlPointNumber(int number)
{
    m_MinControlPointNumber=number;
}

void Contour::SetMaxControlPointNumber(int number)
{
    m_MaxControlPointNumber=number;
}

std::vector<std::array<double,3> > Contour::GetControlPoints()
{
    return m_ControlPoints;
}


std::array<double, 3> Contour::GetControlPoint(int index){
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
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        m_ControlPoints.erase(m_ControlPoints.begin()+index);
        ControlPointsChanged();
    }

}

void Contour::SetControlPoint(int index, std::array<double, 3> point)
{
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

void Contour::SetControlPointSelectedIndex(int index)
{
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
    m_ControlPointSelectedIndex=-2;
}

int Contour::GetControlPointSelectedIndex()
{
    return m_ControlPointSelectedIndex;
}

void Contour::ClearControlPoints()
{
    m_ControlPoints.clear();
}

void Contour::PlaceContour(std::array<double, 3> point)
{
    PlaceControlPoints(point);
    ControlPointsChanged();
}

void Contour::PlaceControlPoints(std::array<double, 3> point)
{
    for ( unsigned int i = 0; i < GetMinControlPointNumber(); ++i )
    {
      m_ControlPoints.push_back( point );
    }

    m_ControlPointSelectedIndex = 1;
}

void Contour::SetControlPoints(std::vector<std::array<double, 3> > controlPoints, bool updateContour)
{
    m_ControlPoints=controlPoints;
    if(updateContour)
        ControlPointsChanged();
}

bool Contour::IsControlPointRemovable(int index)
{
    for(int i=0;i<5;i++)
    {
        if(m_ControlPointNonRemovableIndices[i]==index)
            return false;
    }

    return true;
}

void Contour::ClearContourPoints()
{
    m_ContourPoints.clear();
}

void Contour::CreateContour()
{
    
    if(m_ControlPoints.size()<1)
    {
        return;
    }

    m_ContourPoints.clear();
    CreateContourPoints();
    ContourPointsChanged();
}

void Contour::ControlPointsChanged(){
    CreateContour();
}

void Contour::SetContourPoints(std::vector<std::array<double, 3> > contourPoints, bool update)
{
    m_ContourPoints=contourPoints;
    if(update)
        ContourPointsChanged();
}

int Contour::GetContourPointNumber()
{
    return m_ContourPoints.size();
}

std::array<double, 3>  Contour::GetContourPoint(int index)
{
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
    return m_ContourPoints;
}

void Contour::ContourPointsChanged()
{
    CreateCenterScalingPoints();
    AssignCenterScalingPoints();
}

Contour* Contour::CreateSmoothedContour(int fourierNumber)
{
    if(m_ContourPoints.size()<3)
        return this->Clone();

    Contour* contour=new Contour();
    contour->SetPathPoint(m_PathPoint);
    std::string method=m_Method;
    int idx=method.find("Smoothed");
    if(idx<0)
        method=method+" + Smoothed";

    contour->SetMethod(method);
    contour->SetClosed(m_Closed);

    int pointNumber=m_ContourPoints.size();

    int smoothedPointNumber;

    if((2*pointNumber)<fourierNumber)
        smoothedPointNumber=3*fourierNumber;
    else
        smoothedPointNumber=pointNumber;

    cvMath *cMath = new cvMath();
    std::vector<std::array<double, 3> > smoothedContourPoints=cMath->CreateSmoothedCurve(m_ContourPoints,m_Closed,fourierNumber,0,smoothedPointNumber);
    delete cMath;
    contour->SetContourPoints(smoothedContourPoints);

    return contour;
}
void Contour::AssignCenterScalingPoints()
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

void Contour::CreateCenterScalingPoints()
{
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
        double dis=sqrt(pow(center[0]-point[0],2)+pow(center[1]-point[1],2)+pow(center[2]-point[2],2));
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
    double vec[3];
    SegmentationUtils::getOrthogonalVector(m_vtkPlaneGeometry->GetNormal(),vec);
    scalingPoint[0]=center[0]+minDis/2*vec[0];
    scalingPoint[1]=center[1]+minDis/2*vec[1];
    scalingPoint[2]=center[2]+minDis/2*vec[2];
      
    m_CenterPoint = center;
    m_ScalingPoint = scalingPoint;
}

std::array<double, 3>  Contour::GetCenterPoint()
{
    return m_CenterPoint;
}

vtkSmartPointer<vtkPolyData> Contour::CreateVtkPolyDataFromContour(bool includingAllLines)
{
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
    return contourPointIndex;
}

void Contour::Shift(std::array<double, 3>  dirVec){

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
    Scale(scalingFactor, m_ControlPoints[0]);
}

void Contour::Scale(double scalingFactor, std::array<double, 3>  referencePoint)
{
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

PathElement::PathPoint Contour::GetPathPoint()
{
    return m_PathPoint;
}

void Contour::SetPathPoint(PathElement::PathPoint pathPoint)
{
    m_PathPoint=pathPoint;

    std::array<double, 3>  spacing;
    spacing.fill(0.1);

    m_vtkPlaneGeometry=SegmentationUtils::CreatePlaneGeometry(pathPoint,spacing, 1.0);
}

int Contour::GetPathPosID(){
    return m_PathPoint.id;
}

std::array<double, 3>  Contour::GetPathPosPoint()
{
    return m_PathPoint.pos;
}

vtkImageData* Contour::GetVtkImageSlice()
{
    return m_VtkImageSlice;
}

void Contour::SetVtkImageSlice(vtkImageData* slice)
{
    m_VtkImageSlice=slice;
}

double Contour::GetArea()
{
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

