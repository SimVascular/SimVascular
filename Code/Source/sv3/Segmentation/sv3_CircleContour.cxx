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
#include "sv_FactoryRegistrar.h"

#include "sv3_Contour.h"
#include "sv3_CircleContour.h"
#include "sv3_SegmentationUtils.h"

#include <vtkPoints.h>
#include <vtkCellArray.h>
#include <vtkPolyData.h>
#include <vtkImageReslice.h>
#include <iostream>
using sv3::Contour;
using sv3::circleContour;
using sv3::PathElement;
using sv3::SegmentationUtils;

circleContour::circleContour()
    : Contour()
{
    m_Method="Manual";
    m_Type="Circle";

    m_MinControlPointNumber=2;
    m_MaxControlPointNumber=2;
    m_ControlPointNonRemovableIndices[0]=0;
    m_ControlPointNonRemovableIndices[1]=1;

    m_SubdivisionType=CONSTANT_TOTAL_NUMBER;
    m_SubdivisionNumber=36;
}

circleContour::circleContour(const circleContour &other) 
    : Contour( other )
{
}

circleContour::~circleContour()
{
}

circleContour* circleContour::Clone()
{
    return new circleContour(*this);
}


std::string circleContour::GetClassName()
{
    return "circleContour";
}

//-----------------
// SetControlPoint
//-----------------
// Set the value of a control point.
//
// This sets the circle center (index=0) or boundary point (index=1).
//
void circleContour::SetControlPoint(int index, std::array<double,3> point)
{
    double projPt[3];
    double pt[3];
    pt[0] = point[0]; pt[1] = point[1]; pt[2] = point[2];

    // Project the point onto the plane.
    m_vtkPlaneGeometry->ProjectPoint(point.data(), projPt);

    // Set the circle center.
    //
    if (index == 0) {
        if (m_ControlPoints.size() == 0) {
            m_ControlPoints.push_back(std::array<double,3>{projPt[0],projPt[1],projPt[2]});
        } else {
            std::array<double,3> dirVec;
            for (int i = 0; i < 3; i++) {
                dirVec[i] = projPt[i] - GetControlPoint(0)[i];
            }
            Shift(dirVec);
        }

    // Set the circle radius.
    //
    } else if (index == 1) {
        if (m_ControlPoints.size() < 2) {
            m_ControlPoints.push_back(std::array<double,3>{projPt[0],projPt[1],projPt[2]});
        } else {
            m_ControlPoints[1] = std::array<double,3>{projPt[0],projPt[1],projPt[2]};
        }
        ControlPointsChanged();
    }


}

//-----------
// GetRadius
//-----------
// Get the circle radius.
//
double circleContour::GetRadius()
{
  auto cpt1 = GetControlPoint(0);
  auto cpt2 = GetControlPoint(1);

  double mag = 0.0;
  for (int i = 0; i < 3; i++) {
      mag += (cpt1[i]-cpt2[i]) * (cpt1[i]-cpt2[i]);
  }

  return sqrt(mag);
}

//-----------
// SetRadius
//-----------
//
void circleContour::SetRadius(double radius)
{
  auto center = GetControlPoint(0);
  SetControlPointByRadius(radius, center.data());
}


void circleContour::SetControlPointByRadius(double radius, double* point)
{
    double centerPt[3];
    std::cout <<"Point coords: "<<point[0]<<" "<<point[1]<<" "<<point[2]<<std::endl;
    m_vtkPlaneGeometry->ProjectPoint(point, centerPt);
    std::cout <<"centerPt coords: "<<centerPt[0]<<" "<<centerPt[1]<<" "<<centerPt[2]<<std::endl;
    std::array<double,3> dirVec;
    if(m_ControlPoints.size()==0)
        m_ControlPoints.push_back(std::array<double,3>{centerPt[0],centerPt[1],centerPt[2]});
    else
    {
        for (int i=0; i<3; i++)
            dirVec[i]=centerPt[i]-GetControlPoint(0)[i];
        Shift(dirVec);
    }
    std::array<double,3> boundaryPoint;
    double* normal = m_vtkPlaneGeometry->GetNormal();
    double vec[3];
    
    SegmentationUtils::getOrthogonalVector(normal,vec);
    
    if (vec==NULL)
        return;

    boundaryPoint[0]=centerPt[0]+radius*vec[0];
    boundaryPoint[1]=centerPt[1]+radius*vec[1];
    boundaryPoint[2]=centerPt[2]+radius*vec[2];
    
    if(m_ControlPoints.size()<2)
        m_ControlPoints.push_back(boundaryPoint);
    else
        m_ControlPoints[1]=boundaryPoint;
    ControlPointsChanged();

}

void circleContour::AssignCenterScalingPoints()
{
}

circleContour* circleContour::CreateByFitting(Contour* contour)
{
    double area=contour->GetArea();
    double radius=sqrt(area/vnl_math::pi);
    double centerPoint[3], boundaryPoint[3];
    
    vtkSmartPointer<vtkPlane> plane = contour->GetPlaneGeometry();

    double controlPt[3];
    for (int j=0; j<3;j++)
        controlPt[j] = contour->GetControlPoint(0)[j];
    
    double* normal;
    if(plane)
    {
        plane->ProjectPoint(controlPt, centerPoint );
        normal = plane->GetNormal();
    }
    else
        return NULL;
        
    double vec[3];
    
    SegmentationUtils::getOrthogonalVector(normal,vec);
    if (vec==NULL)
        return NULL;

    
    boundaryPoint[0]=centerPoint[0]+radius*vec[0];
    boundaryPoint[1]=centerPoint[1]+radius*vec[1];
    boundaryPoint[2]=centerPoint[2]+radius*vec[2];

    std::vector<std::array<double,3> > controlPoints;

    std::array<double, 3> center, boundary;
    for (int j = 0; j<3; j++)
    {
        center[j] = centerPoint[j];
        boundary[j] = boundaryPoint[j];
    }
    controlPoints.push_back(center);
    controlPoints.push_back(boundary);

    circleContour* newContour=new circleContour();
    newContour->SetPathPoint(contour->GetPathPoint());
    //newContour->SetPlaced(true);
    newContour->SetMethod(contour->GetMethod());
//    newContour->SetClosed(contour->IsClosed());
    newContour->SetControlPoints(controlPoints);

    newContour->SetSubdivisionType(contour->GetSubdivisionType());
    newContour->SetSubdivisionSpacing(contour->GetSubdivisionSpacing());
    newContour->SetSubdivisionNumber(contour->GetSubdivisionNumber());

    return newContour;
}

void circleContour::CreateContourPoints()
{
    if (m_ControlPoints.size()!=2)
    {
        fprintf(stderr, "Control points haven't been set\n");
        return;
    }
    
    if (m_ContourPoints.size()!=0)
        m_ContourPoints.clear();
    double centerPoint[3], boundaryPoint[3];

    double controlPoint1[3], controlPoint2[3];
    for (int j = 0; j<3; j++)
    {
        controlPoint1[j] = m_ControlPoints[0][j];
        controlPoint2[j] = m_ControlPoints[1][j];
    }
    m_vtkPlaneGeometry->ProjectPoint(controlPoint1, centerPoint );

    m_vtkPlaneGeometry->ProjectPoint(controlPoint2, boundaryPoint );

    double radius = sqrt(pow(centerPoint[0]-boundaryPoint[0],2)+ 
            pow(centerPoint[1]-boundaryPoint[1],2)+
            pow(centerPoint[2]-boundaryPoint[2],2));
    int interNumber;

    switch(m_SubdivisionType)
    {
    case CONSTANT_TOTAL_NUMBER:
        interNumber=m_SubdivisionNumber;
        break;
    case CONSTANT_SPACING:
        interNumber=2.0*vnl_math::pi*radius/m_SubdivisionSpacing;
        if(interNumber<m_SubdivisionNumber)
        {
            interNumber=m_SubdivisionNumber;
        }
        break;
    default:
        break;
    }
    
    double* normal=m_vtkPlaneGeometry->GetNormal();       
    double vec[3];
    SegmentationUtils::getOrthogonalVector(normal,vec);
    if(vec==NULL)
        return;
        
    double cross[3];
    cross[0]=normal[1]*vec[2]-normal[2]*vec[1];
    cross[1]=normal[2]*vec[0]-normal[0]*vec[2];
    cross[2]=normal[0]*vec[1]-normal[1]*vec[0];
    
        
    for ( int i = 0; i < interNumber; ++i )
    {
        double alpha = (double) i * vnl_math::pi * 2.0 / interNumber;

        std::array<double,3> point;

        point[0] = centerPoint[0] + radius * (cos( alpha )*vec[0]+sin( alpha )*cross[0]);
        point[1] = centerPoint[1] + radius * (cos( alpha )*vec[1]+sin( alpha )*cross[1]);
        point[2] = centerPoint[2] + radius * (cos( alpha )*vec[2]+sin( alpha )*cross[2]);
        
        m_ContourPoints.push_back(point);
    }
    
}

circleContour* circleContour::CreateSmoothedContour(int fourierNumber)
{
    if(m_ContourPoints.size()<3)
        return this->Clone();

    circleContour* contour=new circleContour();
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
