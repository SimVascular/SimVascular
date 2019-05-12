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
#include "sv3_ThresholdContour.h"
#include "sv3_SegmentationUtils.h"

#include <vtkPoints.h>
#include <vtkCellArray.h>
#include <vtkPolyData.h>
#include <vtkImageReslice.h>
#include <vtkPolyDataConnectivityFilter.h>
#include <iostream>
using sv3::Contour;
using sv3::thresholdContour;
using sv3::PathElement;
using sv3::SegmentationUtils;

thresholdContour::thresholdContour()
    : Contour()
{
    m_forceClosed = true;
    m_thresholdValue = 0.;
}

thresholdContour::thresholdContour(const thresholdContour &other) 
    : Contour( other )
{
    m_thresholdValue = other.m_thresholdValue;
    m_forceClosed = other.m_forceClosed;
}

thresholdContour::~thresholdContour()
{
}

thresholdContour* thresholdContour::Clone()
{
    return new thresholdContour(*this);
}

std::string thresholdContour::GetClassName()
{
    return "thresholdContour";
}

void thresholdContour::SetThresholdValue(double thresholdValue)
{
    m_thresholdValue = thresholdValue;
}

double thresholdContour::GetThresholdValue()
{
    return m_thresholdValue;
}


void thresholdContour::CreateContourPoints()
{
    if(m_ContourPoints.size()>0)
        m_ContourPoints.clear();
    
    if(m_VtkImageSlice==NULL)
    {
        printf("Image slice is empty.\n");
        return;
    }

    cvStrPts*  strPts=SegmentationUtils::vtkImageData2cvStrPts(m_VtkImageSlice);
    
    vtkSmartPointer<vtkContourFilter> contourFilter=vtkSmartPointer<vtkContourFilter>::New();
    contourFilter->SetInputDataObject(strPts->GetVtkStructuredPoints());
    contourFilter->SetValue(0,m_thresholdValue);
    contourFilter->Update();
    
    vtkSmartPointer<vtkPolyData> uncleanContour=contourFilter->GetOutput();
    cvPolyData* cvUncleanContour=new cvPolyData(uncleanContour);

    vtkSmartPointer<vtkPolyDataConnectivityFilter> connectFilter=vtkSmartPointer<vtkPolyDataConnectivityFilter>::New();
    connectFilter->SetInputData(uncleanContour);
    connectFilter->SetExtractionModeToClosestPointRegion();
    double seedPt[3] = {m_PathPoint.pos[0],m_PathPoint.pos[1],m_PathPoint.pos[2]};
    connectFilter->SetClosestPoint(seedPt);
    connectFilter->Update();
    
    vtkSmartPointer<vtkPolyData> selectedContour=connectFilter->GetOutput();

    cvPolyData *cvSelectedContour=new cvPolyData(selectedContour);

    cvPolyData *dst;
    
    double pos[3],nrm[3],xhat[3];

    pos[0]=m_PathPoint.pos[0];
    pos[1]=m_PathPoint.pos[1];
    pos[2]=m_PathPoint.pos[2];

    nrm[0]=m_PathPoint.tangent[0];
    nrm[1]=m_PathPoint.tangent[1];
    nrm[2]=m_PathPoint.tangent[2];

    xhat[0]=m_PathPoint.rotation[0];
    xhat[1]=m_PathPoint.rotation[1];
    xhat[2]=m_PathPoint.rotation[2];

    sys_geom_OrientProfile(cvSelectedContour, pos, nrm, xhat, &dst);
    
    std::vector<std::array<double,3> > contourPoints;
    
    vtkPolyData* pd=dst->GetVtkPolyData();
    bool ifClosed;
    std::deque<int> IDList=SegmentationUtils::GetOrderedPtIDs(pd->GetLines(),ifClosed);
    double point[3];
    std::array<double,3> pt;
    for(int i=0;i<IDList.size();i++)
    {
        pd->GetPoint(IDList[i],point);
        pt[0]=point[0];
        pt[1]=point[1];
        pt[2]=point[2];
        contourPoints.push_back(pt);
    }

    delete cvUncleanContour;
    delete cvSelectedContour;

    this->SetPathPoint(m_PathPoint);
    //this->SetPlaced(true);
    this->SetMethod("Threshold");
    this->SetClosed(ifClosed||m_forceClosed);
    this->SetContourPoints(contourPoints);

    return;
}

thresholdContour* thresholdContour::CreateSmoothedContour(int fourierNumber)
{
    if(m_ContourPoints.size()<3)
        return this->Clone();

    thresholdContour* contour=new thresholdContour();
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
