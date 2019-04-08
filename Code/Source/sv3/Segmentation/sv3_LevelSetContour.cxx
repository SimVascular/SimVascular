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
#include "sv3_LevelSetContour.h"
#include "sv3_SegmentationUtils.h"

#include <vtkPoints.h>
#include <vtkCellArray.h>
#include <vtkPolyData.h>
#include <vtkImageReslice.h>
#include <iostream>
using sv3::Contour;
using sv3::levelSetContour;
using sv3::PathElement;
using sv3::SegmentationUtils;

levelSetContour::levelSetContour()
    : Contour()
{
    m_forceClosed = true;
}

levelSetContour::levelSetContour(const levelSetContour &other) 
    : Contour()
{
    m_paras = other.m_paras;
    m_forceClosed = other.m_forceClosed;
}

levelSetContour::~levelSetContour()
{
}

void levelSetContour::SetLevelSetParas(sv3::levelSetContour::svLSParam* paras)
{
    m_paras = paras;
}

sv3::levelSetContour::svLSParam* levelSetContour::GetLevelSetParas()
{
    return m_paras;
}

levelSetContour* levelSetContour::Clone()
{
    return new levelSetContour(*this);
}

std::string levelSetContour::GetClassName()
{
    return "levelSetContour";
}

void levelSetContour::CreateContourPoints()
{
        
    if(m_VtkImageSlice==NULL)
    {
        printf("Image slice is empty.\n");
        return;
    }
    
    if(m_paras == NULL)
    {
        printf("Level Set parameters have not been set.\n");
        return;
    }

    std::cout <<"Path point: "<<m_PathPoint.pos[0]<<" "<<m_PathPoint.pos[1]<<" "<<m_PathPoint.pos[2]<<std::endl;
    //stage 1
    //**************************
    cvITKLevelSet *ls;
    ls = new cvITKLevelSet;
    ls->SetDebug(false);
    ls->SetUseInputImageAsFeature(false);

    cvPolyData *seedPd = NULL;
    //    int loc[3];
    //    loc[0] = x;
    //    loc[1] = y;
    //    loc[2] = z;
    double center[3];
    center[0] = m_paras->ctrx;
    center[1] = m_paras->ctry;
    //    center[2] = param->ctrz;
    center[2] = 0.0;

    cvITKLSUtil::vtkGenerateCircle(m_paras->radius,center,50,&seedPd);
    //    vtkGenerateCircle(param->radius,center,50,&seedPd);

    ls->SetMaxIterations(m_paras->maxIter1);//int
    ls->SetMaxRMSError(m_paras->maxErr1);//double
    ls->SetAdvectionScaling(1.0);
    ls->SetCurvatureScaling(1.0);

    cvStrPts*  strPts=SegmentationUtils::vtkImageData2cvStrPts(m_VtkImageSlice);
    ls->SetInputImage(strPts);
    ls->SetSeed(seedPd);

    //$itklset PhaseOneLevelSet -Kc $kThr -expRising $expRise -expFalling $expFall -sigmaFeat $gSigma1 -sigmaAdv $advSigma1

    if(m_paras->sigmaFeat1 >= 0)
    {
        ls->SetSigmaFeature(m_paras->sigmaFeat1);
    }
    if(m_paras->sigmaAdv1 >= 0)
    {
        ls->SetSigmaAdvection(m_paras->sigmaAdv1);
    }

    ls->ComputePhaseOneLevelSet(m_paras->kc, m_paras->expFactorRising,m_paras->expFactorFalling);

    cvPolyData *front1;
    front1=ls->GetFront();

    //stage 2
    //**********************************************
    cvITKLevelSet *ls2;
    ls2 = new cvITKLevelSet;

    ls2->SetDebug(false);
    ls2->SetUseInputImageAsFeature(false);

    ls2->SetMaxIterations(m_paras->maxIter2);
    ls2->SetMaxRMSError(m_paras->maxErr2);
    ls2->SetAdvectionScaling(1.0);
    ls2->SetCurvatureScaling(1.0);

    cvStrPts*  strPts2=SegmentationUtils::vtkImageData2cvStrPts(m_VtkImageSlice);
    ls2->SetInputImage(strPts2);
    ls2->SetSeed(front1);

    if(m_paras->sigmaFeat2 >= 0)
    {
        ls2->SetSigmaFeature(m_paras->sigmaFeat2);
    }
    if(m_paras->sigmaAdv2 >= 0)
    {
        ls2->SetSigmaAdvection(m_paras->sigmaAdv2);
    }

    ls2->ComputePhaseTwoLevelSet(m_paras->kupp,m_paras->klow);

    cvPolyData *front2;
    front2=ls2->GetFront();

    cvPolyData *dst;
    double tol=0.001;
    dst=sys_geom_MergePts_tol(front2, tol );

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

    cvPolyData *dst2;

    sys_geom_OrientProfile(dst, pos, nrm,xhat,&dst2);

    this->SetPathPoint(m_PathPoint);
    //this->SetPlaced(true);
    this->SetMethod("LevelSet");

    std::vector<std::array<double,3> > contourPoints;

    vtkPolyData* pd=dst2->GetVtkPolyData();
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

    this->SetClosed(ifClosed||m_forceClosed);
    this->SetContourPoints(contourPoints);

    return;
}

levelSetContour* levelSetContour::CreateSmoothedContour(int fourierNumber)
{
    if(m_ContourPoints.size()<3)
        return this->Clone();

    levelSetContour* contour=new levelSetContour();
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
