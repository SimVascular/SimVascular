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

#ifndef SVSEGMENTATIONUTILS_H
#define SVSEGMENTATIONUTILS_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "svPathElement.h"
#include "svContour.h"
#include "cvStrPts.h"

#include <deque>

#include <mitkSlicedGeometry3D.h>
#include <mitkImage.h>

#include <vtkTransform.h>
#include <vtkPolyData.h>

class SVSEGMENTATION_EXPORT svSegmentationUtils
{

public:

    struct svLSParam
    {
        double ctrx;
        double ctry;
        double ctrz;
        double radius;

        double sigmaFeat1;
        double sigmaAdv1;
        double kc;
        double expFactorRising;
        double expFactorFalling;
        int maxIter1;
        double maxErr1;

        double sigmaFeat2;
        double sigmaAdv2;
        double kupp;
        double klow;
        int maxIter2;
        double maxErr2;

        svLSParam()
            : ctrx(0.0)
            , ctry(0.0)
            , ctrz(0.0)
            , radius(0.3)
            , sigmaFeat1(2.5)
            , sigmaAdv1(0.0)
            , kc(0.6)
            , expFactorRising(0.25)
            , expFactorFalling(0.5)
            , maxIter1(2000)
            , maxErr1(0.001)
            , sigmaFeat2(1.5)
            , sigmaAdv2(0.0)
            , kupp(0.8)
            , klow(0.09)
            , maxIter2(1000)
            , maxErr2(0.0005)
        {
        }

    };

    svSegmentationUtils();

    virtual ~svSegmentationUtils();


    static vtkTransform* GetvtkTransform(svPathElement::svPathPoint pathPoint);

    static vtkTransform* GetvtkTransformBox(svPathElement::svPathPoint pathPoint, double boxHeight);

    static mitk::PlaneGeometry::Pointer CreatePlaneGeometry(svPathElement::svPathPoint pathPoint, mitk::Vector3D spacing, double size);

    static mitk::PlaneGeometry::Pointer CreatePlaneGeometry(svPathElement::svPathPoint pathPoint, mitk::BaseData* baseData, double size, bool useOnlyMinimumSpacing = false);

    static mitk::SlicedGeometry3D::Pointer CreateSlicedGeometry(std::vector<svPathElement::svPathPoint> pathPoints, mitk::BaseData* baseData, double size, bool useOnlyMinimumSpacing = false);


    static mitk::Image::Pointer GetSliceImage(const mitk::PlaneGeometry* planeGeometry, const mitk::Image* image, unsigned int timeStep = 0);

    static cvStrPts* GetSlicevtkImage(svPathElement::svPathPoint pathPoint, vtkImageData* volumeimage, double size);

    static vtkImageData* GetSlicevtkImage(const mitk::PlaneGeometry* planeGeometry, const mitk::Image* image, unsigned int timeStep = 0);

    static cvStrPts* image2cvStrPts(mitk::Image* image);

    static cvStrPts* vtkImageData2cvStrPts(vtkImageData* vtkImg);


    static svContour* CreateLSContour(svPathElement::svPathPoint pathPoint, vtkImageData* volumeimage, svLSParam* param, double size, bool forceClosed = true);

    static vtkPolyData* orientBack(vtkPolyData* srcPd, mitk::PlaneGeometry* planeGeometry);


    static std::vector<mitk::Point3D> GetThresholdContour(vtkImageData* imageSlice, double thresholdValue, svPathElement::svPathPoint pathPoint, bool& ifClosed, double seedPoint[3]);

    static svContour* CreateThresholdContour(svPathElement::svPathPoint pathPoint, vtkImageData* volumeimage, double thresholdValue, double size, bool forceClosed = true);


    static std::deque<int> GetOrderedPtIDs(vtkCellArray* lines, bool& ifClosed);

};


#endif /* SVSEGMENTATIONUTILS_H */
