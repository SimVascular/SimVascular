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

    static mitk::PlaneGeometry::Pointer CreatePlaneGeometry(svPathElement::svPathPoint pathPoint, mitk::Image* image, double size, bool useOnlyMinimumSpacing = false);

    static mitk::SlicedGeometry3D::Pointer CreateSlicedGeometry(std::vector<svPathElement::svPathPoint> pathPoints, mitk::Image* image, double size, bool useOnlyMinimumSpacing = false);


    static mitk::Image::Pointer GetSliceImage(const mitk::PlaneGeometry* planeGeometry, const mitk::Image* image, unsigned int timeStep = 0);

    static cvStrPts* GetSlicevtkImage(svPathElement::svPathPoint pathPoint, vtkImageData* volumeimage, double size);

    static vtkImageData* GetSlicevtkImage(const mitk::PlaneGeometry* planeGeometry, const mitk::Image* image, unsigned int timeStep = 0);

    static cvStrPts* image2cvStrPts(mitk::Image* image);

    static cvStrPts* vtkImageData2cvStrPts(vtkImageData* vtkImg);


    static svContour* CreateLSContour(svPathElement::svPathPoint pathPoint, vtkImageData* volumeimage, svLSParam* param, double size);

    static vtkPolyData* orientBack(vtkPolyData* srcPd, mitk::PlaneGeometry* planeGeometry);


    static std::vector<mitk::Point3D> GetThresholdContour(vtkImageData* imageSlice, double thresholdValue, svPathElement::svPathPoint pathPoint, bool& ifClosed, double seedPoint[3]);

    static svContour* CreateThresholdContour(svPathElement::svPathPoint pathPoint, vtkImageData* volumeimage, double thresholdValue, double size);


    static std::deque<int> GetOrderedPtIDs(vtkCellArray* lines, bool& ifClosed);

};


#endif /* SVSEGMENTATIONUTILS_H */
