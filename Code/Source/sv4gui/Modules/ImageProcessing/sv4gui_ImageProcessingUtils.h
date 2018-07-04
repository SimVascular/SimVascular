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

#ifndef SV4GUIIMAGEPROCESSINGUTILS_H
#define SV4GUIIMAGEPROCESSINGUTILS_H

#include "sv4guiModuleImageProcessingExports.h"

#include "SimVascular.h"

#include <vtkTransform.h>
#include <vtkImageData.h>
#include <vtkSmartPointer.h>
#include <vtkPolyData.h>

#include <itkImage.h>
#include <string>
#include <itkBinaryBallStructuringElement.h>

class SV4GUIMODULEIMAGEPROCESSING_EXPORT sv4guiImageProcessingUtils
{

public:

    sv4guiImageProcessingUtils();

    virtual ~sv4guiImageProcessingUtils();

    //typedefs
    typedef itk::Image<float, 3> itkImageType;
    typedef itkImageType::Pointer itkImPoint;
    typedef itk::BinaryBallStructuringElement<sv4guiImageProcessingUtils::itkImageType::PixelType, 3> StructElType;

    //functions
    static itkImPoint vtkImageToItkImage(vtkImageData* imageData);
    static vtkSmartPointer<vtkImageData> itkImageToVtkImage(itkImPoint image);

    static vtkSmartPointer<vtkPolyData> marchingCubes(vtkImageData* imageData, double isovalue,
      bool largest_cc);

    static vtkSmartPointer<vtkPolyData> seedMarchingCubes(vtkImageData* imageData, double isovalue,
    double px, double py, double pz);

    static std::vector<int> physicalPointToIndex(itkImPoint image,
      double x, double y, double z);

    static itkImPoint copyImage(itkImPoint image);

    static itkImPoint elementwiseMinimum(itkImPoint image1, itkImPoint image2);

    static itkImPoint threshold(itkImPoint image,
      double lowerThreshold, double upperThreshold);

    static itkImPoint binaryThreshold(itkImPoint image,
        double lowerThreshold, double upperThreshold, double insideValue, double outsideValue);

    static itkImPoint connectedThreshold(itkImPoint image,
      double lowerThreshold, double upperThreshold, double insideValue, std::vector<std::vector<int>> seeds);

    static itkImPoint collidingFronts(itkImPoint image, int x1, int y1, int z1, int x2,
      int y2, int z2, double lowerThreshold, double upperThreshold);

    static itkImPoint zeroLevel(itkImPoint image, double pixelValue);

    static itkImPoint openClose(itkImPoint image, int radius);

    static itkImPoint gradientMagnitude(itkImPoint image, double sigma);
    static itkImPoint smooth(itkImPoint image, double sigma);
    static itkImPoint anisotropicSmooth(itkImPoint image, int iterations, double timeStep, double conductance);
    static itkImPoint fillHoles(itkImPoint image, double foregroundValue);

    static itkImPoint editImage(itkImPoint image,
      int ox, int oy, int oz, int l, int w, int h, double replaceValue);

    static itkImPoint cropImage(itkImPoint image,
        int ox, int oy, int oz, int l, int w, int h);

    static itkImPoint resampleImage(itkImPoint image,
        double space_x, double space_y, double space_z);

    static itkImPoint geodesicLevelSet(itkImPoint initialization, itkImPoint edgeImage, double propagation, double advection, double curvature, int iterations);

    static void writeMHA(itkImPoint, std::string filename);

    static void writeVtkMHA(vtkImageData* vtkImage, std::string filename);
};

#endif /* SV4GUIIMAGEPROCESSINGUTILS_H */
