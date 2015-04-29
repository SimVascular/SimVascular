/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef __CV_A2ITK_H
#define __CV_A2ITK_H

#include "itkImage.h"

template <unsigned int Ta2itkDim, typename InputPixelType1 = float, typename InputPixelType2 = InputPixelType1, typename OutputPixelType = InputPixelType1>
class a2itk {

  public:
     a2itk();
    ~a2itk();
    
    int setInput1(vtkStructuredPoints* img);
    int setInput2(vtkStructuredPoints* img);
    vtkStructuredPoints* getOutput();

    /**
     * Method: itkRegionOfInterestFilter
     * ---------------------------------------
     * Returns a specified region of interest
     * @param starting[3] represents the x,y,z starting coordinates.
     * @param dimensions[3] represents the size of the x,y,z scales
     *
     * requires input1_
     */
    int itkRegionOfInterestFilter(int starting[], int dimensions[]);

    /**
     * Method: itkResampleFilter
     * ---------------------------------------
     * Resamples data according to spacingParam
     * @param 
     * @param dimensions[3] represents the size of the x,y,z scales
     *
     * requires input1_
     */
    int itkResampleFilter(double spacingParam, int dimensions[]);

        /**
     * Function: itkminMaxCurvFlowFilter
     * ----------------------------
     * Uses an itk filter to smooth the image.
     */
    int itkMinMaxCurvFlowFilter(const unsigned int numIter, 
				const long radius, const double timeStep);
    /**
     * Function: itkSigMapFilter
     * ----------------------------
     * Creates a mapping of the image according to a sigmoid curve.
     */
    int itkSigMapFilter(const double min, const double max, const double alpha, const double beta);

    /**
     * Function: itkFmmFilter
     * ----------------------
     * Creates a signed distance image using FMM.  Similar to createSignedDistanceImageWithFmm,
     * but takes different parameters.
     */
    const static int SEEDS_ARRAY_MAX = 1024;
    int itkFmmFilter(const double inputSeeds[SEEDS_ARRAY_MAX][3], int numSeeds, const double start, const double stop);

    /**
     * Function: itkThresholdLevelSet
     * ----------------------
     * Completes a segmentation using the itk threshold level set segmentation filter.
     * Expects input1_ to be the levelset input and input2_ to be the feature file.
     */
    int itkThresholdLevelSet( const double upper, const double lower, const double propagationScaling,
			      const double curvatureScaling, const int numIterations);

    int createSignedDistanceImageWithFMM (int numSeeds, double* pts,
                                                double origin[3],double spacing[3], 
                                                int dims[3], int start[3],
                                                double time);

    // requires input1_
    int expNegativeImageFilter (double factor);

    // requires input1_ (featureImg) and input2_ (phiImg)
    int shapeDetectionLevelSet (double propagationScaling, 
                                      double curvatureScaling,
                                      double maxRMSerror, int maxNumIters);

  protected:
    
  private:
    
  int dimension_;
  typename itk::Image<InputPixelType1, Ta2itkDim>::Pointer input1_;
  typename itk::Image<InputPixelType2, Ta2itkDim>::Pointer input2_;
  typename itk::Image<OutputPixelType, Ta2itkDim>::Pointer output1_;

};

#ifndef ITK_MANUAL_INSTANTIATION
#include "a2itk.cxx"
#endif

#endif
