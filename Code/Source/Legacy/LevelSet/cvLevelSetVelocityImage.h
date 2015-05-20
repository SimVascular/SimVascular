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

// NOTE that, like cvLevelSetVelocity, cvLevelSetVelocityImage is also an abstract base class
// because the pure virtual cvLevelSetVelocity::Evaluate is not implemented
// here.


#ifndef __CVLEVELSETVELOCITYIMAGE_H
#define __CVLEVELSETVELOCITYIMAGE_H

#include "SimVascular.h"
#include "cvLevelSetVelocity.h"
#include "cv_image.h"


class cvLevelSetVelocityImage : public cvLevelSetVelocity {

public:
  cvLevelSetVelocityImage();
  virtual ~cvLevelSetVelocityImage();

  int SetImage( Image_T *i, int closed = 0 );
  int SetImage( char *filebase, int fileNumRange[], int imgDims[],
		double pixDims[], int closed = 0 );
  int SetImage( short *imgData, int numData, int imgDims[],
		double pixDims[], double origin[], int closed = 0 );
  int SetImage( double *imgData, int numData, int imgDims[],
		double pixDims[], double origin[], int closed = 0 );
  int SetImage( float *imgData, int numData, int imgDims[],
		double pixDims[], double origin[], int closed = 0 );
  int GetImage( Image_T **i );
  int GetMagGradRange( double rng[] );
  int GetXYMagGradRange( double rng[] );
  int GetZMagGradRange( double rng[] );
  int GetIntensityRange( double rng[] );
  int ClearImage();
  virtual void PostSetImageAction() { return; };

protected:
  Image_T *image_;
  int GetImgSize() { return Img_GetMemoryUsage( image_ ); }

};


#endif // __VELOCITY_IMAGE_H
