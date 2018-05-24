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

// NOTE that, like cvLevelSetVelocity, cvLevelSetVelocityImage is also an abstract base class
// because the pure virtual cvLevelSetVelocity::Evaluate is not implemented
// here.


#ifndef __CVLEVELSETVELOCITYIMAGE_H
#define __CVLEVELSETVELOCITYIMAGE_H

#include "SimVascular.h"
#include "svLSetExports.h" // For exports
#include "sv2_LevelSetVelocity.h"
#include "sv2_image.h"


class SV_EXPORT_LSET cvLevelSetVelocityImage : public cvLevelSetVelocity {

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
