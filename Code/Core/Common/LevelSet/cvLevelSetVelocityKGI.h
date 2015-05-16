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

// "KGI" refers to curvature ("K") and grad(I) ("GI") dependences.


#ifndef __CVLEVELSETVELOCITYKGI_H
#define __CVLEVELSETVELOCITYKGI_H

#include "SimVascular.h"
#include "cvLevelSetVelocityImage.h"
#include "cv_image.h"


class cvLevelSetVelocityKGI : public cvLevelSetVelocityImage {

public:
  cvLevelSetVelocityKGI();
  ~cvLevelSetVelocityKGI();

  void PostSetImageAction();
  int GetMaxImageIGrad( double *maxGrad );

  int SetEK( double d );
  int GetEK( double *d );

  int SetEI( double d );
  int GetEI( double *d );

  int SetBalloonF( double b );
  int GetBalloonF( double *b );

  int SetGradIPow( double p );
  int GetGradIPow( double *p );

  int SetBeta( double b );
  int GetBeta( double *b );

  int SetApplyLocalStop( int flag );
  int GetApplyLocalStop( int *flag );

  int SetKTol( double t );
  int GetKTol( double *t );

  int Valid();
  int StopCondition();
  int Evaluate( double pos[], double *f0, double *f1, double v[],
		int *forceFlag, double toDot[] );

  int GetMemoryUsage();

private:
  Image_T *potential_;

  void SetUpPotentialField( Image_T *img );

  int eKvalid_;
  int eIvalid_;
  int balloonFvalid_;
  int gradIPowValid_;
  int betaValid_;

  double eK_;
  double eI_;
  double balloonF_;
  double gradIPow_;
  double beta_;
  int localStop_;
  double ktol_;

};


#endif // __VELOCITY_KGI_H
