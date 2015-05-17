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

#ifndef __CVLEVELSETVELOCITYEXPONENTIALDECAY_H
#define __CVLEVELSETVELOCITYEXPONENTIALDECAY_H

#include "SimVascular.h"
#include "cvLevelSetVelocityImage.h"
#include "cv_image.h"

typedef enum { VED_Default_K, VED_Mean_K, VED_Gaussian_K,
	       VED_PrincipleK1_K, VED_PrincipleK2_K,
	       VED_Invalid_K } cvLevelSetVelocityExponentialDecay3DKT;
cvLevelSetVelocityExponentialDecay3DKT DKT_StrToEnum( char *name );
char *DKT_EnumToStr( cvLevelSetVelocityExponentialDecay3DKT t );


class cvLevelSetVelocityExponentialDecay : public cvLevelSetVelocityImage {

public:
  cvLevelSetVelocityExponentialDecay();
  ~cvLevelSetVelocityExponentialDecay();

  int SetEI( double d );
  double GetEI() { return eI_; }

  int SetEIneg( double d );
  int GetEIneg( double *d );

  int SetKt( double d );
  double GetKt() { return Kt_; }

  int Set3DKType( char *kt_name );
  int Set3DKType( cvLevelSetVelocityExponentialDecay3DKT kt );
  int Get3DKType( cvLevelSetVelocityExponentialDecay3DKT *kt );

  void SetClamp( int f );
  int GetClamp() { return clamp_; }

  void SetMonotonic( int f );
  int GetMonotonic() { return monotonic_; }

  void SetExpand( int f );
  int GetExpand() { return expand_; }

  int Valid();
  int StopCondition();
  int Evaluate( double pos[], double *f0, double *f1, double v[],
		int *forceFlag, double toDot[] );

  int GetMemoryUsage();

private:
  double eI_;
  double eIneg_;
  double Kt_;
  int clamp_;
  int monotonic_;
  int expand_;
  int eInegValid_;
  cvLevelSetVelocityExponentialDecay3DKT ktype_;
};


#endif // __VELOCITY_EXPDECAY_H
