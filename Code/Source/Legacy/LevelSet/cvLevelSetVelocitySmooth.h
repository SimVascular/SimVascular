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

#ifndef __CVLEVELSETVELOCITYSMOOTH_H
#define __CVLEVELSETVELOCITYSMOOTH_H

#include "SimVascular.h"
#include "cvLevelSetVelocity.h"


typedef enum { VS_Mean_K, VS_Gaussian_K,
	       VS_PrincipleK1_K, VS_PrincipleK2_K,
	       VS_Invalid_K } cvLevelSetVelocitySmooth3DKT;
cvLevelSetVelocitySmooth3DKT KT_StrToEnum( char *name );
char *KT_EnumToStr( cvLevelSetVelocitySmooth3DKT t );


class cvLevelSetVelocitySmooth : public cvLevelSetVelocity {

public:
  cvLevelSetVelocitySmooth();
  ~cvLevelSetVelocitySmooth();

  int SetKts( double upper_kt, double lower_kt );
  int GetKts( double *upper_kt, double *lower_kt );

  int Set3DKType( char *kt_name );
  int Set3DKType( cvLevelSetVelocitySmooth3DKT kt );
  int Get3DKType( cvLevelSetVelocitySmooth3DKT *kt );

  int Valid();
  int StopCondition();
  int Evaluate( double pos[], double *f0, double *f1, double v[],
		int *forceFlag, double toDot[] );

  int GetMemoryUsage() { return sizeof(this); }

private:
  double upper_kt_;
  double lower_kt_;
  int ktsValid_;
  cvLevelSetVelocitySmooth3DKT ktype_;

};


#endif // __VELOCITY_CONST_H
