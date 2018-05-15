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

#ifndef __CVLEVELSETVELOCITYSMOOTH_H
#define __CVLEVELSETVELOCITYSMOOTH_H

#include "SimVascular.h"
#include "svLSetExports.h" // For exports
#include "sv2_LevelSetVelocity.h"


typedef enum { VS_Mean_K, VS_Gaussian_K,
	       VS_PrincipleK1_K, VS_PrincipleK2_K,
	       VS_Invalid_K } cvLevelSetVelocitySmooth3DKT;

SV_EXPORT_LSET cvLevelSetVelocitySmooth3DKT KT_StrToEnum( char *name );
SV_EXPORT_LSET char *KT_EnumToStr( cvLevelSetVelocitySmooth3DKT t );

class SV_EXPORT_LSET cvLevelSetVelocitySmooth : public cvLevelSetVelocity {

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
