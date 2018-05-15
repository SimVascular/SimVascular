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

#ifndef __CVLEVELSETVELOCITYEXPONENTIALDECAY_H
#define __CVLEVELSETVELOCITYEXPONENTIALDECAY_H

#include "SimVascular.h"
#include "svLSetExports.h" // For exports
#include "sv2_LevelSetVelocityImage.h"
#include "sv2_image.h"

typedef enum { VED_Default_K, VED_Mean_K, VED_Gaussian_K,
	       VED_PrincipleK1_K, VED_PrincipleK2_K,
	       VED_Invalid_K } cvLevelSetVelocityExponentialDecay3DKT;

SV_EXPORT_LSET cvLevelSetVelocityExponentialDecay3DKT DKT_StrToEnum( char *name );
SV_EXPORT_LSET char *DKT_EnumToStr( cvLevelSetVelocityExponentialDecay3DKT t );

class SV_EXPORT_LSET cvLevelSetVelocityExponentialDecay : public cvLevelSetVelocityImage {

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
