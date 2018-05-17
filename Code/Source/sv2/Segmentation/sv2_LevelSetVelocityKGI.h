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

// "KGI" refers to curvature ("K") and grad(I) ("GI") dependences.


#ifndef __CVLEVELSETVELOCITYKGI_H
#define __CVLEVELSETVELOCITYKGI_H

#include "SimVascular.h"
#include "svLSetExports.h" // For exports
#include "sv2_LevelSetVelocityImage.h"
#include "sv2_image.h"


class SV_EXPORT_LSET cvLevelSetVelocityKGI : public cvLevelSetVelocityImage {

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
