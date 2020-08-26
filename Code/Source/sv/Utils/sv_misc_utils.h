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


#ifndef SV_MISC_UTILS_H
#define SV_MISC_UTILS_H

#include <math.h>

#define svmaximum(A, B) ((A) > (B) ? (A) : (B))
#define svminimum(A, B) ((A) < (B) ? (A) : (B))
#define svSqr(A) ((A) * (A))


#define CV_PHI_TOL   0.001

#define CV_STRLEN 1000

#define CV_PI 3.141592653589793324


// KCW [4/18/98]
// ---

#include "SimVascular.h"
#include "svUtilsExports.h" // For exports

// Misc
// ----
SV_EXPORT_UTILS char* PopArg( int *argc, char **argv[] );


// Math
// ----

SV_EXPORT_UTILS double FindMachineEpsilon();


// ==============
//   NormVector
// ==============

inline
void NormVector( double *vx, double *vy, double *vz )
{
  double len;

  len = sqrt( *vx * *vx + *vy * *vy + *vz * *vz );
  if (len > 0.0) {
    (*vx) /= len;
    (*vy) /= len;
    (*vz) /= len;
  }
  return;
}


// =============
//   Magnitude
// =============

inline
double Magnitude( double vx, double vy, double vz )
{
  double len;

  len = sqrt( vx*vx + vy*vy + vz*vz );
  return len;
}


// =========
//   Cross
// =========

inline
void Cross( double ax, double ay, double az,
            double bx, double by, double bz,
            double *prodx, double *prody, double *prodz )
{
  (*prodx) = ay * bz - az * by;
  (*prody) = -( ax * bz - az * bx );
  (*prodz) = ax * by - ay * bx;
  return;
}


// =======
//   Dot
// =======

inline
double Dot( double ax, double ay, double az,
            double bx, double by, double bz )
{
  double product;

  product = ax * bx + ay * by + az * bz;
  return product;
}


// ===============
//   misc_Det3x3
// ===============

inline
double misc_Det3x3( double mat[] )
{
  double a11, a12, a13;
  double a21, a22, a23;
  double a31, a32, a33;
  double det;

  a11 = mat[0];
  a12 = mat[1];
  a13 = mat[2];

  a21 = mat[3];
  a22 = mat[4];
  a23 = mat[5];

  a31 = mat[6];
  a32 = mat[7];
  a33 = mat[8];

  det = a11*a22*a33 - a11*a23*a32 - a12*a21*a33
    + a12*a23*a31 + a13*a21*a32 - a13*a22*a31;

  return det;
}


// =========
//   svRound
// =========

inline
int svRound( double d )
{
  return( (int) floor( d + 0.5 ) );
}


// ============
//   Distance
// ============

inline
double Distance( double ax, double ay, double az,
		 double bx, double by, double bz )
{
  double tmpx, tmpy, tmpz;
  tmpx = (ax-bx) * (ax-bx);
  tmpy = (ay-by) * (ay-by);
  tmpz = (az-bz) * (az-bz);
  return( sqrt( tmpx + tmpy + tmpz ) );
}


// ========
//   Sign
// ========

inline
double cvSign( double d, double tol )
{
  if ( fabs(d) < tol ) {
    return 0.0;
  }
  if ( fabs(d) > d ) {
    return -1.0;
  }
  return 1.0;
}


// ===========
//   IntSign
// ===========

inline
int IntSign( double d, double tol )
{
  if ( fabs(d) < tol ) {
    return 0;
  }
  if ( fabs(d) > d ) {
    return -1;
  }
  return 1;
}


int SV_EXPORT_UTILS Compute3dks( double Kg, double Km, double tol, double ks[] );


// Files
// -----
int SV_EXPORT_UTILS CountLines( char *filename );


// Inline functions
// ----------------

// ---------------
// LinInterp1D_dbl
// ---------------
// Omitting error checking for now for performance...

inline
int LinInterp1D_dbl( double domA, double domB,
		     double rangeA, double rangeB,
		     double domTarget, double *rangeTarget )
{
  double domDelta;
  double rangeDelta;
  double factor;

  domDelta = domB - domA;
  rangeDelta = rangeB - rangeA;
  factor = ( domTarget - domA ) / domDelta;
  *rangeTarget = rangeA + ( factor * rangeDelta );

  return SV_OK;
}


#endif  // __MISC_UTILS_H
