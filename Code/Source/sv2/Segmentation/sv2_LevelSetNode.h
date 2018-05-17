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

#ifndef __CVLEVELSETNODE_H
#define __CVLEVELSETNODE_H

#include "SimVascular.h"
#include "svLSetExports.h" // For exports
#include "sv_misc_utils.h"


#define CV_NODE_ACTIVE      0x01
#define CV_NODE_BAND        0x02
#define CV_NODE_BOUND       0x04
#define CV_NODE_COVERED     0x08
#define CV_NODE_FORCE_MINV  0x10
#define CV_NODE_MINE        0x20
#define CV_NODE_V_ASSIGNED  0x40
#define CV_NODE_BIT7        0x80


typedef enum {
  GS_PHI,
  GS_CURVATURE,
  GS_CURVATURE_3Dm,
  GS_CURVATURE_3Dg,
  GS_CURVATURE_3Dk1,
  GS_CURVATURE_3Dk2,
  GS_INVALID
} GridScalarT;

SV_EXPORT_LSET GridScalarT GridScalarT_StrToEnum( char *name );
SV_EXPORT_LSET char *GridScalarT_EnumToStr( GridScalarT val );

SV_EXPORT_LSET int cvLevelSetNodeCompareFn( const void *keyval, const void *datum );


class SV_EXPORT_LSET cvLevelSetNode {

public:

  cvLevelSetNode();

  double phi_;
  double velocity_;
  double deltaPhi_;
  int i_, j_, k_;
  int logicalIx_;
  int index_;

  double dp_[3];       // < D+x, D+y, D+z >
  double dm_[3];       // < D-x, D-y, D-z >
  double d0_[3];       // < D0x, D0y, D0z >
  double d0x_[3];      // < D0xx, D0xy, D0xz >
  double d0y_[3];      // < D0yx, D0yy, D0yz >
  double d0z_[3];      // < D0zx, D0zy, D0zz >

  double K_;
  double K3dg_;
  double K3dm_;
  double n_[3];        // normal to level set
  double nn_[3];       // natural (i.e. non-normalized) normal vectors
  double pos_[3];

  // These will be indices into the grid's cvLevelSetNode list, whatever the
  // layout might be.
  int xPrevIndex_, yPrevIndex_, zPrevIndex_;
  int xNextIndex_, yNextIndex_, zNextIndex_;

  double F0_, F1_;
  double delPlus_, delMinus_;
  double toDot_[3];   // new vector which is used in the geodesic
                      // image segmentation approach

  // State bits:
  char state_;

  int Contains( GridScalarT t );
  double GetDoubleDatum( GridScalarT t, double tol );

private:
  double ComputeDoubleDatum( GridScalarT t, double tol );

};


// --------
// Contains
// --------

inline
int cvLevelSetNode::Contains( GridScalarT t )
{
  switch (t) {
  case GS_PHI:
    return SV_OK;
  case GS_CURVATURE:
    return SV_OK;
  case GS_CURVATURE_3Dm:
    return SV_OK;
  case GS_CURVATURE_3Dg:
    return SV_OK;
  case GS_CURVATURE_3Dk1:
    return SV_ERROR;
  case GS_CURVATURE_3Dk2:
    return SV_ERROR;
  default:
    return SV_ERROR;
  }
}


// --------------
// GetDoubleDatum
// --------------
// This method allows parameterized access to data members.  It is
// being used by cvLevelSetStructuredGrid::GetBoundaryData.

inline
double cvLevelSetNode::GetDoubleDatum( GridScalarT t, double tol )
{
  switch (t) {
  case GS_PHI:
    return phi_;
  case GS_CURVATURE:
    return K_;
  case GS_CURVATURE_3Dm:
    return K3dm_;
  case GS_CURVATURE_3Dg:
    return K3dg_;
  default:
    return ComputeDoubleDatum( t, tol );
  }
}


// ------------------
// ComputeDoubleDatum
// ------------------

inline
double cvLevelSetNode::ComputeDoubleDatum( GridScalarT t, double tol )
{
  double ks[2];

  switch (t) {
  case GS_CURVATURE_3Dk1:
    if ( Compute3dks( K3dg_, K3dm_, tol, ks ) != SV_OK ) {
      return 0.0;
    }
    return ks[0];
  case GS_CURVATURE_3Dk2:
    if ( Compute3dks( K3dg_, K3dm_, tol, ks ) != SV_OK ) {
      return 0.0;
    }
    return ks[1];
  default:
    return 0.0;
  }
}


#endif // __NODE_H


