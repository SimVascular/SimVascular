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

// Class cvLevelSetVelocity is being created in order to encapsulate all of the
// details of velocity function evaluation.  Previously, methods in
// class Grid were used to directly compute velocities.  Encapsulation
// of velocity parameters will serve to separate the mechanics of Grid
// maintenance from the details of particular velocity functions.

// Note that the code parameter to Evaluate is to be used to indicate
// the direction along with the given cvLevelSetNode's are adjacent.  This
// information should fall directly out of the cvLevelSetNode neighbor
// comparisons that will need to be done in preparation for calls to
// cvLevelSetVelocity::Evaluate.


#ifndef __CVLEVELSETVELOCITY_H
#define __CVLEVELSETVELOCITY_H

#include "SimVascular.h"
#include "svLSetExports.h" // For exports
#include "sv_misc_utils.h"
#include "sv2_LevelSetStructuredGrid.h"


class SV_EXPORT_LSET cvLevelSet;


class SV_EXPORT_LSET cvLevelSetVelocity {

  friend class cvLevelSet;

public:
  cvLevelSetVelocity();
  virtual ~cvLevelSetVelocity();

  int LinkLevelSet( cvLevelSet *ls );
  int UnlinkLevelSet();
  int GetLevelSet( cvLevelSet **ls );
  int UpdateTolerance();

  void SetStopV( double d ) { stopV_ = d; return; }
  double GetStopV() { return stopV_; }

  virtual int Valid();
  virtual int StopCondition() { return SV_ERROR; };

  virtual int Evaluate( double pos[], double *f0, double *f1,
			double v[], int *forceFlag, double toDot[] ) = 0;

  virtual int GetMemoryUsage() = 0;

  char tclName_[CV_STRLEN];

protected:

  // Protected members can be accessed as public by objects of derived
  // classes.
  cvLevelSet *ls_;
  double tol_;
  double stopV_;

};


#endif // __VELOCITY_H
