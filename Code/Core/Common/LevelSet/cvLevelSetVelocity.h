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
#include "cv_misc_utils.h"
#include "cvLevelSetStructuredGrid.h"


class cvLevelSet;


class cvLevelSetVelocity {

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
  virtual int StopCondition() { return 0; };

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
