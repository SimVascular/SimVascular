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

#include "SimVascular.h" 

#include <stdio.h>
#include <math.h>
#include <assert.h>
#include "cvLevelSetStructuredGrid.h"
#include "cvLevelSetVelocity.h"
#include "cv_misc_utils.h"
#include "cvIntArrayList.h"
#include "cvVTK.h"


// --------------
// cvLevelSetStructuredGrid
// --------------
// Some elementary notes on constructor ordering:
//
//   - constructors for member objects in a container class are called
//     BOTTOM-UP... i.e. member objects are constructed before the
//     containing object (see Lippman p. 290)
//   - constructors for a class in a derivation hierarchy are called
//     TOP-DOWN... i.e. parent classes are constructed before derived
//     classes (see Lippman p. 433)
//
// So, the cvLevelSetStructuredGrid constructor will be invoked BEFORE the
// constructor of any derived class (e.g. cvLevelSetDenseGrid, cvLevelSetSparseGrid).
//
// The idea is that cvLevelSetStructuredGrid can set up the abstract, logical
// notion of the grid.  Derived classes will deal with the specifics
// of their physical implementation (e.g. dense vs. sparse storage).

cvLevelSetStructuredGrid::cvLevelSetStructuredGrid( double h[], int dims[], double o[] )
{
  if ( (dims[0] < 1) || (dims[1] < 1) || (dims[2] < 1) ) {
    return;
  }
  I_ = dims[0];
  J_ = dims[1];
  K_ = dims[2];
  numDenseNodes_ = I_ * J_ * K_;

  if ( (h[0] < 0.0) || (h[1] < 0.0) || (h[2] < 0.0) ) {
    return;
  }
  hv_[0] = h[0];
  hv_[1] = h[1];
  hv_[2] = h[2];
  minh_ = minimum( hv_[0], hv_[1] );
  minh_ = minimum( minh_, hv_[2] );
  maxh_ = maximum( hv_[0], hv_[1] );
  maxh_ = maximum( maxh_, hv_[2] );
  mainDiagonal_ = sqrt( sqr( hv_[0] ) + sqr( hv_[1] ) +	sqr( hv_[2] ) );

  if ( I_ == 1 ) {
    dim_ = 0;
  } else if ( J_ == 1 ) {
    dim_ = 1;
  } else if ( K_ == 1 ) {
    dim_ = 2;
  } else {
    dim_ = 3;
  }

  origin_[0] = o[0];
  origin_[1] = o[1];
  if ( dim_ == 2 ) {
    origin_[2] = 0.0;
  } else if ( dim_ == 3 ) {
    origin_[2] = o[2];
  } else {
    assert(0);
  }

  grid_ = NULL;  // May be overridden in a moment if derived class
                 // constructor allocates nodes.
  init_ = 0;
  curr_ = NULL;

  //  tol_ = 0.000001;
  tol_ = 1e6 * FindMachineEpsilon();
  oneOverTol_ = 1 / tol_;
  relTol_ = 0.001;

  d0Valid_ = 0;
  dpiValid_ = 0;
  dmiValid_ = 0;
  delPlusValid_ = 0;
  delMinusValid_ = 0;
  deltaPhiValid_ = 0;

  nValid_ = 0;
  kValid_ = 0;
  k3dmValid_ = 0;
  k3dgValid_ = 0;

  phiVtkValid_ = 0;
  phiVtk_ = NULL;

  velocity_ = NULL;
  velocityVectors_ = NULL;

  ixBuffer_ = NULL;
  nodeSets_ = NULL;
  pSetsValid_ = 0;

  return;
}


// ---------------
// ~cvLevelSetStructuredGrid
// ---------------

cvLevelSetStructuredGrid::~cvLevelSetStructuredGrid()
{
  if ( phiVtk_ ) {
    phiVtk_->Delete();
  }
}


// ----------
// CloseHoles
// ----------
int cvLevelSetStructuredGrid::CloseHoles()
{
  cvLevelSetNode *currNode;

  InitIter();
  while ( currNode = GetNext() ) {
    if ( ( currNode->i_ == 0 ) || ( currNode->i_ == (I_-1) ) ||
	 ( currNode->j_ == 0 ) || ( currNode->j_ == (J_-1) ) ||
	 ( currNode->k_ == 0 ) || ( currNode->k_ == (K_-1) ) ) {
      if ( currNode->phi_ < 0.0 ) {
	currNode->phi_ = 0.0;
      }
    }
  }
  phiVtkValid_ = 0;
  return CV_OK;
}


// ---------
// EvaluateV
// ---------

int cvLevelSetStructuredGrid::EvaluateV( cvLevelSetVelocity *vfn, double factor )
{
  int i, j, k, ix;
  cvLevelSetNode *currNode;
  cvLevelSetNode *adjNode;
  double tol;
  int adjIxs[6];
  char c;
  double f0, f1, mag;
  int status = CV_OK;
  double zls[3];
  double v[3];
  int forceMinVFlag;
  double toDot[3] = {0.0, 0.0, 0.0};  // initialization is critical!

  // Stuff to build up the set of velocity vectors:
  vtkPolyData *pd = vtkPolyData::New();
  vtkPoints *pts = vtkPoints::New();
  vtkFloatingPointArrayType *vec = vtkFloatingPointArrayType::New(); 
  vec->SetNumberOfComponents(3);
  pts->Allocate(100,100);
  vec->Allocate(100,100);
  vtkFloatingPointType zlsf[3];
  vtkFloatingPointType vf[3];

  if ( ! vfn->Valid() ) {
    return CV_ERROR;
  }

  tol = relTol_ * minh_;
  k = 0;

  FindK();
  FindN();
  ClearActive();
  ClearForceMinV();

  // Foreach grid node:
  InitIter();
  while ( currNode = GetNext() ) {

    // If phi(i) ~= 0:
    if ( fabs( currNode->phi_ ) < tol_ ) {
      zls[0] = currNode->pos_[0];
      zls[1] = currNode->pos_[1];
      zls[2] = currNode->pos_[2];
      status = vfn->Evaluate( zls, &f0, &f1, v, &forceMinVFlag, toDot );
      if ( status != CV_OK ) {
	break;  // i.e. out of global node loop
      }
      mag = fabs( f0 + f1 );

      AssignNode( currNode, f0, f1, forceMinVFlag, toDot );

      zlsf[0] = zls[0];
      zlsf[1] = zls[1];
      zlsf[2] = zls[2];
      //pts->InsertPoint( k, zlsf );
      pts->InsertNextPoint(zlsf);
      vf[0] = v[0];
      vf[1] = v[1];
      vf[2] = v[2];
      //vec->InsertTuple( k, vf );
      vec->InsertNextTuple(vf);
      k++;

      continue;
    }

    // Look for active edges radiating from i:
    GetAdjacentIxs( currNode, adjIxs );
    for (j = 0; j < 6; j++) {
      ix = adjIxs[j];
      if ( ix >= 0 ) {
	adjNode = &(grid_[ix]);

	// If this pair of nodes have phi of opposite sign, then:
	//   - set the direction code c
	//   - interpolate the zls position
	//   - evaluate velocity at the zls
	//   - set active bits for curr and adj if not already set
	//   - assign v components as above
	if ( ( currNode->phi_ * adjNode->phi_ ) < 0.0 ) {
	  if ( j < 2 ) {
	    c = 'x';
	  } else if ( j < 4 ) {
	    c = 'y';
	  } else if ( j < 6 ) {
	    c = 'z';
	  }
	  InterpZLS( currNode, adjNode, c, zls );
	  status = vfn->Evaluate( zls, &f0, &f1, v, &forceMinVFlag, toDot );
	  if ( status != CV_OK ) {
	    break;  // i.e. out of local radiating edge loop
	  }
	  mag = fabs( f0 + f1 );

	  AssignNode( currNode, f0, f1, forceMinVFlag, toDot );
	  AssignNode( adjNode, f0, f1, forceMinVFlag, toDot );

	  zlsf[0] = zls[0];
	  zlsf[1] = zls[1];
	  zlsf[2] = zls[2];
	  //pts->InsertPoint( k, zlsf );
          pts->InsertNextPoint( zlsf );
	  vf[0] = v[0];
	  vf[1] = v[1];
	  vf[2] = v[2];
	  //vec->InsertTuple( k, vf );
          vec->InsertNextTuple(vf);
	  k++;

	  continue;
	}
      }
    }
    if ( status != CV_OK ) {
      break;  // i.e. out of global node loop
    }
  }

  if ( status != CV_OK ) {

    pts->Delete();
    vec->Delete();
    pd->Delete();

    return CV_ERROR;
  }

  pd->SetPoints( pts );
  pd->GetPointData()->SetVectors( vec );

  if ( velocityVectors_ != NULL ) delete velocityVectors_;
  velocityVectors_ = new cvPolyData( pd );

  pts->Delete();
  vec->Delete();
  pd->Delete();

  deltaPhiValid_ = 0;
  
  return CV_OK;
}


// ---------------
// ComputeDeltaPhi
// ---------------
// Put phi increment values in the deltaPhi_ field of grid_ cvLevelSetNode's,
// but DON'T actually modify phi_ values.

double cvLevelSetStructuredGrid::ComputeDeltaPhi( double factor )
{
  cvLevelSetNode *currNode;
  double f0Contrib, f1Contrib;
  double maxVal, minVal;
  double tmp, f1MagGradPhi;
  double phi_t;
  double v;
  double dt;

  dt = ComputeDt( factor );

  // ComputeDt returns a negative value if maxF < tol_, signifying a
  // stop condition which is independent of the user-specified stopV
  // parameter.
  if ( dt < 0.0 ) {
    return -1.0;
  }

  if ( deltaPhiValid_ ) {
    return dt;
  }

  FindD0();
  FindDelPlus();
  FindDelMinus();
  FindN();

  InitIter();
  while ( currNode = GetNext() ) {

    // This should be relevant for cvLevelSetSparseGrid only, but in general is
    // not a harmful thing to include at the abstract cvLevelSetStructuredGrid
    // level:
    if ( ! ( currNode->state_ & CV_NODE_COVERED ) &&
	 ! ( currNode->state_ & CV_NODE_ACTIVE ) ) {
      continue;
    }

    // Choose the right entropy-satisfying approximation for use with
    // the constant velocity term F0 in computing the contribution of
    // this term to phi_t:
    maxVal = maximum( currNode->F0_, 0.0 );
    minVal = minimum( currNode->F0_, 0.0 );
    f0Contrib = maxVal * currNode->delPlus_ + minVal * currNode->delMinus_;

    // Put this in on 2/16/00 as part of an attempt to deal with
    // singularities in the distance function:
    /*
    if ( ( fabs(currNode->delPlus_) < tol_ ) &&
	 ( fabs(currNode->delMinus_) < tol_ ) ) {
      f0Contrib = IntSign( currNode->F0_, tol_ ) * relTol_;
    }
    */

    // Use a central difference approximation to the magnitude of the
    // gradient with the curvature-dependent portion of velocity
    // (since curvature dependence corresponds to diffusive flow of
    // information):
    tmp = currNode->d0_[0] * currNode->d0_[0];
    tmp += currNode->d0_[1] * currNode->d0_[1];
    tmp += currNode->d0_[2] * currNode->d0_[2];
    f1MagGradPhi = sqrt( tmp );
    f1Contrib = currNode->F1_ * f1MagGradPhi;

    phi_t = - ( f0Contrib + f1Contrib );

    currNode->velocity_ = f0Contrib + f1Contrib;
    currNode->deltaPhi_ = dt * phi_t;

    // This is a simple but important mechanism by which we can allow
    // singularities to move away from the phi==0 surface.  The
    // problem is that at singularities, where the distance function
    // forms a cusp, curvature values blow up.  In the FindK methods
    // we (arbitrarily) choose oneOverTol_ as the value which
    // represents "blown up" curvatures.  At the same time, values of
    // |grad(phi)| at these singularities will tend towards zero, and
    // we have the situation where computed velocity --> 0, and the
    // cusp will not evolve.  Here, by forcing a bit of evolution at
    // cusps, we allow these cusps to move away from the phi==0
    // surface, where their blown-up curvatures are actually seen
    // during velocity evaluation (cusps away from phi==0 do not
    // contribute large curvatures to computed velocities).

    if ( ( fabs(f1MagGradPhi) < relTol_ ) &&
	 ( currNode->state_ & CV_NODE_ACTIVE ) ) {
      currNode->deltaPhi_ = - IntSign( currNode->phi_, tol_ ) * relTol_;
      currNode->velocity_ = - currNode->deltaPhi_ / dt;
    }

    // Check for CFL violation:
    v = fabs( currNode->F0_ + currNode->F1_ );
    if ( 2*v*dt > minh_ ) {
      if ( fabs( 2*v*dt - minh_ ) > tol_ ) {
	printf( "  CFL violation (nodeId=%d): 2*v*dt = %f\n",
		currNode->index_, 2*v*dt );
	printf("   minH [%f]\t maxF [%f]\t v [%f]\t dt [%f]\n",
	       minh_, GetMaxF(), v, dt );
      }
    }
  }

  deltaPhiValid_ = 1;

  return dt;
}


// ---------
// UpdatePhi
// ---------

int cvLevelSetStructuredGrid::UpdatePhi()
{
  cvLevelSetNode *currNode;
  double dt;

  if ( ! deltaPhiValid_ ) {
    printf("ERR: unexpected call to UpdatePhi\n");
    return CV_ERROR;
  }

  InitIter();
  while ( currNode = GetNext() ) {
    currNode->phi_ += currNode->deltaPhi_;
  }


  /* Old as of 2/19/00:
   * ---

  cvLevelSetNode *currNode;
  double f0Contrib, f1Contrib;
  double maxVal, minVal;
  double tmp, f1MagGradPhi;
  double phi_t;
  double v;
  double dt, maxf;
  double geodesicTerm;

  FindD0();
  FindDelPlus();
  FindDelMinus();
  FindN();

  // The maximum F that we're interested in here is the largest sum of
  // F0_ and F1_.  We then want to find the largest dt which does not
  // violate v * dt <= h, or perhaps v * dt <= h / 2, where v = maxf.
  // Multiplying by factor provides an additional measure of
  // conservatism in satisfying CFL.
  //  maxf = GetMaxF();
  //  dt = factor * h_ / ( 2*maxf );
  //  dt = factor * minh_ / ( 2*maxf );

  dt = ComputeDt( factor );

  InitIter();
  while ( currNode = GetNext() ) {

    // Choose the right entropy-satisfying approximation for use with
    // the constant velocity term F0 in computing the contribution of
    // this term to phi_t:
    maxVal = maximum( currNode->F0_, 0.0 );
    minVal = minimum( currNode->F0_, 0.0 );
    f0Contrib = maxVal * currNode->delPlus_ + minVal * currNode->delMinus_;

    // Put this in on 2/16/00 as part of an attempt to deal with
    // singularities in the distance function:
    if ( ( fabs(currNode->delPlus_) < tol_ ) &&
	 ( fabs(currNode->delMinus_) < tol_ ) ) {
      f0Contrib = IntSign( currNode->F0_, tol_ ) * relTol_;
    }

    // Use a central difference approximation to the magnitude of the
    // gradient with the curvature-dependent portion of velocity
    // (since curvature dependence corresponds to diffusive flow of
    // information):
    tmp = currNode->d0_[0] * currNode->d0_[0];
    tmp += currNode->d0_[1] * currNode->d0_[1];
    tmp += currNode->d0_[2] * currNode->d0_[2];
    f1MagGradPhi = sqrt( tmp );
    f1Contrib = currNode->F1_ * f1MagGradPhi;

    // Also use central diff's with the geodesic term (see Caselles,
    // Kimmel, Sapiro, "Geodesic active contours," Int'l J Computer
    // Vision, 22(1), pp. 61-79, 1997, equation 19).
    // geodesicTerm = currNode->toDot_[0] * currNode->d0_[0] / f1MagGradPhi;
    // geodesicTerm += currNode->toDot_[1] * currNode->d0_[1] / f1MagGradPhi;
    // geodesicTerm += currNode->toDot_[2] * currNode->d0_[2] / f1MagGradPhi;

    // We really want to apply the geodesic term, i.e. the term which
    // is a function of the potential function, in the direction of
    // the front normal.
    geodesicTerm = currNode->toDot_[0] * currNode->n_[0];
    geodesicTerm += currNode->toDot_[1] * currNode->n_[1];
    geodesicTerm += currNode->toDot_[2] * currNode->n_[2];

    // On the other hand, consider the fact that the computed
    // max. allowable time step is a function of max. velocity only.
    // The geodesic term is not currently accounted for in ComputeDt,
    // though this term is a direct part of the time derivative.
    phi_t = - ( f0Contrib + f1Contrib ) + geodesicTerm;

    currNode->phi_ += dt * phi_t;

    // Check for CFL violation:
    v = fabs( currNode->F0_ + currNode->F1_ );
    if ( 2*v*dt > minh_ ) {
      if ( fabs( 2*v*dt - minh_ ) > tol_ ) {
	printf( "  CFL violation (nodeId=%d): 2*v*dt = %f\n",
		currNode->index_, 2*v*dt );
	printf("   minH [%f]\t maxF [%f]\t v [%f]\t dt [%f]\n",
	       minh_, GetMaxF(), v, dt );
      }
    }
  }

  * ---
  */

  d0Valid_ = 0;
  dpiValid_ = 0;
  dmiValid_ = 0;
  delPlusValid_ = 0;
  delMinusValid_ = 0;
  nValid_ = 0;
  kValid_ = 0;
  k3dmValid_ = 0;
  k3dgValid_ = 0;
  phiVtkValid_ = 0;

  return CV_OK;
}


// --------------
// GetAdjacentIxs
// --------------
// I_, J_ and K_ indicate the logical dimensions of the Grid.  Indices
// along I_ vary the fastest, and those along K_ vary the slowest.
// cvLevelSetNode is a generic object used by both cvLevelSetDenseGrid and cvLevelSetSparseGrid,
// which is why this implementation can be inherited.

void cvLevelSetStructuredGrid::GetAdjacentIxs( cvLevelSetNode *n, int ixs[] )
{
  int index, ix, jx, kx;

  index = n->index_;

  if ( n->xPrevIndex_ == index ) {
    ixs[0] = -1;
  } else {
    ixs[0] = n->xPrevIndex_;
  }
  if ( n->xNextIndex_ == index ) {
    ixs[1] = -1;
  } else {
    ixs[1] = n->xNextIndex_;
  }

  if ( n->yPrevIndex_ == index ) {
    ixs[2] = -1;
  } else {
    ixs[2] = n->yPrevIndex_;
  }
  if ( n->yNextIndex_ == index ) {
    ixs[3] = -1;
  } else {
    ixs[3] = n->yNextIndex_;
  }

  if ( n->zPrevIndex_ == index ) {
    ixs[4] = -1;
  } else {
    ixs[4] = n->zPrevIndex_;
  }
  if ( n->zNextIndex_ == index ) {
    ixs[5] = -1;
  } else {
    ixs[5] = n->zNextIndex_;
  }
  return;
}


// --------------
// GetAdjacentIxs
// --------------

void cvLevelSetStructuredGrid::GetAdjacentIxs( int index, int ixs[] )
{
  int ix, jx, kx;

  LogicalIxToIJK( index, &ix, &jx, &kx );

  // x neighbors:
  if ( ! IxWithinExtent( ix-1, jx, kx ) ) {
    ixs[0] = -1;
  } else {
    ixs[0] = IJKToLogicalIx( ix-1, jx, kx );
  }
  if ( ! IxWithinExtent( ix+1, jx, kx ) ) {
    ixs[1] = -1;
  } else {
    ixs[1] = IJKToLogicalIx( ix+1, jx, kx );
  }

  // y neighbors:
  if ( ! IxWithinExtent( ix, jx-1, kx ) ) {
    ixs[2] = -1;
  } else {
    ixs[2] = IJKToLogicalIx( ix, jx-1, kx );
  }
  if ( ! IxWithinExtent( ix, jx+1, kx ) ) {
    ixs[3] = -1;
  } else {
    ixs[3] = IJKToLogicalIx( ix, jx+1, kx );
  }

  // z neighbors:
  if ( ! IxWithinExtent( ix, jx, kx-1 ) ) {
    ixs[4] = -1;
  } else {
    ixs[4] = IJKToLogicalIx( ix, jx, kx-1 );
  }
  if ( ! IxWithinExtent( ix, jx, kx+1 ) ) {
    ixs[5] = -1;
  } else {
    ixs[5] = IJKToLogicalIx( ix, jx, kx+1 );
  }

  return;
}


// ----------------------------
// GetStructuredGridMemoryUsage
// ----------------------------
// Sizes of grid_ and velocity_ are being omitted deliberately.  In
// the case of grid_, derived classes currently use different (and
// specialized) members to keep track of how many cvLevelSetNode's are
// allocated, so we can't access that information here.  In the case
// of velocity_, this is now obsolete (cvLevelSetStructuredGrid's don't use
// cvLevelSetVelocity*'s anymore).

int cvLevelSetStructuredGrid::GetStructuredGridMemoryUsage()
{
  int sz = 0;

  if ( ixBuffer_ ) {
    sz += ixBufferSz_ * sizeof(int);
  }
  if ( nodeSets_ ) {
    sz += nodeSetsSize_ * sizeof(int);
  }
  if ( velocityVectors_ ) {
    sz += velocityVectors_->GetMemoryUsage();
  }
  if ( phiVtk_ ) {
    sz += phiVtk_->GetActualMemorySize() * 1024;  // vtk returns kB
  }

  return sz;
}


// ------
// FindD0
// ------
// Note that there's no validity protection against redundant
// computation in any of the sub-methods called here... that's why
// those are all private.

void cvLevelSetStructuredGrid::FindD0()
{
  if (!d0Valid_) {
    FindD0i();
    FindD0xi();
    FindD0yi();
    FindD0zi();
    d0Valid_ = 1;
  }
  return;
}


// -------
// FindD0i
// -------
// i.e. Find D0x, D0y, D0z.

void cvLevelSetStructuredGrid::FindD0i()
{
  cvLevelSetNode *currNode, *prevNode, *nextNode;
  double prev, next;
  double twoHx, twoHy, twoHz;

  twoHx = 2 * hv_[0];
  twoHy = 2 * hv_[1];
  twoHz = 2 * hv_[2];

  InitIter();
  while ( currNode = GetNext() ) {

    // First find centered x difference:
    prevNode = &(grid_[currNode->xPrevIndex_]);
    nextNode = &(grid_[currNode->xNextIndex_]);
    prev = prevNode->phi_;
    next = nextNode->phi_;
    currNode->d0_[0] = (next - prev) / twoHx;

    // Now find centered y difference:
    prevNode = &(grid_[currNode->yPrevIndex_]);
    nextNode = &(grid_[currNode->yNextIndex_]);
    prev = prevNode->phi_;
    next = nextNode->phi_;
    currNode->d0_[1] = (next - prev) / twoHy;

    // Finally, find centered z difference:
    prevNode = &(grid_[currNode->zPrevIndex_]);
    nextNode = &(grid_[currNode->zNextIndex_]);
    prev = prevNode->phi_;
    next = nextNode->phi_;
    currNode->d0_[2] = (next - prev) / twoHz;
  }

  return;
}


// --------
// FindD0xi
// --------
// i.e. Find D0xx, D0xy, D0xz.

void cvLevelSetStructuredGrid::FindD0xi()
{
  cvLevelSetNode *currNode, *prevNode, *nextNode;
  double prev, next;
  double twoHx, twoHy, twoHz;

  twoHx = 2 * hv_[0];
  twoHy = 2 * hv_[1];
  twoHz = 2 * hv_[2];

  InitIter();
  while ( currNode = GetNext() ) {

    // D0xx:
    prevNode = &(grid_[currNode->xPrevIndex_]);
    nextNode = &(grid_[currNode->xNextIndex_]);
    prev = prevNode->d0_[0];
    next = nextNode->d0_[0];
    currNode->d0x_[0] = (next - prev) / twoHx;

    // D0xy:
    prevNode = &(grid_[currNode->yPrevIndex_]);
    nextNode = &(grid_[currNode->yNextIndex_]);
    prev = prevNode->d0_[0];
    next = nextNode->d0_[0];
    currNode->d0x_[1] = (next - prev) / twoHy;

    // D0xz:
    prevNode = &(grid_[currNode->zPrevIndex_]);
    nextNode = &(grid_[currNode->zNextIndex_]);
    prev = prevNode->d0_[0];
    next = nextNode->d0_[0];
    currNode->d0x_[2] = (next - prev) / twoHz;
  }

  return;
}


// --------
// FindD0yi
// --------
// i.e. Find D0yx, D0yy, D0yz.

void cvLevelSetStructuredGrid::FindD0yi()
{
  cvLevelSetNode *currNode, *prevNode, *nextNode;
  double prev, next;
  double twoHx, twoHy, twoHz;

  twoHx = 2 * hv_[0];
  twoHy = 2 * hv_[1];
  twoHz = 2 * hv_[2];

  InitIter();
  while ( currNode = GetNext() ) {

    // D0yx:
    prevNode = &(grid_[currNode->xPrevIndex_]);
    nextNode = &(grid_[currNode->xNextIndex_]);
    prev = prevNode->d0_[1];
    next = nextNode->d0_[1];
    currNode->d0y_[0] = (next - prev) / twoHx;

    // D0yy:
    prevNode = &(grid_[currNode->yPrevIndex_]);
    nextNode = &(grid_[currNode->yNextIndex_]);
    prev = prevNode->d0_[1];
    next = nextNode->d0_[1];
    currNode->d0y_[1] = (next - prev) / twoHy;

    // D0yz:
    prevNode = &(grid_[currNode->zPrevIndex_]);
    nextNode = &(grid_[currNode->zNextIndex_]);
    prev = prevNode->d0_[1];
    next = nextNode->d0_[1];
    currNode->d0y_[2] = (next - prev) / twoHz;
  }

  return;
}


// --------
// FindD0zi
// --------
// i.e. Find D0zx, D0zy, D0zz.

void cvLevelSetStructuredGrid::FindD0zi()
{
  cvLevelSetNode *currNode, *prevNode, *nextNode;
  double prev, next;
  double twoHx, twoHy, twoHz;

  twoHx = 2 * hv_[0];
  twoHy = 2 * hv_[1];
  twoHz = 2 * hv_[2];

  InitIter();
  while ( currNode = GetNext() ) {

    // D0zx:
    prevNode = &(grid_[currNode->xPrevIndex_]);
    nextNode = &(grid_[currNode->xNextIndex_]);
    prev = prevNode->d0_[2];
    next = nextNode->d0_[2];
    currNode->d0z_[0] = (next - prev) / twoHx;

    // D0zy:
    prevNode = &(grid_[currNode->yPrevIndex_]);
    nextNode = &(grid_[currNode->yNextIndex_]);
    prev = prevNode->d0_[2];
    next = nextNode->d0_[2];
    currNode->d0z_[1] = (next - prev) / twoHy;

    // D0zz:
    prevNode = &(grid_[currNode->zPrevIndex_]);
    nextNode = &(grid_[currNode->zNextIndex_]);
    prev = prevNode->d0_[2];
    next = nextNode->d0_[2];
    currNode->d0z_[2] = (next - prev) / twoHz;
  }

  return;
}


// -------
// FindDpi
// -------
// i.e. Find D+x, D+y, D+z.

void cvLevelSetStructuredGrid::FindDpi()
{
  cvLevelSetNode *currNode, *nextNode;
  double curr, next;

  if (!dpiValid_) {

    InitIter();
    while ( currNode = GetNext() ) {

      curr = currNode->phi_;

      // Dpx:
      nextNode = &(grid_[currNode->xNextIndex_]);
      next = nextNode->phi_;
      currNode->dp_[0] = (next - curr) / hv_[0];

      // Dpy:
      nextNode = &(grid_[currNode->yNextIndex_]);
      next = nextNode->phi_;
      currNode->dp_[1] = (next - curr) / hv_[1];

      // Dpz:
      nextNode = &(grid_[currNode->zNextIndex_]);
      next = nextNode->phi_;
      currNode->dp_[2] = (next - curr) / hv_[2];
    }

    dpiValid_ = 1;
  }

  return;
}


// -------
// FindDmi
// -------
// i.e. Find D-x, D-y, D-z.

void cvLevelSetStructuredGrid::FindDmi()
{
  cvLevelSetNode *currNode, *prevNode;
  double curr, prev;

  if (!dmiValid_) {

    InitIter();
    while ( currNode = GetNext() ) {

      curr = currNode->phi_;

      // Dmx:
      prevNode = &(grid_[currNode->xPrevIndex_]);
      prev = prevNode->phi_;
      currNode->dm_[0] = (curr - prev) / hv_[0];

      // Dmy:
      prevNode = &(grid_[currNode->yPrevIndex_]);
      prev = prevNode->phi_;
      currNode->dm_[1] = (curr - prev) / hv_[1];

      // Dmz:
      prevNode = &(grid_[currNode->zPrevIndex_]);
      prev = prevNode->phi_;
      currNode->dm_[2] = (curr - prev) / hv_[2];
    }

    dmiValid_ = 1;
  }

  return;
}


// -----------
// FindDelPlus
// -----------

void cvLevelSetStructuredGrid::FindDelPlus()
{
  cvLevelSetNode *currNode;
  double tmp, acc;

  if (!delPlusValid_) {
    FindDpi();
    FindDmi();
    InitIter();
    while ( currNode = GetNext() ) {
      acc = 0.0;
      tmp = maximum( currNode->dm_[0], 0.0 );
      acc += tmp * tmp;
      tmp = minimum( currNode->dp_[0], 0.0 );
      acc += tmp * tmp;
      tmp = maximum( currNode->dm_[1], 0.0 );
      acc += tmp * tmp;
      tmp = minimum( currNode->dp_[1], 0.0 );
      acc += tmp * tmp;
      tmp = maximum( currNode->dm_[2], 0.0 );
      acc += tmp * tmp;
      tmp = minimum( currNode->dp_[2], 0.0 );
      acc += tmp * tmp;
      currNode->delPlus_ = sqrt( acc );
    }
    delPlusValid_ = 1;
  }
  return;
}


// ------------
// FindDelMinus
// ------------

void cvLevelSetStructuredGrid::FindDelMinus()
{
  cvLevelSetNode *currNode;
  double tmp, acc;

  if (!delMinusValid_) {
    FindDpi();
    FindDmi();
    InitIter();
    while ( currNode = GetNext() ) {
      acc = 0.0;
      tmp = maximum( currNode->dp_[0], 0.0 );
      acc += tmp * tmp;
      tmp = minimum( currNode->dm_[0], 0.0 );
      acc += tmp * tmp;
      tmp = maximum( currNode->dp_[1], 0.0 );
      acc += tmp * tmp;
      tmp = minimum( currNode->dm_[1], 0.0 );
      acc += tmp * tmp;
      tmp = maximum( currNode->dp_[2], 0.0 );
      acc += tmp * tmp;
      tmp = minimum( currNode->dm_[2], 0.0 );
      acc += tmp * tmp;
      currNode->delMinus_ = sqrt( acc );
    }
    delMinusValid_ = 1;
  }
  return;
}


// -----
// FindK
// -----

void cvLevelSetStructuredGrid::FindK()
{
  cvLevelSetNode *currNode;

  if ( ! kValid_ ) {
    if ( dim_ == 2 ) {
      FindK2d();
    } else {
      FindK3dm();
      FindK3dg();
      InitIter();
      while ( currNode = GetNext() ) {
	currNode->K_ = currNode->K3dm_;
      }
    }
    kValid_ = 1;
  }

  return;
}


// -------
// FindK2d
// -------
// See Sethian eq. (5.32) 1st ed., which is eq. (6.35) in the 2nd ed.

void cvLevelSetStructuredGrid::FindK2d()
{
  int i;
  cvLevelSetNode *currNode;
  double num, den;
  double k;

  if ( ! kValid_ ) {

    FindD0();

    InitIter();
    while ( currNode = GetNext() ) {

      num = currNode->d0x_[0] * currNode->d0_[1] * currNode->d0_[1];
      num -= 2 * currNode->d0_[1] * currNode->d0_[0] * currNode->d0x_[1];
      num += currNode->d0y_[1] * currNode->d0_[0] * currNode->d0_[0];
      den = currNode->d0_[0] * currNode->d0_[0];
      den += currNode->d0_[1] * currNode->d0_[1];
      den = pow( sqrt(den), 3 );
      if ( fabs(den) < tol_ ) {
	if ( fabs(num) < tol_ ) {
	  currNode->K_ = 1.0;
	} else {
	  currNode->K_ = oneOverTol_;
	}
      } else {
	k = num / den;
	currNode->K_ = k;
      }
    }

    kValid_ = 1;
  }
}


// --------
// FindK3dm
// --------
// See Sethian eq. (6.36) 2nd ed.
// Note that, at least for spherical test cases, Km computed with
// eq. (6.36) seems to give values 2x expected mean curvature.

void cvLevelSetStructuredGrid::FindK3dm()
{
  int i;
  cvLevelSetNode *currNode;
  double num, den;
  double *d0, *d0x, *d0y, *d0z;

  if ( ! k3dmValid_ ) {
    FindD0();
    InitIter();
    while ( currNode = GetNext() ) {

      d0 = currNode->d0_;
      d0x = currNode->d0x_;
      d0y = currNode->d0y_;
      d0z = currNode->d0z_;

      num = ( d0y[1] + d0z[2] ) * d0[0] * d0[0];
      num += ( d0x[0] + d0z[2] ) * d0[1] * d0[1];
      num += ( d0x[0] + d0y[1] ) * d0[2] * d0[2];
      num -= 2 * d0[0] * d0[1] * d0x[1];
      num -= 2 * d0[0] * d0[2] * d0x[2];
      num -= 2 * d0[1] * d0[2] * d0y[2];

      den = d0[0] * d0[0];
      den += d0[1] * d0[1];
      den += d0[2] * d0[2];
      den = pow( sqrt(den), 3 );

      if ( fabs(den) < tol_ ) {
	if ( fabs(num) < tol_ ) {
	  currNode->K3dm_ = 1.0;
	} else {
	  currNode->K3dm_ = oneOverTol_;
	}
      } else {
	currNode->K3dm_ = num / den;
	currNode->K3dm_ /= 2;
      }
    }

    k3dmValid_ = 1;
  }
}


// --------
// FindK3dg
// --------
// See Sethian eq. (6.37) 2nd ed.  Note the typo (missing square
// brace)...

void cvLevelSetStructuredGrid::FindK3dg()
{
  int i;
  cvLevelSetNode *currNode;
  double num, den;
  double *d0, *d0x, *d0y, *d0z;

  if ( ! k3dgValid_ ) {
    FindD0();
    InitIter();
    while ( currNode = GetNext() ) {

      d0 = currNode->d0_;
      d0x = currNode->d0x_;
      d0y = currNode->d0y_;
      d0z = currNode->d0z_;

      num = d0[0] * d0[0] * ( d0y[1] * d0z[2] - d0y[2] * d0y[2] );
      num += d0[1] * d0[1] * ( d0x[0] * d0z[2] - d0x[2] * d0x[2] );
      num += d0[2] * d0[2] * ( d0x[0] * d0y[1] - d0x[1] * d0x[1] );
      num += 2 * ( d0[0] * d0[1] * ( d0x[2] * d0y[2] - d0x[1] * d0z[2] )
		   + d0[1] * d0[2] * ( d0x[1] * d0x[2] - d0y[2] * d0x[0] )
		   + d0[0] * d0[2] * ( d0x[1] * d0y[2] - d0x[2] * d0y[1] ) );

      den = d0[0] * d0[0];
      den += d0[1] * d0[1];
      den += d0[2] * d0[2];
      den = den * den;

      if ( fabs(den) < tol_ ) {
	if ( fabs(num) < tol_ ) {
	  currNode->K3dg_ = 1.0;
	} else {
	  currNode->K3dg_ = oneOverTol_;
	}
      } else {
	currNode->K3dg_ = num / den;
      }
    }

    k3dgValid_ = 1;
  }
}


// -----
// FindN
// -----
// See Sethian eq. (5.36).  Adding 2D / 3D switches.  In 3D, consider
// each of the following combinations:
//
//   (+,+,+) (-,+,+) (+,-,+) (-,-,+)
//   (+,+,-) (-,+,-) (+,-,-) (-,-,-)
//
// where +/- refer to forward / backward differences, respectively.

void cvLevelSetStructuredGrid::FindN()
{
  int i;
  cvLevelSetNode *currNode;
  double dpx, dmx, dpy, dmy, dpz, dmz;
  double den1, den2, den3, den4;
  double den5, den6, den7, den8;
  double nx, ny, nz;

  if ( ! nValid_ ) {

    FindDpi();
    FindDmi();

    InitIter();
    while ( currNode = GetNext() ) {

      dpx = currNode->dp_[0];
      dmx = currNode->dm_[0];
      dpy = currNode->dp_[1];
      dmy = currNode->dm_[1];
      dpz = currNode->dp_[2];
      dmz = currNode->dm_[2];

      if ( dim_ == 2 ) {
	den1 = sqrt( sqr(dpx) + sqr(dpy) );
	den2 = sqrt( sqr(dmx) + sqr(dpy) );
	den3 = sqrt( sqr(dpx) + sqr(dmy) );
	den4 = sqrt( sqr(dmx) + sqr(dmy) );
      } else {
	den1 = sqrt( sqr(dpx) + sqr(dpy) + sqr(dpz) );
	den2 = sqrt( sqr(dmx) + sqr(dpy) + sqr(dpz) );
	den3 = sqrt( sqr(dpx) + sqr(dmy) + sqr(dpz) );
	den4 = sqrt( sqr(dmx) + sqr(dmy) + sqr(dpz) );
	den5 = sqrt( sqr(dpx) + sqr(dpy) + sqr(dmz) );
	den6 = sqrt( sqr(dmx) + sqr(dpy) + sqr(dmz) );
	den7 = sqrt( sqr(dpx) + sqr(dmy) + sqr(dmz) );
	den8 = sqrt( sqr(dmx) + sqr(dmy) + sqr(dmz) );
      }

      nx = 0.0;
      ny = 0.0;
      nz = 0.0;

      if ( dim_ == 2 ) {
	if (den1 > 0.0) {
	  nx += dpx / den1;
	  ny += dpy / den1;
	}
	if (den2 > 0.0) {
	  nx += dmx / den2;
	  ny += dpy / den2;
	}
	if (den3 > 0.0) {
	  nx += dpx / den3;
	  ny += dmy / den3;
	}
	if (den4 > 0.0) {
	  nx += dmx / den4;
	  ny += dmy / den4;
	}
      } else {
	if (den1 > 0.0) {
	  nx += dpx / den1;
	  ny += dpy / den1;
	  nz += dpz / den1;
	}
	if (den2 > 0.0) {
	  nx += dmx / den2;
	  ny += dpy / den2;
	  nz += dpz / den2;
	}
	if (den3 > 0.0) {
	  nx += dpx / den3;
	  ny += dmy / den3;
	  nz += dpz / den3;
	}
	if (den4 > 0.0) {
	  nx += dmx / den4;
	  ny += dmy / den4;
	  nz += dpz / den4;
	}
	if (den5 > 0.0) {
	  nx += dpx / den5;
	  ny += dpy / den5;
	  nz += dmz / den5;
	}
	if (den6 > 0.0) {
	  nx += dmx / den6;
	  ny += dpy / den6;
	  nz += dmz / den6;
	}
	if (den7 > 0.0) {
	  nx += dpx / den7;
	  ny += dmy / den7;
	  nz += dmz / den7;
	}
	if (den8 > 0.0) {
	  nx += dmx / den8;
	  ny += dmy / den8;
	  nz += dmz / den8;
	}
      }

      currNode->nn_[0] = nx;
      currNode->nn_[1] = ny;
      currNode->nn_[2] = nz;

      NormVector( &nx, &ny, &nz );
      currNode->n_[0] = nx;
      currNode->n_[1] = ny;
      currNode->n_[2] = nz;
    }

    nValid_ = 1;
  }
}


// -----------
// GetAnchorIx
// -----------
// Grid has its origin at origin_[].
// The "anchor index" is the index (i.e. in 0 .. numNodes_) of the
// cvLevelSetNode which is to the lower-left of the given pos.  That is, the
// pos_ of the resulting cvLevelSetNode is less than or equal to the given pos
// in all directions.

int cvLevelSetStructuredGrid::GetAnchorIx( double pos[], int *ix )
{
  double pp[3];
  int logicalIxs[3];
  int i;
  double tol;

  tol = relTol_ * minh_;
  for (i = 0; i < 3; i++) {
    pp[i] = pos[i] - origin_[i] + tol;
    logicalIxs[i] = (int)floor( pp[i] / hv_[i] );
  }

  *ix = IJKToIndex( logicalIxs[0], logicalIxs[1], logicalIxs[2] );

  return CV_OK;
}


// --------
// FindEdge
// --------
// Find the pair of neighboring nodes which form the edge on which the
// given position lies.  Return those nodes in a and b, along with a
// cartesian direction flag in dir.
//
// This method is called by both InterpK and InterpN.

int cvLevelSetStructuredGrid::FindEdge( double pos[], cvLevelSetNode **a, cvLevelSetNode **b, char *dir )
{
  int anchorIx, nextIx, i;
  int adjIxs[6];
  cvLevelSetNode *anchor;
  double tol;

  *a = NULL;
  *b = NULL;

  if ( GetAnchorIx( pos, &anchorIx ) != CV_OK ) {
    return CV_ERROR;
  }

  if ( anchorIx < 0 ) {
    return CV_ERROR;
  }

  tol = relTol_ * minh_;
  anchor = &( grid_[anchorIx] );
  GetAdjacentIxs( anchor, adjIxs );
  for (i = 0; i < 3; i++) {

    // If pos is out along an edge, then we've found the target cvLevelSetNode's:
    if ( fabs( pos[i] - anchor->pos_[i] ) > tol ) {
      switch (i) {
      case 0:
	*dir = 'x';
	break;
      case 1:
	*dir = 'y';
	break;
      case 2:
	*dir = 'z';
	break;
      default:
	*a = NULL;
	*b = NULL;
	return CV_ERROR;
      }
      *a = anchor;
      nextIx = adjIxs[2*i + 1];
      *b = &( grid_[nextIx] );
      return CV_OK;
    }
  }

  // If pos did not lie towards any of the "next" nodes (i.e. next in
  // x-dir, y-dir, z-dir), then pos must lie ~on the anchor node:
  *a = anchor;
  *b = NULL;

  return CV_OK;
}


// ---------
// InterpZLS
// ---------

int cvLevelSetStructuredGrid::InterpZLS( cvLevelSetNode *a, cvLevelSetNode *b, char code, double zls[] )
{
  double phia, phib;
  double tol;

  tol = relTol_ * minh_;
  phia = a->phi_;

  if ( a == NULL ) return CV_ERROR;

  if ( b == NULL ) {

    // If b is NULL, then phi at a is expected to be ~0:
    if ( fabs(phia) >= tol ) {
      return CV_ERROR;
    }

    // ZLS passes through node a:
    zls[0] = a->pos_[0];
    zls[1] = a->pos_[1];
    zls[2] = a->pos_[2];

  } else {
    // Zero level set lies between a and b:
    phib = b->phi_;

    // Check that phia and phib have opposite signs:
    if ( (phia*phib) >= 0.0 ) {
      return CV_ERROR;
    }

    switch (code) {
    case 'x':
      LinInterp1D_dbl( phia, phib, a->pos_[0], b->pos_[0], 0.0, &(zls[0]) );
      zls[1] = a->pos_[1];
      zls[2] = a->pos_[2];
      break;
    case 'y':
      zls[0] = a->pos_[0];
      LinInterp1D_dbl( phia, phib, a->pos_[1], b->pos_[1], 0.0, &(zls[1]) );
      zls[2] = a->pos_[2];
      break;
    case 'z':
      zls[0] = a->pos_[0];
      zls[1] = a->pos_[1];
      LinInterp1D_dbl( phia, phib, a->pos_[2], b->pos_[2], 0.0, &(zls[2]) );
      break;
    default:
      return CV_ERROR;
    }
  }

  return CV_OK;
}


// -------
// InterpK
// -------
// Things are much simplified when we recognize that queries will be
// at positions on Grid edges.

int cvLevelSetStructuredGrid::InterpK( double pos[], double *k )
{
  cvLevelSetNode *a, *b;
  char dir;

  if ( FindEdge( pos, &a, &b, &dir ) != CV_OK ) {
    return CV_ERROR;
  }
  if ( a == NULL ) {
    return CV_ERROR;
  }
  if ( b == NULL ) {
    *k = a->K_;
    return CV_OK;
  }
  switch (dir) {
  case 'x':
    LinInterp1D_dbl( a->pos_[0], b->pos_[0], a->K_, b->K_, pos[0], k );
    break;
  case 'y':
    LinInterp1D_dbl( a->pos_[1], b->pos_[1], a->K_, b->K_, pos[1], k );
    break;
  case 'z':
    LinInterp1D_dbl( a->pos_[2], b->pos_[2], a->K_, b->K_, pos[2], k );
    break;
  default:
    return CV_ERROR;
  }
  return CV_OK;
}


// --------
// InterpKm
// --------

int cvLevelSetStructuredGrid::InterpKm( double pos[], double *k )
{
  cvLevelSetNode *a, *b;
  char dir;

  if ( FindEdge( pos, &a, &b, &dir ) != CV_OK ) {
    return CV_ERROR;
  }
  if ( a == NULL ) {
    return CV_ERROR;
  }
  if ( b == NULL ) {
    *k = a->K3dm_;
    return CV_OK;
  }
  switch (dir) {
  case 'x':
    LinInterp1D_dbl( a->pos_[0], b->pos_[0], a->K3dm_, b->K3dm_, pos[0], k );
    break;
  case 'y':
    LinInterp1D_dbl( a->pos_[1], b->pos_[1], a->K3dm_, b->K3dm_, pos[1], k );
    break;
  case 'z':
    LinInterp1D_dbl( a->pos_[2], b->pos_[2], a->K3dm_, b->K3dm_, pos[2], k );
    break;
  default:
    return CV_ERROR;
  }
  return CV_OK;
}


// --------
// InterpKg
// --------

int cvLevelSetStructuredGrid::InterpKg( double pos[], double *k )
{
  cvLevelSetNode *a, *b;
  char dir;

  if ( FindEdge( pos, &a, &b, &dir ) != CV_OK ) {
    return CV_ERROR;
  }
  if ( a == NULL ) {
    return CV_ERROR;
  }
  if ( b == NULL ) {
    *k = a->K3dg_;
    return CV_OK;
  }
  switch (dir) {
  case 'x':
    LinInterp1D_dbl( a->pos_[0], b->pos_[0], a->K3dg_, b->K3dg_, pos[0], k );
    break;
  case 'y':
    LinInterp1D_dbl( a->pos_[1], b->pos_[1], a->K3dg_, b->K3dg_, pos[1], k );
    break;
  case 'z':
    LinInterp1D_dbl( a->pos_[2], b->pos_[2], a->K3dg_, b->K3dg_, pos[2], k );
    break;
  default:
    return CV_ERROR;
  }
  return CV_OK;
}


// -------
// InterpN
// -------

int cvLevelSetStructuredGrid::InterpN( double pos[], double n[] )
{
  cvLevelSetNode *a, *b;
  char dir;

  FindEdge( pos, &a, &b, &dir );
  if ( a == NULL ) {
    return CV_ERROR;
  }
  if ( b == NULL ) {
    n[0] = a->n_[0];
    n[1] = a->n_[1];
    n[2] = a->n_[2];
    return CV_OK;
  }
  switch (dir) {
  case 'x':
    LinInterp1D_dbl( a->pos_[0], b->pos_[0], a->n_[0], b->n_[0],
		     pos[0], &(n[0]) );
    LinInterp1D_dbl( a->pos_[0], b->pos_[0], a->n_[1], b->n_[1],
		     pos[0], &(n[1]) );
    LinInterp1D_dbl( a->pos_[0], b->pos_[0], a->n_[2], b->n_[2],
		     pos[0], &(n[2]) );
    break;
  case 'y':
    LinInterp1D_dbl( a->pos_[1], b->pos_[1], a->n_[0], b->n_[0],
		     pos[1], &(n[0]) );
    LinInterp1D_dbl( a->pos_[1], b->pos_[1], a->n_[1], b->n_[1],
		     pos[1], &(n[1]) );
    LinInterp1D_dbl( a->pos_[1], b->pos_[1], a->n_[2], b->n_[2],
		     pos[1], &(n[2]) );
    break;
  case 'z':
    LinInterp1D_dbl( a->pos_[2], b->pos_[2], a->n_[0], b->n_[0],
		     pos[2], &(n[0]) );
    LinInterp1D_dbl( a->pos_[2], b->pos_[2], a->n_[1], b->n_[1],
		     pos[2], &(n[1]) );
    LinInterp1D_dbl( a->pos_[2], b->pos_[2], a->n_[2], b->n_[2],
		     pos[2], &(n[2]) );
    break;
  default:
    return CV_ERROR;
  }
  return CV_OK;
}


// ------------------
// GetVelocityVectors
// ------------------
// This method makes use of the fact that vtkPolyData can be used to
// store arbitrary collections of geometry, topology and data.  Here
// we specify unstructured points (i.e. geometry with no topology) and
// associate a vector data set (i.e. normals) with that point set.

// How do we associate a data set name with the attribute that we add
// to a vtkPolyData?  The book, in describing the file format (p.601)
// indicates that a name can be associated with each attribute block
// so that multiple blocks of the same type can be used in a single
// file.  Does this not apply to the in-core representation?

cvPolyData *cvLevelSetStructuredGrid::GetVelocityVectors()
{
  cvPolyData *result;

  if ( velocityVectors_ == NULL ) {
    return NULL;
  }
  result = new cvPolyData( velocityVectors_ );
  return result;
}


// --------
// GetFront
// --------

cvPolyData *cvLevelSetStructuredGrid::GetFront( int closed )
{
  vtkPolyData *geometry;
  cvPolyData *front;

  MakePhiVtk( closed );

  // Get zero-valued contour.  SetValue(0,0.0) sets the zero'th
  // contour value to 0.0.
  vtkContourFilter *contour = vtkContourFilter::New();
  contour->SetInputDataObject( phiVtk_ );
  contour->SetValue( 0, 0.0 );
  contour->Update();

  geometry = contour->GetOutput();

  // cvPolyData constructor Register's new ref to vtkPolyData.
  front = new cvPolyData( geometry );
  contour->Delete();

  // Note that I don't think we want to do geometry->Delete() because
  // geometry was not created by calling a vtk New() method, nor was
  // it Register'ed.  As a result, no ref counts were incremented for
  // geometry, and it's just a pointer.

  return front;
}


// --------------
// GetActiveNodes
// --------------

cvPolyData *cvLevelSetStructuredGrid::GetActiveNodes()
{
  cvPolyData **nodeSets;
  int numSets;
  cvPolyData *result;
  int i;

  SaveActiveNodes();
  if ( GetNodeSets( &nodeSets, &numSets ) != CV_OK ) {
    return NULL;
  }

  if ( numSets != 1 ) {
    for ( i = 0; i < numSets; i++ ) {
      delete nodeSets[i];
      delete [] nodeSets;
    }
    return NULL;
  }

  result = nodeSets[0];
  delete [] nodeSets;

  return result;
}


// ---------------
// GetBoundaryData
// ---------------
// This method performs a zero level set extraction, then interpolates
// nodal data onto the zero level set for any of the data fields
// described by GridScalarT (defined in cvLevelSetNode.h).  Note that extraction
// of the zero level set first enables us to return a cvPolyData which
// includes the topology of the boundary.  This is being motivated by
// the need to colormap scalars on a level set surface.  As such, it
// may not be as useful for vector data (would we ever want to
// colormap vectors somehow?).  However, you might imagine additional
// mechanisms being added to this method so that non-scalar fields
// could be dealt with in the same (more general) way.

cvPolyData *cvLevelSetStructuredGrid::GetBoundaryData( GridScalarT t )
{
  int numPts, i;
  cvPolyData *zls;
  vtkFloatingPointType *ptf;
  double pt[3];
  vtkDataArray *data;
  double datumA, datumB, datum;
  cvLevelSetNode *a = NULL;
  cvLevelSetNode *b = NULL;
  char dir;

  switch (t) {
  case GS_PHI:
    break;
  case GS_CURVATURE:
    FindK();
    break;
  case GS_CURVATURE_3Dm:
    FindK3dm();
    break;
  case GS_CURVATURE_3Dg:
    FindK3dg();
    break;
  case GS_CURVATURE_3Dk1:
  case GS_CURVATURE_3Dk2:
    FindK3dm();
    FindK3dg();
    break;
  default:
    return NULL;
    break;
  }

  zls = GetFront();
  numPts = zls->GetVtkPolyData()->GetNumberOfPoints();
  data = vtkFloatingPointArrayType::New();

  for (i = 0; i < numPts; i++) {
    ptf = zls->GetVtkPolyData()->GetPoint(i);
    pt[0] = ptf[0];
    pt[1] = ptf[1];
    pt[2] = ptf[2];
    if ( ( FindEdge( pt, &a, &b, &dir ) != CV_OK ) ||
	 ( a == NULL ) ) {
      data->Delete();
      delete zls;
      return NULL;
    }
    if ( b == NULL ) {
      datum = a->GetDoubleDatum( t, tol_ );
      data->InsertTuple1( i, (vtkFloatingPointType)datum );
      continue;
    }
    switch (dir) {
    case 'x':
      LinInterp1D_dbl( a->pos_[0], b->pos_[0],
		       a->GetDoubleDatum( t, tol_ ),
		       b->GetDoubleDatum( t, tol_ ),
		       pt[0], &datum );
      break;
    case 'y':
      LinInterp1D_dbl( a->pos_[1], b->pos_[1],
		       a->GetDoubleDatum( t, tol_ ),
		       b->GetDoubleDatum( t, tol_ ),
		       pt[1], &datum );
      break;
    case 'z':
      LinInterp1D_dbl( a->pos_[2], b->pos_[2],
		       a->GetDoubleDatum( t, tol_ ),
		       b->GetDoubleDatum( t, tol_ ),
		       pt[2], &datum );
      break;
    default:
      data->Delete();
      delete zls;
      return NULL;
    }
    data->InsertTuple1( i, (vtkFloatingPointType)datum );
  }

  zls->GetVtkPolyData()->GetPointData()->SetScalars( data );
  data->Delete();
  return zls;
}


// ---------------
// SaveActiveNodes
// ---------------

int cvLevelSetStructuredGrid::SaveActiveNodes()
{
  cvLevelSetNode *currNode;

  ResetNodeSets();
  InitIter();
  while ( currNode = GetNext() ) {
    if ( currNode->state_ & CV_NODE_ACTIVE ) {
      nodeSets_[nodeSetsPos_] = currNode->index_;
      nodeSetsPos_++;
    }
  }
  pSetsValid_ = 0;
  return CV_OK;
}


// --------------
// NumActiveNodes
// --------------

int cvLevelSetStructuredGrid::NumActiveNodes()
{
  cvLevelSetNode *currNode;
  int num = 0;

  InitIter();
  while ( currNode = GetNext() ) {
    if ( currNode->state_ & CV_NODE_ACTIVE ) {
      num++;
    }
  }
  return num;
}


// ----------------
// SaveCoveredNodes
// ----------------

int cvLevelSetStructuredGrid::SaveCoveredNodes()
{
  cvLevelSetNode *currNode;

  ResetNodeSets();
  InitIter();
  while ( currNode = GetNext() ) {
    if ( currNode->state_ & CV_NODE_COVERED ) {
      nodeSets_[nodeSetsPos_] = currNode->index_;
      nodeSetsPos_++;
    }
  }
  pSetsValid_ = 0;
  return CV_OK;
}


// ------------------
// SaveUncoveredNodes
// ------------------

int cvLevelSetStructuredGrid::SaveUncoveredNodes()
{
  cvLevelSetNode *currNode;

  ResetNodeSets();
  InitIter();
  while ( currNode = GetNext() ) {
    if ( ( ! ( currNode->state_ & CV_NODE_COVERED ) ) &&
	 ( ! ( currNode->state_ & CV_NODE_ACTIVE ) ) ) {
      nodeSets_[nodeSetsPos_] = currNode->index_;
      nodeSetsPos_++;
    }
  }
  pSetsValid_ = 0;
  return CV_OK;
}


// ------------------
// SaveForceMinVNodes
// ------------------

int cvLevelSetStructuredGrid::SaveForceMinVNodes()
{
  cvLevelSetNode *currNode;

  ResetNodeSets();
  InitIter();
  while ( currNode = GetNext() ) {
    if ( currNode->state_ & CV_NODE_FORCE_MINV ) {
      nodeSets_[nodeSetsPos_] = currNode->index_;
      nodeSetsPos_++;
    }
  }
  pSetsValid_ = 0;
  return CV_OK;
}


// -------------
// SaveMineNodes
// -------------

int cvLevelSetStructuredGrid::SaveMineNodes()
{
  cvLevelSetNode *currNode;

  ResetNodeSets();
  InitIter();
  while ( currNode = GetNext() ) {
    if ( currNode->state_ & CV_NODE_MINE ) {
      nodeSets_[nodeSetsPos_] = currNode->index_;
      nodeSetsPos_++;
    }
  }
  pSetsValid_ = 0;
  return CV_OK;
}


// ------------------
// CreateGridPolyData
// ------------------

cvPolyData *cvLevelSetStructuredGrid::CreateGridPolyData()
{
  printf("ERR: cvLevelSetStructuredGrid's do not in general generate cvPolyData's.\n");
  printf("  Only objects of certain derived classes (e.g. cvLevelSetSparseGrid) do.\n");
  return NULL;
}


// ---------
// ComputeDt
// ---------

double cvLevelSetStructuredGrid::ComputeDt( double cflFactor )
{
  double maxf, dt;

  maxf = GetMaxF();
  dt = ComputeDt( maxf, cflFactor );
  return dt;
}


// ---------
// ComputeDt
// ---------

double cvLevelSetStructuredGrid::ComputeDt( double maxf, double cflFactor )
{
  double dt;

  // This is not the right way to deal with this, but...
  if ( fabs( maxf ) < tol_ ) {
    return -1.0;
  }

  dt = cflFactor * minh_ / ( 2 * maxf );
  return dt;
}


// -------
// GetMaxF
// -------
// Check only active nodes (i.e. those nodes which were involved
// during the EvaluateV phase) for velocity magnitudes.

double cvLevelSetStructuredGrid::GetMaxF()
{
  cvLevelSetNode *currNode;
  double currF;
  double currMaxF = 0.0;

  InitIter();
  while ( currNode = GetNext() ) {
    if ( ! ( currNode->state_ & CV_NODE_ACTIVE ) ) {
      continue;
    }
    currF = fabs( currNode->F0_ + currNode->F1_ );
    currMaxF = maximum( currF, currMaxF );
  }
  return currMaxF;
}


// -------
// GetMaxV
// -------

vtkFloatingPointType cvLevelSetStructuredGrid::GetMaxV()
{
  cvLevelSetNode *currNode;
  double currV;
  vtkFloatingPointType currMaxV = 0.0;

  if ( ! deltaPhiValid_ ) {
    printf("ERR: unexpected call to GetMaxV\n");
    return oneOverTol_;
  }

  InitIter();
  while ( currNode = GetNext() ) {
    if ( ! ( currNode->state_ & CV_NODE_ACTIVE ) ) {
      continue;
    }
    currV = fabs( currNode->velocity_ );
    currMaxV = maximum( currV, currMaxV );
  }
  return currMaxV;
}


// -------------
// GetMaxPhiIncr
// -------------

double cvLevelSetStructuredGrid::GetMaxPhiIncr()
{
  cvLevelSetNode *currNode;
  double curr;
  double currMax = 0.0;

  if ( ! deltaPhiValid_ ) {
    printf("ERR: unexpected call to GetMaxPhiIncr\n");
    return oneOverTol_;
  }

  InitIter();
  while ( currNode = GetNext() ) {
    if ( ! ( currNode->state_ & CV_NODE_COVERED ) &&
	 ! ( currNode->state_ & CV_NODE_ACTIVE ) ) {
      continue;
    }
    curr = fabs( currNode->deltaPhi_ );
    currMax = maximum( curr, currMax );
  }
  return currMax;
}


// --------
// ProjectV
// --------

int cvLevelSetStructuredGrid::ProjectV( int posFlag, int save )
{
  int i, j, k;
  int m = 0;
  cvLevelSetNode *currNode, *neighborNode;
  cvIntArrayList ixList( ixBuffer_, ixBufferSz_ );
  int adjIxs[6];
  int currIx, neighborIx, neighborDstIx;
  int status;
  int stranded;

  pSetsValid_ = 0;

  if ( ( save ) && ( nodeSets_ == NULL ) ) {
    return CV_ERROR;
  }

  // For each Grid node:
  InitIter();
  while ( currNode = GetNext() ) {

    if ( currNode->state_ & CV_NODE_ACTIVE ) {

      if ( ( posFlag && ( currNode->phi_ >= 0.0 ) ) ||
	   ( !posFlag && ( currNode->phi_ <= 0.0 ) ) ) {

	// Initialize the fan-out node list:
	ixList.Append( currNode->index_ );
	stranded = 1;

	// While the list of pending fan-out nodes is not empty:
	while ( ! ( ixList.IsEmpty() ) ) {

	  // Retrieve the next index from which to project:
	  status = ixList.RemoveFromHead( &currIx );
	  if ( status != CV_OK ) {
	    return CV_ERROR;
	  }

	  // Save current index into the nodeSets_ array:
	  if ( save ) {
	    nodeSets_[nodeSetsPos_] = currIx;
	    nodeSetsPos_++;
	  }

	  // For each neighbor of current node:
	  GetAdjacentIxs( &(grid_[currIx]), adjIxs );
	  for (j = 0; j < 6; j++) {
	    neighborIx = adjIxs[j];
	    if ( neighborIx < 0 ) {
	      continue;
	    }
	    neighborNode = &(grid_[neighborIx]);

	    // If this neighbor node lies in the *other* of
	    // {inside,outside} then continue:
	    if ( ( posFlag && ( neighborNode->phi_ < 0.0 ) ) ||
		 ( !posFlag && (neighborNode->phi_ > 0.0 ) ) ) {
	      continue;
	    }

	    if ( neighborNode->state_ & CV_NODE_ACTIVE ) {
	      continue;
	    }
	    if ( neighborNode->state_ & CV_NODE_COVERED ) {
	      continue;
	    }

	    if ( posFlag ) {
	      neighborDstIx = SteepestDescentPhi( neighborIx );
	    } else {
	      neighborDstIx = SteepestAscentPhi( neighborIx );
	    }
	    if ( neighborDstIx == currIx ) {
	 
	      neighborNode->F0_ = currNode->F0_;
	      neighborNode->F1_ = currNode->F1_;

	      neighborNode->toDot_[0] = currNode->toDot_[0];
	      neighborNode->toDot_[1] = currNode->toDot_[1];
	      neighborNode->toDot_[2] = currNode->toDot_[2];

	      neighborNode->state_ |= CV_NODE_COVERED;
	      status = ixList.Append( neighborIx );
	      if ( status != CV_OK ) {
		return CV_ERROR;
	      }
	      stranded = 0;
	    } else {
	      if ( neighborDstIx == -1 ) {
		printf("ERR: steepest descent/ascent error\n");
		return CV_ERROR;
	      }
	    }
	  }
	}  // end while -> current fan-out list is empty

	// Mark end of this set of nodes:
	if ( save ) {
	  nodeSets_[nodeSetsPos_] = -1;
	  nodeSetsPos_++;
	}

	// Reset queue for next projection:
	ixList.Reset();

	// Check to see if this was an active node with no children in
	// its projection set:
	//	if (stranded) {
	//	  printf("stranded node [%d]\n", currIx);
	//	}
      }
    }
  }

  if ( save ) {
    pSetsValid_ = 1;
  }

  return CV_OK;
}


// --------------------
// CheckProjectCoverage
// --------------------
// For debug purposes.  Returns 1 iff all nodes are either active or
// covered, implying that complete coverage of the cvLevelSetStructuredGrid was
// achieved by ProjectV.

int cvLevelSetStructuredGrid::CheckProjectCoverage()
{
  cvLevelSetNode *currNode;
  int i;

  InitIter();
  while ( currNode = GetNext() ) {
    if ( ! ( currNode->state_ & CV_NODE_ACTIVE ) &&
	 ! ( currNode->state_ & CV_NODE_COVERED ) ) {
      return 0;
    }
  }

  return 1;
}


// ------------------
// SteepestDescentPhi
// ------------------

int cvLevelSetStructuredGrid::SteepestDescentPhi( int ix )
{
  int adjIxs[6];
  cvLevelSetNode *me;
  int i, currIx;
  int maxIx = -1;
  double currDPhi, maxDPhi;
  double dspace;
  int first = 1;

  if ( ! init_ ) {
    return -1;
  }

  me = &(grid_[ix]);
  GetAdjacentIxs( me, adjIxs );
  for (i = 0; i < 6; i++) {
    currIx = adjIxs[i];
    if ( currIx < 0 ) {
      continue;
    }
    dspace = hv_[i/2];
    currDPhi = ( me->phi_ - grid_[currIx].phi_ ) / dspace;
    if ( first ) {
      maxDPhi = currDPhi;
      maxIx = currIx;
      first = 0;
      continue;
    }
    if ( currDPhi > maxDPhi ) {
      maxDPhi = currDPhi;
      maxIx = currIx;
    }
  }
  return maxIx;
}


// -----------------
// SteepestAscentPhi
// -----------------

int cvLevelSetStructuredGrid::SteepestAscentPhi( int ix )
{
  int adjIxs[6];
  cvLevelSetNode *me;
  int i, currIx;
  int maxIx = -1;
  double currDPhi, maxDPhi;
  double dspace;
  int first = 1;

  if ( ! init_ ) {
    return -1;
  }

  me = &(grid_[ix]);
  GetAdjacentIxs( me, adjIxs );
  for (i = 0; i < 6; i++) {
    currIx = adjIxs[i];
    if ( currIx < 0 ) {
      continue;
    }
    dspace = hv_[i/2];

    // This is the only difference from SteepestDescentPhi:
    currDPhi = ( grid_[currIx].phi_ - me->phi_ ) / dspace;

    if ( first ) {
      maxDPhi = currDPhi;
      maxIx = currIx;
      first = 0;
      continue;
    }
    if ( currDPhi > maxDPhi ) {
      maxDPhi = currDPhi;
      maxIx = currIx;
    }
  }
  return maxIx;
}


// -------------
// ResetNodeSets
// -------------
// This internal (private) method is used to initialize the state of
// the nodeSets_ array.

void cvLevelSetStructuredGrid::ResetNodeSets()
{
  int i;

  if ( nodeSets_ != NULL ) {
    for (i = 0; i < nodeSetsSize_; i++) {
      nodeSets_[i] = -1;
    }
    nodeSetsPos_ = 0;
  }
}


// -----------
// GetNodeSets
// -----------
// Allocate and return an array of cvPolyData*'s which point to sets of
// nodes as currently stored in the nodeSets_ array.  Caller is
// responsible for deleting memory pointed to by nodeSets.

int cvLevelSetStructuredGrid::GetNodeSets( cvPolyData ***nodeSets, int *numSets )
{
  cvPolyData **result;
  int i, j;
  int currIx;
  int num = 0;
  vtkPolyData *pd;
  vtkPoints *pts;
  vtkFloatingPointType crd[3];
  int numPossibleSets = nodeSetsSize_ / 2;

  result = new cvPolyData* [numPossibleSets];
  for (i = 0; i < nodeSetsSize_; i++) {
    currIx = nodeSets_[i];
    if ( currIx >= 0 ) {
      pd = vtkPolyData::New();
      pts = vtkPoints::New();
      pts->Allocate( numPossibleSets );
      j = 0;
      while ( currIx >= 0 ) {
	crd[0] = grid_[currIx].pos_[0];
	crd[1] = grid_[currIx].pos_[1];
	crd[2] = grid_[currIx].pos_[2];
	pts->InsertPoint( j, crd );
	j++;
	currIx = nodeSets_[i+j];
      }
      pts->Squeeze();
      pd->SetPoints( pts );
      pts->Delete();
      result[num] = new cvPolyData( pd );
      pd->Delete();
      i += j;
      num++;
    }
  }

  *nodeSets = result;
  *numSets = num;
  return CV_OK;
}


// -----------
// ClearActive
// -----------

void cvLevelSetStructuredGrid::ClearActive()
{
  cvLevelSetNode *currNode;

  InitIter();
  while ( currNode = GetNext() ) {
    currNode->state_ &= ~CV_NODE_ACTIVE;
  }
  return;
}


// --------------
// ClearForceMinV
// --------------

void cvLevelSetStructuredGrid::ClearForceMinV()
{
  cvLevelSetNode *currNode;

  InitIter();
  while ( currNode = GetNext() ) {
    currNode->state_ &= ~CV_NODE_FORCE_MINV;
  }
  return;
}


// ------------
// ClearCovered
// ------------

void cvLevelSetStructuredGrid::ClearCovered()
{
  cvLevelSetNode *currNode;

  InitIter();
  while ( currNode = GetNext() ) {
    currNode->state_ &= ~CV_NODE_COVERED;
  }
  return;
}


// ----------
// AssignNode
// ----------
// Assign the largest value of gradient(g) (i.e. gg).

void cvLevelSetStructuredGrid::AssignNode( cvLevelSetNode *n, double f0, double f1,
				 int forceMinVFlag, double toDot[] )
{
  double ggMagIn, ggMagCurr;
  double fmag;

  ggMagIn = Magnitude( toDot[0], toDot[1], toDot[2] );
  ggMagCurr = Magnitude( n->toDot_[0], n->toDot_[1], n->toDot_[2] );
  if ( ggMagIn > ggMagCurr ) {
    n->toDot_[0] = toDot[0];
    n->toDot_[1] = toDot[1];
    n->toDot_[2] = toDot[2];
  }

  // If this node has previously had a forced velocity applied to it,
  // then do not use the current velocity (i.e. f0, f1).

  if ( n->state_ & CV_NODE_FORCE_MINV ) {
    return;
  }

  if ( forceMinVFlag ) {
    n->state_ |= CV_NODE_FORCE_MINV;
  }
  if ( ! ( n->state_ & CV_NODE_ACTIVE ) ) {
    n->state_ |= CV_NODE_ACTIVE;
    n->F0_ = f0;
    n->F1_ = f1;
    return;
  }

  fmag = fabs( f0 + f1 );

  if ( forceMinVFlag ) {

    // If forceMinVFlag was returned by cvLevelSetVelocity::Evaluate as
    // true, then we want to ensure that the velocity applied at
    // this node is at *least* the returned value.

    if ( fmag > fabs( n->F0_ + n->F1_ ) ) {
      n->F0_ = f0;
      n->F1_ = f1;
    }

  } else {

    // We do not want to force a minimum velocity on this node, so
    // apply the LESSER of the returned magnitude and the current
    // velocity value if any.
    /*
    if ( fmag < fabs( n->F0_ + n->F1_ ) ) {
      n->F0_ = f0;
      n->F1_ = f1;
    }
    */

    // Average the currently stored value with the new value (note
    // that the following actually weights nodes which are evaluated
    // later more heavily than earlier ones, but that's a pretty
    // minute issue).
    n->F0_ += f0;
    n->F0_ /= 2.0;
    n->F1_ += f1;
    n->F1_ /= 2.0;
  }

  return;
}


// -------------
// AssignVToNode
// -------------
// This is a trial method to clean up the assignment of velocity
// values to cvLevelSetNode's.  What had been happening is that a single
// velocity value from a position on the zero level set could be
// differently assigned to the nodes on the containing active edges
// due to interactions from other velocity values at those nodes.
// Confused?  Here's a drawing:
//
//   Xi: i_th active nodes
//   Oj: j_th zero level set position
//
//    Xa --- O1 ---- Xb
//                   |
//                   |
//                   O2
//                   |
//                   Xc
//
// So, what had been happening is that the situation would arise that
// v(O1) would be assigned to nodes Xa and Xb.  Then, it could be the
// case that v(O2) would later over-write v(O1) at node Xb.  In
// special cases, this would lead to a dead-lock condition in which no
// front motion occurred, but max. F throughout grid never fell below
// the stop criterion.  See /home/wang/local/geomsh.run/cvLevelSetVelocityThreshold/.
//
// This method is intended to be called only when the zero level set
// passes essentially directly through this node.  The
// CV_NODE_V_ASSIGNED state bit will be set, so that no later visits
// to this node will change the velocity.

void cvLevelSetStructuredGrid::AssignVToNode( cvLevelSetNode *n, double f0, double f1,
				    int forceMinVFlag, double toDot[] )
{
  double ggMagIn, ggMagCurr;
  double fmag;

  if ( n->state_ & CV_NODE_V_ASSIGNED ) {
    return;
  }

  ggMagIn = Magnitude( toDot[0], toDot[1], toDot[2] );
  ggMagCurr = Magnitude( n->toDot_[0], n->toDot_[1], n->toDot_[2] );
  if ( ggMagIn > ggMagCurr ) {
    n->toDot_[0] = toDot[0];
    n->toDot_[1] = toDot[1];
    n->toDot_[2] = toDot[2];
  }

  // If this node has previously had a forced velocity applied to it,
  // then do not use the current velocity (i.e. f0, f1).

  if ( n->state_ & CV_NODE_FORCE_MINV ) {
    return;
  }

  if ( forceMinVFlag ) {
    n->state_ |= CV_NODE_FORCE_MINV;
  }
  if ( ! ( n->state_ & CV_NODE_ACTIVE ) ) {
    n->state_ |= CV_NODE_ACTIVE;
    n->F0_ = f0;
    n->F1_ = f1;
    n->state_ |= CV_NODE_V_ASSIGNED;
    return;
  }

  fmag = fabs( f0 + f1 );

  if ( forceMinVFlag ) {

    // If forceMinVFlag was returned by cvLevelSetVelocity::Evaluate as
    // true, then we want to ensure that the velocity applied at
    // this node is at *least* the returned value.

    if ( fmag > fabs( n->F0_ + n->F1_ ) ) {
      n->F0_ = f0;
      n->F1_ = f1;
      n->state_ |= CV_NODE_V_ASSIGNED;
    }

  } else {

    // We do not want to force a minimum velocity on this node, so
    // apply the LESSER of the returned magnitude and the current
    // velocity value if any.

    if ( fmag < fabs( n->F0_ + n->F1_ ) ) {
      n->F0_ = f0;
      n->F1_ = f1;
      n->state_ |= CV_NODE_V_ASSIGNED;
    }
  }

  return;
}


// -------------
// AssignVToEdge
// -------------

void cvLevelSetStructuredGrid::AssignVToEdge( cvLevelSetNode *a, cvLevelSetNode *b, double f0, double f1,
				    int forceMinVFlag, double toDot[] )
{
  printf("ERR: What are you doing here?\n");
  return;
}


// ---------------
// GridT_StrToEnum
// ---------------

GridT GridT_StrToEnum( char *name )
{
  if ( !strcmp( name, "DenseGrid" ) ) {
    return Dense_GridT;
  } else if ( !strcmp( name, "SparseGrid" ) ) {
    return Sparse_GridT;
  } else {
    return Invalid_GridT;
  }
}


// ---------------
// GridT_EnumToStr
// ---------------
// Caller need NOT worry about result clean up.

char *GridT_EnumToStr( GridT t )
{
  static Tcl_DString ds;

  Tcl_DStringFree( &ds );  // both frees and reinitializes

  switch (t) {
  case Dense_GridT:
    Tcl_DStringAppend( &ds, "DenseGrid", -1 );
    break;
  case Sparse_GridT:
    Tcl_DStringAppend( &ds, "SparseGrid", -1 );
    break;
  default:
    Tcl_DStringAppend( &ds, "Invalid GridT... must be one of { DenseGrid, "
		       "SparseGrid }", -1 );
    break;
  }

  return Tcl_DStringValue( &ds );
}
