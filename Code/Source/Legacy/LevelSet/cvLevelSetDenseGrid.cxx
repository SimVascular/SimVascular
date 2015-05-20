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
#include "cvLevelSetDenseGrid.h"
#include "cvLevelSetVelocity.h"
#include "cvIntArrayList.h"
#include "cv_misc_utils.h"
#include "cvVTK.h"
#include "cvSolidModel.h"


// ---------
// cvLevelSetDenseGrid
// ---------
// We want to store nodes s.t. the z-index varies the *slowest*, where
// i is the x-index, j the y-index and k the z-index.

cvLevelSetDenseGrid::cvLevelSetDenseGrid( double h[], int dims[], double o[] )
  : cvLevelSetStructuredGrid( h, dims, o )
{
  int i, j, k, n;
  int kOffset, jOffset;

  int xPrevI, xPrevJ, xPrevK;
  int yPrevI, yPrevJ, yPrevK;
  int zPrevI, zPrevJ, zPrevK;
  int xNextI, xNextJ, xNextK;
  int yNextI, yNextJ, yNextK;
  int zNextI, zNextJ, zNextK;

  numNodes_ = I_ * J_ * K_;
  grid_ = new cvLevelSetNode [numNodes_];
  if ( grid_ == NULL ) {
    printf( "ERR: Couldn't allocate memory for %d cvLevelSetNode's.\n", numNodes_ );
    return;
  }

  ixBuffer_ = new int [numNodes_];
  ixBufferSz_ = numNodes_;
  nodeSetsSize_ = 2 * numNodes_;
  nodeSets_ = new int [nodeSetsSize_];
  nodeSetsPos_ = 0;

  for (k = 0; k < K_; k++) {
    kOffset = k * I_ * J_;
    for (j = 0; j < J_; j++) {
      jOffset = j * I_;
      for (i = 0; i < I_; i++) {

	n = i + jOffset + kOffset;
	grid_[n].phi_ = -1;
	grid_[n].state_ = 0;

	grid_[n].pos_[0] = i * hv_[0] + origin_[0];
	grid_[n].pos_[1] = j * hv_[1] + origin_[1];
	grid_[n].pos_[2] = k * hv_[2] + origin_[2];

	grid_[n].index_ = n;
	grid_[n].i_ = i;
	grid_[n].j_ = j;
	grid_[n].k_ = k;

	if ( dim_ == 1 ) {
	  if ( (i == 0) || (i == I_-1) ) {
	    grid_[n].state_ |= CV_NODE_BOUND;
	  }
	} else if ( dim_ == 2 ) {
	  if ( (i == 0) || (i == I_-1) ||
	       (j == 0) || (j == J_-1) ) {
	    grid_[n].state_ |= CV_NODE_BOUND;
	  }
	} else if ( dim_ == 3 ) {
	  if ( (i == 0) || (i == I_-1) ||
	       (j == 0) || (j == J_-1) ||
	       (k == 0) || (k == K_-1) ) {
	    grid_[n].state_ |= CV_NODE_BOUND;
	  }
	}

	// Set up neighbor information for this grid node:

	// Previous node in x direction:
	if (i == 0) {
	  xPrevI = i;
	} else {
	  xPrevI = i - 1;
	}
	xPrevJ = j;
	xPrevK = k;

	// Previous node in y direction:
	if (j == 0) {
	  yPrevJ = j;
	} else {
	  yPrevJ = j - 1;
	}
	yPrevI = i;
	yPrevK = k;

	// Previous node in z direction:
	if (k == 0) {
	  zPrevK = k;
	} else {
	  zPrevK = k - 1;
	}
	zPrevI = i;
	zPrevJ = j;

	// Next node in x direction:
	if (i == I_-1) {
	  xNextI = i;
	} else {
	  xNextI = i+1;
	}
	xNextJ = j;
	xNextK = k;

	// Next node in y direction:
	if (j == J_-1) {
	  yNextJ = j;
	} else {
	  yNextJ = j+1;
	}
	yNextI = i;
	yNextK = k;

	// Next node in z direction:
	if (k == K_-1) {
	  zNextK = k;
	} else {
	  zNextK = k+1;
	}
	zNextI = i;
	zNextJ = j;

	grid_[n].xPrevIndex_ = IJKToIndex( xPrevI, xPrevJ, xPrevK );
	grid_[n].yPrevIndex_ = IJKToIndex( yPrevI, yPrevJ, yPrevK );
	grid_[n].zPrevIndex_ = IJKToIndex( zPrevI, zPrevJ, zPrevK );

	grid_[n].xNextIndex_ = IJKToIndex( xNextI, xNextJ, xNextK );
	grid_[n].yNextIndex_ = IJKToIndex( yNextI, yNextJ, yNextK );
	grid_[n].zNextIndex_ = IJKToIndex( zNextI, zNextJ, zNextK );
      }
    }
  }

  scalars_ = NULL;

  return;
}


// ----------
// ~cvLevelSetDenseGrid
// ----------

cvLevelSetDenseGrid::~cvLevelSetDenseGrid()
{
  DeallocateNodes();
  delete [] ixBuffer_;
  delete [] nodeSets_;
  //if ( phiVtk_ ) {
  //  phiVtk_->Delete();
  //}

  return;
}


// ---------------
// DeallocateNodes
// ---------------

void cvLevelSetDenseGrid::DeallocateNodes()
{
  if ( grid_ != NULL ) {
    delete [] grid_;
  }
  return;
}


// --------
// InitIter
// --------

void cvLevelSetDenseGrid::InitIter()
{
  curr_ = grid_;
  return;
}


// -------
// GetNext
// -------

cvLevelSetNode *cvLevelSetDenseGrid::GetNext()
{
  cvLevelSetNode *result;

  if ( curr_ == NULL ) {
    return NULL;
  }
  result = curr_;
  if ( curr_->index_ == (numNodes_-1) ) {
    curr_ = NULL;
  } else {
    curr_++;
  }
  return result;
}


// ----------
// IJKToIndex
// ----------

int cvLevelSetDenseGrid::IJKToIndex( int i, int j, int k )
{
  int kOffset, jOffset, index;

  kOffset = k * I_ * J_;
  jOffset = j * I_;
  index = i + jOffset + kOffset;
  if ( index >= numNodes_ ) {
    printf( "  ERR: Invalid grid access (i,j,k) = (%d,%d,%d).\n", i, j, k );
    return -1;
  } else {
    return index;
  }
}


// ---------
// UpdatePhi
// ---------

// This is where we use each node's F (as stored in F0 and F1) in
// conjunction with current phi and a simple forward Euler time scheme
// to produce phi at time t + delta_t.  Recall that we broke F into
// two components (F0 and F1) because each of these is compatible with
// a different approximation to the spatial gradient of phi.  We also
// have to remember to unset / reset state bits which will be
// invalidated by the update of the scalar phi field.

// Use GetMaxF to compute a maximum allowable time step size under CFL
// for each time step.

/*
double cvLevelSetDenseGrid::UpdatePhi( double factor )
{
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
    geodesicTerm = currNode->toDot_[0] * currNode->d0_[0] / f1MagGradPhi;
    geodesicTerm += currNode->toDot_[1] * currNode->d0_[1] / f1MagGradPhi;
    geodesicTerm += currNode->toDot_[2] * currNode->d0_[2] / f1MagGradPhi;

    phi_t = - ( f0Contrib + f1Contrib ) + geodesicTerm;
    currNode->phi_ += dt * phi_t;

    // Check for CFL violation:
    v = fabs( currNode->F0_ + currNode->F1_ );
    if ( 2*v*dt > minh_ ) {
      printf( "  CFL violation (nodeId=%d): 2*v*dt = %f\n",
	       currNode->index_, 2*v*dt );
      printf("   maxF [%f]\t v [%f]\t dt [%f]\n", GetMaxF(), v, dt );
    }
  }

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

  return dt;
}
*/


// -------
// InitPhi
// -------

// KCW [4/7/99]
// ---
// Now that it looks like we can do point classification operations
// with class cvSolidModel, we can add the ability for InitPhi to
// determine sign without relying on the previous sign.  For purposes
// of phi reinitialization, it's still better to just use the previous
// sign.  But if we're initializing Grid for the first time based on a
// cvPolyData object, we need to use the classification operation to
// determine sign.

int cvLevelSetDenseGrid::InitPhi( cvPolyData *front )
{
  cvLevelSetNode *currNode;
  double x, y, z;
  double d;
  double sign;
  cvSolidModel *sm = NULL;
  int c;

  if (!init_) {
    sm = cvSolidModel::DefaultInstantiateSolidModel();
    if ( sm == NULL ) {
      return CV_ERROR;
    }
    if ( dim_ == 3 ) {
      sm->MakePoly3dSolid( front , 0);
    } else if ( dim_ == 2 ) {
      sm->MakePoly2d( front );
    } else {
      assert(0);
    }
  }

  InitIter();
  while ( currNode = GetNext() ) {

    x = currNode->pos_[0];
    y = currNode->pos_[1];
    z = currNode->pos_[2];
    d = front->FindDistance( x, y, z );

    // cvPolyData::FindDistance returns a negative value on error.
    if ( d < 0.0 ) {
      init_ = 0;
      if (sm) {
	delete sm;
      }
      return CV_ERROR;
    }

    // If Grid is uninitialized, use the cvSolidModel::ClassifyPt method
    // to determine sign of phi:
    if (sm) {

      if ( sm->ClassifyPt( x, y, 0, &c ) != CV_OK ) {
	printf("  ERR (cvLevelSetDenseGrid::InitPhi): cvSolidModel::ClassifyPt error.\n");
	init_ = 0;
	if (sm) {
	  delete sm;
	}
	return CV_ERROR;
      }

      // Mapping classification to distance, sign:
      //   in --> sign = negative
      //   on --> phi = 0.0
      //   out --> sign = positive

      if ( c > 0 ) {
	sign = -1.0;
      } else if ( c == 0 ) {
	sign = 0.0;
      } else {
	sign = 1.0;
      }

    // Otherwise use previous sign:
    } else {
      if (currNode->phi_ < 0.0) {
	sign = -1.0;
      } else if (currNode->phi_ == 0.0) {
	sign = 0.0;
      } else {
	sign = 1.0;
      }
    }

    currNode->phi_ = sign * d;
  }
  init_ = 1;
  if (sm) {
    delete sm;
  }

  phiVtkValid_ = 0;
  return CV_OK;
}


// --------------------------------
// InitPhi( double *pos, double r )
// --------------------------------

int cvLevelSetDenseGrid::InitPhi( double ctr[], double radius )
{
  cvLevelSetNode *currNode;
  double x, y, z;
  double cx, cy, cz;
  double tmp, rsq, factor, dist;

  rsq = radius * radius;
  cx = ctr[0];
  cy = ctr[1];
  if ( dim_ == 3 ) {
    cz = ctr[2];
  }
  InitIter();
  while ( currNode = GetNext() ) {
    x = currNode->pos_[0];
    y = currNode->pos_[1];
    if ( dim_ == 3 ) {
      z = currNode->pos_[2];
      tmp = (x-cx)*(x-cx) + (y-cy)*(y-cy) + (z-cz)*(z-cz);
    } else {
      tmp = (x-cx)*(x-cx) + (y-cy)*(y-cy);
    }
    dist = fabs( radius - sqrt(tmp) );
    if (tmp < rsq) {
      factor = -1.0;
    } else {
      factor = 1.0;
    }
    currNode->phi_ = factor * dist;
  }
  init_ = 1;

  phiVtkValid_ = 0;
  return CV_OK;
}


// -------
// InitPhi
// -------

int cvLevelSetDenseGrid::InitPhi( cvSolidModel *sm )
{
  cvLevelSetNode *currNode;
  double dist;
  double upperLimit = 4 * minh_;
  double tol = relTol_ * minh_;

  InitIter();
  while ( currNode = GetNext() ) {

    if ( sm->Distance( currNode->pos_, upperLimit, &dist ) != CV_OK ) {
      printf("  ERR (cvLevelSetDenseGrid::InitPhi): cvSolidModel::Distance error.\n");
      init_ = 0;
      return CV_ERROR;
    }
    if ( fabs( dist ) < tol ) {
      dist = 0.0;
    }
    currNode->phi_ = dist;
  }

  init_ = 1;
  phiVtkValid_ = 0;
  return CV_OK;


  // Old as of 9/29/99:
  // ------------------
  /*
  cvLevelSetNode *currNode;
  int i, c;
  cvPolyData *front;
  cvPolyData *surf;

  for (i = 0; i < numNodes_; i++) {
    currNode = &(grid_[i]);

    if ( sm->ClassifyPt( currNode->pos_, 0, &c ) != CV_OK ) {
      printf("  ERR (cvLevelSetDenseGrid::InitPhi): cvSolidModel::ClassifyPt error.\n");
      init_ = 0;
      return;
    }

    // Mapping classification to distance, sign:
    //   in --> sign = negative
    //   on --> phi = 0.0
    //   out --> sign = positive

    if ( c > 0 ) {
      currNode->phi_ = -1.0;
    } else if ( c == 0 ) {
      currNode->phi_ = 0.0;
    } else {
      currNode->phi_ = 1.0;
    }
  }

  init_ = 1;

//  front = this->GetFront();
//  this->InitPhi( front );
//  delete front;

  surf = sm->GetPolyData();
  this->InitPhi( surf );
  delete surf;

  phiVtkValid_ = 0;
  return;
  */
}


// -------
// InitPhi
// -------

int cvLevelSetDenseGrid::InitPhi( cvStrPts *img, double thr )
{
  return CV_ERROR;
}


// ---------
// ReinitPhi
// ---------
// To be called by cvLevelSet::RebuildPhi().

int cvLevelSetDenseGrid::ReinitPhi()
{
  cvPolyData *front;

  front = GetFront();
  if ( front->GetVtkPolyData()->GetNumberOfCells() < 1 ) {
    printf("ERR: Current zero level set has no geometry.\n");
    delete front;
    return CV_ERROR;
  }
  InitPhi( front );
  delete front;
  return CV_OK;
}


// ------
// GetPhi
// ------

cvDataObject *cvLevelSetDenseGrid::GetPhi()
{
  cvStrPts *result;

  MakePhiVtk();
  result = new cvStrPts( (vtkStructuredPoints *)phiVtk_ );
  return result;
}


// ------------
// GetCurvature
// ------------

cvStrPts *cvLevelSetDenseGrid::GetCurvature()
{
  cvStrPts *result;

  FindK();
  MakeScalarsVtk( GS_CURVATURE );
  if ( scalars_ != NULL ) {
    result = new cvStrPts( scalars_ );
    return result;
  } else {
    return NULL;
  }
}


// ----------
// MakePhiVtk
// ----------

void cvLevelSetDenseGrid::MakePhiVtk( int closed )
{
  int i;
  cvLevelSetNode *currNode;
  double datum;

  if ( (!phiVtkValid_) || closed ) {

    if ( phiVtk_ ) {
      phiVtk_->Delete();
    }

    phiVtk_ = vtkStructuredPoints::New();
    ((vtkStructuredPoints *)phiVtk_)->SetDimensions( I_, J_, K_ );
    ((vtkStructuredPoints *)phiVtk_)->SetOrigin( origin_[0], origin_[1],
						 origin_[2] );
    ((vtkStructuredPoints *)phiVtk_)->SetSpacing( hv_[0], hv_[1], hv_[2] );

    // Fill in scalar point data:
    vtkFloatingPointArrayType *scalars = vtkFloatingPointArrayType::New();
    for (i = 0; i < numNodes_; i++) {
      currNode = &(grid_[i]);
      datum = currNode->GetDoubleDatum( GS_PHI, tol_ );
      if ( closed ) {
	if ( ( currNode->i_ == 0 ) || ( currNode->i_ == (I_-1) ) ||
	     ( currNode->j_ == 0 ) || ( currNode->j_ == (J_-1) ) ||
	     ( ( dim_ > 2 ) && 
	       ( ( currNode->k_ == 0 ) || ( currNode->k_ == (K_-1) ) ) ) ) {
	  if ( datum < 0.0 ) {
	    datum = 0.0;
	  }
	}
      }
      scalars->InsertTuple1( i, (vtkFloatingPointType)datum );
    }

    // Connect data to grid:
    ((vtkStructuredPoints *)phiVtk_)->GetPointData()->SetScalars( scalars );
    scalars->Delete();

    phiVtkValid_ = 1;
  }

  return;
}


// --------------
// MakeScalarsVtk
// --------------

void cvLevelSetDenseGrid::MakeScalarsVtk( GridScalarT scalarType ) {
  int i;

  if ( scalars_ ) {
    scalars_->Delete();
  }

  scalars_ = vtkStructuredPoints::New();
  scalars_->SetDimensions( I_, J_, K_ );
  scalars_->SetOrigin( origin_[0], origin_[1], origin_[2] );
  //  scalars_->SetSpacing( h_, h_, h_ );
  scalars_->SetSpacing( hv_[0], hv_[1], hv_[2] );

  // Fill in scalar point data:
  vtkFloatingPointArrayType *data = vtkFloatingPointArrayType::New();
  for (i = 0; i < numNodes_; i++) {
    switch (scalarType) {
    case GS_PHI:
      data->InsertTuple1( i, (vtkFloatingPointType)(grid_[i].phi_) );
      break;
    case GS_CURVATURE:
      data->InsertTuple1( i, (vtkFloatingPointType)(grid_[i].K_) );
      break;
    default:
      scalars_->Delete();
      data->Delete();
      scalars_ = NULL;
      return;
      break;
    }
  }

  // Connect data to grid:
  scalars_->GetPointData()->SetScalars( data );
  data->Delete();

  return;
}


// -------
// ExtendV
// -------
// This will be superseded by cvLevelSetDenseGrid::ProjectV.

void cvLevelSetDenseGrid::ExtendV()
{
  int i;
  cvLevelSetNode *currNode, *nearestActiveNode;

  for (i = 0; i < numNodes_; i++) {
    currNode = &(grid_[i]);
    if ( ! (currNode->state_ & CV_NODE_ACTIVE) ) {
      nearestActiveNode = FindNearestActiveNode( currNode );
      currNode->F0_ = nearestActiveNode->F0_;
      currNode->F1_ = nearestActiveNode->F1_;

      currNode->toDot_[0] = nearestActiveNode->toDot_[0];
      currNode->toDot_[1] = nearestActiveNode->toDot_[1];
      currNode->toDot_[2] = nearestActiveNode->toDot_[2];
    }
  }
}


// ---------------------
// FindNearestActiveNode
// ---------------------
// This method supports ExtendV, which is from the very first days of
// getting level set calculations going, and was superseded by
// ProjectV.  It's incredibly slow and is quite obsolete, but is being
// kept in place as a place holder for other velocity extension
// methods which we might put in later.

cvLevelSetNode *cvLevelSetDenseGrid::FindNearestActiveNode( cvLevelSetNode *n )
{
  cvLevelSetNode *c, *t;
  int maxDim;
  double currDist, minDist;

  InitIter();
  maxDim = maximum(I_, J_);
  maxDim = maximum(maxDim, K_);
  minDist = maxDim * minh_ * sqrt(2.0);
  t = NULL;
  while ( c = GetNext() ) {
    if (c->state_ & CV_NODE_ACTIVE) {
      currDist = Distance( c->pos_[0], c->pos_[1], c->pos_[2],
			   n->pos_[0], n->pos_[1], n->pos_[2] );
      if (currDist < minDist) {
	t = c;
      }
    }
  }
  assert(t);
  return t;
}


// --------
// ProjectV
// --------
// Things limiting portability to other subclasses of cvLevelSetStructuredGrid:
//   - the nodeSets_ mechanism is currently private to cvLevelSetDenseGrid
// This version of ProjectV is the one that is externally called
// (e.g. by class cvLevelSet).

int cvLevelSetDenseGrid::ProjectV( int save )
{
  ClearCovered();
  ResetNodeSets();

  // See Lippman (2nd ed.) p.401: inherited methods whose names are
  // re-used in the derived class must be accessed with the class
  // scope operator.  In this case, the signatures of cvLevelSetStructuredGrid's
  // ProjectV and cvLevelSetDenseGrid's ProjectV (which is itself the definition
  // of a pure virtual) are distinct, but we need to use the scoping
  // operator anyway.

  if ( this->cvLevelSetStructuredGrid::ProjectV( 1, save ) != CV_OK ) {
    return CV_ERROR;
  }
  if ( this->cvLevelSetStructuredGrid::ProjectV( 0, save ) != CV_OK ) {
    return CV_ERROR;
  }

  // If complete coverage, was not achieved, then rebuild phi and try
  // again:
  if ( ! CheckProjectCoverage() ) {
    if ( ReinitPhi() != CV_OK ) {
      return CV_ERROR;
    }
    ClearCovered();
    ResetNodeSets();
    if ( this->cvLevelSetStructuredGrid::ProjectV( 1, save ) != CV_OK ) {
      return CV_ERROR;
    }
    if ( this->cvLevelSetStructuredGrid::ProjectV( 0, save ) != CV_OK ) {
      return CV_ERROR;
    }

    // Now, if complete coverage is STILL not achieved, then return an
    // error:
    if ( ! CheckProjectCoverage() ) {
      return CV_ERROR;
    }
  }

  return CV_OK;
}


// ----------
// StatString
// ----------

char *cvLevelSetDenseGrid::StatString()
{
  char *result;

  result = new char [CV_STRLEN];
  sprintf( result, "nodes %d", numNodes_ );

  return result;
}


// --------------
// GetMemoryUsage
// --------------

int cvLevelSetDenseGrid::GetMemoryUsage()
{
  int sz = 0;

  sz += sizeof( this );
  sz += GetStructuredGridMemoryUsage();
  if ( grid_ ) {
    sz += numNodes_ * sizeof(cvLevelSetNode);
  }
  if ( scalars_ ) {
    sz += scalars_->GetActualMemorySize() * 1024;  // vtk return kB
  }

  return sz;
}
