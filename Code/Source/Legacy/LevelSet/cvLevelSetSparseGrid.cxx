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

#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "cvLevelSetSparseGrid.h"
#include "cv_misc_utils.h"
#include "cv_image.h"
#include "cv_sys_geom.h"
#include "cvIntArrayList.h"
#include "cvUnstructuredGrid.h"
#include "cvVTK.h"
#include "cvSolidModel.h"


// ----------
// cvLevelSetSparseGrid
// ----------

cvLevelSetSparseGrid::cvLevelSetSparseGrid( double h[], int dims[], double o[] )
  : cvLevelSetStructuredGrid( h, dims, o )
{
  int i;

  currIx_ = 0;

  overlaySz_ = I_ * J_ * K_;
  topoOverlay_ = new cvStateArray( overlaySz_ );  // lifetime same as cvLevelSetSparseGrid

  numBuckets_ = sqrt( 1.0 * numDenseNodes_ );
  nodeTable_ = new cvSortedList<TableStruct*> * [numBuckets_];
  if ( nodeTable_ == NULL ) {
    printf("ERR: Couldn't allocate cvLevelSetSparseGrid::nodeTable_.\n");
    return;
  }
  for (i = 0; i < numBuckets_; i++) {
    nodeTable_[i] = new cvSortedList<TableStruct*>( TableStructCompareFn );
  }

  numSparseNodes_ = 0;
  numSparseEdges_ = 0;
  htIter_ = NULL;

  adjIa_ = NULL;
  adjJa_ = NULL;
  iaSz_ = 0;
  jaSz_ = 0;

  color_ = NULL;
  L1map_ = NULL;
  L2map_ = NULL;

  gridState_ = SGST_Start;

  // The mineHit_ flag is set to true during the time step which
  // causes a mine node to be reached by the zero level set.  The flag
  // stays true up until the next time step is invoked.
  mineHit_ = 0;

  seedInterface_ = NULL;
  bandMethod_ = BAND_SWEEP;
  closedPhiVtkValid_ = 0;

  return;
}


/*
cvLevelSetSparseGrid::cvLevelSetSparseGrid( MPI_Comm comm )
{
  // - receive packaged sub-domain
  // - set packageSize_
  // - set localSize_
  // - gridState_ == SGST_PackagingComplete upon successful construction
}
*/


// -----------
// ~cvLevelSetSparseGrid
// -----------

cvLevelSetSparseGrid::~cvLevelSetSparseGrid()
{
  DeallocateNodes();
  ClearSeedInterface();

  delete topoOverlay_;
  delete [] adjIa_;
  delete [] adjJa_;
}


// ---------------
// DeallocateNodes
// ---------------
// Free up all of the following:
//   - hash table nodeTable_
//   - CSR structure adjIa_, adjJa_, grid_
//   - partition vector

void cvLevelSetSparseGrid::DeallocateNodes()
{
  ClearHT();
  DeallocateCSR();
  ClearPartitioning();
  numSparseNodes_ = 0;
  return;
}


// -------
// ClearHT
// -------

void cvLevelSetSparseGrid::ClearHT()
{
  int i;
  cvSortedList<TableStruct*> *bucket;
  TableStruct *entry;

  for (i = 0; i < numBuckets_; i++) {
    bucket = nodeTable_[i];
    while ( ! bucket->IsEmpty() ) {
      entry = bucket->RemoveFront();
      delete entry;
    }
  }

  return;
}


// -------------
// DeallocateCSR
// -------------

void cvLevelSetSparseGrid::DeallocateCSR()
{
  if ( grid_ != NULL ) {
    delete [] grid_;
    grid_ = NULL;
  }
  if ( adjIa_ != NULL ) {
    delete [] adjIa_;
    adjIa_ = NULL;
    iaSz_ = 0;
  }
  if ( adjJa_ != NULL ) {
    delete [] adjJa_;
    adjJa_ = NULL;
    jaSz_ = 0;
  }
  return;
}


// -----------------
// ClearPartitioning
// -----------------

void cvLevelSetSparseGrid::ClearPartitioning()
{
  if ( color_ != NULL ) {
    delete [] color_;
    color_ = NULL;
  }
  return;
}


// ---------------
// GetNumBandNodes
// ---------------

int cvLevelSetSparseGrid::GetNumBandNodes() const
{
  if ( gridState_ < SGST_HashTable ) {
    return -1;
  } else {
    return numSparseNodes_;
  }
}


// -------------
// SetBandParams
// -------------
// Note that the inner band extent is expected to be a negative number
// (phi inside < 0) and the outer extent to be a positive number (phi
// outside > 0).

int cvLevelSetSparseGrid::SetBandParams( double innerPhi, double outerPhi,
			       double mineWidth )
{
  // mineWidth must be greater than the main diagonal across a grid
  // cell (see notes 12/6/99)... otherwise, it is possible for nodes
  // on the grid boundary to *not* be tagged as mine nodes, which can
  // lead to holes in the boundary when it reaches the edge of the
  // sparse domain:
  if ( mineWidth <= mainDiagonal_ ) {
    printf("WARNING: mine width must be >= %f\n", mainDiagonal_);
    mineWidth = mainDiagonal_ * ( 1 + relTol_ );
    printf("WARNING: applying mine width %f instead\n", mineWidth);
  }
  mineWidth_ = mineWidth;
  mineWidth2_ = mineWidth * mineWidth;

  // innerPhi must be NEGATIVE:
  if ( innerPhi >= 0.0 ) {
    printf("ERR: inner band extent must be negative\n");
    return CV_ERROR;
  } else if ( fabs(innerPhi) < mineWidth ) {
    printf("ERR: fabs( inner band extent ) must be >= mine width\n");
    return CV_ERROR;
  } else {
    innerExtent_ = innerPhi;
    innerExtent2_ = innerPhi * innerPhi;
  }

  // outerPhi must be POSITIVE
  if ( outerPhi <= 0.0 ) {
    printf("ERR: outer band extent must be positive\n");
    return CV_ERROR;
  } else if ( fabs(outerPhi) < mineWidth ) {
    printf("ERR: outer band extent must be >= mine width\n");
    return CV_ERROR;
  } else {
    outerExtent_ = outerPhi;
    outerExtent2_ = outerPhi * outerPhi;
  }

  gridState_ = SGST_ExtentDefined;
  return CV_OK;
}


// -------------
// GetBandParams
// -------------

int cvLevelSetSparseGrid::GetBandParams( double *innerPhi, double *outerPhi,
			       double *mineWidth )
{
  if ( gridState_ < SGST_ExtentDefined ) {
    return CV_ERROR;
  }
  *innerPhi = innerExtent_;
  *outerPhi = outerExtent_;
  *mineWidth = mineWidth_;
  return CV_OK;
}


// -------
// InitPhi
// -------

int cvLevelSetSparseGrid::InitPhi( double ctr[], double radius )
{
  if ( ConstructHT( ctr, radius ) != CV_OK ) {
    init_ = 0;
    return CV_ERROR;
  }
  init_ = 1;
  phiVtkValid_ = 0;
  closedPhiVtkValid_ = 0;
  return CV_OK;
}


// -------
// InitPhi
// -------
// This is the publicly-callable cvPolyData-based phi initialization.
// This means that it handles the case where init_ is NOT true.  In
// this case, sign initialization could be done by either:
//   - cvSolidModel point classify (SLOW)
//   - Graphics Gems algorithms (faster)

// Although cvSolidModel's CAN be much more general than the simple
// polygons / polyhedra handled by the Graphics Gems algorithms, there
// is currently no easy way to SPECIFY structure of non-simple objects
// through the cvPolyData interface which we're dealing with in this
// method.  Generally, by "simple" I mean objects which consist of a
// single, closed, manifold region.  Note that cvPolyData can be readily
// used to transmit such objects, including ones with holes.  Since
// the point-in-polyhedron algorithm from Graphics Gems does NOT
// appear to function for simple objects with holes, cvSolidModel can
// provide a more general solution for this method than other
// algorithms.  However, since general objects can already be used for
// initialization via the cvSolidModel-based InitPhi, I will assume that
// all cvPolyData's passed to this method are assumed to be simple
// (i.e. single closed manifold region) and to have NO HOLES.

// One important distinction, however, remains: cvSolidModel methods
// generally will be able to distinguish among three cases: in, out
// and on.  The Graphics Gems algorithms do not generally make
// consistent distinction of the on case, classifying such points as
// either in or out instead.  The reason this is OK is that the +/-
// sign which is (erroneously) applied to "on" points will be caught
// in the ConstructHT(_Sweep) methods: when distance is computed and
// found to be under a certain precision-related magnitude, then sign
// is ignored, and the "on" state and zero distance are applied.

int cvLevelSetSparseGrid::InitPhi( cvPolyData *front )
{
  if ( ! init_ ) {
    if ( InitSign_Internal( front ) != CV_OK ) {  // on success, init_ --> 1
      return CV_ERROR;
    }
  }
  return InitPhi_Internal( front );
}


// -----------------
// InitSign_Internal
// -----------------
// Use point-classify operations (either from cvSolidModel or some other
// source) to set values throughout the dense topoOverlay_ array.

int cvLevelSetSparseGrid::InitSign_Internal( cvPolyData *front )
{
  int i, j, k;
  int n;
  double pos[3];
  int clsfy;
  int status;

  for (k = 0; k < K_; k++) {
    for (j = 0; j < J_; j++) {
      for (i = 0; i < I_; i++) {

	n = IJKToDenseIx( i, j, k );

	pos[0] = i * hv_[0] + origin_[0];
	pos[1] = j * hv_[1] + origin_[1];
	pos[2] = k * hv_[2] + origin_[2];

	switch (dim_) {
	case 2:
	  status = sys_geom_PtInPoly( front, pos, 0, &clsfy );
	  break;
	case 3:
	  status = sys_geom_Classify( front, pos, &clsfy );
	  break;
	default:
	  assert(0);
	}

	if ( ! status ) {
	  printf( "ERR: pt-in-{polygon/polyhedron} error\n" );
	  return CV_ERROR;
	}

	if ( clsfy < 0 ) {
	  (*topoOverlay_)[n] |= STATE_PHI_OUTSIDE;
	} else {
	  (*topoOverlay_)[n] |= STATE_PHI_INSIDE;
	}
      }
    }
  }
  init_ = 1;
  return CV_OK;


  // cvSolidModel-based sign initialization:
  /*
  cvSolidModel *sm;
  sm = cvSolidModel::DefaultInstantiateSolidModel();
  if ( sm == NULL ) {
    return CV_ERROR;
  }
  if ( dim_ == 3 ) {
    sm->MakePoly3dSolid( front , 0 );
  } else if ( dim_ == 2 ) {
    sm->MakePoly2d( front );
  } else {
    assert(0);
  }
  delete sm;
  */
}


// ----------------
// InitPhi_Internal
// ----------------
// This method refers to the situation in which we want to find values
// for the level set function phi, but where we've already got sign
// information.  We could call this from:
//   - ProjectV
//   - InitPhi( cvStrPts*, double )

// This method should be the SOLE ENTRY POINT for:
//   - ConstructHT( cvPolyData* )
//   - ConstructHT_Sweep( cvPolyData* )

int cvLevelSetSparseGrid::InitPhi_Internal( cvPolyData *front )
{
  if ( ! init_ ) {
    return CV_ERROR;
  }

  switch (bandMethod_) {

  case BAND_SDIST:
    if ( ConstructHT( front ) != CV_OK ) {
      init_ = 0;
      return CV_ERROR;
    }
    init_ = 1;
    phiVtkValid_ = 0;
    closedPhiVtkValid_ = 0;
    return CV_OK;

  case BAND_SWEEP:

    // Currently, init_ has to be true for ConstructHT_Sweep to
    // succeed.

    if ( ConstructHT_Sweep( front ) != CV_OK ) {
      init_ = 0;
      return CV_ERROR;
    }
    init_ = 1;
    phiVtkValid_ = 0;
    closedPhiVtkValid_ = 0;
    return CV_OK;

  default:
    break;
  }

  return CV_ERROR;
}


// -------
// InitPhi
// -------

int cvLevelSetSparseGrid::InitPhi( cvSolidModel *solid )
{
  if ( ConstructHT( solid ) != CV_OK ) {
    init_ = 0;
    return CV_ERROR;
  }
  init_ = 1;
  phiVtkValid_ = 0;
  closedPhiVtkValid_ = 0;
  return CV_OK;
}


// -------
// InitPhi
// -------
// There's a somewhat pathological case which can arise.  If there is
// an isolated pixel which is equal to the given threshold, then it
// will be labelled here as having STATE_PHI_ON, since in general we
// expect values at the threshold to be on the initial zero level
// set.  However, with an isolated pixel, vtk's contouring does not
// generate any contour geometry.  As a result, the computed distance
// at this position will be something larger than the STATE_PHI_ON
// topological tag would lead us to expect.

int cvLevelSetSparseGrid::InitPhi( cvStrPts *img, double thr )
{
  int i, j, k, n;
  double pos[3];
  cvPolyData *front;
  vtkStructuredPoints *vtksp;
  vtkDataArray *vtkdata;
  int numdata;
  float *fdata;
  Image_T *local_img;
  int imgDims[3];
  vtkFloatingPointType fpixDims[3];
  double pixDims[3];
  vtkFloatingPointType vtkorig[3];
  double orig[3];
  int status;
  double intensity;
  double delta;

  // Set topological state {on, out, in} based on data compared to
  // given threshold:

  vtksp = (vtkStructuredPoints*)(img->GetVtkPtr());
  vtkdata = vtksp->GetPointData()->GetScalars();
  if ( vtkdata == NULL ) {
    printf( "ERR: cvStrPts used for lset init needs to have scalar data\n" );
    return CV_ERROR;
  }

  // Copy vtk scalars into a local array so we can make an Image_T
  // which we can then use for bi-/tri-linearly interpolated queries:
  numdata = vtkdata->GetNumberOfTuples();
  fdata = new float [numdata];
  for ( i = 0; i < numdata; i++ ) {
    fdata[i] = (float)(vtkdata->GetTuple1(i));
  }
  vtksp->GetDimensions( imgDims );
  vtksp->GetSpacing( fpixDims );
  vtksp->GetOrigin( vtkorig );
  pixDims[0] = fpixDims[0];
  pixDims[1] = fpixDims[1];
  pixDims[2] = fpixDims[2];
  orig[0] = vtkorig[0] - (pixDims[0] / 2.0);
  orig[1] = vtkorig[1] - (pixDims[0] / 2.0);
  if ( imgDims[2] > 1 ) {
    orig[2] = vtkorig[2] - (pixDims[0] / 2.0);
  } else {
    orig[2] = vtkorig[2];
  }

  // CreateImage allocates its own memory, so we don't need fdata
  // anymore once this call returns:
  local_img = CreateImage( (void*)fdata, numdata, "-float", imgDims, pixDims );
  delete [] fdata;
  if ( local_img == NULL ) {
    printf( "ERR: couldn't create scalar map from given cvStrPts\n" );
    return CV_ERROR;
  }
  SetLowerLeft( local_img, orig );

  for (k = 0; k < K_; k++) {
    for (j = 0; j < J_; j++) {
      for (i = 0; i < I_; i++) {

	n = IJKToDenseIx( i, j, k );

	pos[0] = i * hv_[0] + origin_[0];
	pos[1] = j * hv_[1] + origin_[1];
	pos[2] = k * hv_[2] + origin_[2];

	status = GetIntensity( local_img, pos, &intensity );
	if ( ! status ) {
	  printf( "ERR: image query error\n" );
	  Image_Delete( local_img );
	  return CV_ERROR;
	}

	delta = thr - intensity;
	if ( fabs( delta ) < tol_ ) {
	  (*topoOverlay_)[n] |= STATE_PHI_ON;
	} else if ( delta > 0.0 ) {
	  (*topoOverlay_)[n] |= STATE_PHI_OUTSIDE;
	} else {
	  (*topoOverlay_)[n] |= STATE_PHI_INSIDE;
	}
      }
    }
  }
  Image_Delete( local_img );
  init_ = 1;

  // Extract contour geometry based on given threshold:

  vtkContourFilter *contour = vtkContourFilter::New();
  contour->SetInputDataObject( img->GetVtkPtr() );
  contour->SetValue( 0, (vtkFloatingPointType)thr );
  contour->Update();
  front = new cvPolyData( contour->GetOutput() );
  contour->Delete();

  // Now call InitPhi( cvPolyData* ):

  bandMethod_ = BAND_SWEEP;
  return InitPhi( front );
}


// ------------------
// ClearSeedInterface
// ------------------

void cvLevelSetSparseGrid::ClearSeedInterface()
{
  if ( seedInterface_ != NULL ) {
    delete seedInterface_;
    seedInterface_ = NULL;
  }
  return;
}


// ------------------
// BuildSeedInterface
// ------------------
// - make a bounding box of the logical grid domain
// - make a polygonal surface representation of the given seed
// - intersect these two to produce the portion of the seed surface
//   contained within the logical grid domain... this surface is
//   stored in seedInterface_

int cvLevelSetSparseGrid::BuildSeedInterface( cvSolidModel *seed )
{
  double boxDims[3];
  double boxCtr[3];
  cvPolyData *seedPd;
  double bboxSeed[6];
  double bboxGrid[6];
  int status;

  cvSolidModel *logicalSolid;
  cvSolidModel *seedBound;
  cvSolidModel *seedIntfSolid;

  // First, check to see if this is necessary at all.  If the seed
  // model is entirely contained within the logical grid domain, then
  // we have no need to construct a seed interface because the seed
  // itself gives us seedInterface_.

  boxDims[0] = ( I_ - 1 ) * hv_[0];  // think carefully about the fencepost
  boxDims[1] = ( J_ - 1 ) * hv_[1];  // phenomenon going on here... I_-1
  boxDims[2] = ( K_ - 1 ) * hv_[2];  // IS correct... this is NOT an image

  boxCtr[0] = origin_[0] + boxDims[0] / 2;
  boxCtr[1] = origin_[1] + boxDims[1] / 2;
  boxCtr[2] = origin_[2] + boxDims[2] / 2;

  seedPd = seed->GetPolyData(0,0);
  if ( sys_geom_BBox( seedPd, bboxSeed ) != CV_OK ) {
    delete seedPd;
    return CV_ERROR;
  }
  bboxGrid[0] = origin_[0];
  bboxGrid[1] = origin_[0] + boxDims[0];
  bboxGrid[2] = origin_[1];
  bboxGrid[3] = origin_[1] + boxDims[1];
  bboxGrid[4] = origin_[2];
  bboxGrid[5] = origin_[2] + boxDims[2];

  if ( ( bboxSeed[0] > bboxGrid[0] ) && ( bboxSeed[1] < bboxGrid[1] ) &&
       ( bboxSeed[2] > bboxGrid[2] ) && ( bboxSeed[3] < bboxGrid[3] ) &&
       ( bboxSeed[4] > bboxGrid[4] ) && ( bboxSeed[5] < bboxGrid[5] ) ) {
    seedInterface_ = seedPd;
    return CV_OK;
  }

  logicalSolid = cvSolidModel::DefaultInstantiateSolidModel();
  seedBound = cvSolidModel::DefaultInstantiateSolidModel();
  seedIntfSolid = cvSolidModel::DefaultInstantiateSolidModel();

  if ( ( logicalSolid == NULL ) || ( seedBound == NULL ) ||
       ( seedIntfSolid == NULL ) ) {
    return CV_ERROR;
  }

  ClearSeedInterface();

  if ( dim_ == 2 ) {
    logicalSolid->MakeBox2d( boxDims, boxCtr );
    seedBound->MakePoly2d( seedPd );

  } else if ( dim_ == 3 ) {
    logicalSolid->MakeBox3d( boxDims, boxCtr );
    seedBound->SetPoly3dFacetMethod( SM_Facet_Union );
    seedBound->MakePoly3dSurface( seedPd );

  } else {
    return CV_ERROR;
  }

  status = seedIntfSolid->Intersect( logicalSolid, seedBound );

  delete logicalSolid;
  delete seedPd;
  delete seedBound;

  if ( status != CV_OK ) {
    delete seedIntfSolid;
    return CV_ERROR;
  }

  seedInterface_ = seedIntfSolid->GetPolyData(0,0);

  delete seedIntfSolid;

  if ( seedInterface_ == NULL ) {
    return CV_ERROR;
  }

  return CV_OK;
}


// ---------
// ReinitPhi
// ---------
// This method does "in-place" re-construction of the distance fn,
// i.e. a phi evaluation which uses the current grid structure.

int cvLevelSetSparseGrid::ReinitPhi()
{
  cvPolyData *front;
  cvLevelSetNode *currNode;
  double dist;
  int sign;

  if ( gridState_ < SGST_CSR ) {
    return CV_ERROR;
  }

  front = GetFront();
  if ( front->GetVtkPolyData()->GetNumberOfCells() < 1 ) {
    printf("ERR: Current zero level set has no geometry.\n");
    delete front;
    return CV_ERROR;
  }

  InitIter();
  while ( currNode = GetNext() ) {
    dist = front->FindDistance( currNode->pos_[0], currNode->pos_[1],
				currNode->pos_[2] );
    if ( dist < 0.0 ) {
      delete front;
      return CV_ERROR;
    }
    if ( fabs( currNode->phi_ ) < tol_ ) {
      sign = 0;
    } else if ( currNode->phi_ < 0.0 ) {
      sign = -1;
    } else {
      sign = 1;
    }
    currNode->phi_ = sign * dist;
  }

  delete front;
  return CV_OK;
}


// --------------
// InitHTIterator
// --------------
// Provide a consistent mechanism for iterating through the hash table
// once it has been defined.  This is useful to provide clients with
// an abstract way in which to access hash table entries in a way
// which will always visit entries in the same order.

void cvLevelSetSparseGrid::InitHTIterator()
{
  if ( gridState_ < SGST_HashTable ) {
    return;
  }
  htCurrBucketId_ = 0;
  if ( htIter_ != NULL ) {
    delete htIter_;
  }
  htIter_ = new cvLispListIterator<TableStruct*>(nodeTable_[htCurrBucketId_]);
  return;
}


// -------------
// GetNextHTItem
// -------------

TableStruct *cvLevelSetSparseGrid::GetNextHTItem()
{
  TableStruct *item;

  if ( gridState_ < SGST_HashTable ) {
    return NULL;
  }
  if ( htIter_ == NULL ) {
    return NULL;
  }
  if ( htCurrBucketId_ >= numBuckets_ ) {
    return NULL;
  }

  while ( htIter_->IsDone() ) {
    delete htIter_;
    htIter_ = NULL;
    htCurrBucketId_++;
    if ( htCurrBucketId_ >= numBuckets_ ) {
      return NULL;
    }
    htIter_ = new cvLispListIterator<TableStruct*>(nodeTable_[htCurrBucketId_]);
  }

  item = htIter_->Item();
  htIter_->Next();
  return item;
}


// -----------
// ConstructHT
// -----------

int cvLevelSetSparseGrid::ConstructHT( double ctr[], double radius )
{
  int i, j, k;
  double pos[3];
  double x, y, z;
  double tmp, dist, rsq, sign;
  TableStruct *htEntry;
  int numSparseNodes = 0;
  int logicalIx, hashIx;
  double irsq, orsq;

  if ( gridState_ < SGST_ExtentDefined ) {
    return CV_ERROR;
  }

  DeallocateNodes();

  rsq = radius * radius;
  if ( fabs(innerExtent_) > radius ) {
    irsq = -1.0;
  } else {
    irsq = ( radius - fabs(innerExtent_) ) * ( radius - fabs(innerExtent_) );
  }
  orsq = ( outerExtent_ + radius ) * ( outerExtent_ + radius );

  // Given this indexing scheme, i varies the fastest and k the
  // slowest as we march sequentially through the physical memory of
  // topoOverlay_.  Anyway, march through all nodes in the dense grid,
  // and push any nodes within the band boundaries into the nodeTable_
  // hash table.

  for (k = 0; k < K_; k++) {
    for (j = 0; j < J_; j++) {
      for (i = 0; i < I_; i++) {

	logicalIx = IJKToDenseIx( i, j, k );

	pos[0] = i * hv_[0] + origin_[0];
	pos[1] = j * hv_[1] + origin_[1];
	pos[2] = k * hv_[2] + origin_[2];

	x = pos[0] - ctr[0];
	y = pos[1] - ctr[1];
	z = pos[2] - ctr[2];

	tmp = (x*x) + (y*y) + (z*z);

	// If grid does not contain previous sign data:
	if ( ! init_ ) {

	  // Initialize topological state:
	  (*topoOverlay_)[logicalIx] = 0;

	  // Skip nodes which lie outside the band:
	  if ( tmp > orsq ) {
	    (*topoOverlay_)[logicalIx] |= STATE_PHI_OUTSIDE;
	    continue;
	  }
	  if ( tmp < irsq ) {
	    (*topoOverlay_)[logicalIx] |= STATE_PHI_INSIDE;
	    continue;
	  }

	  dist = sqrt(tmp) - radius;

	  if ( fabs(dist) < tol_ ) {
	    (*topoOverlay_)[logicalIx] |= STATE_PHI_ON;
	  } else if ( tmp < rsq ) {
	    (*topoOverlay_)[logicalIx] |= STATE_PHI_INSIDE;
	  } else {
	    (*topoOverlay_)[logicalIx] |= STATE_PHI_OUTSIDE;
	  }

	} else {

	  // See the comments for topoOverlay_ in cvLevelSetSparseGrid.h.  This
	  // switch DEPENDS on a TopoUpdate method to update
	  // topoOverlay_ before a band is discarded.  We ASSUME that
	  // when we get to this point, topoOverlay_ is consistent,
	  // i.e. any nodes which flipped sign during evolution in the
	  // last band have had their topological states adjusted to
	  // reflect this.  Therefore, we don't need to set a value
	  // for topoOverlay_ here.

	  switch ( (*topoOverlay_)[logicalIx] & STATE_PHI_MASK ) {
	  case STATE_PHI_ON:
	    sign = 0.0;
	    break;
	  case STATE_PHI_INSIDE:
	    sign = -1.0;
	    break;
	  case STATE_PHI_OUTSIDE:
	    sign = 1.0;
	    break;
	  default:
	    assert(0);
	  }
	  dist = sign * fabs( radius - sqrt(tmp) );
	}

	if ( ( innerExtent_ <= dist ) && ( dist <= outerExtent_ ) ) {
	  htEntry = new TableStruct;
	  htEntry->phi_ = dist;
	  htEntry->logicalIx_[0] = i;
	  htEntry->logicalIx_[1] = j;
	  htEntry->logicalIx_[2] = k;
	  htEntry->tag_ = IJKToDenseIx( i, j, k );
	  numSparseNodes++;
	  htEntry->state_ = (*topoOverlay_)[logicalIx];

	  // Test for mine condition and set state accordingly:
	  if ( ( dist < 0.0 ) && ( ( dist - innerExtent_ ) < mineWidth_ ) ||
	       ( dist > 0.0 ) && ( ( outerExtent_ - dist ) < mineWidth_ ) ) {
	    htEntry->state_ |= STATE_MINE;
	  }

	  hashIx = ComputeHashIx( i, j, k );
	  nodeTable_[hashIx]->Insert( htEntry );
	}

      } // i
    }   // j
  }     // k

  numSparseNodes_ = numSparseNodes;
  gridState_ = SGST_HashTable;

  return ConstructCSR();
}


// -----------
// ConstructHT
// -----------
// Initialization with both cvPolyData and cvSolidModel was originally
// combined in this one function.  This was done because we were using
// cvPolyData::FindDistance to compute distances with both surfaces and
// solids.  Things have since changed in the following ways:
//
//   - a distance method has been added to cvSolidModel 
//
//   - distance to cvPolyData has been improved s.t. the result is no
//     longer to vertices of the cvPolyData, but instead to the closest
//     point on the closest cell of the cvPolyData as computed by
//     vtkCellLocator
//
// The important distinction is that distance to a cvSolidModel is
// signed, which we need for level set initialization.  Distances to
// cvPolyData's, on the other hand, are not signed... instead, we use
// these distances together with the sign stored in the topoOverlay_
// array to assign properly signed phi values to grid nodes.

int cvLevelSetSparseGrid::ConstructHT( cvSolidModel *sm )
{
  int i, j, k;
  double pos[3];
  double udist2, dist2;
  double udist, dist;
  int cls;
  TableStruct *htEntry;
  int numSparseNodes = 0;
  int logicalIx, hashIx;
  double distLimit;
  int sign;

  if ( gridState_ < SGST_ExtentDefined ) {
    return CV_ERROR;
  }

  DeallocateNodes();
  if ( BuildSeedInterface( sm ) != CV_OK ) {
    printf("  ERR (cvLevelSetSparseGrid::ConstructHT): BuildSeedInterface error.\n");
    return CV_ERROR;
  }

  // Compute a fairly arbitrary limit on distance calculations.  That
  // is, do not bother to find an exact distance for any points which
  // are > distLimit from the cvSolidModel.  We want distLimit, then, to
  // be something which captures all band members but doesn't waste
  // time on points that will be excluded.  I'm adding in a buffer
  // region of 2*maxh_ to ensure that we catch all band nodes.
  distLimit = maximum( fabs(innerExtent_), fabs(outerExtent_) );
  distLimit += 2 * maxh_;

  for (k = 0; k < K_; k++) {
    for (j = 0; j < J_; j++) {
      for (i = 0; i < I_; i++) {

	logicalIx = IJKToDenseIx( i, j, k );

	pos[0] = i * hv_[0] + origin_[0];
	pos[1] = j * hv_[1] + origin_[1];
	pos[2] = k * hv_[2] + origin_[2];

	(*topoOverlay_)[logicalIx] = 0;

	/*
	// Note that cvSolidModel provides a member, tol_, which is to
	// be used by concrete subclasses as the tolerance for
	// Distance calculations.  tol_ is set in the cvSolidModel
	// constructor as a multiple of machine "epsilon", or the
	// smallest interval which the machine can distinguish.

	if ( sm->Distance( pos, distLimit, &dist ) != CV_OK ) {
	  printf("  ERR (cvLevelSetSparseGrid::ConstructHT): "
		 "cvSolidModel::Distance error.\n");
	  init_ = 0;
	  return CV_ERROR;
	}
	*/

	// In order to use seedInterface_, we want to do the
	// following:
	//   - compute (unsigned) distance to cvPolyData *seedInterface_
	//   - do point classification on cvSolidModel *sm
	//   - combine these to form signed distance

	udist2 = seedInterface_->FindDistance2( pos[0], pos[1], pos[2],
						distLimit );
	if ( udist2 < 0.0 ) {
	  printf( "  ERR (cvLevelSetSparseGrid::ConstructHT): "
		  "cvPolyData::FindDistance error.\n" );
	  init_ = 0;
	  return CV_ERROR;
	}

	if ( sm->ClassifyPt( pos, 0, &cls ) != CV_OK ) {
	  printf( "  ERR (cvLevelSetSparseGrid::ConstructHT): "
		  "cvSolidModel::ClassifyPt error.\n" );
	  init_ = 0;
	  return CV_ERROR;
	}

	if ( cls > 0 ) {
	  dist2 = -1.0 * udist2;
	} else if ( cls == 0 ) {
	  dist2 = 0.0;
	} else {
	  dist2 = udist2;
	}

	sign = IntSign( dist2, tol_ );
	switch (sign) {
	case -1:
	  (*topoOverlay_)[logicalIx] |= STATE_PHI_INSIDE;
	  break;
	case 0:
	  (*topoOverlay_)[logicalIx] |= STATE_PHI_ON;
	  break;
	case 1:
	  (*topoOverlay_)[logicalIx] |= STATE_PHI_OUTSIDE;
	  break;
	default:
	  assert(0);
	}

	if ( ( (-innerExtent2_) <= dist2 ) && ( dist2 <= outerExtent2_ ) ) {

	  udist = sqrt( udist2 );
	  
	  if ( cls > 0 ) {
	    dist = -1.0 * udist;
	  } else if ( cls == 0 ) {
	    dist = 0.0;
	  } else {
	    dist = udist;
	  }

	  htEntry = new TableStruct;
	  htEntry->phi_ = dist;
	  htEntry->logicalIx_[0] = i;
	  htEntry->logicalIx_[1] = j;
	  htEntry->logicalIx_[2] = k;
	  htEntry->tag_ = IJKToDenseIx( i, j, k );
	  numSparseNodes++;
	  htEntry->state_ = (*topoOverlay_)[logicalIx];

	  // Test for mine condition and set state accordingly:
	  if ( ( dist < 0.0 ) && ( ( dist - innerExtent_ ) < mineWidth_ ) ||
	       ( dist > 0.0 ) && ( ( outerExtent_ - dist ) < mineWidth_ ) ) {
	    htEntry->state_ |= STATE_MINE;
	  }

	  hashIx = ComputeHashIx( i, j, k );
	  nodeTable_[hashIx]->Insert( htEntry );
	}

      } // i
    }   // j
  }     // k

  numSparseNodes_ = numSparseNodes;
  gridState_ = SGST_HashTable;

  return ConstructCSR();
}


// -----------
// ConstructHT
// -----------

int cvLevelSetSparseGrid::ConstructHT( cvPolyData *front )
{
  int i, j, k;
  double pos[3];
  double udist2, dist2;
  double udist, dist, sign;
  int cls;
  TableStruct *htEntry;
  int numSparseNodes = 0;
  int logicalIx, hashIx;
  cvSolidModel *sm;
  int result;

  // This block should now be made unreachable by the
  // InitSign_Internal method, which is called from
  // InitPhi(cvPolyData*).

  if ( ! init_ ) {
    sm = cvSolidModel::DefaultInstantiateSolidModel();
    if ( sm == NULL ) {
      return CV_ERROR;
    }
    if ( dim_ == 3 ) {
      sm->MakePoly3dSolid( front , 0 );
    } else if ( dim_ == 2 ) {
      sm->MakePoly2d( front );
    } else {
      assert(0);
    }
    result = ConstructHT( sm );
    delete sm;
    return result;
  }

  // End unreachable block.


  if ( gridState_ < SGST_ExtentDefined ) {
    return CV_ERROR;
  }

  TopoUpdate();
  DeallocateNodes();

  for (k = 0; k < K_; k++) {
    for (j = 0; j < J_; j++) {
      for (i = 0; i < I_; i++) {

	logicalIx = IJKToDenseIx( i, j, k );

	pos[0] = i * hv_[0] + origin_[0];
	pos[1] = j * hv_[1] + origin_[1];
	pos[2] = k * hv_[2] + origin_[2];

	// cvPolyData::FindDistance returns a negative value on error.
	udist2 = front->FindDistance2( pos[0], pos[1], pos[2] );
	if ( udist2 < 0.0 ) {
	  init_ = 0;
	  return CV_ERROR;
	}

	// See the comments for topoOverlay_ in cvLevelSetSparseGrid.h.  This
	// switch DEPENDS on a TopoUpdate method.

	switch ( (*topoOverlay_)[logicalIx] & STATE_PHI_MASK ) {
	case STATE_PHI_ON:
	  sign = 0.0;
	  cls = 0;
	  break;
	case STATE_PHI_INSIDE:
	  sign = -1.0;
	  cls = 1;
	  break;
	case STATE_PHI_OUTSIDE:
	  sign = 1.0;
	  cls = -1;
	  break;
	default:
	  assert(0);
	}

	dist2 = sign * udist2;

	if ( ( (-innerExtent2_) <= dist2 ) && ( dist2 <= outerExtent2_ ) ) {
	  udist = sqrt( udist2 );
	  if ( cls > 0 ) {
	    dist = -1.0 * udist;
	  } else if ( cls == 0 ) {
	    dist = 0.0;
	  } else {
	    dist = udist;
	  }

	  htEntry = new TableStruct;
	  htEntry->phi_ = dist;
	  htEntry->logicalIx_[0] = i;
	  htEntry->logicalIx_[1] = j;
	  htEntry->logicalIx_[2] = k;
	  htEntry->tag_ = IJKToDenseIx( i, j, k );
	  numSparseNodes++;
	  htEntry->state_ = (*topoOverlay_)[logicalIx];

	  // Test for mine condition and set state accordingly:
	  if ( ( dist < 0.0 ) && ( ( dist - innerExtent_ ) < mineWidth_ ) ||
	       ( dist > 0.0 ) && ( ( outerExtent_ - dist ) < mineWidth_ ) ) {
	    htEntry->state_ |= STATE_MINE;
	  }

	  hashIx = ComputeHashIx( i, j, k );
	  nodeTable_[hashIx]->Insert( htEntry );
	}

      } // i
    }   // j
  }     // k

  numSparseNodes_ = numSparseNodes;
  gridState_ = SGST_HashTable;

  return ConstructCSR();
}


// -----------------
// ConstructHT_Sweep
// -----------------
// It is assumed that the given front is the current zero level set.
// Before this method is called, the logically dense topoOverlay_
// array must have been set.  Possible scenarios include:
//
//   - sign initialization (i.e. setting of topoOverlay_) based on
//     threshold of an initial image... subsequent need to construct a
//     sparse grid around the given threshold uses the sign
//     information in topoOverlay_ to drive the sweeping mechanism
//     plus the threshold contour as a geometry to which to compute
//     distances 
//
//   - sign init based on a cvSolidModel --> topoOverlay_ set via point
//     classifications... pass a facetted representation of the
//     cvSolidModel as the zero level set geometry to which to compute
//     distances
//
//   - sign information stored in topoOverlay_ from a level set
//     evolution already in progress (a sparse grid already exists)...
//     given front is the zero level set geometry extracted from the
//     current values of phi on the current grid (which is about to be
//     discarded for a new sparse grid)

int cvLevelSetSparseGrid::ConstructHT_Sweep( cvPolyData *front )
{
  int num = 0;
  int n, numActive;
  int i, j, k;
  double pos[3];
  int hashIx;
  TableStruct *htEntry;
  TableStruct *entryToRm;
  cvSortedList<TableStruct*> *bucket;
  cvSortedList<TableStruct*> *targetBucket;
  double udist, dist;
  int blotDims[3];


  // This should now be impossible:
  if ( ! init_ ) {
    return CV_ERROR;
  }
  // End impossibility.


  if ( gridState_ < SGST_ExtentDefined ) {
    return CV_ERROR;
  }

  TopoUpdate();
  DeallocateNodes();

  Sweep_MarkActiveNodes();
  numActive = topoOverlay_->GetNumActive();
  if ( numActive == 0 ) {
    return CV_ERROR;
  }

  /*
  levels = ceil( fabs( innerExtent_ / minh_ ) );
  if ( Sweep( sign = 1, levels ) != CV_OK ) {
    return CV_ERROR;
  }
  levels = ceil( fabs( outerExtent_ / minh_ ) );
  if ( Sweep( sign = -1, levels ) != CV_OK ) {
    return CV_ERROR;
  }
  */

  blotDims[0] = maximum( ceil( fabs( innerExtent_ / minh_ ) ),
			 ceil( fabs( outerExtent_ / minh_ ) ) ) + 1;
  blotDims[1] = blotDims[0];
  blotDims[2] = blotDims[0];
  if ( Blot( blotDims ) != CV_OK ) {
    return CV_ERROR;
  }

  numSparseNodes_ = 0;

  for (k = 0; k < K_; k++) {
    for (j = 0; j < J_; j++) {
      for (i = 0; i < I_; i++) {

	n = IJKToDenseIx( i, j, k );

	pos[0] = i * hv_[0] + origin_[0];
	pos[1] = j * hv_[1] + origin_[1];
	pos[2] = k * hv_[2] + origin_[2];

	if ( ( (*topoOverlay_)[n] & STATE_NODE_ACTIVE ) ||
	     ( (*topoOverlay_)[n] & STATE_NODE_COVERED ) ) {

	  udist = front->FindDistance( pos[0], pos[1], pos[2] );
	  if ( udist < tol_ ) {
	    udist = 0.0;
	    dist = 0.0;
	    (*topoOverlay_)[n] &= STATE_PHI_ON;
	  } else {

	    // Special case: If the current grid position has something
	    // *other* than an essentially zero distance to the zero
	    // level set geometry, but is also tagged as STATE_PHI_ON,
	    // this means that the pathological case described at the
	    // cvStrPts-based InitPhi method has occurred at the current
	    // grid position.  We may or may not want to exclude these
	    // nodes from the sparse grid.  This determination will have
	    // to be deferred until after the whole hash table has been
	    // filled, when we can tell if individual entries are
	    // actually isolated or not.  For now, we need to include
	    // these nodes, but we also need to do a special operation
	    // to handle sign in order to prevent STATE_PHI_ON from
	    // giving sign == 0 --> dist == 0 (which it is not,
	    // according to the udist measurement).

	    if ( (*topoOverlay_)[n] & STATE_PHI_ON ) {
	      // Determine sign for this position based on an average of
	      // neighbor states.
	      dist = udist * PollNeighborSigns( i, j, k );
	    } else {
	      dist = udist * ( topoOverlay_->GetSign( n ) );
	    }
	  }

	  if ( ( innerExtent_ <= dist ) && ( dist <= outerExtent_ ) ) {
	    htEntry = new TableStruct;
	    htEntry->phi_ = dist;
	    htEntry->logicalIx_[0] = i;
	    htEntry->logicalIx_[1] = j;
	    htEntry->logicalIx_[2] = k;
	    htEntry->tag_ = n;
	    htEntry->state_ = (*topoOverlay_)[n] & STATE_PHI_MASK;

	    // Test for mine condition and set state accordingly:
	    if ( ( dist < 0.0 ) && ( ( dist - innerExtent_ ) < mineWidth_ ) ||
		 ( dist > 0.0 ) && ( ( outerExtent_ - dist ) < mineWidth_ ) ) {
	      htEntry->state_ |= STATE_MINE;
	    }
	    hashIx = ComputeHashIx( i, j, k );
	    nodeTable_[hashIx]->Insert( htEntry );

	    numSparseNodes_++;
	  }
	}
      }
    }
  }

  // Post-process the table and remove any entries which have NO
  // cardinal neighbors:
  for (k = 0; k < K_; k++) {
    for (j = 0; j < J_; j++) {
      for (i = 0; i < I_; i++) {
	if ( ( IJKPresentInHT( i, j, k, &targetBucket, &entryToRm ) ) &&
	     ( ! IJKPresentInHT( i-1, j, k, &bucket, &htEntry ) ) &&
	     ( ! IJKPresentInHT( i+1, j, k, &bucket, &htEntry ) ) &&
	     ( ! IJKPresentInHT( i, j-1, k, &bucket, &htEntry ) ) &&
	     ( ! IJKPresentInHT( i, j+1, k, &bucket, &htEntry ) ) &&
	     ( ! IJKPresentInHT( i, j, k-1, &bucket, &htEntry ) ) &&
	     ( ! IJKPresentInHT( i, j, k+1, &bucket, &htEntry ) ) ) {
	  targetBucket->Remove( entryToRm );
	  numSparseNodes_--;
	}
      }
    }
  }

  gridState_ = SGST_HashTable;

  return ConstructCSR();
}


// -----------------
// PollNeighborSigns
// -----------------

int cvLevelSetSparseGrid::PollNeighborSigns( int i, int j, int k )
{
  int ni, nj, nk, nx;
  int acc = 0.0;

  ni = i-1;
  if ( ni >= 0 ) {
    nx = IJKToDenseIx( ni, j, k );
    acc += topoOverlay_->GetSign(nx);
  }
  ni = i+1;
  if ( ni < (I_-1) ) {
    nx = IJKToDenseIx( ni, j, k );
    acc += topoOverlay_->GetSign(nx);
  }

  nj = j-1;
  if ( nj >= 0 ) {
    nx = IJKToDenseIx( i, nj, k );
    acc += topoOverlay_->GetSign(nx);
  }
  nj = j+1;
  if ( nj < (J_-1) ) {
    nx = IJKToDenseIx( i, nj, k );
    acc += topoOverlay_->GetSign(nx);
  }

  nk = k-1;
  if ( nk >= 0 ) {
    nx = IJKToDenseIx( i, j, nk );
    acc += topoOverlay_->GetSign(nx);
  }
  nk = k+1;
  if ( nk < (K_-1) ) {
    nx = IJKToDenseIx( i, j, nk );
    acc += topoOverlay_->GetSign(nx);
  }

  if ( acc < 0.0 ) {
    return -1;
  } else if ( acc > 0.0 ) {
    return 1;
  } else {
    assert(0);
  }
}


// ---------------------
// Sweep_MarkActiveNodes
// ---------------------

void cvLevelSetSparseGrid::Sweep_MarkActiveNodes()
{
  int i, j, k;
  int currIx;
  int currSign;

  topoOverlay_->ClearActive();
  topoOverlay_->ClearCovered();
  for (k = 0; k < K_; k++) {
    for (j = 0; j < J_; j++) {
      for (i = 0; i < I_; i++) {
	currIx = IJKToDenseIx( i, j, k );
	currSign = topoOverlay_->GetSign( currIx );
	if ( currSign == 0 ) {
	  topoOverlay_->SetActive( currIx );
	  continue;
	}
	Sweep_ComparePair( currIx, currSign, i-1, j, k );
	Sweep_ComparePair( currIx, currSign, i+1, j, k );
	Sweep_ComparePair( currIx, currSign, i, j-1, k );
	Sweep_ComparePair( currIx, currSign, i, j+1, k );
	Sweep_ComparePair( currIx, currSign, i, j, k-1 );
	Sweep_ComparePair( currIx, currSign, i, j, k+1 );
      }
    }
  }
  return;
}


// -----------------
// Sweep_ComparePair
// -----------------

void cvLevelSetSparseGrid::Sweep_ComparePair( int currIx, int currSign,
				    int ip, int jp, int kp )
{
  int neighborIx;
  int neighborSign;

  if ( IxWithinExtent( ip, jp, kp ) ) {
    neighborIx = IJKToDenseIx( ip, jp, kp );
    neighborSign = topoOverlay_->GetSign( neighborIx );
    if ( ( fabs( 1.0 * currSign * neighborSign ) > 0 ) &&
	 ( currSign != neighborSign ) ) {
      topoOverlay_->SetActive( currIx );
      topoOverlay_->SetActive( neighborIx );
    }
  }
  return;
}


// -----
// Sweep
// -----

int cvLevelSetSparseGrid::Sweep( int sign, int levels )
{
  int i, j, k;
  int n, ncnt;
  int l;
  int currIx, neighborIx;
  int neighborSign;
  int adjIxs[6];
  int status;

  if ( ( sign != 1 ) && ( sign != -1 ) ) {
    return CV_ERROR;
  }

  if ( levels < 1 ) {
    return CV_ERROR;
  }

  // We might think about allocating a sweep buffer at the time of
  // band parameter specification, which eliminates redundant dynamic
  // allocation.

  int buffer_sz = pow( 2.0*levels + 1, 3 );
  int *buffer = new int [buffer_sz];
  cvIntArrayList ixList( buffer, buffer_sz );

  for (k = 0; k < K_; k++) {
    for (j = 0; j < J_; j++) {
      for (i = 0; i < I_; i++) {
	currIx = IJKToDenseIx( i, j, k );
	if ( ! ( (*topoOverlay_)[currIx] & STATE_NODE_ACTIVE ) ) {
	  continue;
	}
	l = 0;
	ncnt = 0;
	topoOverlay_->ClearSwept();
	ixList.Reset();
	ixList.Append( currIx );
	while ( ( ! ( ixList.IsEmpty() ) ) && ( l < levels ) ) {
	  status = ixList.RemoveFromHead( &currIx );
	  if ( status != CV_OK ) {
	    printf( "ERR: index list access error\n" );
	    delete [] buffer;
	    return CV_ERROR;
	  }
	  GetAdjacentIxs( currIx, adjIxs );
	  for (n = 0; n < 6; n++) {
	    neighborIx = adjIxs[n];
	    if ( neighborIx < 0 ) {
	      continue;
	    }
	    neighborSign = topoOverlay_->GetSign( neighborIx );
	    if ( ( neighborSign == sign ) &&
		 ( ! ( (*topoOverlay_)[neighborIx] & STATE_NODE_SWEPT ) ) ) {
	      topoOverlay_->SetSwept( neighborIx );
	      status = ixList.Append( neighborIx );
	      if ( status != CV_OK ) {
		printf( "ERR: index list overflow\n" );
		delete [] buffer;
		return CV_ERROR;
	      }
	    }
	  }
	  ncnt += 6;
	  if ( ncnt == pow( 6.0, l+1 ) ) {
	    l++;
	  }
	} // end sweep from current active node

	topoOverlay_->SweptToCovered();
      } // end active node iterator loop
    }
  }
  delete [] buffer;
  return CV_OK;
}


// ----
// Blot
// ----

int cvLevelSetSparseGrid::Blot( int dim[3] )
{
  int i, j, k;
  int a, b, c;
  int currIx, blotIx;

  // Foreach active node:
  for (k = 0; k < K_; k++) {
    for (j = 0; j < J_; j++) {
      for (i = 0; i < I_; i++) {
	currIx = IJKToDenseIx( i, j, k );
	if ( ! ( (*topoOverlay_)[currIx] & STATE_NODE_ACTIVE ) ) {
	  continue;
	}

	// Blot (i.e. mark as covered) a volume of size 2*dim[0]+1 x
	// 2*dim[1]+1 x 2*dim[2]+1:
	for (c = (k-dim[2]); c < (k+dim[2]); c++) {
	  if ( ( c < 0 ) || ( c >= K_ ) ) {
	    continue;
	  }
	  for (b = (j-dim[1]); b < (j+dim[1]); b++) {
	    if ( ( b < 0 ) || ( b >= J_ ) ) {
	      continue;
	    }
	    for (a = (i-dim[0]); a < (i+dim[0]); a++) {
	      if ( ( a < 0 ) || ( a >= I_ ) ) {
		continue;
	      }

	      // Neighbor is inside grid domain:
	      blotIx = IJKToDenseIx( a, b, c );
	      (*topoOverlay_)[blotIx] |= STATE_NODE_COVERED;
	      
	    }
	  }
	}
      }
    }
  }
  return CV_OK;
}


// ------------
// ConstructCSR
// ------------
// This method needs to be preceded by a call to a ConstructHT
// method.  Once the hash table has been built, this method then
// builds the adjacency matrix in adjIa_ and adjJa_.  We also allocate
// the array of cvLevelSetNode's here, which parallels adjIa_.

int cvLevelSetSparseGrid::ConstructCSR()
{
  int arrPos;
  int i, j, k;
  TableStruct *currItem;
  int edgeCnt;
  int n, offset;
  cvLevelSetNode *currNode;
  int xprevIx, xnextIx, yprevIx, ynextIx, zprevIx, znextIx;

  if ( gridState_ < SGST_HashTable ) {
    return CV_ERROR;
  }

  DeallocateCSR();
  grid_ = new cvLevelSetNode [numSparseNodes_];
  //  color_ = new int [numSparseNodes_];
  if ( grid_ == NULL ) {
    DeallocateCSR();
    return CV_ERROR;
  }

  // Now go back through hash table and fill out the cvLevelSetNode list:
  //   - phi_
  //   - (i_,j_,k_)
  //   - index_     ... i.e. index into grid_
  //   - pos_
  //   - state_
  //   - xPrevIndex_, yPrevIndex_, zPrevIndex_
  //   - xNextIndex_, yNextIndex_, zNextIndex_

  arrPos = 0;
  InitHTIterator();
  while ( currItem = GetNextHTItem() ) {
    grid_[arrPos].phi_ = currItem->phi_;
    i = currItem->logicalIx_[0];
    j = currItem->logicalIx_[1];
    k = currItem->logicalIx_[2];
    grid_[arrPos].i_ = i;
    grid_[arrPos].j_ = j;
    grid_[arrPos].k_ = k;
    grid_[arrPos].logicalIx_ = IJKToDenseIx( i, j, k );
    grid_[arrPos].pos_[0] = i*hv_[0] + origin_[0];
    grid_[arrPos].pos_[1] = j*hv_[1] + origin_[1];
    grid_[arrPos].pos_[2] = k*hv_[2] + origin_[2];
    grid_[arrPos].state_ = 0;
    if ( currItem->state_ & STATE_MINE ) {
      grid_[arrPos].state_ |= CV_NODE_MINE;
    }
    arrPos++;
  }

  // Sort grid_ on cvLevelSetNode::logicalIx_:
  qsort( (void *)grid_, numSparseNodes_, sizeof(cvLevelSetNode), cvLevelSetNodeCompareFn );

  // *Now* set cvLevelSetNode::index_:
  n = 0;
  InitIter();
  while ( currNode = GetNext() ) {
    currNode->index_ = n;
    n++;
  }

  gridState_ = SGST_GridAllocated;

  // *Now* set neighbor info:
  edgeCnt = 0;
  InitIter();
  while ( currNode = GetNext() ) {
    edgeCnt += SetNeighborInfo( currNode );
  }

  // Debug:
  /*
  if ( SanityCheckCSR() != CV_OK ) {
    printf("ERR: sanity check failed\n");
    return CV_ERROR;
  }
  */

  // edgeCnt should now be equal to twice the number of edges in the
  // graph, which is what we need in order to allocate a METIS-style
  // adjacency matrix, i.e. one in which both symmetric matrix halves
  // are stored.

  if ( (edgeCnt % 2) != 0 ) {
    DeallocateCSR();
    return CV_ERROR;
  }
  numSparseEdges_ = edgeCnt / 2;
  adjIa_ = new int [numSparseNodes_ + 1];
  adjJa_ = new int [edgeCnt];
  iaSz_ = numSparseNodes_ + 1;
  jaSz_ = edgeCnt;

  if ( (adjIa_ == NULL) || (adjJa_ == NULL) ) {
    DeallocateCSR();
    return CV_ERROR;
  }

  // The reason I'm not using the cvLevelSetNode iterator in the following loop
  // is just because we're accessing the adjIa_ array which is
  // parallel to grid_ so we need an incrementing index anyway.
  offset = 0;
  for ( n = 0; n < numSparseNodes_; n++ ) {
    adjIa_[n] = offset;
    currNode = &(grid_[n]);
    assert( currNode->index_ == n );
    xprevIx = currNode->xPrevIndex_;
    xnextIx = currNode->xNextIndex_;
    yprevIx = currNode->yPrevIndex_;
    ynextIx = currNode->yNextIndex_;
    zprevIx = currNode->zPrevIndex_;
    znextIx = currNode->zNextIndex_;
    if ( xprevIx != currNode->index_ ) {
      adjJa_[offset] = xprevIx;
      offset++;
    }
    if ( xnextIx != currNode->index_ ) {
      adjJa_[offset] = xnextIx;
      offset++;
    }
    if ( yprevIx != currNode->index_ ) {
      adjJa_[offset] = yprevIx;
      offset++;
    }
    if ( ynextIx != currNode->index_ ) {
      adjJa_[offset] = ynextIx;
      offset++;
    }
    if ( zprevIx != currNode->index_ ) {
      adjJa_[offset] = zprevIx;
      offset++;
    }
    if ( znextIx != currNode->index_ ) {
      adjJa_[offset] = znextIx;
      offset++;
    }
  }
  adjIa_[n] = offset;
  assert( offset == edgeCnt );

  gridState_ = SGST_CSR;

  InitIxBuffer();
  InitNodeSets();

  return CV_OK;
}


// --------------------
// GetNeighborSparseIxs
// --------------------

int cvLevelSetSparseGrid::GetNeighborSparseIxs( cvLevelSetNode *node, int ixs[] )
{
  cvLevelSetNode *neighbor;
  int num = 0;
  int i, j, k;

  i = node->i_;
  j = node->j_;
  k = node->k_;

  // Neighbors in x direction:
  if ( ( neighbor = IJKPresentInCSR( i-1, j, k ) ) != NULL ) {
    ixs[0] = neighbor->index_;
    num++;
  } else {
    ixs[0] = -1;
  }
  if ( ( neighbor = IJKPresentInCSR( i+1, j, k ) ) != NULL ) {
    ixs[1] = neighbor->index_;
    num++;
  } else {
    ixs[1] = -1;
  }

  // Neighbors in y direction:
  if ( ( neighbor = IJKPresentInCSR( i, j-1, k ) ) != NULL ) {
    ixs[2] = neighbor->index_;
    num++;
  } else {
    ixs[2] = -1;
  }
  if ( ( neighbor = IJKPresentInCSR( i, j+1, k ) ) != NULL ) {
    ixs[3] = neighbor->index_;
    num++;
  } else {
    ixs[3] = -1;
  }

  // Neighbors in z direction:
  if ( ( neighbor = IJKPresentInCSR( i, j, k-1 ) ) != NULL ) {
    ixs[4] = neighbor->index_;
    num++;
  } else {
    ixs[4] = -1;
  }
  if ( ( neighbor = IJKPresentInCSR( i, j, k+1 ) ) != NULL ) {
    ixs[5] = neighbor->index_;
    num++;
  } else {
    ixs[5] = -1;
  }

  return num;
}


// --------------
// SanityCheckCSR
// --------------

int cvLevelSetSparseGrid::SanityCheckCSR()
{
  cvLevelSetNode *currNode, *neighbor;
  int i, j, k;
  int m, n;
  int nixs[6], nnixs[6];
  int edgeCnt = 0;
  int foundMatch;
  int a, b;
  int *edgeArr;
  int cnt;
  int status;

  if ( grid_ == NULL ) {
    return CV_ERROR;
  }

  edgeArr = new int [numSparseNodes_ * numSparseNodes_];
  for ( i = 0; i < numSparseNodes_; i++ ) {
    for ( j = 0; j < numSparseNodes_; j++ ) {
      edgeArr[i*numSparseNodes_+j] = 0;
    }
  }

  InitIter();
  while ( currNode = GetNext() ) {
    i = currNode->i_;
    j = currNode->j_;
    k = currNode->k_;
    a = currNode->index_;
    if ( ( i < 0 ) || ( i >= I_ ) ||
	 ( j < 0 ) || ( j >= J_ ) ||
	 ( k < 0 ) || ( k >= K_ ) ) {
      printf("ERR: (i,j,k) out of range (%d,%d,%d)\n", i, j, k);
      delete [] edgeArr;
      return CV_ERROR;
    }
    if ( IJKPresentInCSR( i, j, k ) == NULL ) {
      printf("ERR: self-lookup in CSR failed\n");
      delete [] edgeArr;
      return CV_ERROR;
    }
    if( currNode->index_ != IJKToIndex( i, j, k ) ) {
      printf("ERR: sparse index mismatch\n");
      delete [] edgeArr;
      return CV_ERROR;
    }
    if( currNode->logicalIx_ != IJKToDenseIx( i, j, k ) ) {
      printf("ERR: logical index mismatch\n");
      delete [] edgeArr;
      return CV_ERROR;
    }

    GetNeighborSparseIxs( currNode, nixs );

    // Foreach neighbor m of currNode:
    for ( m = 0; m < 6; m++ ) {
      if ( nixs[m] < 0 ) {
	continue;
      }
      b = nixs[m];
      edgeArr[a*numSparseNodes_+b] += 1;
      if ( ( a*numSparseNodes_+b ) == 798342 ) {
	printf("edgeArr[a*numSparseNodes_+b] --> %d\n",
	       edgeArr[a*numSparseNodes_+b]);
      }

      // Search m's neighbor's for the connection back to currNode:
      foundMatch = 0;
      GetNeighborSparseIxs( &(grid_[b]), nnixs );
      for ( n = 0; n < 6; n++ ) {
	if ( nnixs[n] == a ) {
	  foundMatch = 1;
	  break;
	}
      }

      // If our search on m failed, this is a bad connection:
      if ( ! foundMatch ) {
	printf("ERR: no matching connection for %d --> %d\n", a, nixs[m]);
	delete [] edgeArr;
	return CV_ERROR;
      }
    }

    // Neighbors in x direction:
    if ( ( neighbor = IJKPresentInCSR( i-1, j, k ) ) != NULL ) {
      if ( ( neighbor->i_ != (i-1) ) ||
	   ( neighbor->j_ != j ) ||
	   ( neighbor->k_ != k ) ) {
	printf("ERR: index inconsistency\n");
	delete [] edgeArr;
	return CV_ERROR;
      }
      edgeCnt++;
    }

    if ( ( neighbor = IJKPresentInCSR( i+1, j, k ) ) != NULL ) {
      if ( ( neighbor->i_ != (i+1) ) ||
	   ( neighbor->j_ != j ) ||
	   ( neighbor->k_ != k ) ) {
	printf("ERR: index inconsistency\n");
	delete [] edgeArr;
	return CV_ERROR;
      }
      edgeCnt++;
    }

    // Neighbors in y direction:
    if ( ( neighbor = IJKPresentInCSR( i, j-1, k ) ) != NULL ) {
      if ( ( neighbor->i_ != i ) ||
	   ( neighbor->j_ != (j-1) ) ||
	   ( neighbor->k_ != k ) ) {
	printf("ERR: index inconsistency\n");
	delete [] edgeArr;
	return CV_ERROR;
      }
      edgeCnt++;
    }
    if ( ( neighbor = IJKPresentInCSR( i, j+1, k ) ) != NULL ) {
      if ( ( neighbor->i_ != i ) ||
	   ( neighbor->j_ != (j+1) ) ||
	   ( neighbor->k_ != k ) ) {
	printf("ERR: index inconsistency\n");
	delete [] edgeArr;
	return CV_ERROR;
      }
      edgeCnt++;
    }

    // Neighbors in z direction:
    if ( ( neighbor = IJKPresentInCSR( i, j, k-1 ) ) != NULL ) {
      if ( ( neighbor->i_ != i ) ||
	   ( neighbor->j_ != j ) ||
	   ( neighbor->k_ != (k-1) ) ) {
	printf("ERR: index inconsistency\n");
	delete [] edgeArr;
	return CV_ERROR;
      }
      edgeCnt++;
    }
    if ( ( neighbor = IJKPresentInCSR( i, j, k+1 ) ) != NULL ) {
      if ( ( neighbor->i_ != i ) ||
	   ( neighbor->j_ != j ) ||
	   ( neighbor->k_ != (k+1) ) ) {
	printf("ERR: index inconsistency\n");
	delete [] edgeArr;
	return CV_ERROR;
      }
      edgeCnt++;
    }
  }

  status = CV_OK;
  cnt = 0;
  for ( i = 0; i < numSparseNodes_; i++ ) {
    for ( j = 0; j < numSparseNodes_; j++ ) {
      cnt += edgeArr[i*numSparseNodes_+j];
      if ( edgeArr[i*numSparseNodes_+j] == 0 ) {
	continue;
      }
      if ( edgeArr[i*numSparseNodes_+j] == 1 ) {
	if ( edgeArr[j*numSparseNodes_+i] != 1 ) {
	  printf("ERR: odd # references to edge %d --> %d\n", i, j);
	  status = CV_ERROR;
	}
      }
      if ( edgeArr[i*numSparseNodes_+j] > 1 ) {
	printf("ERR: unexpected # references to edge %d --> %d\n", i, j);
	status = CV_ERROR;
      }
    }
  }

  delete [] edgeArr;
  if ( status != CV_OK ) {
    return CV_ERROR;
  }

  printf("OK: %d connections checked\n", edgeCnt);
  return CV_OK;
}


// ----------
// TopoUpdate
// ----------
// Update the state stored in the logically-dense topoOverlay_ array
// to reflect the current sign of phi at each logical grid position.

void cvLevelSetSparseGrid::TopoUpdate()
{
  cvLevelSetNode *currNode;
  int logicalIx;
  int sign;

  if ( gridState_ < SGST_CSR ) {
    return;
  }

  InitIter();
  while ( currNode = GetNext() ) {
    logicalIx = IJKToDenseIx( currNode->i_, currNode->j_, currNode->k_ );
    (*topoOverlay_)[logicalIx] &= ~STATE_PHI_MASK; // reset phi bits only to 0
    sign = IntSign( currNode->phi_, tol_ );
    switch (sign) {
    case -1:
      (*topoOverlay_)[logicalIx] |= STATE_PHI_INSIDE;
      break;
    case 0:
      (*topoOverlay_)[logicalIx] |= STATE_PHI_ON;
      break;
    case 1:
      (*topoOverlay_)[logicalIx] |= STATE_PHI_OUTSIDE;
      break;
    default:
      assert(0);
    }
  }
}


// --------------
// IJKPresentInHT
// --------------
// Doing a more efficient search through these lists would be a
// pain...

int cvLevelSetSparseGrid::IJKPresentInHT( int i, int j, int k,
				cvSortedList<TableStruct*> **bucket,
				TableStruct **entry )
{
  int hashIx;
  cvSortedList<TableStruct*> *currList;
  TableStruct *htEntry;
  int tagToMatch;
  int tag;
  cvLispListIterator<TableStruct*> *iter;

  (*bucket) = NULL;
  (*entry) = NULL;

  if ( (i < 0) || (i >= I_) ||
       (j < 0) || (j >= J_) ||
       (k < 0) || (k >= K_) ) {
    return 0;
  }

  hashIx = ComputeHashIx( i, j, k );
  tagToMatch = IJKToDenseIx( i, j, k );
  currList = nodeTable_[hashIx];
  (*bucket) = currList;

  iter = new cvLispListIterator<TableStruct*>(currList);
  for ( ; !iter->IsDone(); iter->Next() ) {
    htEntry = iter->Item();
    tag = htEntry->tag_;
    if ( tag < tagToMatch ) {
      continue;
    } else if ( tag == tagToMatch ) {
      (*entry) = htEntry;
      delete iter;
      return 1;
    } else {
      delete iter;
      return 0;
    }
  }
  delete iter;
  return 0;
}


// --------------------
// TableStructCompareFn
// --------------------
// This function is to be passed to the hash table list class as the
// function to use in sorting table entries (i.e. TableStruct*'s).

int TableStructCompareFn( TableStruct *a, TableStruct *b )
{
  int tagA, tagB;

  tagA = a->tag_;
  tagB = b->tag_;

  if ( tagA < tagB ) return -1;
  if ( tagA == tagB ) return 0;
  return 1;
}


// ---------------
// SetNeighborInfo
// ---------------
// We are going to assume that this is ONLY called from a
// ConstructBand method AFTER all member nodes have been pushed into
// nodeTable_.  Then, inside this method, we're going to search
// nodeTable_ for neighbors by (i,j,k), exploiting the way in which
// nodeTable_ was populated... i.e. hashed on (i,j) with k inserted
// into its corresponding bucket IN SORTED ORDER.  There are six
// neighbors to search for.  If we don't find a particular neighbor,
// then store node's index_ instead.

int cvLevelSetSparseGrid::SetNeighborInfo( cvLevelSetNode *node )
{
  int i, j, k;
  cvLevelSetNode *neighbor;
  int cnt = 0;

  i = node->i_;
  j = node->j_;
  k = node->k_;

  // Neighbors in x direction:
  if ( ( neighbor = IJKPresentInCSR( i-1, j, k ) ) != NULL ) {
    node->xPrevIndex_ = neighbor->index_;
    cnt++;
  } else {
    node->xPrevIndex_ = node->index_;
  }
  if ( ( neighbor = IJKPresentInCSR( i+1, j, k ) ) != NULL ) {
    node->xNextIndex_ = neighbor->index_;
    cnt++;
  } else {
    node->xNextIndex_ = node->index_;
  }

  // Neighbors in y direction:
  if ( ( neighbor = IJKPresentInCSR( i, j-1, k ) ) != NULL ) {
    node->yPrevIndex_ = neighbor->index_;
    cnt++;
  } else {
    node->yPrevIndex_ = node->index_;
  }
  if ( ( neighbor = IJKPresentInCSR( i, j+1, k ) ) != NULL ) {
    node->yNextIndex_ = neighbor->index_;
    cnt++;
  } else {
    node->yNextIndex_ = node->index_;
  }

  // Neighbors in z direction:
  if ( ( neighbor = IJKPresentInCSR( i, j, k-1 ) ) != NULL ) {
    node->zPrevIndex_ = neighbor->index_;
    cnt++;
  } else {
    node->zPrevIndex_ = node->index_;
  }
  if ( ( neighbor = IJKPresentInCSR( i, j, k+1 ) ) != NULL ) {
    node->zNextIndex_ = neighbor->index_;
    cnt++;
  } else {
    node->zNextIndex_ = node->index_;
  }

  return cnt;
}


// -------------
// IJKToSparseIx
// -------------

int cvLevelSetSparseGrid::IJKToSparseIx( int i, int j, int k )
{
  int baseIx_2, boundIx_2, range_2, searchIx_2, offset_2;
  int targetIx_1, currIx_1;
  cvLevelSetNode *currNode;

  if ( gridState_ < SGST_GridAllocated ) {
    return -1;
  }

  baseIx_2 = 0;
  boundIx_2 = numSparseNodes_;
  range_2 = boundIx_2 - baseIx_2;
  searchIx_2 = baseIx_2 + range_2 / 2;
  targetIx_1 = IJKToDenseIx( i, j, k );

  while ( ( range_2 > 1 ) && ( searchIx_2 < numSparseNodes_ ) ) {
    currNode = &(grid_[searchIx_2]);
    currIx_1 = IJKToDenseIx( currNode->i_, currNode->j_, currNode->k_ );
    if ( currIx_1 == targetIx_1 ) {
      return currNode->index_;
    } else if ( currIx_1 < targetIx_1 ) {
      baseIx_2 = searchIx_2;
    } else {
      boundIx_2 = searchIx_2;
    }
    range_2 = boundIx_2 - baseIx_2;
    offset_2 = range_2 / 2;
    searchIx_2 = baseIx_2 + offset_2;
  }

  if ( range_2 == 1 ) {
    currNode = &(grid_[baseIx_2]);
    currIx_1 = IJKToDenseIx( currNode->i_, currNode->j_, currNode->k_ );
    if ( currIx_1 == targetIx_1 ) {
      return currNode->index_;
    }
    if ( boundIx_2 > numSparseNodes_ ) {
      assert(0);
    } else if ( boundIx_2 == numSparseNodes_ ) {
      return -1;
    } else {
      currNode = &(grid_[boundIx_2]);
      currIx_1 = IJKToDenseIx( currNode->i_, currNode->j_, currNode->k_ );
      if ( currIx_1 == targetIx_1 ) {
	return currNode->index_;
      }
    }
  }

  return -1;
}


// ---------------
// IJKPresentInCSR
// ---------------
// Used by GetFront and GetCSRStructure to determine which cells have
// the appropriate neighbors to indicate voxel creation.  Use a binary
// search scheme since we know grid_ cvLevelSetNode's are in index_ / hash table
// tag / global sparse index order.

cvLevelSetNode *cvLevelSetSparseGrid::IJKPresentInCSR( int i, int j, int k )
{
  int ix;

  if ( (i < 0) || (i >= I_) ||
       (j < 0) || (j >= J_) ||
       (k < 0) || (k >= K_) ) {
    return NULL;
  }

  ix = IJKToSparseIx( i, j, k );
  if ( ix < 0 ) {
    return NULL;
  }
  return &(grid_[ix]);
}


// --------
// ProjectV
// --------

int cvLevelSetSparseGrid::ProjectV( int save )
{
  if ( gridState_ < SGST_CSR ) {
    return CV_ERROR;
  }

  // If the front has left the grid everywhere, return an error:
  if ( NumActiveNodes() == 0 ) {
    return CV_ERROR;
  }

  ClearCovered();  // turns covered bit off in cvLevelSetNode::state_
  ResetNodeSets();

  // ProjectV( int ) returns an error if the "fan-out list" did not
  // terminate or otherwise behave as expected.  First we project in
  // the plus-phi direction (outwards from front), and then we project
  // in the minus-phi direction (inwards from front).

  if ( this->cvLevelSetStructuredGrid::ProjectV( 1, save ) != CV_OK ) {
    return CV_ERROR;
  }
  if ( this->cvLevelSetStructuredGrid::ProjectV( 0, save ) != CV_OK ) {
    return CV_ERROR;
  }

  // Now, if projection did not achieve full coverage, then
  // reinitialize phi and try again:

  if ( ! CheckProjectCoverage() ) {  // inherited from cvLevelSetStructuredGrid
    ReinitPhi();
    ClearCovered();
    if ( this->cvLevelSetStructuredGrid::ProjectV( 1, save ) != CV_OK ) {
      return CV_ERROR;
    }
    if ( this->cvLevelSetStructuredGrid::ProjectV( 0, save ) != CV_OK ) {
      return CV_ERROR;
    }

    // At this point, if full coverage has still not been achieved, we
    // can continue without error.  Notice the fundamental difference
    // here compared to the situation in cvLevelSetDenseGrid.  In cvLevelSetDenseGrid,
    // failure to achieve full coverage at this point indicates that
    // the front has either left the domain everywhere, or there is an
    // error in the projection algorithm.  In cvLevelSetSparseGrid, on the other
    // hand, this means that the front has left the domain in only
    // *some* places.  Since we're already trapping the condition of
    // the front having left the grid everywhere (see test of
    // NumActiveNodes at top), we know that the front exists
    // somewhere.  As a result, we will take failure to achieve full
    // coverage at this point as an indication that the front has left
    // some (_disjoint_) parts of the grid, which will now ignore (see
    // modifications to UpdatePhi).

  }

  return CV_OK;
}


// ------------
// InitIxBuffer
// ------------
// Allocate the int array which is used to contain the recursively
// built and serviced velocity projection tree.  This single array is
// meant to be allocated once per sparse grid construction, since the
// number of nodes is constant between constructions.

int cvLevelSetSparseGrid::InitIxBuffer()
{
  if ( gridState_ < SGST_CSR ) {
    return CV_ERROR;
  }
  if ( ixBuffer_ != NULL ) {
    delete [] ixBuffer_;
  }
  ixBuffer_ = new int [numSparseNodes_];
  ixBufferSz_ = numSparseNodes_;
  return CV_OK;
}


// ------------
// InitNodeSets
// ------------

int cvLevelSetSparseGrid::InitNodeSets()
{
  if ( gridState_ < SGST_CSR ) {
    return CV_ERROR;
  }
  if ( nodeSets_ != NULL ) {
    delete [] nodeSets_;
  }
  nodeSetsSize_ = 2 * numSparseNodes_;
  nodeSets_ = new int [nodeSetsSize_];
  nodeSetsPos_ = 0;
  return CV_OK;
}


// -------
// ExtendV
// -------
// I doubt this will ever be used again.

void cvLevelSetSparseGrid::ExtendV()
{
  return;
}


// ---------
// UpdatePhi
// ---------
// Overriding the default implementation of this virtual method in
// order to provide cvLevelSetSparseGrid-specific behavior w.r.t. mine hit
// condition.

int cvLevelSetSparseGrid::UpdatePhi()
{

  /* Old as of 2/19/00:
   * ---

  cvLevelSetNode *currNode;
  double f0Contrib, f1Contrib;
  double maxVal, minVal;
  double tmp, f1MagGradPhi;
  double phi_t;
  double v;
  double dt;
  double geodesicTerm;
  int sign_T, sign_Tp;
  cvPolyData *front;

  FindD0();
  FindDelPlus();
  FindDelMinus();
  FindN();

  dt = ComputeDt( factor );

  mineHit_ = 0;
  InitIter();
  while ( currNode = GetNext() ) {

    // The following exclusions are part of why we've re-implemented
    // UpdatePhi for cvLevelSetSparseGrid.  By skipping nodes that we not
    // covered by the projection phase, we basically avoid computing
    // on nodes which we are assuming lie in disjoint sections of the
    // sparse grid where there is no longer any zero level set
    // geometry (i.e. phi is either >0 or <0 everywhere in the
    // disjoint section).
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

    // Keeping Fapplied_ lets us make GetMaxF reflective of actual
    // velocities applied:
    currNode->Fapplied_ = (vtkFloatingPointType)(f0Contrib + f1Contrib);

    // Also use central diff's with the geodesic term (see Caselles,
    // Kimmel, Sapiro, "Geodesic active contours," Int'l J Computer
    // Vision, 22(1), pp. 61-79, 1997, equation 19).

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

    sign_T = IntSign( currNode->phi_, tol_ );

    currNode->deltaPhi_ = dt * phi_t;
    currNode->phi_ += currNode->deltaPhi_;

    if ( currNode->state_ & CV_NODE_MINE ) {
      sign_Tp = IntSign( currNode->phi_, tol_ );
      if ( sign_T != sign_Tp ) {
	mineHit_ = 1;
      }
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

  * ---
  */


  cvLevelSetNode *currNode;
  int sign_t, sign_tn;
  cvPolyData *front;

  if ( ! deltaPhiValid_ ) {
    printf("ERR: unexpected call to UpdatePhi\n");
    return CV_ERROR;
  }

  mineHit_ = 0;
  InitIter();
  while ( currNode = GetNext() ) {
    if ( ! ( currNode->state_ & CV_NODE_COVERED ) &&
	 ! ( currNode->state_ & CV_NODE_ACTIVE ) ) {
      continue;
    }
    sign_t = IntSign( currNode->phi_, tol_ );
    currNode->phi_ += currNode->deltaPhi_;
    if ( currNode->state_ & CV_NODE_MINE ) {
      sign_tn = IntSign( currNode->phi_, tol_ );
      if ( sign_t != sign_tn ) {
	mineHit_ = 1;
	break;
      }
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
  closedPhiVtkValid_ = 0;

  // If we hit a mine node, then rebuild the grid structure.  Note
  // that we call cvLevelSetStructuredGrid::GetFront, which in turn calls the
  // virtual method MakePhiVtk, which will only get the current front
  // if we've waited until after setting phiVtkValid_ back to 0.
  if ( mineHit_ ) {
    UndoTimeStep();
    front = GetFront();
    bandMethod_ = BAND_SWEEP;
    if ( InitPhi( front ) != CV_OK ) {
      return CV_ERROR;
    }
  }

  return CV_OK;
}


// ------------
// UndoTimeStep
// ------------

void cvLevelSetSparseGrid::UndoTimeStep()
{
  cvLevelSetNode *currNode;

  InitIter();
  while ( currNode = GetNext() ) {
    currNode->phi_ -= currNode->deltaPhi_;
  }
  return;
}


/*
// --------
// FindEdge
// --------
// The purpose of this method is to find the grid edge on which a
// given position lies.  The assumption is that this position was
// generated by a front extraction, and as such, it must lie on an
// edge.  The reason that we need to do this is to enable the
// interpolation of nodal values (e.g. K, n) by cvLevelSetVelocity objects
// during velocity evaluation.  Note that this is inefficient from the
// standpoint of the fact that all front-straddling edges were already
// traversed during front extraction, and that by doing additional
// computation to re-find those edges is wasteful.  The argument in
// favor of maintaining this strategy centers on software engineering
// costs.  Front extraction is already a well-defined, well-optimized,
// well-encapsulated function (in vtk).  In order to integrate nodal
// value interpolation into the front extraction process would incur
// the non-trivial expense of re-implementing contouring algorithms
// within the context of cvLevelSetStructuredGrid.

int cvLevelSetSparseGrid::FindEdge( double pos[], cvLevelSetNode **a, cvLevelSetNode **b, char *dir )
{
  return CV_ERROR;
}
*/


// ----------
// MakePhiVtk
// ----------
// Build a set of vtkVoxel's with scalar data.

void cvLevelSetSparseGrid::MakePhiVtk( int closed )
{
  if ( closed ) {
    MakeClosedPhiVtk();
    return;
  }

  vtkPoints *pts;
  vtkDataArray *data;
  vtkCellArray *cells;
  int numCells;
  int *voxtypes;
  int i;
  vtkIdType voxelIds[8];
  cvLevelSetNode *currNode;
  cvLevelSetNode *xyDiagNode;
  cvLevelSetNode *yzDiagNode;
  cvLevelSetNode *xzDiagNode;
  cvLevelSetNode *xyzDiagNode;
  vtkFloatingPointType crd[3];
  double datum;

  if ( gridState_ < SGST_CSR ) {
    phiVtkValid_ = 0;
    closedPhiVtkValid_ = 0;
    return;
  }
  if ( phiVtkValid_ && ( ! closed ) ) {
    return;
  }
  if ( phiVtk_ ) {
    phiVtk_->Delete();
  }

  phiVtk_ = vtkUnstructuredGrid::New();
  pts = vtkPoints::New();
  pts->Allocate( numSparseNodes_ );
  data = vtkFloatingPointArrayType::New();

  // Create points:
  for ( i = 0; i < numSparseNodes_; i++ ) {
    currNode = &(grid_[i]);
    crd[0] = currNode->pos_[0];
    crd[1] = currNode->pos_[1];
    crd[2] = currNode->pos_[2];
    pts->InsertPoint( i, crd );

    datum = currNode->GetDoubleDatum( GS_PHI, tol_ );
    if ( closed ) {
      if ( ( currNode->i_ == 0 ) || ( currNode->i_ == (I_-1) ) ||
	   ( currNode->j_ == 0 ) || ( currNode->j_ == (J_-1) ) ||
	   ( currNode->k_ == 0 ) || ( currNode->k_ == (K_-1) ) ) {
	if ( datum < 0.0 ) {
	  datum = 0.0;
	}
      }
    }
    data->InsertTuple1( i, (vtkFloatingPointType)datum );
  }
  ((vtkUnstructuredGrid *)phiVtk_)->SetPoints( pts );
  pts->Delete();
  ((vtkUnstructuredGrid *)phiVtk_)->GetPointData()->SetScalars( data );
  data->Delete();

  // Create voxels:
  cells = vtkCellArray::New();
  InitIter();
  while ( currNode = GetNext() ) {
    xyDiagNode = IJKPresentInCSR( currNode->i_+1, currNode->j_+1,
				  currNode->k_ );
    if ( xyDiagNode == NULL ) {
      continue;
    }
    if ( dim_ == 3 ) {
      yzDiagNode = IJKPresentInCSR( currNode->i_, currNode->j_+1,
				    currNode->k_+1 );
      xzDiagNode = IJKPresentInCSR( currNode->i_+1, currNode->j_,
				    currNode->k_+1 );
      xyzDiagNode = IJKPresentInCSR( currNode->i_+1, currNode->j_+1,
				     currNode->k_+1 );
      if ( ( yzDiagNode == NULL ) || ( xzDiagNode == NULL ) ||
	   ( xyzDiagNode == NULL ) ) {
	continue;
      }
    }
    if ( ( currNode->xNextIndex_ != currNode->index_ ) &&
	 ( currNode->yNextIndex_ != currNode->index_ ) &&
	 ( (dim_==2) || ( currNode->zNextIndex_ != currNode->index_ ) ) ) {

      // Insert point id's as per the definition given on p.119 (vtk book):

      /* You'd think we could insert vtkVoxel's directly, as I tried
	 to do here.  You'd be wrong.  The following fails utterly to
	 work, though I have no idea why.  vtkCell has a member,
	 vtkPoints *Points, which may somehow be significant (in that
	 it doesn't get set), but I don't think it's getting set in
	 the alternate (functioning) approach.  Who knows??? */

      /*
      voxel = vtkVoxel::New();
      voxel->PointIds->InsertNextId( currNode->index_ );
      voxel->PointIds->InsertNextId( currNode->xNextIndex_ );
      voxel->PointIds->InsertNextId( currNode->yNextIndex_ );
      voxel->PointIds->InsertNextId( xyDiagNode->index_ );
      voxel->PointIds->InsertNextId( currNode->zNextIndex_ );
      voxel->PointIds->InsertNextId( xzDiagNode->index_ );
      voxel->PointIds->InsertNextId( yzDiagNode->index_ );
      voxel->PointIds->InsertNextId( xyzDiagNode->index_ );
      cells->InsertNextCell( voxel );
      */

      if ( dim_ == 3 ) {
	voxelIds[0] = currNode->index_;
	voxelIds[1] = currNode->xNextIndex_;
	voxelIds[2] = currNode->yNextIndex_;
	voxelIds[3] = xyDiagNode->index_;
	voxelIds[4] = currNode->zNextIndex_;
	voxelIds[5] = xzDiagNode->index_;
	voxelIds[6] = yzDiagNode->index_;
	voxelIds[7] = xyzDiagNode->index_;
	cells->InsertNextCell( 8, voxelIds );
      } else if ( dim_ == 2 ) {
	voxelIds[0] = currNode->index_;
	voxelIds[1] = currNode->xNextIndex_;
	voxelIds[2] = currNode->yNextIndex_;
	voxelIds[3] = xyDiagNode->index_;
	cells->InsertNextCell( 4, voxelIds );
      }
    }
  }
  numCells = cells->GetNumberOfCells();
  voxtypes = new int [numCells];
  for ( i = 0; i < numCells; i++ ) {
    if ( dim_ == 3 ) {
      voxtypes[i] = VTK_VOXEL;
    } else if ( dim_ == 2 ) {
      voxtypes[i] = VTK_PIXEL;
    }
  }
  ((vtkUnstructuredGrid *)phiVtk_)->SetCells( voxtypes, cells );
  cells->Delete();
  delete [] voxtypes;

  phiVtkValid_ = 1;
  closedPhiVtkValid_ = 0;

  return;
}


// ----------------
// MakeClosedPhiVtk
// ----------------

void cvLevelSetSparseGrid::MakeClosedPhiVtk()
{
  int i, j, k;
  int logicalIx;
  cvLevelSetNode *currNode;
  double datum;
  int bound;
  int cnt = 0;

  if ( gridState_ < SGST_CSR ) {
    phiVtkValid_ = 0;
    closedPhiVtkValid_ = 0;
    return;
  }
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

  for (k = 0; k < K_; k++) {
    for (j = 0; j < J_; j++) {
      for (i = 0; i < I_; i++) {

	currNode = IJKPresentInCSR( i, j, k );

	// If (i,j,k) is on the logical boundary:
	bound = 0;
	if ( ( i == 0 ) || ( i == (I_-1) ) ||
	     ( j == 0 ) || ( j == (J_-1) ) ||
	     ( ( dim_ > 2 ) && ( ( k == 0 ) || ( k == (K_-1) ) ) ) ) {
	  bound = 1;
	}

	// If (i,j,k) is in the current sparse grid:
	if ( currNode != NULL ) {
	  datum = currNode->GetDoubleDatum( GS_PHI, tol_ );
	  if ( IntSign( datum, tol_ ) == -1 ) {
	    if (bound) {
	      datum = 0.0;
	    }
	  }
	}

	// Handle nodes outside the current sparse grid, which should
	// be away from the front:
	else {
	  logicalIx = IJKToDenseIx( i, j, k );
	  switch ( (*topoOverlay_)[logicalIx] & STATE_PHI_MASK ) {
	  case STATE_PHI_ON:

	    // 2/17/00: Recall, the way this whole situation arose was
	    // because no contour geometry was created for isolated
	    // points whose values precisely equal the contour value.
	    // Now, as we are trying to get geometry out, we should be
	    // able to use this behavior by setting datum to 0.0 and
	    // expect no geometry to be created.

	    datum = 0.0;
	    break;

	    /*
	    printf("ERR: Unexpected grid mismatch.\n");
	    phiVtkValid_ = 0;
	    phiVtk_->Delete();
	    phiVtk_ = NULL;
	    return;
	    */

	  case STATE_PHI_OUTSIDE:
	    datum = 1.0;
	    break;
	  case STATE_PHI_INSIDE:
	    if (bound) {
	      datum = 0.0;
	    } else {
	      datum = -1.0;
	    }
	    break;
	  default:
	    assert(0);
	  }
	}

	scalars->InsertTuple1( cnt++, (vtkFloatingPointType)datum );

      }  // i
    }    // j
  }      // k

  // Connect data to grid:
  ((vtkStructuredPoints *)phiVtk_)->GetPointData()->SetScalars( scalars );
  scalars->Delete();

  phiVtkValid_ = 0;
  closedPhiVtkValid_ = 1;

  return;
}


// ------
// GetPhi
// ------

cvDataObject *cvLevelSetSparseGrid::GetPhi()
{
  cvUnstructuredGrid *result;

  MakePhiVtk();
  result = new cvUnstructuredGrid( (vtkUnstructuredGrid *)phiVtk_ );
  return result;
}


// ------------
// GetCurvature
// ------------

cvStrPts *cvLevelSetSparseGrid::GetCurvature()
{
  return CV_ERROR;
}


// ----------
// StatString
// ----------

char *cvLevelSetSparseGrid::StatString()
{
  char *result;

  result = new char [CV_STRLEN];
  if ( gridState_ < SGST_CSR ) {
    sprintf( result, "cvLevelSetSparseGrid state is pre-CSR." );
  } else {
    sprintf( result, "nodes %d hash_buckets %d",
	     numSparseNodes_, numBuckets_ );
  }

  return result;
}


// -----------
// GetPartSize
// -----------

int cvLevelSetSparseGrid::GetPartSize( int partNum )
{
  int i;
  int partSz = 0;

  if ( ( color_ == NULL ) || ( partNum >= numParts_ ) ) {
    return -1;
  }

  for ( i = 0; i < numSparseNodes_; i++ ) {
    if ( color_[i] == partNum ) {
      partSz++;
    }
  }

  return partSz;

}


// ----------
// BuildL1Map
// ----------
// "L1" refers to the mapping between global dense indices
// (i.e. (i,j,k) triples, which correspond directly to a global index
// into a dense linear array) and global sparse indices (which range
// from 0 to numSparseNodes_-1).  Note the following semantic
// convention:
//   - forward mapping == global dense --> global sparse
//   - reverse mapping == global sparse --> global dense
//
// Only the master / slave_0 process should be building the L1 map,
// because only the master / slave_0 should have the full grid graph
// (i.e. CSR) data structure.

int cvLevelSetSparseGrid::BuildL1Map()
{
  int i;
  int prevIx = -1;

  if ( gridState_ < SGST_CSR ) {
    return CV_ERROR;
  }
  if ( L1map_ != NULL ) {
    return CV_OK;
  }

  L1map_ = new int [numSparseNodes_];
  for (i = 0; i < numSparseNodes_; i++) {

    // See how items are hashed into the hash table (ComputeHashIx)
    // and how they are traversed (GetNextHTItem).  These determine
    // the order in which grid_ elements are written, which must be
    // s.t. the global dense indices (i.e. hash table entry tag_'s)
    // are ordered consistently with the global sparse ordering.
    // Confused?  See notes, week of 10/18/99.

    L1map_[i] = IJKToDenseIx( grid_[i].i_, grid_[i].j_, grid_[i].k_ );

    // Verification of map correctness:
    assert( L1map_[i] > prevIx );
    prevIx = L1map_[i];
  }

  return CV_OK;
}


// ----------
// BuildL2Map
// ----------
// Ideally, slaves which each build this map themselves.  But the
// problem is that you need the full color_[] array in order to do
// this.  So the two choices are:
//   - have master build L2map_ for each slave, and then send them
//   - master broadcasts color_ to all slaves, each builds its own L2map_

int cvLevelSetSparseGrid::BuildL2Map()
{
  return CV_ERROR;
}


// ---------------------
// BuildGlobalToLocalMap
// ---------------------
// Map is allocated by this function... needs to be deallocated by
// caller.  Look up a global sparse index in the returned map.  If the
// map entry at that position is non-negative, then this is the local
// (i.e. intra-partition) node number.  The caller should note that
// the returned, allocated map is of size numSparseNodes_.

int cvLevelSetSparseGrid::BuildGlobalToLocalMap( int partNum, int **map )
{
  int i;
  int localIx = 0;

  if ( ( color_ == NULL ) || ( partNum >= numParts_ ) ) {
    return CV_ERROR;
  }

  *map = new int [numSparseNodes_];
  if ( *map == NULL ) {
    printf( "ERR: Memory allocation failure [cvLevelSetSparseGrid::"
	    "BuildGlobalToLocalMap].\n" );
    return CV_ERROR;
  }

  for ( i = 0; i < numSparseNodes_; i++ ) {
    if ( color_[i] == partNum ) {
      (*map)[i] = localIx;
      localIx++;
    } else {
      (*map)[i] = -1;
    }
  }

  return CV_OK;
}


// ---------------------
// BuildLocalToGlobalMap
// ---------------------
// Map is allocated by this function... needs to be deallocated by
// caller.  Note that the returned, allocated map, l2gMap, is of the
// size returned as *l2gSz.  Looking up a node in l2gMap based on its
// local index gives a global sparse index.  Also note that this
// method requires the caller to pass in a global-to-local map instead
// of simply a partition number.  This prevents potential duplication
// of the global-to-local map generation.

int cvLevelSetSparseGrid::BuildLocalToGlobalMap( int g2lMap[], int **l2gMap, int *l2gSz )
{
  int i;
  int partSz = 0;
  int partPos = 0;

  if ( color_ == NULL ) {
    return CV_ERROR;
  }

  for ( i = 0; i < numSparseNodes_; i++ ) {
    if ( g2lMap[i] >= 0 ) {
      partSz++;
    }
  }

  *l2gMap = new int [partSz];
  if ( *l2gMap == NULL ) {
    printf( "ERR: Memory allocation failure [cvLevelSetSparseGrid::"
	    "BuildLocalToGlobalMap].\n" );
    return CV_ERROR;
  }
  *l2gSz = partSz;

  for ( i = 0; i < numSparseNodes_; i++ ) {
    if ( g2lMap[i] >= 0 ) {
      (*l2gMap)[partPos] = i;
      partPos++;
    }
  }

  return CV_OK;
}


// -------------
// CSRToPolyData
// -------------
// Note that we're excluding edge duplications, which exist in the CSR
// representation because METIS requires the full (symmetric)
// adjacency matrix for partitioning.

cvPolyData *cvLevelSetSparseGrid::CSRToPolyData( GridScalarT t )
{
  cvPolyData *result;
  vtkExtractEdges *getEdges = vtkExtractEdges::New();

  MakePhiVtk();
  getEdges->SetInputDataObject( phiVtk_ );
  getEdges->Update();
  result = new cvPolyData( getEdges->GetOutput() );
  getEdges->Delete();
  return result;
}


// -------------------
// PartitionToPolyData
// -------------------

cvPolyData *cvLevelSetSparseGrid::PartitionToPolyData( int partNum )
{
  int partSz;
  int *g2lMap;
  cvLevelSetNode *currNode;
  vtkPolyData *pd;
  vtkPoints *pts;
  vtkCellArray *lines;
  int i, j;
  int jaStart, jaEnd;
  int itemNum;
  vtkFloatingPointType crd[3];
  vtkIdType lineIds[2];
  cvPolyData *result;

  if ( gridState_ < SGST_Partitioned ) {
    return CV_ERROR;
  }
  if ( partNum < 0 ) {
    return CV_ERROR;
  }

  if ( BuildGlobalToLocalMap( partNum, &g2lMap ) != CV_OK ) {
    return CV_ERROR;
  }
  partSz = GetPartSize( partNum );
  pd = vtkPolyData::New();
  pts = vtkPoints::New();
  pts->Allocate( partSz );
  itemNum = 0;
  for ( i = 0; i < numSparseNodes_; i++ ) {
    if ( g2lMap[i] >= 0 ) {
      currNode = &(grid_[i]);
      crd[0] = currNode->pos_[0];
      crd[1] = currNode->pos_[1];
      crd[2] = currNode->pos_[2];
      pts->InsertPoint( itemNum, crd );
      itemNum++;
    }
  }

  lines = vtkCellArray::New();
  for ( i = 0; i < numSparseNodes_; i++ ) {
    if ( g2lMap[i] >= 0 ) {
      jaStart = adjIa_[i];
      jaEnd = adjIa_[i+1];
      for ( j = jaStart; j < jaEnd; j++ ) {
	if ( adjJa_[j] > i ) {
	  lineIds[0] = g2lMap[i];
	  lineIds[1] = g2lMap[adjJa_[j]];
	  lines->InsertNextCell( 2, lineIds );
	}
      }
    }
  }

  pd->SetPoints( pts );
  pts->Delete();
  pd->SetLines( lines );
  lines->Delete();
  result = new cvPolyData( pd );
  pd->Delete();

  return result;
}


// ---------------
// GetCSRStructure
// ---------------
// Build only the geometry of a mesh... do not create any associated
// data sets.

cvDataObject *cvLevelSetSparseGrid::GetCSRStructure()
{
  vtkUnstructuredGrid *ug;
  vtkPoints *pts;
  vtkCellArray *cells;
  int numCells;
  int *voxtypes;
  int i;
  vtkIdType voxelIds[8];
  cvLevelSetNode *currNode;
  cvLevelSetNode *xyDiagNode;
  cvLevelSetNode *yzDiagNode;
  cvLevelSetNode *xzDiagNode;
  cvLevelSetNode *xyzDiagNode;
  vtkFloatingPointType crd[3];

  if ( gridState_ < SGST_CSR ) {
    return NULL;
  }

  ug = vtkUnstructuredGrid::New();
  pts = vtkPoints::New();
  pts->Allocate( numSparseNodes_ );

  // Create points:
  for ( i = 0; i < numSparseNodes_; i++ ) {
    currNode = &(grid_[i]);
    crd[0] = currNode->pos_[0];
    crd[1] = currNode->pos_[1];
    crd[2] = currNode->pos_[2];
    pts->InsertPoint( i, crd );
  }
  ug->SetPoints( pts );

  // Create voxels:
  cells = vtkCellArray::New();
  for ( i = 0; i < numSparseNodes_; i++ ) {
    currNode = &(grid_[i]);
    xyDiagNode = IJKPresentInCSR( currNode->i_+1, currNode->j_+1,
				  currNode->k_ );
    if ( dim_ == 3 ) {
      yzDiagNode = IJKPresentInCSR( currNode->i_, currNode->j_+1,
				    currNode->k_+1 );
      xzDiagNode = IJKPresentInCSR( currNode->i_+1, currNode->j_,
				    currNode->k_+1 );
      xyzDiagNode = IJKPresentInCSR( currNode->i_+1, currNode->j_+1,
				     currNode->k_+1 );
    }
    if ( ( currNode->xNextIndex_ != currNode->index_ ) &&
	 ( currNode->yNextIndex_ != currNode->index_ ) &&
	 ( (dim_==2) || ( currNode->zNextIndex_ != currNode->index_ ) ) &&
	 ( xyDiagNode != NULL ) &&
	 ( (dim_==2) || ( yzDiagNode != NULL ) ) &&
	 ( (dim_==2) || ( xzDiagNode != NULL ) ) &&
	 ( (dim_==2) || ( xyzDiagNode != NULL ) ) ) {

      // Insert point id's as per the definition given on p.119 (vtk book):
      if ( dim_ == 3 ) {
	voxelIds[0] = currNode->index_;
	voxelIds[1] = currNode->xNextIndex_;
	voxelIds[2] = currNode->yNextIndex_;
	voxelIds[3] = xyDiagNode->index_;
	voxelIds[4] = currNode->zNextIndex_;
	voxelIds[5] = xzDiagNode->index_;
	voxelIds[6] = yzDiagNode->index_;
	voxelIds[7] = xyzDiagNode->index_;
	cells->InsertNextCell( 8, voxelIds );
      } else if ( dim_ == 2 ) {
	voxelIds[0] = currNode->index_;
	voxelIds[1] = currNode->xNextIndex_;
	voxelIds[2] = currNode->yNextIndex_;
	voxelIds[3] = xyDiagNode->index_;
	cells->InsertNextCell( 4, voxelIds );
      }
    }
  }
  numCells = cells->GetNumberOfCells();
  voxtypes = new int [numCells];
  for ( i = 0; i < numCells; i++ ) {
    if ( dim_ == 3 ) {
      voxtypes[i] = VTK_VOXEL;
    } else if ( dim_ == 2 ) {
      voxtypes[i] = VTK_PIXEL;
    }
  }
  ug->SetCells( voxtypes, cells );
  cells->Delete();
  delete [] voxtypes;
  pts->Delete();

  ug->Delete();

  return NULL;
}


// --------------
// GetMemoryUsage
// --------------

int cvLevelSetSparseGrid::GetMemoryUsage()
{
  int sz = 0;

  sz += sizeof( this );
  sz += GetStructuredGridMemoryUsage();
  if ( grid_ ) {
    sz += numSparseNodes_ * sizeof(cvLevelSetNode);
  }
  if ( seedInterface_ ) {
    sz += seedInterface_->GetMemoryUsage();
  }
  if ( nodeTable_ ) {
    sz += numBuckets_ * sizeof( cvSortedList<TableStruct*> );
  }
  if ( htIter_ ) {
    sz += sizeof( cvLispListIterator<TableStruct*> );
  }
  sz += topoOverlay_->GetMemoryUsage();
  if ( adjIa_ ) {
    sz += iaSz_ * sizeof(int);
  }
  if ( adjJa_ ) {
    sz += jaSz_ * sizeof(int);
  }
  if ( color_ ) {
    printf("ERR: unexpected allocated component color_ found.\n");
    sz += numSparseNodes_ * sizeof(int);
  }
  if ( L1map_ ) {
    printf("ERR: unexpected allocated component L1map_ found.\n");
    sz += numSparseNodes_ * sizeof(int);
  }
  if ( L2map_ ) {
    printf("ERR: unexpected allocated component L2map_ found.\n");
  }

  return sz;
}


// --------------
// PartitionGraph
// --------------

int cvLevelSetSparseGrid::PartitionGraph( int numParts )
{
  int i;

  if ( gridState_ < SGST_CSR ) {
    return CV_ERROR;
  }
  if ( gridState_ == SGST_Partitioned ) {
    return CV_OK;
  }

  // Prepare weightings:
  //   - nodes (based on phi value?)
  //   - edges (based on proximity to zls, which is essentially
  //     equivalent to phi value?)

  // Allocate partition vector.  Note that we could do this earlier
  // on, at the same time that we allocate grid_ since the size of the
  // partition vector is equal to the global number of sparse
  // nodes... but by deferring allocation until we're actually about
  // to do a partitioning, we avoid using up that memory in serial
  // (non-partitioned) cases.

  // Call METIS.

  // For now, serial partitioning:
  for ( i = 0; i < numSparseNodes_; i++ ) {
    color_[i] = 0;
  }

  gridState_ = SGST_Partitioned;
  numParts_ = numParts;
  return CV_OK;
}


// ----------------
// PackagePartition
// ----------------
// This method is for execution by the master / slave_0 process only.
// It takes in a partition number (from 0 to numParts_-1) and does the
// following:
//
//   - finds all nodes on the edge of the given partition which have
//     neighboring nodes in another partition
//   - foreach such node, stores a copy of that node
//
// Ultimate, these copies are to be stored by the owning slave's
// cvLevelSetSparseGrid as part of the grid_ array.  These cached copies are to
// be invalidated every time an UpdatePhi is completed.

int cvLevelSetSparseGrid::PackagePartition( int partNum )
{
  if ( gridState_ < SGST_Partitioned ) {
    return CV_ERROR;
  }

  // Serial: no packaging needed.
  packageSize_ = numSparseNodes_;
  localSize_ = numSparseNodes_;

  gridState_ = SGST_PackagingComplete;

  return CV_OK;
}
