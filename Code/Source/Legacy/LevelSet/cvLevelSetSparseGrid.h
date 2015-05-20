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

#ifndef __CVLEVELSETSPARSEGRID_H
#define __CVLEVELSETSPARSEGRID_H

#include "SimVascular.h"
#include "cvLevelSetStructuredGrid.h"
#include "cvStateArray.h"
#include "cvLispList.h"


// It's probably true that none of these enum's should be exposed in
// the header...

// These states are specified, very intentionally, in logical order.

typedef enum {
  SGST_Start,
  SGST_ExtentDefined,
  SGST_HashTable,
  SGST_GridAllocated,
  SGST_CSR,
  SGST_Partitioned,
  SGST_PackagingComplete
} cvLevelSetSparseGridStateT;


typedef struct {
  double phi_;
  int logicalIx_[3];
  int tag_;
  StateT state_;
} TableStruct;

int TableStructCompareFn( TableStruct *a, TableStruct *b );


// This type indicates which method to use for band construction.
// Note that these methods apply only to band construction
// (i.e. InitPhi) based on a cvPolyData (i.e. zero level set).
//
// - BAND_SDIST: signed distance based determination (i.e. does signed
//   distance fall within band extent?)
// - BAND_SWEEP: sweep away from zero level set a certain number of
//   levels to construct band

typedef enum {
  BAND_SDIST,
  BAND_SWEEP
} cvLevelSetSparseGridBandT;


class cvLevelSetSparseGrid : public cvLevelSetStructuredGrid {

public:

  cvLevelSetSparseGrid( double h[], int dims[], double o[] );
  //  cvLevelSetSparseGrid( MPI_Comm comm );  // for use by slaves in parallel mode
  ~cvLevelSetSparseGrid();
  void DeallocateNodes();

  // Initialize distance function phi:
  int InitPhi( double ctr[], double radius );
  int InitPhi( cvPolyData *front );
  int InitPhi( cvSolidModel *solid );
  int InitPhi( cvStrPts *img, double thr );
  int ReinitPhi();

  // Control band extent:
  int SetBandParams( double innerPhi, double outerPhi, double mineWidth );
  int GetBandParams( double *innerPhi, double *outerPhi, double *mineWidth );

  // Methods for use in an update loop:
  //  int EvaluateV( cvLevelSetVelocity *vfn, double factor = 1.0 );
  int ProjectV( int save );
  void ExtendV();
  int UpdatePhi();

  // Query nodal values:
  cvDataObject *GetPhi();
  cvStrPts *GetCurvature();

  char *StatString();

  // Non-virtual public methods:
  int GetNumBandNodes() const;

  // Do stuff with the sparse representation:
  cvPolyData *CreateGridPolyData() { return CSRToPolyData( GS_INVALID ); };
  int GridModified() const { return mineHit_; };
  cvPolyData *CSRToPolyData( GridScalarT t );
  cvPolyData *PartitionToPolyData( int partNum );  // only called by master
  cvDataObject *GetCSRStructure();

  // Memory usage:
  int GetMemoryUsage();

  // Parallel stuff:
  int PartitionGraph( int numParts );
  int PackagePartition( int partNum );

private:

  int InitSign_Internal( cvPolyData *front );
  int InitPhi_Internal( cvPolyData *front );

  // cvLevelSetNode list iterator:
  inline void InitIter() { currIx_ = 0; }
  inline cvLevelSetNode *GetNext();
  inline int IJKToIndex( int i, int j, int k );
  int IJKToSparseIx( int i, int j, int k );
  inline int IJKToDenseIx( int i, int j, int k );
  int currIx_;

  // UndoTimeStep is needed to roll back phi values to their
  // pre-UpdatePhi state.  This must be done in situations where a
  // mine node has been hit.  We want to revert to the previous phi in
  // this event because the mine node which was hit might be on the
  // edge of the grid.  If it is, then the front has left the grid,
  // and attempts to rebuild the grid based on the current phi values
  // will not capture that part of the front.
  void UndoTimeStep();

  // seedInterface_ lets us build our grid near the interface of a
  // given cvSolidModel seed subject to the constraints of a given
  // logical domain (as defined by grid dim_, hv_ and origin_).
  cvPolyData *seedInterface_;
  void ClearSeedInterface();
  int BuildSeedInterface( cvSolidModel *seed );

  // Hash table for band definition:
  // ---
  // Note that this hash table is implemented as an *array* of the
  // template class SortedList from the Berkeley list package.  As
  // such, the nodeTable_ array pointer has no sorting associated with
  // it... only the items of each array element (i.e. the items of
  // each SortedList) are sorted.
  // ---
  // In choosing a hash function for the table, we need to consider
  // the fact that we will need to search the table by (i,j,k) logical
  // node indices.  This search will be done several times for each
  // sparse node as part of the cvLevelSetSparseGrid::ConstructNeighborInfo
  // method.  Some alternatives:
  //
  //   1. h = (i+j) % n;        --> potentially many (i,j)'s per bucket
  //   2. h = (j * iDim) + i;   --> one (i,j) per bucket
  //
  // Once we've stored all the entries and are actually ready to
  // search the table, if we use (1), we'll have a more compact table,
  // but we'll have to do a more complicated search within the bucket
  // (i.e. SortedList).  If we use (2), we can do simple sorting /
  // searching within buckets based on k, but there could be many many
  // empty buckets in the table.
  // ---
  int ConstructHT( double ctr[], double radius );
  int ConstructHT( cvPolyData *front );
  int ConstructHT( cvSolidModel *sm );

  int ConstructHT_Sweep( cvPolyData *front );
  int PollNeighborSigns( int i, int j, int k );
  void Sweep_MarkActiveNodes();
  void Sweep_ComparePair( int currIx, int currSign, int ip, int jp, int kp );
  int Sweep( int sign, int levels );
  int Blot( int dim[3] );

  void ClearHT();
  int IJKPresentInHT( int i, int j, int k, cvSortedList<TableStruct*> **bucket,
		      TableStruct **entry );
  inline int ComputeHashIx( int i, int j, int k );
  cvSortedList<TableStruct*> **nodeTable_;
  int numBuckets_;
  void InitHTIterator();
  TableStruct *GetNextHTItem();
  int htCurrBucketId_;
  cvLispListIterator<TableStruct*> *htIter_;

  // Topological overlay.  Keep in mind that this overlay is being
  // kept for sign storage between band constructions.  That is, nodes
  // *outside* the band will have their topological state (in, out or
  // on) stored in topoOverlay_.  NOTE that for nodes which were
  // *inside* the preceding band need to depend on the last sign they
  // contained for re-initialization.  Perhaps this implies there
  // should be a method to update topoOverlay_ before an old band is
  // discarded.
  cvStateArray *topoOverlay_;
  int overlaySz_;
  void TopoUpdate();

  // Build CSR (i.e. adjacency matrix, cvLevelSetNode list):
  int ConstructCSR();
  int SanityCheckCSR();
  int GetNeighborSparseIxs( cvLevelSetNode *node, int ixs[] );
  void DeallocateCSR();
  int SetNeighborInfo( cvLevelSetNode *node );
  cvLevelSetNode *IJKPresentInCSR( int i, int j, int k );
  int numSparseNodes_;
  int numSparseEdges_;

  int *adjIa_;
  int *adjJa_;
  int iaSz_;
  int jaSz_;

  void MakePhiVtk( int closed = 0 );
  void MakeClosedPhiVtk();
  int closedPhiVtkValid_;

  // Partitioning:
  int *color_;
  int numParts_;
  void ClearPartitioning();
  int GetPartSize( int partNum );
  int localSize_;    // not known until partitioning is complete
  int packageSize_;  // not known until packaging is complete

  // Index maps:
  int BuildL1Map();  // global dense  <--> global sparse
  int BuildL2Map();  // global sparse <--> local sparse
  int L1_FwdMap( int i, int j, int k );
  int L1_RevMap( int ixIn, int *i, int *j, int *k );
  int L2_FwdMap( int ixIn, int *ixOut );
  int L2_RevMap( int ixIn, int *ixOut );
  int *L1map_;
  int *L2map_;

  int BuildGlobalToLocalMap( int partNum, int **map );
  int BuildLocalToGlobalMap( int g2lMap[], int **l2gMap, int *l2gSz );

  // Band extent:
  double outerExtent_;
  double innerExtent_;
  double mineWidth_;
  double outerExtent2_;
  double innerExtent2_;
  double mineWidth2_;

  int mineHit_;

  // cvLevelSetVelocity projection:
  int InitIxBuffer();
  int InitNodeSets();

  cvLevelSetSparseGridStateT gridState_;
  cvLevelSetSparseGridBandT bandMethod_;

};


// ---
// See ARM p. 104 for illustration of a subtlety related to inline
// member declaration / definition.
// ---


// -------------
// ComputeHashIx
// -------------
// Note that this hash fn provides the needed functionality that
// in-order traversal of (i,j,k)'s leads to a monotonic traversal of
// hash table buckets.  This is the critical characteristic which
// enables node-number-maps to be used efficiently (i.e. forward
// mappings in O(log n) instead of O(n)).

inline
int cvLevelSetSparseGrid::ComputeHashIx( int i, int j, int k )
{
  return ( IJKToDenseIx( i, j, k ) % numBuckets_ );
  return k;
  return ( (j*I_) + i );
}


// ------------
// IJKToDenseIx
// ------------

inline
int cvLevelSetSparseGrid::IJKToDenseIx( int i, int j, int k )
{
  return ( (k*I_*J_) + (j*I_) + i );
}


// ----------
// IJKToIndex
// ----------

inline
int cvLevelSetSparseGrid::IJKToIndex( int i, int j, int k )
{
  return IJKToSparseIx( i, j, k );
}


// -------
// GetNext
// -------

inline
cvLevelSetNode *cvLevelSetSparseGrid::GetNext()
{
  cvLevelSetNode *result;

  if ( grid_ == NULL ) {
    return NULL;
  }
  if ( currIx_ >= numSparseNodes_ ) {
    return NULL;
  }
  result = &(grid_[currIx_]);
  currIx_++;
  return result;
}


#endif // __SPARSE_GRID_H
