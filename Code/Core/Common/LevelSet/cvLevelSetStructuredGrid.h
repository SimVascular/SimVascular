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

#ifndef __CVLEVELSETSTRUCTUREDGRID_H
#define __CVLEVELSETSTRUCTUREDGRID_H

#include "SimVascular.h"
#include "cvLevelSetNode.h"
#include "cvPolyData.h"
#include "cvSolidModel.h"
#include "cvStrPts.h"
#include "cv_misc_utils.h"


typedef enum { Dense_GridT, Sparse_GridT, Invalid_GridT } GridT;
GridT GridT_StrToEnum( char *name );
char *GridT_EnumToStr( GridT t );


class cvLevelSetVelocity;


class cvLevelSetStructuredGrid {

public:

  cvLevelSetStructuredGrid( double h[], int dims[], double o[] );
  virtual ~cvLevelSetStructuredGrid();
  virtual void DeallocateNodes() = 0;

  // Initialize distance function phi:
  virtual int InitPhi( double ctr[], double radius ) = 0;
  virtual int InitPhi( cvPolyData *front ) = 0;
  virtual int InitPhi( cvSolidModel *solid ) = 0;
  virtual int InitPhi( cvStrPts *img, double thr ) = 0;
  virtual int ReinitPhi() = 0;
  int CloseHoles();

  // Band parameters will be relevant only for cvLevelSetSparseGrid:
  virtual int SetBandParams( double innerPhi, double outerPhi,
			     double mineWidth ) { return CV_ERROR; };
  virtual int GetBandParams( double *innerPhi, double *outerPhi,
			     double *mineWidth ) { return CV_ERROR; };

  // Methods for use in an update loop:
  int EvaluateV( cvLevelSetVelocity *vfn, double factor = 1.0 );
  virtual int ProjectV( int save ) = 0;
  virtual void ExtendV() = 0;
  double ComputeDeltaPhi( double factor = 1.0 );
  virtual int UpdatePhi();

  // Manage nodes:
  void GetAdjacentIxs( cvLevelSetNode *n, int ixs[] );
  void GetAdjacentIxs( int index, int ixs[] );
  inline int IxWithinExtent( int i, int j, int k );

  // Invoke difference calculations:
  void FindDpi();    // 1st-order forward
  void FindDmi();    // 1st-order backward
  void FindD0();     // 1st- and 2nd-order centered

  // Compute geometric quantities:
  void FindK();    // entry method for curvature calculation
  void FindK2d();
  void FindK3dg();
  void FindK3dm();
  void FindN();

  // Interpolate nodal values:
  int GetAnchorIx( double pos[], int *ix );
  int FindEdge( double pos[], cvLevelSetNode **a, cvLevelSetNode **b, char *dir );
  int InterpZLS( cvLevelSetNode *a, cvLevelSetNode *b, char code, double zls[] );
  int InterpK( double pos[], double *k );
  int InterpKm( double pos[], double *k );
  int InterpKg( double pos[], double *k );
  int InterpN( double pos[], double n[] );

  // Query nodal values:
  double GetMaxF();
  vtkFloatingPointType GetMaxV();
  double GetMaxPhiIncr();
  cvPolyData *GetVelocityVectors();
  cvPolyData *GetFront( int closed = 0 );
  cvPolyData *GetActiveNodes();
  virtual cvDataObject *GetPhi() = 0;
  virtual cvStrPts *GetCurvature() = 0;
  virtual cvPolyData *GetBoundaryData( GridScalarT t );

  // cvLevelSetNode sets:
  int GetNodeSets( cvPolyData ***nodeSets, int *numSets );
  int SaveActiveNodes();
  int NumActiveNodes();
  int SaveCoveredNodes();
  int SaveUncoveredNodes();
  int SaveForceMinVNodes();
  int SaveMineNodes();
  int PSetsValid() { return pSetsValid_; }

  // Get grid-dependent info:
  virtual cvPolyData *CreateGridPolyData();
  virtual int GridModified() const { return 0; };
  virtual char *StatString() = 0;  // should return a dynamically-alloc'd str
  inline int GetDim();
  inline int GetHv( double *hx, double *hy, double *hz );
  inline double GetMinH();
  double ComputeDt( double cflFactor );
  double ComputeDt( double maxf, double cflFactor );
  double GetMainDiagonal() { return mainDiagonal_; };

  // Memory usage interface:
  virtual int GetMemoryUsage() = 0;

protected:

  // Memory usage local to cvLevelSetStructuredGrid:
  int GetStructuredGridMemoryUsage();

  // Compute centered diff's:
  void FindD0i();    // 1st-order centered
  void FindD0xi();   // 2nd-order centered of D0x
  void FindD0yi();   // 2nd-order centered of D0y
  void FindD0zi();   // 2nd-order centered of D0z

  // Compute entropy-satisfying terms:
  void FindDelPlus();
  void FindDelMinus();

  // cvLevelSetNode list iterator (leave structure of list to derived classes):
  cvLevelSetNode *curr_;
  virtual void InitIter() = 0;
  virtual cvLevelSetNode *GetNext() = 0;
  virtual int IJKToIndex( int i, int j, int k ) = 0;
  inline int IJKToLogicalIx( int i, int j, int k );
  inline int LogicalIxToIJK( int log_ix, int *i, int *j, int *k );

  // Projection helpers:
  int ProjectV( int posFlag, int save );
  int CheckProjectCoverage();
  int SteepestDescentPhi( int ix );
  int SteepestAscentPhi( int ix );
  int *ixBuffer_;
  int ixBufferSz_;

  // The following are for general-purpose use in saving sets of node
  // indices.  A value of -1 is used to denote an invalid entry / end
  // of a particular set of indices.  A size of 2*numNodes_ allows for
  // each node to have its own set, assuming that no sets overlap.
  // Ultimately, the nodeSets_ buffer could be used in place of
  // ixBuffer_ as well.
  int *nodeSets_;
  int nodeSetsSize_;
  int nodeSetsPos_;
  void ResetNodeSets();
  int pSetsValid_;

  // Manage node state:
  void ClearActive();
  void ClearForceMinV();
  void ClearCovered();

  cvLevelSetNode *grid_;      // allocated by derived class
  int numNodes_;    // set by derived class

  int I_, J_, K_;
  int numDenseNodes_;
  double hv_[3];
  double minh_, maxh_;
  double origin_[3];
  int dim_;
  double mainDiagonal_;

  void AssignNode( cvLevelSetNode *n, double f0, double f1, int forceMinVFlag,
		   double toDot[] );

  void AssignVToNode( cvLevelSetNode *n, double f0, double f1, int forceMinVFlag,
		      double toDot[] );
  void AssignVToEdge( cvLevelSetNode *a, cvLevelSetNode *b, double f0, double f1,
		      int forceMinVFlag, double toDot[] );

  double tol_, oneOverTol_, relTol_;

  // State:
  int init_;   // indicates whether InitPhi has been called
  int d0Valid_, dpiValid_, dmiValid_, delPlusValid_, delMinusValid_;
  int deltaPhiValid_;
  int nValid_, kValid_, k3dmValid_, k3dgValid_;

  // It is useful for several methods to be able to construct a vtk
  // representation of the grid with its scalar data.  Those methods
  // include:
  //   - GetFront
  //   - GetPhi

  virtual void MakePhiVtk( int closed = 0 ) = 0;
  vtkDataSet *phiVtk_;
  int phiVtkValid_;

  cvPolyData *velocityVectors_;
  cvLevelSetVelocity *velocity_;  // obsolete
  
};


// --------------
// IxWithinExtent
// --------------

int cvLevelSetStructuredGrid::IxWithinExtent( int i, int j, int k )
{
  if ( ( i >= 0 ) && ( i < I_ ) &&
       ( j >= 0 ) && ( j < J_ ) &&
       ( k >= 0 ) && ( k < K_ ) ) {
    return 1;
  } else {
    return 0;
  }
}


// ------
// GetDim
// ------

int cvLevelSetStructuredGrid::GetDim()
{
  return dim_;
}


// -----
// GetHv
// -----

int cvLevelSetStructuredGrid::GetHv( double *hx, double *hy, double *hz )
{
  *hx = hv_[0];
  *hy = hv_[1];
  *hz = hv_[2];
  return CV_OK;
}


// -------
// GetMinH
// -------

double cvLevelSetStructuredGrid::GetMinH()
{
  return minh_;
}


// --------------
// IJKToLogicalIx
// --------------

int cvLevelSetStructuredGrid::IJKToLogicalIx( int i, int j, int k )
{
  return ( (k*I_*J_) + (j*I_) + i );
}


// --------------
// LogicalIxToIJK
// --------------

int cvLevelSetStructuredGrid::LogicalIxToIJK( int log_ix, int *i, int *j, int *k )
{
  if ( ( log_ix < 0 ) || ( log_ix >= I_*J_*K_ ) ) {
    return CV_ERROR;
  }
  *k = log_ix / (I_*J_);
  *j = ( log_ix % (I_*J_) ) / I_;
  *i = log_ix % I_;
  return CV_OK;
}


#endif // __STRUCTURED_GRID_H
