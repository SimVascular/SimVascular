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

#include "SimVascular.h"

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>
#include "sv_misc_utils.h"
#include "sv_cgeom.h"

#include <vtkDataSetSurfaceFilter.h>
#include <vtkSmartPointer.h>
#include <vtkThreshold.h>
#include <vtkUnstructuredGrid.h>

#define START  -1
#define INTERMED  0
#define DEADEND   1
#define CLOSED    2

typedef struct {
  double x, y, z;
  int postId;
} PrePt_T;

typedef struct {
  double x, y, z;
  int uncompressedId;
} PostPt_T;

#include "sv_vtk_utils.h"

// Static helpers
// --------------

static int SomeFalse( int *arr, int sz );

static int FirstFalsePos( int *arr, int sz );

static int SearchComplete( int *state, int stateSz );

static void MarkClosed( int *state, int stateSz );

static void MarkDeadEnd( int *state, int stateSz );

static int GetNextPt( int prev, int curr, int *state, int stateSz,
		      int *lines, int numLines );

static int GetStartPt( int *state, int stateSz );

static int UpdateCells( int *cells, int numCells,
			vtkIdType **newCells, int *numNewCells );

//-------------------------
// VtkUtils_ThresholdUgrid
//-------------------------
// Create a vtkUnstructuredGrid object from a threshold of a cellular 
// data array contained in a vtkDataObject object.
//
vtkSmartPointer<vtkUnstructuredGrid>
VtkUtils_ThresholdUgrid(const double lower, const double upper, const std::string& data_name, 
    vtkDataObject* vtk_data)
{
  int idx = 0;
  int port = 0;
  int connection = 0;
  auto fieldAssociation = vtkDataObject::FieldAssociations::FIELD_ASSOCIATION_CELLS;

  auto threshold = vtkSmartPointer<vtkThreshold>::New();
  threshold->SetLowerThreshold(lower);
  threshold->SetUpperThreshold(upper);
  threshold->SetInputData(vtk_data);
  threshold->SetInputArrayToProcess(idx, port, connection, fieldAssociation, data_name.c_str());
  threshold->Update();
  return threshold->GetOutput();
}

//---------------------------
// VtkUtils_ThresholdSurface
//---------------------------
// Create a vtkPolyData object from a threshold of a cellular 
// data array contained in a vtkDataObject object.
//
vtkSmartPointer<vtkPolyData>
VtkUtils_ThresholdSurface(const double lower, const double upper, const std::string& data_name, 
    vtkDataObject* vtk_data)
{
  auto threshold_ugrid = VtkUtils_ThresholdUgrid(lower, upper, data_name, vtk_data);

  auto surfacer = vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
  surfacer->SetInputData(vtk_data);
  surfacer->Update();
  return surfacer->GetOutput();
}

// -----------------------
// VtkUtils_NewVtkPolyData
// -----------------------

int VtkUtils_NewVtkPolyData( vtkPolyData **pd, int numPts, vtkFloatingPointType pts[],
			     int numCells, vtkIdType polys[] )
{
  int i;
  vtkPoints *tmpPts;
  vtkCellArray *tmpPolys;
  vtkFloatingPointType x, y, z;
  int numInPoly;
  int n = 0;

  (*pd) = vtkPolyData::New();

  tmpPts = vtkPoints::New();
  for ( i = 0; i < numPts; i++ ) {
    x = (vtkFloatingPointType)pts[3*i];
    y = (vtkFloatingPointType)pts[3*i+1];
    z = (vtkFloatingPointType)pts[3*i+2];
    tmpPts->InsertNextPoint( x, y, z );
  }
  (*pd)->SetPoints( tmpPts );
  tmpPts->Delete();

  tmpPolys = vtkCellArray::New();
  tmpPolys->InitTraversal();
  for ( i = 0; i < numCells; i++ ) {
    numInPoly = polys[n];
    tmpPolys->InsertNextCell( numInPoly, &(polys[n+1]) );
    n += numInPoly + 1;
  }
  (*pd)->SetPolys( tmpPolys );
  tmpPolys->Delete();

  return SV_OK;
}


// ----------------------------
// VtkUtils_NewVtkPolyDataLines
// ----------------------------

int VtkUtils_NewVtkPolyDataLines( vtkPolyData **pd, int numPts, vtkFloatingPointType pts[],
				  int numLines, vtkIdType lines[] )
{
  int i;
  vtkPoints *tmpPts;
  vtkCellArray *tmpLines;
  vtkFloatingPointType x, y, z;
  int numInLine;
  int n = 0;

  (*pd) = vtkPolyData::New();

  tmpPts = vtkPoints::New();
  for ( i = 0; i < numPts; i++ ) {
    x = (vtkFloatingPointType)pts[3*i];
    y = (vtkFloatingPointType)pts[3*i+1];
    z = (vtkFloatingPointType)pts[3*i+2];
    tmpPts->InsertNextPoint( x, y, z );
  }
  (*pd)->SetPoints( tmpPts );
  tmpPts->Delete();

  tmpLines = vtkCellArray::New();
  tmpLines->InitTraversal();
  for ( i = 0; i < numLines; i++ ) {
    numInLine = lines[n];
    tmpLines->InsertNextCell( numInLine, &(lines[n+1]) );
    n += numInLine + 1;
  }
  (*pd)->SetLines( tmpLines );
  tmpLines->Delete();

  return SV_OK;
}


// -----------
// UpdateCells
// -----------
// This is a helper for FixTopology.  We take in a vtk-style cell list
// (i.e. each cell descriptor is of size n+1, if there are n points in
// the cell, and the descriptor is of the format: {numIds, id0, id1,
// ...}).  We generate on output an "updated" cell list which reflects
// any duplicate point id's (as generated by the point-merging
// process) which lead to degenerate cells (i.e. line to point,
// triangle to line).
//
// NOTE that only lines and triangles are currently handled!
//
// The pointer returned as *newCells needs to be free'd by the caller.

static int UpdateCells( int *cells, int numCells,
			vtkIdType **newCells, int *numNewCells )
{
  vtkIdType *tmpNewCells;
  int numTmpNewCells;
  int cellIter, newCellIter;
  int i, j, k;
  int degen;

  // Check for newly-created duplications in point ID lists.  If
  // such a duplication is found, then this is a degenerate cell, so
  // exclude it from the newCells list.  Otherwise, enter this cell
  // into the newCells list.

  tmpNewCells = new vtkIdType [numCells*4];
  numTmpNewCells = 0;
  cellIter = newCellIter = 0;

  // For each cell:
  for (i = 0; i < numCells; i++) {
    degen = 0;

    // For each point ID in this cell:
    for (j = 0; j < cells[cellIter]; j++) {

      // Compare to every other point ID in this same cell, avoiding
      // self comparisions and redundant comparisons:
      for (k = 0; k < cells[cellIter]; k++) {
	if (j == k) continue;

	// If the two indices being compared are the same, then this
	// is a degenerate cell (e.g. a line has collapsed to a
	// point, triangle to a line).  Note that this does NOT hold
	// for other cell types (e.g a four-noded polygon which has
	// two coicident points is still a polygon).  For now, we are
	// NOT handling such cases.
	if ( cells[ cellIter + j + 1 ] == cells[ cellIter + k + 1 ] ) {
	  degen = 1;
	  break;
	}
      }
      if (degen) break;
    }

    // If no points in the current cell were merged, then copy the
    // cell to the newCells list:
    if (!degen) {
      tmpNewCells[newCellIter] = cells[cellIter];
      for (j = 0; j < tmpNewCells[newCellIter]; j++) {
	tmpNewCells[ newCellIter + j + 1 ] = cells[ cellIter + j + 1 ];
      }
      newCellIter = newCellIter + tmpNewCells[newCellIter] + 1;
      numTmpNewCells++;
    } else {
      /*
      printf("Cell[%d] degenerate: (%d,%d)\n", i,
	     cells[cellIter+1], cells[cellIter+2]);
      */
    }

    // Advance the "iterator" to the next cell position:
    cellIter = cellIter + cells[cellIter] + 1;
  }

  *newCells = tmpNewCells;
  *numNewCells = numTmpNewCells;
  return SV_OK;
}


// --------------------
// VtkUtils_FixTopology
// --------------------
// The basic strategy we will take has two main steps:
//   1. Rebuild point list.
//   2. Rebuild connectivities.

// NOTE that the only elements we fix are line segments and triangles.

int VtkUtils_FixTopology( vtkPolyData *pd, double tol )
{
  PrePt_T *prePtList;
  PostPt_T *postPtList;
  int preNumPts, preNumElems, preNumLines, preNumTris;
  int postNumPts = 0, postNumElems = 0;
  vtkFloatingPointType pt[3];
  int found;
  vtkFloatingPointType curr[3];
  vtkCellArray *lines, *tris, *newLines, *newTris;
  int numCells, *cells, cellIter;
  int numNewCells, newCellIter;
  vtkIdType *newCells;
  vtkIdType npts;
  const vtkIdType *pts;
  int oldId, mappedId;
  vtkPoints *compressedPts;

  if ( pd == nullptr ) return SV_ERROR;

  preNumPts = pd->GetNumberOfPoints();
  preNumElems = pd->GetNumberOfLines() + pd->GetNumberOfPolys();

  prePtList = new PrePt_T [preNumPts];
  postPtList = new PostPt_T [preNumPts];

  // First, we want to iterate over all points to clean up "close"
  // points and to build the post-processed pt list in this process.
  int j;
  for (int i = 0; i < preNumPts; i++) {
    found = 0;
    pd->GetPoint( i, pt );
    for (j = 0; j < postNumPts; j++) {
      curr[0] = postPtList[j].x;
      curr[1] = postPtList[j].y;
      curr[2] = postPtList[j].z;
      if ( ( fabs(pt[0]-curr[0]) < tol ) &&
	   ( fabs(pt[1]-curr[1]) < tol ) &&
	   ( fabs(pt[2]-curr[2]) < tol ) ) {
	found = 1;
	/*
	printf("Merging pt[%d] and pt[%d].\n",
	       i, postPtList[j].uncompressedId);
	*/
	break;
      }
    }

    // Make a new entry in the non-compressed list:
    prePtList[i].x = pt[0];
    prePtList[i].y = pt[1];
    prePtList[i].z = pt[2];

    // If this was a distinct point, add it to the compressed list:
    if (!found) {
      postPtList[postNumPts].x = pt[0];
      postPtList[postNumPts].y = pt[1];
      postPtList[postNumPts].z = pt[2];
      postPtList[postNumPts].uncompressedId = i;

      // Make the non-compressed entry point to this compressed
      // entry.  We could set postId to postNumPts, in which case the
      // new index is in the compressed list.  Or, we could simply set
      // postId to i, which is the uncompressed index.  The latter is
      // what we actually want, since we are going to simply put a new
      // vtkCellArray into pd (for Lines and/or Polys) which we want
      // to reference the ORIGINAL point list.

      prePtList[i].postId = postNumPts; // use compressed pt list

      //prePtList[i].postId = i; // use orig pt list

      postNumPts++;

    } else {
      // Make the non-compressed entry point to the point in the
      // compressed list that matched:

      prePtList[i].postId = j; // use compressed pt list

      //prePtList[i].postId = postPtList[j].uncompressedId; // use orig pt list
    }
  }

  // Build compressed point list:
  compressedPts = vtkPoints::New();
  for (int i = 0; i < postNumPts; i++) {
    compressedPts->InsertNextPoint( (vtkFloatingPointType)postPtList[i].x,
				    (vtkFloatingPointType)postPtList[i].y,
				    (vtkFloatingPointType)postPtList[i].z );
  }
  pd->SetPoints( compressedPts );
  compressedPts->Delete();

  // Now we have a compressed point list and a mapping of old id's to
  // new ones (via the postId fields in prePtList).  What we want to
  // do now is propagate changes in the id's to the element
  // connectivity descriptions.

  // Use vtkPolyData::SetLines and SetPolys.  And perhaps
  // vtkCellArray::InsertNextCell.

  // First make a working copy of the cell list:
  //  cells = new int [preNumElems*4];
  cells = new int [pd->GetNumberOfLines() * 3];
  lines = pd->GetLines();
  preNumLines = lines->GetNumberOfCells();
  lines->InitTraversal();
  numCells = 0;
  while ( lines->GetNextCell( npts, pts ) ) {
    assert( npts == 2 );
    cells[numCells*3] = npts;
    cells[numCells*3+1] = pts[0];
    cells[numCells*3+2] = pts[1];
    numCells++;
  }

  // Now update point ID's used in our working copy.  Note that this
  // update is done IN-PLACE in the "cells" list.  New connectivities
  // are written over the old ones in this working copy.
  cellIter = 0;
  for (int i = 0; i < numCells; i++) {
    for (j = 0; j < cells[cellIter]; j++) {
      oldId = cells[ cellIter + j + 1 ];
      mappedId = prePtList[oldId].postId;
      cells[ cellIter + j + 1 ] = mappedId;
    }
    // Advance to next cell description (i.e. size followed by list of
    // point ID's):
    cellIter = cellIter + cells[cellIter] + 1;
  }

  UpdateCells( cells, numCells, &newCells, &numNewCells );

  newLines = vtkCellArray::New();

  // We want the indices used in newLines to actually point into the
  // old (uncompressed) point list.
  newCellIter = 0;
  newLines->InitTraversal();
  for (int i = 0; i < numNewCells; i++) {
    newLines->InsertNextCell( newCells[newCellIter],
			      &(newCells[newCellIter+1]) );
    newCellIter = newCellIter + newCells[newCellIter] + 1;
  }
  pd->SetLines( newLines );
  newLines->Delete();

  delete [] cells;
  delete [] newCells;

  // Now handle triangles:
  cells = new int [pd->GetNumberOfPolys() * 4];
  tris = pd->GetPolys();
  preNumTris = tris->GetNumberOfCells();
  tris->InitTraversal();
  numCells = 0;
  while ( tris->GetNextCell( npts, pts ) ) {
    assert( npts == 3 );
    cells[numCells*4] = npts;
    cells[numCells*4+1] = pts[0];
    cells[numCells*4+2] = pts[1];
    cells[numCells*4+3] = pts[2];
    numCells++;
  }

  // Now update point ID's used in our working copy.  Note that this
  // update is done IN-PLACE in the "cells" list.  New connectivities
  // are written over the old ones in this working copy.
  cellIter = 0;
  for (int i = 0; i < numCells; i++) {
    for (j = 0; j < cells[cellIter]; j++) {
      oldId = cells[ cellIter + j + 1 ];
      mappedId = prePtList[oldId].postId;
      cells[ cellIter + j + 1 ] = mappedId;
    }
    // Advance to next cell description (i.e. size followed by list of
    // point ID's):
    cellIter = cellIter + cells[cellIter] + 1;
  }

  UpdateCells( cells, numCells, &newCells, &numNewCells );

  newTris = vtkCellArray::New();

  // We want the indices used in newTris to actually point into the
  // old (uncompressed) point list.
  newCellIter = 0;
  newTris->InitTraversal();
  for (int i = 0; i < numNewCells; i++) {
    newTris->InsertNextCell( newCells[newCellIter],
			      &(newCells[newCellIter+1]) );
    newCellIter = newCellIter + newCells[newCellIter] + 1;
  }
  pd->SetPolys( newTris );
  newTris->Delete();


  printf("  \n\n------  VtkUtils_FixTopology  ------\n");
  printf("  >>>>>>  orig. num pts   [%d]\n", preNumPts);
  printf("  >>>>>>  new   num pts   [%d]\n", pd->GetNumberOfPoints());
  printf("  >>>>>>  orig. num lines [%d]\n", preNumLines);
  printf("  >>>>>>  new   num lines [%d]\n", pd->GetNumberOfLines());
  printf("  >>>>>>  orig. num tris  [%d]\n", preNumTris);
  printf("  >>>>>>  new   num tris  [%d]\n", pd->GetNumberOfPolys());
  printf("\n");

  delete [] cells;
  delete [] newCells;

  delete [] prePtList;
  delete [] postPtList;

  return SV_OK;
}


// ------------------
// VtkUtils_GetPoints
// ------------------

int VtkUtils_GetPoints( vtkPolyData *pd, double **pts, int *numPts )
{
  int i;
  vtkFloatingPointType tmp[3];

  *numPts = pd->GetNumberOfPoints();
  if (pd->GetNumberOfPoints() == 0) {
      return SV_ERROR;
  }

  *pts = new double [(*numPts) * 3];

  for (i = 0; i < (*numPts); i++) {
    pd->GetPoint( i, tmp );
    (*pts)[3*i] = tmp[0];
    (*pts)[3*i+1] = tmp[1];
    (*pts)[3*i+2] = tmp[2];
  }

  return SV_OK;
}


// -----------------------
// VtkUtils_GetPointsFloat
// -----------------------

int VtkUtils_GetPointsFloat( vtkPolyData *pd, vtkFloatingPointType **pts, int *numPts )
{
  int i;
  vtkFloatingPointType tmp[3];

  *numPts = pd->GetNumberOfPoints();
  if (pd->GetNumberOfPoints() == 0) {
      return SV_ERROR;
  }

  *pts = new vtkFloatingPointType [(*numPts) * 3];

  for (i = 0; i < (*numPts); i++) {
    pd->GetPoint( i, tmp );
    (*pts)[3*i] = tmp[0];
    (*pts)[3*i+1] = tmp[1];
    (*pts)[3*i+2] = tmp[2];
  }

  return SV_OK;
}


// --------------------
// VtkUtils_GetAllLines
// --------------------
// This function simply returns a copy of all the connectivities in
// the vtkPolyData's vtkCellArray for lines.

int VtkUtils_GetAllLines( vtkPolyData *pd, int *numLines, vtkIdType **lines )
{
  vtkCellArray *pdLines;
  int size, i;
  vtkIdType npts;
  const vtkIdType *pts;
  int pos = 0;

  (*numLines) = pd->GetNumberOfLines();
  pdLines = pd->GetLines();
  size = pdLines->GetNumberOfConnectivityEntries();
  (*lines) = new vtkIdType [size];

  pdLines->InitTraversal();
  while ( pdLines->GetNextCell( npts, pts ) ) {
    (*lines)[pos] = npts;
    for (i = 0; i < npts; i++) {
      (*lines)[pos+i+1] = pts[i];
    }
    pos += (npts+1);
  }

  if ( pos > size ) {
    printf("ERR [VtkUtils_GetAllLines]: unexpected vtkCellArray result\n");
    delete [] (*lines);
    return SV_ERROR;
  }

  return SV_OK;
}


// --------------------
// VtkUtils_GetAllPolys
// --------------------
// This function simply returns a copy of all the connectivities in
// the vtkPolyData's vtkCellArray for polygons.  (Recall that
// vtkPolyData uses four separate instances of vtkCellArray to
// describe each of: {vertices, polyvertices}, {lines, polylines},
// {polygons}, {triangle strips}.

// Contrast these semantics with VtkUtils_GetLines, which returns only
// line segments, failing if any polylines are encountered.

int VtkUtils_GetAllPolys( vtkPolyData *pd, int *numPgns, vtkIdType **pgns )
{
  vtkCellArray *pdPgns;
  int size, i;
  vtkIdType npts;
  const vtkIdType *pts;
  int pos = 0;

  (*numPgns) = pd->GetNumberOfPolys();
  pdPgns = pd->GetPolys();
  size = pdPgns->GetNumberOfConnectivityEntries();
  (*pgns) = new vtkIdType [size];

  pdPgns->InitTraversal();
  while ( pdPgns->GetNextCell( npts, pts ) ) {
    (*pgns)[pos] = npts;
    for (i = 0; i < npts; i++) {
      (*pgns)[pos+i+1] = pts[i];
    }
    pos += (npts+1);
  }

  if ( pos > size ) {
    printf("ERR [VtkUtils_GetAllPolys]: unexpected vtkCellArray result\n");
    delete [] (*pgns);
    return SV_ERROR;
  }

  return SV_OK;
}


// -----------------
// VtkUtils_GetLines
// -----------------
// The result is a set of (*numLines) index pairs.  No polylines are
// described by the (*lines) array.  The caller must be sure to delete
// (*lines) if the function succeeds.

int VtkUtils_GetLines( vtkPolyData *pd, vtkIdType **lines, int *numLines )
{
  vtkCellArray *pdLines;
  vtkIdType npts;
  const vtkIdType *pts;
  int size;
  int pos = 0;

  *numLines = pd->GetNumberOfLines();
  if (pd->GetNumberOfLines() == 0) {
      return SV_ERROR;
  }

  pdLines = pd->GetLines();
  size = pdLines->GetNumberOfConnectivityEntries();
  if ( size < ( (*numLines) * 2 ) ) {
    assert(0);
  }
  *lines = new vtkIdType [size];

  pdLines->InitTraversal();
  while ( pdLines->GetNextCell( npts, pts ) ) {
    if ( npts != 2 ) {
      printf("ERR: unexpected polyline encountered\n");
      delete [] (*lines);
      return SV_ERROR;
    }
    (*lines)[2*pos] = pts[0];
    (*lines)[2*pos+1] = pts[1];
    pos++;
  }

  return SV_OK;
}


// -----------------------
// VtkUtils_GetLinkedLines
// -----------------------
// Returns the line indices of all lines which refer to the given
// ptIx.

int VtkUtils_GetLinkedLines( vtkIdType *lines, int numLines, int ptIx,
			     int **lineIxs, int *numLineIxs )
{
  int i, a, b;
  int num = 0;
  int *tmp;
  int result = 0;

  // Extreme case: *all* lines refer to ptIx
  tmp = new int [numLines];

  for (i = 0; i < numLines; i++) {
    a = lines[i*2];
    b = lines[(i*2)+1];
    if ( ( a == ptIx ) || ( b == ptIx ) ) {
      tmp[num] = i;
      num++;
      result = 1;
    }
  }

  *numLineIxs = num;
  if ( num > 0 ) {
    *lineIxs = new int [num];
    for (i = 0; i < num; i++) {
      (*lineIxs)[i] = tmp[i];
    }
  }
  // (*lineIxs) now contains a list of (*numLineIxs) line indices

  delete [] tmp;
  return result;
}


// --------------
// SearchComplete
// --------------

static int SearchComplete( int *state, int stateSz )
{
  int i;

  for (i = 0; i < stateSz; i++) {
    if ( ( state[i] == START ) || ( state[i] == INTERMED ) ) {
      return SV_ERROR;
    }
  }
  return SV_OK;
}


// ----------
// MarkClosed
// ----------

static void MarkClosed( int *state, int stateSz )
{
  int i;

  for (i = 0; i < stateSz; i++) {
    if ( state[i] == INTERMED ) {
      state[i] = CLOSED;
    }
  }
}


// -----------
// MarkDeadEnd
// -----------

static void MarkDeadEnd( int *state, int stateSz )
{
  int i;

  for (i = 0; i < stateSz; i++) {
    if ( state[i] == INTERMED ) {
      state[i] = DEADEND;
    }
  }
}


// ---------
// GetNextPt
// ---------
// Returns the index of a point linked to curr but which is not prev.
// If prev is -1, then just choose one of curr's neighbors.

static int GetNextPt( int prev, int curr, int *state, int stateSz,
		      vtkIdType *lines, int numLines )
{
  int *linkedLineIxs = nullptr;
  int numLinkedLineIxs;
  int next;
  int a1, a2, b1, b2;

  VtkUtils_GetLinkedLines( lines, numLines, curr, &linkedLineIxs,
			   &numLinkedLineIxs );

  if ( numLinkedLineIxs != 2 ) {
    if ( linkedLineIxs != nullptr ) delete [] linkedLineIxs;
    return -1;
  }

  a1 = lines[ ( linkedLineIxs[0] * 2 ) ];
  a2 = lines[ ( linkedLineIxs[0] * 2 ) + 1 ];
  b1 = lines[ ( linkedLineIxs[1] * 2 ) ];
  b2 = lines[ ( linkedLineIxs[1] * 2 ) + 1 ];

  if ( prev < 0 ) {
    if ( curr == a1 ) {
      next = a2;
    } else if ( curr == a2 ) {
      next = a1;
    } else if ( curr == b1 ) {
      next = b2;
    } else if ( curr == b2 ) {
      next = b1;
    } else {
      printf("RUH-ROH...\n");
      next = -1;
    }
  } else {
    if ( ( prev == a1 ) && ( curr == b1 ) ) {
      next = b2;
    } else if ( ( prev == a1 ) && ( curr == b2 ) ) {
      next = b1;
    } else if ( ( prev == a2 ) && ( curr == b1 ) ) {
      next = b2;
    } else if ( ( prev == a2 ) && ( curr == b2 ) ) {
      next = b1;
    } else if ( ( prev == b1 ) && ( curr == a1 ) ) {
      next = a2;
    } else if ( ( prev == b1 ) && ( curr == a2 ) ) {
      next = a1;
    } else if ( ( prev == b2 ) && ( curr == a1 ) ) {
      next = a2;
    } else if ( ( prev == b2 ) && ( curr == a2 ) ) {
      next = a1;
    } else {
      printf("RUH-ROH...\n");
      next = -1;
    }
  }

  delete [] linkedLineIxs;
  return next;
}


// ----------
// GetStartPt
// ----------

static int GetStartPt( int *state, int stateSz )
{
  int i;

  for (i = 0; i < stateSz; i++) {
    if ( state[i] == START ) {
      return i;
    }
  }
  return -1;
}


// ------------------------------
// VtkUtils_FindClosedLineRegions
// ------------------------------
// Outputs
//   startIxs: list of pt id's at which to start region traversals
//   numregions: number of items in startIxs <--> number of closed regions

int VtkUtils_FindClosedLineRegions( vtkIdType *lines, int numLines, int numPts,
				    int **startIxs, int *numRegions )
{
  int *state;
  int prev, curr, next;
  int status = SV_OK;
  int i;

  if ( ( numLines == 0 ) || ( numPts == 0 ) ) {
    *numRegions = 0;
    return SV_ERROR;
  }

  // Init:
  *numRegions = 0;
  *startIxs = new int [numPts];
  state = new int [numPts];
  for (i = 0; i < numPts; i++) {
    state[i] = START;
  }

  prev = lines[0];
  curr = lines[1];

  state[prev] = INTERMED;
  state[curr] = INTERMED;

  //  printf( "start --> %d --> %d --> ", prev, curr );

  while ( ! SearchComplete( state, numPts ) ) {

    next = GetNextPt( prev, curr, state, numPts, lines, numLines );

    if ( next < 0 ) {

      //      printf( "deadend\n" );

      MarkDeadEnd( state, numPts );
      prev = -1;
      curr = GetStartPt( state, numPts );
      if (curr == -1) break;
      state[curr] = INTERMED;

      //      printf( "start --> %d --> ", curr );

      continue;
    }

    switch ( state[next] ) {

    case START:

      //      printf( "%d --> ", next );

      state[next] = INTERMED;
      prev = curr;
      curr = next;
      break;

    case INTERMED:

      //      printf( "%d --> closed\n", next );

      MarkClosed( state, numPts );
      (*startIxs)[*numRegions] = next;
      (*numRegions)++;
      prev = -1;
      curr = GetStartPt( state, numPts );
      if (curr == -1) break;
      state[curr] = INTERMED;

      //      printf( "start --> %d --> ", curr );

      break;

    case DEADEND:

      //      printf( "%d --> joining deadend\n", next );

      MarkDeadEnd( state, numPts );
      prev = -1;
      curr = GetStartPt( state, numPts );
      if (curr == -1) break;
      state[curr] = INTERMED;

      //      printf( "start --> %d --> ", curr );

      break;

    case CLOSED:
      printf("ERR: Unexpected node state transition.\n");
      status = SV_ERROR;
      break;

    default:
      printf("RUH-ROH...\n");
      status = SV_ERROR;
      break;
    }

  }

  // Clean up;
  delete [] state;

  return status;
}


// ----------------------------
// VtkUtils_GetClosedLineRegion
// ----------------------------

int VtkUtils_GetClosedLineRegion( vtkIdType *lines, int numLines, int startIx,
				  int **lineIds, int *numLineIds )
{
  int *tmpLines;
  int numTmp = 0;
  int *linkedLineIxs;
  int numLinkedLineIxs;
  int *lineVisited;
  int i, lineA, lineB, targetIx;
  int a, b, c, d;

  tmpLines = new int [numLines];
  lineVisited = new int [numLines];
  for (i = 0; i < numLines; i++) {
    lineVisited[i] = 0;
  }

  targetIx = startIx;
  while ( VtkUtils_GetLinkedLines( lines, numLines, targetIx,
				   &linkedLineIxs, &numLinkedLineIxs ) ) {

    //    printf("%d --> ", targetIx);

    // Open contour: error
    if ( numLinkedLineIxs == 1 ) {
      printf("ERR: free edge found\n");
      delete [] tmpLines;
      delete [] lineVisited;
      delete [] linkedLineIxs;
      return SV_ERROR;
    }

    // Weird connection (e.g. triple point?):
    else if ( numLinkedLineIxs != 2 ) {
      printf("ERR: unexpected topology\n");
      delete [] tmpLines;
      delete [] lineVisited;
      delete [] linkedLineIxs;
      return SV_ERROR;
    }

    // Normal case:
    else {

      lineA = linkedLineIxs[0];
      lineB = linkedLineIxs[1];
      delete [] linkedLineIxs;

      a = lines[ lineA * 2 ];
      b = lines[ (lineA * 2) + 1 ];
      c = lines[ lineB * 2 ];
      d = lines[ (lineB * 2) + 1 ];

      if ( ( lineVisited[ lineA ] ) && ( lineVisited[ lineB ] ) ) {
	break;
      }

      if ( lineVisited[ lineA ] ) {
	tmpLines[numTmp] = lineB; // lineB is the next segment we want to visit
	lineVisited[ lineB ] = 1; // mark lineB as visited
	if ( ( a == c ) || ( b == c ) ) {
	  targetIx = d;
	} else {
	  targetIx = c;
	}
      } else {
	tmpLines[numTmp] = lineA;
	lineVisited[ lineA ] = 1;
	if ( ( c == a ) || ( d == a ) ) {
	  targetIx = b;
	} else {
	  targetIx = a;
	}
      }
      numTmp++;
    }
  }

  //  printf("\n");

  *lineIds = new int [numTmp];
  *numLineIds = numTmp;
  for (i = 0; i < numTmp; i++) {
    (*lineIds)[i] = tmpLines[i];
  }


  delete [] tmpLines;
  delete [] lineVisited;

  return SV_OK;
}


// --------------------------------
// VtkUtils_MakePolyDataFromLineIds
// --------------------------------

int VtkUtils_MakePolyDataFromLineIds( double *pts, int numPts, vtkIdType *lines,
				      int *lineIds, int numLineIds,
				      vtkPolyData **pd )
{
  int i;
  vtkPoints *pdPts = vtkPoints::New();
  vtkCellArray *pdLines = vtkCellArray::New();

  *pd = vtkPolyData::New();

  for (i = 0; i < numPts; i++) {
    pdPts->InsertNextPoint( (vtkFloatingPointType)(pts[i*3]), (vtkFloatingPointType)(pts[i*3+1]),
			    (vtkFloatingPointType)(pts[i*3+2]) );
  }

  pdLines->InitTraversal();
  for (i = 0; i < numLineIds; i++) {

    //    printf("[%d]: %d, %d\n", i, lines[ lineIds[i] * 2 ],
    //	   lines[ lineIds[i] * 2 + 1]);

    pdLines->InsertNextCell( 2, &(lines[ lineIds[i] * 2 ]) );
  }

  (*pd)->SetPoints( pdPts );
  (*pd)->SetLines( pdLines );
  pdPts->Delete();
  pdLines->Delete();

  return SV_OK;
}



// ---------
// SomeFalse
// ---------

static int SomeFalse( int *arr, int sz )
{
  int i;

  for (i = 0; i < sz; i++) {
    if ( ! (arr[i]) ) {
      return SV_OK;
    }
  }
  return SV_ERROR;
}


// -------------
// FirstFalsePos
// -------------

static int FirstFalsePos( int *arr, int sz )
{
  int i;

  for (i = 0; i < sz; i++) {
    if ( ! (arr[i]) ) {
      return i;
    }
  }
  return -1;
}


// -----------------------
// VtkUtils_MakeShortArray
// -----------------------
// Allocates and returns memory which the caller becomes responsible for.

int VtkUtils_MakeShortArray( vtkDataArray *s, int *num, short **dataOut )
{
  int sz;
  int i;

  // Data types are set in vtk/common/vtkSetGet.h.
  if ( s->GetDataType() != VTK_SHORT ) {
    return SV_ERROR;
  }

  sz = s->GetNumberOfTuples();
  *dataOut = new short [sz];
  if ( *dataOut == nullptr ) {
    return SV_ERROR;
  }

  *num = sz;
  for ( i = 0; i < sz; i++ ) {
    (*dataOut)[i] = (short)s->GetTuple1(i);
  }

  return SV_OK;
}


// -----------------------
// VtkUtils_MakeFloatArray
// -----------------------
// Allocates and returns memory which the caller becomes responsible for.

int VtkUtils_MakeFloatArray( vtkDataArray *s, int *num, float **dataOut )
{
  int sz;
  int i;

  // Data types are set in vtk/common/vtkSetGet.h.
  if ( s->GetDataType() != VTK_FLOAT ) {
    return SV_ERROR;
  }

  sz = s->GetNumberOfTuples();
  *dataOut = new float [sz];
  if ( *dataOut == nullptr ) {
    return SV_ERROR;
  }

  *num = sz;
  for ( i = 0; i < sz; i++ ) {
    (*dataOut)[i] = (float)s->GetTuple1(i);
  }

  return SV_OK;
}


// -----------------------
// VtkUtils_DeepCopyPoints
// -----------------------

vtkPoints *VtkUtils_DeepCopyPoints( vtkPoints *ptsIn )
{
  int numPts;
  vtkPoints *ptsOut;
  vtkFloatingPointType pt[3];
  int i;

  if ( ptsIn == nullptr ) {
    return nullptr;
  }

  numPts = ptsIn->GetNumberOfPoints();
  ptsOut = vtkPoints::New();

  for ( i = 0; i < numPts; i++ ) {
    ptsIn->GetPoint( i, pt );
    ptsOut->InsertNextPoint( pt[0], pt[1], pt[2] );
  }

  return ptsOut;
}


// ----------------------
// VtkUtils_DeepCopyCells
// ----------------------
// This should achieve the desired deep copy.

vtkCellArray *VtkUtils_DeepCopyCells( vtkCellArray *cellsIn )
{
  vtkCellArray *cellsOut;
  vtkIdType npts;
  const vtkIdType *idlist;

  if ( cellsIn == nullptr ) {
    return nullptr;
  }

  cellsOut = vtkCellArray::New();

  cellsIn->InitTraversal();
  cellsOut->InitTraversal();
  while ( cellsIn->GetNextCell( npts, idlist ) ) {
    cellsOut->InsertNextCell( npts, idlist );
  }

  return cellsOut;
}


// ----------------------------
// VtkUtils_MakePolysConsistent
// ----------------------------

int VtkUtils_MakePolysConsistent( vtkPolyData *pd )
{
  vtkPolyData *tmp;

  vtkPolyDataNormals *nrm = vtkPolyDataNormals::New();
  nrm->ConsistencyOn();
  nrm->SplittingOff();
  nrm->SetInputDataObject( pd );
  nrm->Update();

  tmp = nrm->GetOutput();
  pd->CopyStructure( tmp );
  nrm->Delete();

  return SV_OK;

  /* I give up! [12/14/99]
   * ---

  int i, j, k, npts;
  int numCells = pd->GetNumberOfCells();
  int *visited;
  vtkCell *cell;
  vtkIdList *ptIds;
  vtkIdList *neighborIds = vtkIdList::New();
  int curr, next;
  int numReversed = 0;
  int foundNeighborEdge;

  // Neighbor cell:
  int numNeighbors, n, o, p, neighborId;
  vtkCell *neighborCell;
  vtkIdList *neighborPtIds;
  int neighborNpts;
  int neighborCurr, neighborNext;

  visited = new int [numCells];
  for ( i = 0; i < numCells; i++ ) {
    visited[i] = 0;
  }

  pd->BuildCells();
  pd->BuildLinks();  // GetCellEdgeNeighbors depends on vtkCellLinks

  // Foreach cell i:
  for ( i = 0; i < numCells; i++ ) {

    if ( visited[i] ) {
      continue;
    }

    cell = pd->GetCell( i );
    ptIds = cell->GetPointIds();
    npts = ptIds->GetNumberOfIds();
    if ( ( cell->GetCellType() == VTK_VERTEX ) ||
	 ( cell->GetCellType() == VTK_POLY_VERTEX ) ||
	 ( cell->GetCellType() == VTK_LINE ) ||
	 ( cell->GetCellType() == VTK_POLY_LINE ) ||
	 ( cell->GetCellType() == VTK_TRIANGLE_STRIP ) ) {
      visited[i] = 1;
      continue;
    }
    assert( npts > 2 );

    // Foreach edge e of cell i:
    for ( j = 0; j < npts; j++ ) {
      if ( j == (npts-1) ) {
	k = 0;
      } else {
	k = j + 1;
      }
      curr = ptIds->GetId(j);
      next = ptIds->GetId(k);

      // Now we have an edge (curr,next) of the current cell.  We want
      // to take this edge and query the poly data for cell edge
      // neighbors.  The GetCellEdgeNeighbors method returns the
      // global cell ids of any cell which uses both points curr and
      // next.  Note that as of vtk 2.4, no check is done to see that
      // curr and next are in order.
      pd->GetCellEdgeNeighbors( i, curr, next, neighborIds );
      numNeighbors = neighborIds->GetNumberOfIds();

      if ( numNeighbors > 2 ) {
	printf("ERR: Non-manifold edge encountered [%d, %d].\n", curr, next);
      }

      // Foreach neighboring cell n along edge e:
      for ( n = 0; n < numNeighbors; n++ ) {
	neighborId = neighborIds->GetId(n);
	if ( neighborId == i ) {
	  continue;
	}
	if ( visited[neighborId] ) {
	  continue;
	}
	neighborCell = pd->GetCell( neighborId );
	neighborPtIds = neighborCell->GetPointIds();
	neighborNpts = neighborPtIds->GetNumberOfIds();
	if ( ( neighborCell->GetCellType() == VTK_VERTEX ) ||
	     ( neighborCell->GetCellType() == VTK_POLY_VERTEX ) ||
	     ( neighborCell->GetCellType() == VTK_LINE ) ||
	     ( neighborCell->GetCellType() == VTK_POLY_LINE ) ||
	     ( neighborCell->GetCellType() == VTK_TRIANGLE_STRIP ) ) {
	  continue;
	}
	assert( neighborNpts > 2 );

	// Foreach consecutive point pair in neighborPtIds:
	foundNeighborEdge = 0;
	for ( o = 0; o < neighborNpts; o++ ) {
	  if ( o == (neighborNpts-1) ) {
	    p = 0;
	  } else {
	    p = o + 1;
	  }
	  neighborCurr = ptIds->GetId(o);
	  neighborNext = ptIds->GetId(p);

	  if ( ! ( ( neighborCurr == curr ) && ( neighborNext == next ) ) &&
	       ! ( ( neighborCurr == next ) && ( neighborNext == curr ) ) ) {
	    continue;
	  }

	  if ( ( neighborCurr == curr ) && ( neighborNext == next ) ) {
	    pd->ReverseCell( neighborId );
	    numReversed++;
	  }
	  visited[neighborId] = 1;
	  foundNeighborEdge = 1;
	  break;
	} // edges of cell-edge-neighbor

	if ( foundNeighborEdge ) {
	  break;
	} else {
	  printf( "ERR: Failed to find neighbor at [%d, %d].\n", curr, next );
	}
      } // cell-edge-neighbor traversal

    } // edges of ith cell

    visited[i] = 1;

  } // cell traversal

  printf("  \n\n------ VtkUtils_MakePolysConsistent ------\n");
  printf("  >>>>>> num polys           [%d]\n", pd->GetNumberOfPolys());
  printf("  >>>>>> num polys reversed  [%d]\n", numReversed);

  neighborIds->Delete();
  delete [] visited;

  return SV_OK;

  * --- */
}


// ------------------------
// VtkUtils_ReverseAllCells
// ------------------------

int VtkUtils_ReverseAllCells( vtkPolyData *pd )
{
  int i;
  int numCells = pd->GetNumberOfCells();

  for ( i = 0; i < numCells; i++ ) {
    pd->ReverseCell( i );
  }

  return SV_OK;
}


/**************************************************************
 *                                                            *
 * VtkUtils_GetOrderedPoints                                  *
 * -------------------------                                  *
 *                                                            *
 * Returns orderedPts, the set of points associated with      *
 * inputData.  The points appear in the same order as if you  *
 * were tracing out the contour associated with inputData.    *
 * Direction determines whether the points appear in clockwise*
 * or counter-clockwise order.                                *
 *                                                            *
 * direction: 0 (CCW), 1 (CW)                                 *
 * format for orderedPts:  pt1_X, pt1_Y, pt1_Z, pt2_X, pt2_Y, *
 *                        pt2_Z, ....                         *
 *                                                            *
 **************************************************************/


int VtkUtils_GetOrderedPoints(vtkPolyData *inputData, int direction,  double **orderedPts, int *numOrderedPts)
{
  double *pts, *reversedPts;

  int numPts, numPtsPerPoly;

  int numLines;

  vtkIdType *lines;

  vtkIdType *formattedLines;

  int startIndex = 0;

  int numOrderedLineIndices;

  int *orderedLineIndices;

  int i, j;

  int lineIndex, ptIndex;

  int currentDirection;

  /*****************
   *     body      *
   *****************/

  // Get point and line data.  Note that VtkUtils_GetPoints,
  // VtkUtils_GetAllLines, VtkUtils_ReformatLineIndices
  // allocate memory for their results.

  VtkUtils_GetPoints(inputData, &pts, &numPts);
  VtkUtils_GetAllLines(inputData, &numLines, &lines);


  // Reformat the array lines so it can be used with
  // VtkUtils_GetClosedLineRegion.

  numPtsPerPoly = lines[0];
  formattedLines = new vtkIdType [numPtsPerPoly * numLines];

  for (i = 0; i < numLines; i++)
    for (j = 0; j < numPtsPerPoly; j++)
      formattedLines[i * numPtsPerPoly + j] = lines[i * (numPtsPerPoly + 1) + 1 + j];


  // This returns a list of line indices in order of connectivity
  VtkUtils_GetClosedLineRegion (formattedLines, numLines, startIndex, &orderedLineIndices, &numOrderedLineIndices);


  // Allocate memory for the points and get the coordinates for the
  // points, using the ordered line indices information.
  *orderedPts = new double [numPts * 3];

  for (i = 0; i < numOrderedLineIndices; i++)
    {
      lineIndex = orderedLineIndices[i];
      ptIndex = formattedLines[lineIndex * 2];

      // Copy point coordinates to orderedPts array
      for (j = 0; j < 3; j++)
 	(*orderedPts)[i * 3 + j] = pts[ptIndex * 3 + j];
    }

  *numOrderedPts = numPts;


  // Compute the direction/orientation of the points.  If the
  // direction of the points isn't the desired direction, reverse
  // the order of the points.

  VtkUtils_CalcDirection(*orderedPts, *numOrderedPts, &currentDirection);


  if (currentDirection != direction)
    {
      VtkUtils_ReversePtList(*numOrderedPts, *orderedPts, &reversedPts);

      for (i = 0; i < *numOrderedPts; i++)
	for (j = 0; j < 3; j++)
	  (*orderedPts)[i * 3 + j] = reversedPts[i * 3 + j];

      delete [] reversedPts;
    }

  // Clean up
  delete [] pts;
  delete [] lines;
  delete [] orderedLineIndices;
  delete [] formattedLines;

  return SV_OK;
}



/**************************************************************
 *                                                            *
 * VtkUtils_CalcDirection                                     *
 * ----------------------                                     *
 *                                                            *
 * An ad-hoc method for determining whether an array of points*
 * are ordered in a clockwise or counter-clockwise direction. *
 *                                                            *
 * format for pts:  pt1_X, pt1_Y, pt1_Z, pt2_X, pt2_Y,        *
 *                        pt2_Z, ....                         *
 * Output = currentDirection: 0 (CCW), 1 (CW)                 *
 *                                                            *
 **************************************************************/


int VtkUtils_CalcDirection(double *pts, int numPts, int *currentDirection)
{
  double centroid[3], tmpPt[3];

  double *angles, tmpAngle;

  int incrAngleCount, decrAngleCount;

  int index1, index2;

  int i, j;

  int numDim;

  /*****************
   *     body      *
   *****************/

  numDim = 3;

  // Now figure out direction of points

  cgeom_CalcCentroid(pts, numPts, numDim, centroid);

  angles = new double[numPts];
  incrAngleCount = 0;
  decrAngleCount = 0;

  // Loop 1:  Calculate angles for all points
  for (i = 0; i < numPts; i++)
    {
      for (j = 0; j < 3; j++)
	tmpPt[j] = pts[i * 3 + j];

      cgeom_CalcAngle(centroid, tmpPt, &tmpAngle);

      angles[i] = tmpAngle;
    }


  // Loop 2:  Track whether angles are increasing or decreasing
  for (i = 0; i < numPts; i++)
    {
      index1 = i;

      if (i == 0)
	index2 = numPts - 1;
      else
	index2 = i - 1;

      // NOTE:  Angles are between 0 and 360.  This is a simplification
      // that doesn't account for moving between points on opposite sides
      // of the 0 deg/360 deg boundary (e.g. point1 has angle 10, point2
      // has angle 354.  point1 is oriented CCW relative to point2, so we
      // should increment incrAngleCount, but the math will cause us to
      // increment decrAngleCount.) This is okay if most of the other points
      // are well-behaved.
      if ((angles[index1] - angles[index2]) >= 0)
	incrAngleCount++;
      else
	decrAngleCount++;
    }

  if (incrAngleCount > decrAngleCount)
    *currentDirection = 0;  // CCW direction
  else
    *currentDirection = 1;  // CW direction


  // Clean up
  delete [] angles;
  //delete [] centroid;
  //delete [] tmpPt;

  return SV_OK;
}


// ----------------------
// VtkUtils_ReversePtList
// ----------------------

int VtkUtils_ReversePtList( int num, double ptsIn[], double *ptsOut[] )
{
  int i;
  int rev;

  *ptsOut = new double [3*num];
  for ( i = 0; i < num; i++ ) {
    rev = num - i - 1;
    (*ptsOut)[3*i] = ptsIn[3*rev];
    (*ptsOut)[3*i+1] = ptsIn[3*rev+1];
    (*ptsOut)[3*i+2] = ptsIn[3*rev+2];
  }

  return SV_OK;
}

// ----------------------
// VtkUtils_PDCheckArrayName
// ----------------------
/**
 * @brief Function to check is array with name exists in cell or point data
 * @param object this is the object to check if the array exists
 * @param datatype this is point or cell. point =0,cell=1
 * @param arrayname this is the name of the array to check
 * @reutrn this returns 1 if the array exists and zero if it doesn't
 * or the function does not return properly.
 */

int VtkUtils_PDCheckArrayName(vtkPolyData *object,int datatype,std::string arrayname )
{
  vtkIdType i;
  int numArrays;
  int exists =0;

  if (datatype == 0)
  {
    numArrays = object->GetPointData()->GetNumberOfArrays();
    for (i=0;i<numArrays;i++)
    {
      if (!strcmp(object->GetPointData()->GetArrayName(i),arrayname.c_str()))
      {
	exists =1;
      }
    }
  }
  else
  {
    numArrays = object->GetCellData()->GetNumberOfArrays();
    for (i=0;i<numArrays;i++)
    {
      if (!strcmp(object->GetCellData()->GetArrayName(i),arrayname.c_str()))
      {
	exists =1;
      }
    }
  }

  if (exists == 1)
  {
    return SV_OK;
  }
  else
  {
    return SV_ERROR;
  }
}

// -------------------
// VtkUtils_UGCheckArrayName
// -------------------
/**
 * @brief Function to check is array with name exists in cell or point data
 * @param object this is the object to check if the array exists
 * @param datatype this is point or cell. point =0,cell=1
 * @param arrayname this is the name of the array to check
 * @reutrn this returns 1 if the array exists and zero if it doesn't
 * or the function does not return properly.
 */

int VtkUtils_UGCheckArrayName(vtkUnstructuredGrid *object,int datatype,std::string arrayname)
{
  vtkIdType i;
  int numArrays;
  int exists =0;

  if (datatype == 0)
  {
    numArrays = object->GetPointData()->GetNumberOfArrays();
    for (i=0;i<numArrays;i++)
    {
      if (!strcmp(object->GetPointData()->GetArrayName(i),arrayname.c_str()))
      {
	exists =1;
      }
    }
  }
  else
  {
    numArrays = object->GetCellData()->GetNumberOfArrays();
    for (i=0;i<numArrays;i++)
    {
      if (!strcmp(object->GetCellData()->GetArrayName(i),arrayname.c_str()))
      {
	exists =1;
      }
    }
  }

  if (exists == 1)
  {
    return SV_OK;
  }
  else
  {
    return SV_ERROR;
  }
}
