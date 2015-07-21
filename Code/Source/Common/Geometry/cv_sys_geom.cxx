/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
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
 *
*=========================================================================*/

#include "SimVascular.h" 

#include <stdio.h>
#include <assert.h>
#include <math.h>
#include "cv_sys_geom.h"
#include "cvVTK.h"

#include "cv_vtk_utils.h"
#include "cv_misc_utils.h"
#include "cv_ggems.h"
#include "cvMath.h"
#include "cvSolidModel.h"

#include "vtkSurfaceBooleanOperations.h"
#include "vtkSmartPointer.h"
#include "vtkSortDataArray.h"
#include "vtkPolygon.h"
#include "vtkFillHolesFilterWithIds.h"
#include "vtkThreshold.h"
#include "vtkConnectivityFilter.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkAppendPolyData.h"

#include "vtkLocalQuadricDecimation.h"
#include "vtkLocalSmoothPolyDataFilter.h"
#include "vtkLocalLinearSubdivisionFilter.h"
#include "vtkCGSmooth.h"
#include "vtkFindSeparateRegions.h"
#include "vtkGetSphereRegions.h"

#define vtkNew(type,name) \
  vtkSmartPointer<type> name = vtkSmartPointer<type>::New()

#ifdef USE_VMTK
#include "vtkvmtkPolyDataDistanceToCenterlines.h"
#include "vtkvmtkPolyDataCenterlines.h"
#include "vtkvmtkCapPolyData.h"
#include "vtkvmtkSimpleCapPolyData.h"
#endif

#include "vtkMultiplePolyDataIntersectionFilter.h"
#include "vtkBooleanOperationPolyDataFilter2.h"
#include "vtkIntersectionPolyDataFilter2.h"
#include "vtkLoftPolyDataSolid.h"

#include "cv_polydatasolid_utils.h"

/* ----------------- */
/* sys_geom_DeepCopy */
/* ----------------- */

cvPolyData *sys_geom_DeepCopy( cvPolyData *src )
{
  cvPolyData *dst;
  vtkPolyData *srcPd = src->GetVtkPolyData();
  vtkPolyData *pd;
  vtkPoints *pts;
  vtkCellArray *verts, *lines, *polys, *strips;

  pts = VtkUtils_DeepCopyPoints( srcPd->GetPoints() );
  if ( pts == NULL ) {
    return NULL;
  }

  verts = VtkUtils_DeepCopyCells( srcPd->GetVerts() );
  lines = VtkUtils_DeepCopyCells( srcPd->GetLines() );
  polys = VtkUtils_DeepCopyCells( srcPd->GetPolys() );
  strips = VtkUtils_DeepCopyCells( srcPd->GetStrips() );

  pd = vtkPolyData::New();
  pd->SetPoints( pts );
  pd->SetVerts( verts );
  pd->SetLines( lines );
  pd->SetPolys( polys );
  pd->SetStrips( strips );

  pts->Delete();
  verts->Delete();
  lines->Delete();
  polys->Delete();
  strips->Delete();

  pd->GetPointData()->DeepCopy( srcPd->GetPointData() );
  pd->GetCellData()->DeepCopy( srcPd->GetCellData() );

  dst = new cvPolyData( pd );
  pd->Delete();
  return dst;
}


/* ----------------- */
/* sys_geom_MergePts */
/* ----------------- */

cvPolyData *sys_geom_MergePts( cvPolyData *src )
{
  double tol = 1e10 * FindMachineEpsilon();
  return sys_geom_MergePts_tol( src, tol );
}


/* --------------------- */
/* sys_geom_MergePts_tol */
/* --------------------- */

cvPolyData *sys_geom_MergePts_tol( cvPolyData *src, double tol )
{
  cvPolyData *dst;

  vtkCleanPolyData *merge = vtkCleanPolyData::New();
  merge->SetTolerance( tol );
  //  merge->ConvertLinesToPointsOn();  // new method as of vtk 3.2.0
  merge->SetInputDataObject( src->GetVtkPolyData() );
  merge->Update();

  dst = new cvPolyData( merge->GetOutput() );
  merge->Delete();  
  return dst;
}


/* ----------------------------- */
/* sys_geom_NumClosedLineRegions */
/* ----------------------------- */

int sys_geom_NumClosedLineRegions( cvPolyData *src, int *num )
{
  cvPolyData *merged_pd;
  vtkPolyData *pd;
  int numPts;
  double *pts;
  vtkIdType *lines;
  int numLines;
  int *startIxs;
  int numRegions;

  merged_pd = sys_geom_MergePts( src );
  if ( merged_pd == NULL ) {
    return CV_ERROR;
  }
  pd = merged_pd->GetVtkPolyData();

  if ( VtkUtils_GetPoints( pd, &pts, &numPts ) != CV_OK ) {
    printf("ERR: VtkUtils_GetPoints failed\n");
    delete merged_pd;
    return CV_ERROR;
  }

  if ( VtkUtils_GetLines( pd, &lines, &numLines ) != CV_OK ) {
    printf("ERR: VtkUtils_GetLines failed\n");
    delete merged_pd;
    delete [] pts;
    return CV_ERROR;
  }

  if ( VtkUtils_FindClosedLineRegions( lines, numLines, numPts,
				       &startIxs, &numRegions ) != CV_OK ) {
    printf("ERR: VtkUtils_FindClosedLineRegions failed\n");
    delete merged_pd;
    delete [] pts;
    delete [] lines;
    return CV_ERROR;
  }

  *num = numRegions;
  delete merged_pd;
  delete [] pts;
  delete [] lines;
  return CV_OK;
}


/* ---------------------------- */
/* sys_geom_GetClosedLineRegion */
/* ---------------------------- */

int sys_geom_GetClosedLineRegion( cvPolyData *src, int id, cvPolyData **dst )
{
  cvPolyData *merged_pd;
  vtkPolyData *pd;
  vtkPolyData *tmp = NULL;
  int numPts;
  double *pts;
  vtkIdType *lines;
  int numLines;
  int *startIxs;
  int numRegions;
  int *regionLines;
  int numRegionLines;
  int status = CV_ERROR;

  merged_pd = sys_geom_MergePts( src );
  if ( merged_pd == NULL ) {
    return CV_ERROR;
  }
  pd = merged_pd->GetVtkPolyData();

  if ( VtkUtils_GetPoints( pd, &pts, &numPts ) != CV_OK ) {
    printf("ERR: VtkUtils_GetPoints failed\n");
    return CV_ERROR;
  }

  if ( VtkUtils_GetLines( pd, &lines, &numLines ) != CV_OK ) {
    printf("ERR: VtkUtils_GetLines failed\n");
    delete [] pts;
    return CV_ERROR;
  }

  if ( VtkUtils_FindClosedLineRegions( lines, numLines, numPts,
				       &startIxs, &numRegions ) != CV_OK ) {
    printf("ERR: VtkUtils_FindClosedLineRegions failed\n");
    delete [] pts;
    delete [] lines;
    return CV_ERROR;
  }

  if ( ( id < 0 ) || ( id >= numRegions ) ) {
    printf("ERR: region id [%d] out of range\n", id);
    delete [] pts;
    delete [] lines;
    delete [] startIxs;
    return CV_ERROR;
  }

  if ( VtkUtils_GetClosedLineRegion( lines, numLines, startIxs[id],
				     &regionLines, &numRegionLines )
       != CV_OK ) {
    printf("ERR: VtkUtils_GetClosedLineRegion failed\n");
    delete [] pts;
    delete [] lines;
    delete [] startIxs;
    return CV_ERROR;
  }

  if ( VtkUtils_MakePolyDataFromLineIds( pts, numPts, lines, regionLines,
					 numRegionLines, &tmp ) != CV_OK ) {
    printf("ERR: VtkUtils_MakePolyDataFromLineIds failed\n");
    delete [] pts;
    delete [] lines;
    delete [] startIxs;
    delete [] regionLines;
    return CV_ERROR;
  }

  (*dst) = new cvPolyData( tmp );
  delete [] pts;
  delete [] lines;
  delete [] startIxs;
  delete [] regionLines;
  tmp->Delete();
  return CV_OK;
}


/* ------------- */
/* sys_geom_Pick */
/* ------------- */

int sys_geom_Pick( cvPolyData *src, double pos[], cvPolyData **dst )
{
  cvPolyData *merged_pd = NULL;
  vtkPolyData *pd = NULL;
  vtkPolyData *tmp = NULL;
  cvPolyData *tmpPd = NULL;
  int numPts;
  double *pts = NULL;
  vtkIdType *lines = NULL;
  int numLines;
  int *startIxs = NULL;
  int numRegions;
  int *regionLines = NULL;
  int numRegionLines;
  int i, classification;
  int status = CV_ERROR;
  cvSolidModel *solid = NULL;
  int foundOne = 0;

  solid = cvSolidModel::DefaultInstantiateSolidModel();
  if ( solid == NULL ) {
    printf("ERR: default instantiate solid model failed\n");
    return CV_ERROR;
  }

  merged_pd = sys_geom_MergePts( src );
  if ( merged_pd == NULL ) {
    printf("ERR: merge points failed failed\n");
    return CV_ERROR;
  }
  pd = merged_pd->GetVtkPolyData();

  if ( VtkUtils_GetPoints( pd, &pts, &numPts ) != CV_OK ) {
    printf("ERR: VtkUtils_GetPoints failed\n");
    return CV_ERROR;
  }

  if ( VtkUtils_GetLines( pd, &lines, &numLines ) != CV_OK ) {
    printf("ERR: VtkUtils_GetLines failed\n");
    delete [] pts;
    return CV_ERROR;
  }

  if ( VtkUtils_FindClosedLineRegions( lines, numLines, numPts,
				       &startIxs, &numRegions ) != CV_OK ) {
    printf("ERR: VtkUtils_FindClosedLineRegions failed\n");
    delete [] pts;
    delete [] lines;
    return CV_ERROR;
  }

  // Now, foreach closed region, get an ordered list of segments,
  // create the corresponding solid, and check for point
  // classification of the given pos.

  printf("  ------  sys_geom_Pick  ------\n");
  printf("  >>>>>>  num closed regions [%d]\n", numRegions);

  for (i = 0; i < numRegions; i++) {

    if ( VtkUtils_GetClosedLineRegion( lines, numLines, startIxs[i],
				       &regionLines, &numRegionLines )
	 != CV_OK ) {
      printf("ERR: VtkUtils_GetClosedLineRegion failed\n");
      delete [] pts;
      delete [] lines;
      delete [] startIxs;
      return CV_ERROR;
    }

    if ( tmp != NULL ) {
      tmp->Delete();
      tmp = NULL;
    }

    if ( VtkUtils_MakePolyDataFromLineIds( pts, numPts, lines, regionLines,
					   numRegionLines, &tmp ) != CV_OK ) {
      printf("ERR: VtkUtils_MakePolyDataFromLineIds failed\n");
      delete [] pts;
      delete [] lines;
      delete [] startIxs;
      delete [] regionLines;
      return CV_ERROR;
    } else {
      solid->Clear();
      tmpPd = new cvPolyData( tmp );
      if ( solid->MakePoly2d( tmpPd ) == CV_OK ) {
	if ( solid->ClassifyPt( pos[0], pos[1], 0, &classification )
	     == CV_OK ) {
	  if ( classification >= 0 ) {
	    *dst = tmpPd;
	    status = CV_OK;
	    printf("  >>>>>>  picked region lines [%d]\n", numRegionLines);
	    foundOne = 1;
	    break;
	  }
	} else {
	  printf("ERR: cvSolidModel::ClassifyPt failed\n");
	}
      } else {
	printf("ERR: cvSolidModel::MakePoly2d failed\n");
      }
      delete tmpPd;
    }
  }

  if ( ! foundOne ) {
    printf("  >>>>>>  no closed region selected\n");
  }

  delete [] pts;
  delete [] lines;
  delete [] startIxs;
  // be careful here since these objects will not exist if numRegions == 0
  if (numRegions != NULL) delete [] regionLines;
  if ( tmp != NULL ) tmp->Delete();
  return status;
}


/* --------------- */
/* sys_geom_Reduce */
/* --------------- */
/* Caller is responsible for cleaning up the result. */

int sys_geom_Reduce( cvPolyData *src, double tol, cvPolyData **dst )
{
  cvPolyData *merged_pd;
  vtkPolyData *pd;
  int status;
  
  merged_pd = sys_geom_MergePts( src );
  if ( merged_pd == NULL ) {
    return CV_ERROR;
  }
  pd = merged_pd->GetVtkPolyData();

  status = VtkUtils_FixTopology( pd, tol );
  if ( status != CV_OK ) {
    delete merged_pd;
    return CV_ERROR;
  }
  
  *dst = new cvPolyData( pd );
  delete merged_pd;  // virtual destructor calls Delete on vtk data obj

  return CV_OK;
}


/* ---------------------------- */
/* sys_geom_MakePolysConsistent */
/* ---------------------------- */
/* Caller is responsible for cleaning up the result. */

int sys_geom_MakePolysConsistent( cvPolyData *src, cvPolyData **dst )
{
  vtkPolyData *pdIn = src->GetVtkPolyData();
  vtkPolyData *pdCopy = vtkPolyData::New();
  vtkPoints *ptsCopy = VtkUtils_DeepCopyPoints( pdIn->GetPoints() );
  vtkCellArray *polysCopy = VtkUtils_DeepCopyCells( pdIn->GetPolys() );
  cvPolyData *result;
  int status;

  pdCopy->SetPoints( ptsCopy );
  pdCopy->SetPolys( polysCopy );
  ptsCopy->Delete();
  polysCopy->Delete();
  result = new cvPolyData( pdCopy );
  
  status = VtkUtils_MakePolysConsistent( result->GetVtkPolyData() );
  if ( status != CV_OK ) {
    delete result;
    return CV_ERROR;
  }

  *dst = result;

  return CV_OK;
}

/* -------------- */
/* sys_geom_union */
/* -------------- */

int sys_geom_union( cvPolyData *srcA, cvPolyData *srcB, double tolerance, cvPolyData **dst )
{
  vtkPolyData *a = srcA->GetVtkPolyData();
  vtkPolyData *b = srcB->GetVtkPolyData();
  cvPolyData *result = NULL;
  *dst = NULL;

  try {
    vtkNew(vtkBooleanOperationPolyDataFilter2,booleanOperator);
    booleanOperator->SetInputData(0,a);
    booleanOperator->SetInputData(1,b);
    booleanOperator->SetOperationToUnion();
    booleanOperator->SetTolerance(tolerance);
    booleanOperator->Update();

    result = new cvPolyData( booleanOperator->GetOutput() );
    *dst = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in boolean operation.\n");
    fflush(stderr);
    return CV_ERROR;
  }
  return CV_OK;
}

/* -------------- */
/* sys_geom_all_union */
/* -------------- */

int sys_geom_all_union( cvPolyData **srcs,int numSrcs,int nointerbool,double tolerance,cvPolyData **dst )
{
  cvPolyData *result = NULL;
  *dst = NULL;

  vtkNew(vtkMultiplePolyDataIntersectionFilter,vesselInter);
  for (int i=0;i<numSrcs;i++)
  {
    vtkPolyData *newPd = srcs[i]->GetVtkPolyData();
    vesselInter->AddInputData(newPd);
  }
  vesselInter->SetPassInfoAsGlobal(1);
  vesselInter->SetAssignSurfaceIds(1);
  vesselInter->SetNoIntersectionOutput(nointerbool);
  vesselInter->SetTolerance(tolerance);
  try {
    vesselInter->Update();
    result = new cvPolyData(vesselInter->GetOutput());
    *dst = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in boolean operation.\n");
    fflush(stderr);
    return CV_ERROR;
  }
  if (vesselInter->GetStatus() == 0)
  { 
    return CV_ERROR;
  }
  return CV_OK;
}

/* -------------- */
/* sys_geom_assign_ids_based_on_faces */
/* -------------- */

int sys_geom_assign_ids_based_on_faces( cvPolyData *model, cvPolyData **faces,int numFaces,int *ids,cvPolyData **dst )
{
  cvPolyData *result = NULL;
  *dst = NULL;
  vtkIdType cellId = 0;
  vtkIdType closestCell;
  vtkIdType npts;
  vtkIdType *pts;
  int subId = 0;
  double distance;
  double centroid[3];
  double closestPt[3];
  vtkNew(vtkGenericCell,genericCell);

  vtkPolyData *fullPd = model->GetVtkPolyData();
  fullPd->BuildLinks();
  vtkNew(vtkAppendPolyData,appender);
  vtkNew(vtkPolyData,facePd);
  for (int i=0;i<numFaces;i++)
  {
    vtkPolyData *newPd = faces[i]->GetVtkPolyData();
    vtkNew(vtkIntArray,scalarArray);
    scalarArray->SetName("ModelFaceID");
    for (vtkIdType cellId=0;cellId<newPd->GetNumberOfCells();cellId++)
      scalarArray->InsertNextValue(ids[i]);
    newPd->GetCellData()->AddArray(scalarArray);
    appender->AddInputData(newPd);
  }
  appender->Update();
  facePd->DeepCopy(appender->GetOutput());

  vtkNew(vtkCellLocator,cellLocator);
  cellLocator->SetDataSet(facePd);
  cellLocator->BuildLocator();
  vtkNew(vtkIntArray,newIdArray);
  newIdArray->SetName("ModelFaceID");
  vtkNew(vtkIntArray,oldIdArray);
  oldIdArray = vtkIntArray::SafeDownCast(facePd->GetCellData()->GetArray("ModelFaceID"));

  for (vtkIdType cellId=0;cellId<fullPd->GetNumberOfCells();cellId++)
  {
    fullPd->GetCellPoints(cellId,npts,pts);

    vtkNew(vtkPoints,polyPts);
    vtkNew(vtkIdTypeArray,polyPtIds);
    for (int i=0;i<npts;i++)
    {
      polyPtIds->InsertValue(i,i);
      polyPts->InsertNextPoint(fullPd->GetPoint(pts[i]));
    }
    vtkPolygon::ComputeCentroid(polyPtIds,polyPts,centroid);

    cellLocator->FindClosestPoint(centroid,closestPt,genericCell,closestCell,
	  subId,distance);
    vtkIdType faceValue = oldIdArray->GetValue(closestCell);
    newIdArray->InsertValue(cellId,faceValue);
  }
  fullPd->GetCellData()->AddArray(newIdArray);
  result = new cvPolyData( fullPd);
  *dst = result;

  return CV_OK;
}

/* ------------------ */
/* sys_geom_intersect */
/* ------------------ */

int sys_geom_intersect( cvPolyData *srcA, cvPolyData *srcB,double tolerance, cvPolyData **dst )
{
  vtkPolyData *a = srcA->GetVtkPolyData();
  vtkPolyData *b = srcB->GetVtkPolyData();
  cvPolyData *result = NULL;
  *dst = NULL;

  try {
    vtkNew(vtkBooleanOperationPolyDataFilter2,booleanOperator);
    booleanOperator->SetInputData(0,a);
    booleanOperator->SetInputData(1,b);
    booleanOperator->SetOperationToIntersection();
    booleanOperator->SetTolerance(tolerance);
    booleanOperator->Update();

    result = new cvPolyData( booleanOperator->GetOutput() );
    *dst = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in boolean operation.\n");
    fflush(stderr);
    return CV_ERROR;
  }
  return CV_OK;
}

/* ----------------- */
/* sys_geom_subtract */
/* ----------------- */

int sys_geom_subtract( cvPolyData *srcA, cvPolyData *srcB, double tolerance,cvPolyData **dst )
{
  vtkPolyData *a = srcA->GetVtkPolyData();
  vtkPolyData *b = srcB->GetVtkPolyData();
  cvPolyData *result = NULL;
  *dst = NULL;

  try {
    vtkNew(vtkBooleanOperationPolyDataFilter2,booleanOperator);
    booleanOperator->SetInputData(0,a);
    booleanOperator->SetInputData(1,b);
    booleanOperator->SetOperationToDifference();
    booleanOperator->SetTolerance(tolerance);
    booleanOperator->Update();

    result = new cvPolyData( booleanOperator->GetOutput() );
    *dst = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in boolean operation.\n");
    fflush(stderr);
    return CV_ERROR;
  }
  return CV_OK;
}

int sys_geom_checksurface( cvPolyData *src, int stats[],double tolerance)
{
  vtkPolyData *pd = src->GetVtkPolyData();

  try { 
    double surfstats[2];
    vtkIntersectionPolyDataFilter2::CleanAndCheckSurface(pd,surfstats,tolerance);
    stats[0] = surfstats[0];
    stats[1] = surfstats[1];

    //double fe[2];double bc[2];
    //pd->GetCellData()->GetArray("FreeEdge")->GetRange(fe,0);
    //pd->GetCellData()->GetArray("BadTri")->GetRange(bc,0);

    //stats[0] = fe[0];
    //stats[1] = fe[1];
    //stats[2] = bc[0];
    //stats[3] = bc[1];
  }
  catch (...) {
    fprintf(stderr,"ERROR in checking of surface.\n");
    fflush(stderr);
    return CV_ERROR;
  }
  return CV_OK;
}

#ifdef USE_GTS
/* -------------- */
/* sys_geom_union_gts */
/* -------------- */

int sys_geom_union_gts( cvPolyData *srcA, cvPolyData *srcB, cvPolyData **dst )
{
  vtkPolyData *a = srcA->GetVtkPolyData();
  vtkPolyData *b = srcB->GetVtkPolyData();
  cvPolyData *result = NULL;
  *dst = NULL;

  try {
    vtkNew(vtkSurfaceBooleanOperations,booleanOperator);
    booleanOperator->AddInputData(a);
    booleanOperator->AddInputData(b);
    booleanOperator->SetModeToUnion();
    booleanOperator->Update();
    result = new cvPolyData( booleanOperator->GetOutput() );
    *dst = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in boolean operation.\n");
    fflush(stderr);
    return CV_ERROR;
  }
  return CV_OK;
}

/* ------------------ */
/* sys_geom_intersect_gts */
/* ------------------ */

int sys_geom_intersect_gts( cvPolyData *srcA, cvPolyData *srcB, cvPolyData **dst )
{
  vtkPolyData *a = srcA->GetVtkPolyData();
  vtkPolyData *b = srcB->GetVtkPolyData();
  cvPolyData *result = NULL;
  *dst = NULL;

  try {
    vtkNew(vtkSurfaceBooleanOperations,booleanOperator);
    booleanOperator->AddInputData(a);
    booleanOperator->AddInputData(b);
    booleanOperator->SetModeToIntersection();
    booleanOperator->Update();
    result = new cvPolyData( booleanOperator->GetOutput() );
    *dst = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in boolean operation.\n");
    fflush(stderr);
    return CV_ERROR;
  }
  return CV_OK;
}


/* ----------------- */
/* sys_geom_subtract_gts */
/* ----------------- */

int sys_geom_subtract_gts( cvPolyData *srcA, cvPolyData *srcB, cvPolyData **dst )
{
  vtkPolyData *a = srcA->GetVtkPolyData();
  vtkPolyData *b = srcB->GetVtkPolyData();
  cvPolyData *result = NULL;
  *dst = NULL;

  try {
    vtkNew(vtkSurfaceBooleanOperations,booleanOperator);
    booleanOperator->AddInputData(a);
    booleanOperator->AddInputData(b);
    booleanOperator->SetModeToDifference();
    booleanOperator->Update();
    result = new cvPolyData( booleanOperator->GetOutput() );
    *dst = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in boolean operation.\n");
    fflush(stderr);
    return CV_ERROR;
  }
  return CV_OK;
}

#endif

/* ------------------------ */
/* sys_geom_ReverseAllCells */
/* ------------------------ */
/* Caller is responsible for cleaning up the result. */

int sys_geom_ReverseAllCells( cvPolyData *src, cvPolyData **dst )
{
  vtkPolyData *pdIn = src->GetVtkPolyData();
  vtkPolyData *pdCopy = vtkPolyData::New();
  vtkPoints *ptsCopy = VtkUtils_DeepCopyPoints( pdIn->GetPoints() );
  vtkCellArray *vertsCopy = VtkUtils_DeepCopyCells( pdIn->GetVerts() );
  vtkCellArray *linesCopy = VtkUtils_DeepCopyCells( pdIn->GetLines() );
  vtkCellArray *polysCopy = VtkUtils_DeepCopyCells( pdIn->GetPolys() );
  vtkCellArray *stripsCopy = VtkUtils_DeepCopyCells( pdIn->GetStrips() );
  cvPolyData *result;
  int status;

  pdCopy->SetPoints( ptsCopy );
  pdCopy->SetVerts( vertsCopy );
  pdCopy->SetLines( linesCopy );
  pdCopy->SetPolys( polysCopy );
  pdCopy->SetStrips( stripsCopy );
  pdCopy->GetPointData()->DeepCopy( pdIn->GetPointData() );
  ptsCopy->Delete();
  vertsCopy->Delete();
  linesCopy->Delete();
  polysCopy->Delete();
  stripsCopy->Delete();

  result = new cvPolyData( pdCopy );

  status = VtkUtils_ReverseAllCells( result->GetVtkPolyData() );
  if ( status != CV_OK ) {
    delete result;
    return CV_ERROR;
  }

  *dst = result;

  return CV_OK;
}


/* ---------------------- */
/* sys_geom_GetOrderedPts */
/* ---------------------- */

int sys_geom_GetOrderedPts( cvPolyData *src, double **ord_pts, int *num )
{
  cvPolyData *merged_pd;
  vtkPolyData *pd;
  double *pts;
  int numPts;
  vtkIdType *lines;
  int numLines;
  int *linkedLineIxs;
  int numLinkedLineIxs;
  int *lineVisited;
  int targetIx;
  int i, lineA, lineB, a, b, c, d;
  int status = CV_ERROR;
  double x, y, z;
  int startIx;

  merged_pd = sys_geom_MergePts( src );
  if ( merged_pd == NULL ) {
    return CV_ERROR;
  }
  pd = merged_pd->GetVtkPolyData();

  if ( VtkUtils_GetPoints( pd, &pts, &numPts ) != CV_OK ) {
    return CV_ERROR;
  }

  if ( VtkUtils_GetLines( pd, &lines, &numLines ) != CV_OK ) {
    delete [] pts;
    return CV_ERROR;
  }

  /*
  printf("\n--- sys_geom_GetOrderedPts ---\n\n");
  printf("    num pts   [%d]\n", numPts);
  printf("    num lines [%d]\n\n", numLines);
  */

  if (numPts == 0 || numLines == 0) {
    fprintf(stderr,"ERROR:  no points or lines in polydata object!\n");
    // don't know if I should free pts & lines here or not, but to be
    // safe I wont
    return CV_ERROR;
  } else if (numPts < 3 || numLines < 3) {
    // assume we need at least 3 pts and 3 lines to define a contour. 
    fprintf(stderr,"ERROR:  not enough pts (%i) or lines (%i)\n", numPts, numLines);
    return CV_ERROR;
  }

  lineVisited = new int [numLines];
  for (i = 0; i < numLines; i++) {
    lineVisited[i] = 0;
  }

  *ord_pts = new double [numPts * 3];
  *num = 0;

  targetIx = lines[0];
  startIx = lines[0];

  x = pts[targetIx*3];
  y = pts[targetIx*3 + 1];
  z = pts[targetIx*3 + 2];

  (*ord_pts)[(*num)*3] = x;
  (*ord_pts)[(*num)*3 + 1] = y;
  (*ord_pts)[(*num)*3 + 2] = z;
  (*num)++;

  targetIx = lines[1];

  x = pts[targetIx*3];
  y = pts[targetIx*3 + 1];
  z = pts[targetIx*3 + 2];

  (*ord_pts)[(*num)*3] = x;
  (*ord_pts)[(*num)*3 + 1] = y;
  (*ord_pts)[(*num)*3 + 2] = z;
  (*num)++;

  lineVisited[0] = 1;

  // Things which are allocated which we need to delete:
  //   - pts (VtkUtils_GetPoints)                    <--
  //   - lines (VtkUtils_GetLines)                   <--
  //   - lineVisited [numLines]                      <--
  //   - linkedLineIxs (VtkUtils_GetLinkedLines)
  //   - *ord_pts [numPts * 3] (delete if error)

  while ( VtkUtils_GetLinkedLines( lines, numLines, targetIx,
				   &linkedLineIxs, &numLinkedLineIxs ) ) {

    // Open contour:
    if ( numLinkedLineIxs == 1 ) {
      delete [] linkedLineIxs;
      delete [] (*ord_pts);
      break;
    }

    // Weird connection:
    else if ( numLinkedLineIxs != 2 ) {
      delete [] linkedLineIxs;
      delete [] (*ord_pts);
      break;
    }

    // Normal:

    lineA = linkedLineIxs[0];
    lineB = linkedLineIxs[1];

    delete [] linkedLineIxs;

    a = lines[ lineA * 2 ];
    b = lines[ (lineA * 2) + 1 ];
    c = lines[ lineB * 2 ];
    d = lines[ (lineB * 2) + 1 ];

    if ( ( ! lineVisited[ lineA ] ) && ( ! lineVisited[ lineB ] ) ) {
      printf("ERR: line traversal error\n");
      delete [] (*ord_pts);
      break;
    }

    if ( lineVisited[ lineA ] ) {
      lineVisited[ lineB ] = 1;
      if ( ( a == c ) || ( b == c ) ) {
	targetIx = d;
      } else {
	targetIx = c;
      }
    } else {
      lineVisited[ lineA ] = 1;
      if ( ( c == a ) || ( d == a ) ) {
	targetIx = b;
      } else {
	targetIx = a;
      }
    }

    if ( targetIx == startIx ) {
      status = CV_OK;
      break;
    }

    x = pts[targetIx*3];
    y = pts[targetIx*3 + 1];
    z = pts[targetIx*3 + 2];

    (*ord_pts)[(*num)*3] = x;
    (*ord_pts)[(*num)*3 + 1] = y;
    (*ord_pts)[(*num)*3 + 2] = z;
    (*num)++;

    if ( (*num) > numPts ) {
      printf("ERR: ordered pt list overflow\n");
      delete [] (*ord_pts);
      break;
    }
  }

  delete [] pts;
  delete [] lines;
  delete [] lineVisited;

  return status;
}


// ------------------
// sys_geom_Get2DPgon
// ------------------

int sys_geom_Get2DPgon( cvPolyData *src, double **pgon, int *num )
{
  double bbox[6];
  double tol = 1e10 * FindMachineEpsilon();  // looser than in other places
  double *ord_pts;
  double *rev_pts = NULL;
  double *pts;
  int i;
  int wnum;

  sys_geom_BBox( src, bbox );
  if ( ( fabs(bbox[4]) > tol ) || ( fabs(bbox[5]) > tol ) ) {
    printf("ERR: sys_geom_Get2DPgon called with non-planar input cvPolyData\n");
    return CV_ERROR;
  }
  if ( sys_geom_GetOrderedPts( src, &ord_pts, num ) != CV_OK ) {
    return CV_ERROR;
  }

  // We want pgon to have points in CCW order:
  wnum = sys_geom_2DWindingNum( src );
  if ( wnum < 0 ) {
    sys_geom_ReversePtList( *num, ord_pts, &rev_pts );
    pts = rev_pts;
  } else {
    pts = ord_pts;
  }

  // Transfer (x,y)'s to output:
  *pgon = new double [(*num)*2];
  for ( i = 0; i < (*num); i++ ) {
    (*pgon)[i*2] = pts[i*3];
    (*pgon)[i*2+1] = pts[i*3+1];
  }

  // Clean up stuff:
  delete [] ord_pts;
  if ( rev_pts != NULL ) {
    delete [] rev_pts;
  }

  return CV_OK;
}


// ----------------------
// sys_geom_ReversePtList
// ----------------------

int sys_geom_ReversePtList( int num, double ptsIn[], double *ptsOut[] )
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

  return CV_OK;
}


/* ------------------------ */
/* sys_geom_WriteOrderedPts */
/* ------------------------ */

int sys_geom_WriteOrderedPts( cvPolyData *src, char *fn )
{
  double *pts;
  int num_pts;
  int i;
  FILE *fp;

  if ( sys_geom_GetOrderedPts( src, &pts, &num_pts ) != CV_OK ) {
    return CV_ERROR;
  }
  fp = fopen( fn, "w" );
  if ( fp == NULL ) {
    delete [] pts;
    return CV_ERROR;
  }
  for ( i = 0; i < num_pts; i++ ) {
    fprintf( fp, "%f %f %f\n", pts[3*i], pts[3*i+1], pts[3*i+2] );
  }
  fclose( fp );
  delete [] pts;
  return CV_OK;


  /*
  vtkPolyData *pd;
  double *pts;
  int numPts;
  int *lines;
  int numLines;
  int *linkedLineIxs;
  int numLinkedLineIxs;
  int *lineVisited;
  int targetIx;
  int i, lineA, lineB, a, b, c, d;
  int status = CV_ERROR;
  FILE *fp;
  double x, y;

  pd = src->GetVtkPolyData();
  fp = fopen( fn, "w" );
  if ( fp == NULL ) {
    return CV_ERROR;
  }

  if ( VtkUtils_GetPoints( pd, &pts, &numPts ) != CV_OK ) {
    fclose( fp );
    return CV_ERROR;
  }

  if ( VtkUtils_GetLines( pd, &lines, &numLines ) != CV_OK ) {
    delete [] pts;
    fclose( fp );
    return CV_ERROR;
  }

  lineVisited = new int [numLines];
  for (i = 0; i < numLines; i++) {
    lineVisited[i] = 0;
  }

  targetIx = lines[0];
  x = pts[targetIx*3];
  y = pts[targetIx*3 + 1];
  fprintf( fp, "%f %f 0.0 0.0 0.0 0.0\n", x, y );
  while ( VtkUtils_GetLinkedLines( lines, numLines, targetIx,
				   &linkedLineIxs, &numLinkedLineIxs ) ) {

    // Open contour:
    if ( numLinkedLineIxs == 1 ) {
      delete [] linkedLineIxs;
      break;
    }

    // Weird connection:
    else if ( numLinkedLineIxs != 2 ) {
      delete [] linkedLineIxs;
      break;
    }

    // Normal:
    else {

      lineA = linkedLineIxs[0];
      lineB = linkedLineIxs[1];

      a = lines[ lineA * 2 ];
      b = lines[ (lineA * 2) + 1 ];
      c = lines[ lineB * 2 ];
      d = lines[ (lineB * 2) + 1 ];

      if ( ( lineVisited[ lineA ] ) && ( lineVisited[ lineB ] ) ) {
	status = CV_OK;
	break;
      }

      if ( lineVisited[ lineA ] ) {
	lineVisited[ lineB ] = 1;
	if ( a == c ) {
	  targetIx = d;
	} else {
	  targetIx = c;
	}
      } else {
	lineVisited[ lineA ] = 1;
	if ( c == a ) {
	  targetIx = b;
	} else {
	  targetIx = a;
	}
      }
      x = pts[targetIx*3];
      y = pts[targetIx*3 + 1];
      fprintf( fp, "%f %f 0.0 0.0 0.0 0.0\n", x, y );
      delete [] linkedLineIxs;
    }
  }

  delete [] lineVisited;
  delete [] pts;
  delete [] lines;
  fclose( fp );
  return status;
  */
}


/* ------------------- */
/* sys_geom_WriteLines */
/* ------------------- */

int sys_geom_WriteLines( cvPolyData *src, char *fn )
{
  vtkPolyData *pd;
  double *pts;
  int numPts;
  vtkIdType *lines;
  int numLines;
  int i, ptAIx, ptBIx;
  FILE *fp;

  pd = src->GetVtkPolyData();
  fp = fopen( fn, "w" );
  if ( fp == NULL ) {
    return CV_ERROR;
  }

  if ( VtkUtils_GetPoints( pd, &pts, &numPts ) != CV_OK ) {
    fclose( fp );
    return CV_ERROR;
  }

  if ( VtkUtils_GetLines( pd, &lines, &numLines ) != CV_OK ) {
    delete [] pts;
    fclose( fp );
    return CV_ERROR;
  }

  for (i = 0; i < numLines; i++) {
    ptAIx = lines[2*i];
    ptBIx = lines[2*i + 1];
    fprintf( fp, "%f %f %f %f %f %f\n",
	     pts[3*ptAIx], pts[3*ptAIx + 1], pts[3*ptAIx + 2],
	     pts[3*ptBIx], pts[3*ptBIx + 1], pts[3*ptBIx + 2] );
  }

  fclose( fp );
  delete [] pts;
  delete [] lines;
  return CV_OK;
}


// --------------------
// sys_geom_PolysClosed
// --------------------

int sys_geom_PolysClosed( cvPolyData *src, int *closed )
{
  vtkPolyData *pd;
  int numPts, numPolys;
  vtkFloatingPointType *pts;
  vtkIdType *polys;

  pd = src->GetVtkPolyData();

  if ( VtkUtils_GetPointsFloat( pd, &pts, &numPts ) != CV_OK ) {
    printf("ERR: VtkUtils_GetPoints failed\n");
    return CV_ERROR;
  }
  if ( VtkUtils_GetAllPolys( pd, &numPolys, &polys ) != CV_OK ) {
    printf("ERR: VtkUtils_GetAllPolys failed\n");
    return CV_ERROR;
  }

  cgeom_PolysClosed( numPts, pts, numPolys, polys, closed );

  return CV_OK;
}


// -----------------
// sys_geom_SurfArea
// -----------------

int sys_geom_SurfArea( cvPolyData *src, double *area )
{
  vtkPolyData *pd;
  int numPts, numPolys;
  vtkFloatingPointType *pts;
  vtkIdType *polys;
  vtkFloatingPointType fArea;
 
  // since cgeom_CompArea requires triangles, we will
  // create triangles before we call that routine

  vtkTriangleFilter *tri = vtkTriangleFilter::New();
  tri->SetInputData(src->GetVtkPolyData());
  tri->Update();
  pd = tri->GetOutput();

  if ( VtkUtils_GetPointsFloat( pd, &pts, &numPts ) != CV_OK ) {
    printf("ERR: VtkUtils_GetPoints failed\n");
    tri->Delete();
    return CV_ERROR;
  }
  if ( VtkUtils_GetAllPolys( pd, &numPolys, &polys ) != CV_OK ) {
    printf("ERR: VtkUtils_GetAllPolys failed\n");
    tri->Delete();
    return CV_ERROR;
  }

  cgeom_CompArea( numPts, pts, numPolys, polys, &fArea );
  *area = fArea;

  tri->Delete();

  return CV_OK;
}


// ------------------------
// sys_geom_getPolyCentroid
// ------------------------

// Interface from VtkPolyData to use cgeom_GetPolyCentroid routine
// centroid must be an array of at least THREE elements.

int sys_geom_getPolyCentroid( cvPolyData *src, double centroid[])
{
  vtkPolyData *pd;
  int numPts, numPolys;
  vtkFloatingPointType *pts;
  vtkIdType *polys;
 
  // since cgeom_CalcPolyCentroid requires triangles, we will
  // create triangles before we call that routine

  vtkTriangleFilter *tri = vtkTriangleFilter::New();
  tri->SetInputData(src->GetVtkPolyData());
  tri->Update();
  pd = tri->GetOutput();

  if ( VtkUtils_GetPointsFloat( pd, &pts, &numPts ) != CV_OK ) {
    printf("ERR: VtkUtils_GetPoints failed\n");
    tri->Delete();
    return CV_ERROR;
  }
  if ( VtkUtils_GetAllPolys( pd, &numPolys, &polys ) != CV_OK ) {
    printf("ERR: VtkUtils_GetAllPolys failed\n");
    tri->Delete();
    return CV_ERROR;
  }

  cgeom_GetPolyCentroid ( numPts, pts, numPolys, polys, centroid);
 
  tri->Delete();

  return CV_OK;
}


// ----------------------
// sys_geom_PrintTriStats
// ----------------------

int sys_geom_PrintTriStats( cvPolyData *surf )
{
  vtkPolyData *pd;
  int numPts, numPolys;
  double *pts;
  vtkIdType *polys;
  int i, pos;
  int numTri = 0;
  int numOther = 0;
  int a, b, c;
  double minEdge, currMinEdge, currMaxEdge;
  double minArea, currArea;
  double ab[3];
  double ac[3];
  double cp[3];
  double minHeight, currMinHeight;
  double len_ab, len_ac, len_bc;
  int min_e_id, min_a_id, min_h_id;
  double tol = 1e6 * FindMachineEpsilon();

  pd = surf->GetVtkPolyData();

  if ( VtkUtils_GetPoints( pd, &pts, &numPts ) != CV_OK ) {
    printf("ERR: VtkUtils_GetPoints failed\n");
    return CV_ERROR;
  }
  if ( VtkUtils_GetAllPolys( pd, &numPolys, &polys ) != CV_OK ) {
    printf("ERR: VtkUtils_GetAllPolys failed\n");
    delete [] pts;
    return CV_ERROR;
  }

  printf( "\n\n  ------ sys_geom_PrintTriStats ------\n" );

  pos = 0;
  minEdge = 0.0;    // these are needed
  minArea = 0.0;    // only to prevent
  minHeight = 0.0;  // compiler warnings
  min_e_id = -1;
  min_a_id = -1;
  min_h_id = -1;
  for ( i = 0; i < numPolys; i++ ) {
    if ( polys[pos] == 3 ) {
      numTri++;

      a = polys[pos+1];
      b = polys[pos+2];
      c = polys[pos+3];

      // Find the shortest edge of the triangle:
      len_ab = Distance( pts[3*a], pts[3*a+1], pts[3*a+2],
			 pts[3*b], pts[3*b+1], pts[3*b+2] );
      len_ac = Distance( pts[3*a], pts[3*a+1], pts[3*a+2],
			 pts[3*c], pts[3*c+1], pts[3*c+2] );
      len_bc = Distance( pts[3*b], pts[3*b+1], pts[3*b+2],
			 pts[3*c], pts[3*c+1], pts[3*c+2] );
      currMinEdge = minimum( len_ab, len_ac );
      currMinEdge = minimum( currMinEdge, len_bc );
      if ( ( i == 0 ) || ( currMinEdge < minEdge ) ) {
	minEdge = currMinEdge;
	min_e_id = i;
      }

      // Find triangle area:
      ab[0] = pts[3*b] - pts[3*a];
      ab[1] = pts[3*b+1] - pts[3*a+1];
      ab[2] = pts[3*b+2] - pts[3*a+2];
      ac[0] = pts[3*c] - pts[3*a];
      ac[1] = pts[3*c+1] - pts[3*a+1];
      ac[2] = pts[3*c+2] - pts[3*a+2];
      Cross( ab[0], ab[1], ab[2], ac[0], ac[1], ac[2],
	     &(cp[0]), &(cp[1]), &(cp[2]) );
      currArea = Magnitude( cp[0], cp[1], cp[2] ) / 2.0;
      if ( ( i == 0 ) || ( currArea < minArea ) ) {
	minArea = currArea;
	min_a_id = i;
      }

      // Find the smallest triangle height:
      // A(tri) = 1/2 (base) (height)
      currMaxEdge = maximum( len_ab, len_ac );
      currMaxEdge = maximum( currMaxEdge, len_bc );
      currMinHeight = 2 * currArea / currMaxEdge;
      if ( ( i == 0 ) || ( currMinHeight < minHeight ) ) {
	minHeight = currMinHeight;
	min_h_id = i;
      }
    } else {
      numOther++;
    }
    pos += polys[pos] + 1;
  }

  printf( "  >>>>>> num tri       [%d]\n", numTri );
  printf( "  >>>>>> num non-tri   [%d]\n", numOther );
  printf( "  >>>>>> tot polys     [%d]\n", numPolys );
  printf( "  >>>>>> min edge      [%f]\n", minEdge );
  printf( "  >>>>>> min edge id   [%d]\n", min_e_id );
  printf( "  >>>>>> min area      [%f]\n", minArea );
  printf( "  >>>>>> min area id   [%d]\n", min_a_id );
  printf( "  >>>>>> min height    [%f]\n", minHeight );
  printf( "  >>>>>> min height id [%d]\n", min_h_id );
  printf( "\n\n" );

  return CV_OK;
}


// ------------------------
// sys_geom_PrintSmallPolys
// ------------------------

int sys_geom_PrintSmallPolys( cvPolyData *src, double sideTol )
{
  vtkPolyData *pd;
  int numPts, numPolys;
  int numFound, minPolyId;
  vtkFloatingPointType *pts;
  vtkIdType *polys;

  pd = src->GetVtkPolyData();

  if ( VtkUtils_GetPointsFloat( pd, &pts, &numPts ) != CV_OK ) {
    printf("ERR: VtkUtils_GetPoints failed\n");
    return CV_ERROR;
  }
  if ( VtkUtils_GetAllPolys( pd, &numPolys, &polys ) != CV_OK ) {
    printf("ERR: VtkUtils_GetAllPolys failed\n");
    delete [] pts;
    return CV_ERROR;
  }

  printf ( "\n\n  ------ sys_geom_PrintSmallPolys ------\n" );

  cgeom_FindDegen( numPts, pts, numPolys, polys, sideTol, &numFound,
		   &minPolyId );

  printf ( "  >>>>>>  num degen polys [%d]. \n", numFound );
  printf ( "  >>>>>>  min poly id [%d]. \n", minPolyId );
  printf ( "\n\n");

  delete [] pts;
  delete [] polys;

  return CV_OK;
}


// ---------------------
// sys_geom_RmSmallPolys
// ---------------------

int sys_geom_RmSmallPolys( cvPolyData *src, double sideTol, cvPolyData **dst )
{
  vtkPolyData *pd;
  int numPts, numPolys;
  vtkFloatingPointType *pts;
  vtkIdType *polys;
  int numRemoved, minPolyId;
  int numNewPts, numNewPolys;
  vtkFloatingPointType *newPts;
  vtkIdType *newPolys;
  vtkPolyData *result;

  pd = src->GetVtkPolyData();

  if ( VtkUtils_GetPointsFloat( pd, &pts, &numPts ) != CV_OK ) {
    printf("ERR: VtkUtils_GetPoints failed\n");
    return CV_ERROR;
  }
  if ( VtkUtils_GetAllPolys( pd, &numPolys, &polys ) != CV_OK ) {
    printf("ERR: VtkUtils_GetAllPolys failed\n");
    delete [] pts;
    return CV_ERROR;
  }

  printf ( "\n\n  ------ sys_geom_RmSmallPolys ------\n" );

  cgeom_FindDegen( numPts, pts, numPolys, polys, sideTol,
		   &numRemoved, &minPolyId );

  printf ( "  >>>>>>  num degen polys [%d]. \n", numRemoved );
  printf ( "  >>>>>>  min poly id [%d]. \n", minPolyId );
  printf ( "\n\n"); 

  cgeom_FixDegen( numPts, pts, numPolys, polys, sideTol, 
                  &numNewPts, &newPts, &numNewPolys, &newPolys );
  
  printf ( "  >>>>>>  num new pts   [%d]. \n", numNewPts );
  printf ( "  >>>>>>  num new polys [%d]. \n", numNewPolys );
  printf ( "\n\n" ); 

  if ( VtkUtils_NewVtkPolyData( &result, numNewPts, newPts, numNewPolys,
				newPolys ) != CV_OK ) {
    printf("ERR: VtkUtils_NewVtkPolyData failed\n");
    delete [] pts;
    delete [] polys;
    delete [] newPts;
    delete [] newPolys;
    return CV_ERROR;
  }

  (*dst) = new cvPolyData( result );
  result->Delete();

  delete [] pts;
  delete [] polys;
  delete [] newPts;
  delete [] newPolys;

  return CV_OK;
}


/* ------------- */
/* sys_geom_BBox */
/* ------------- */
// bbox MUST be a caller-allocated array of 6 doubles.  bbox is filled
// as follows:
//   bbox[0]  min x
//   bbox[1]  max x
//   bbox[2]  min y
//   bbox[3]  max y
//   bbox[4]  min z
//   bbox[5]  max z

int sys_geom_BBox( cvPolyData *obj, double bbox[] )
{
  vtkPolyData *pd;
  double *pts;
  int numPts;
  int i;

  pd = obj->GetVtkPolyData();

  if ( VtkUtils_GetPoints( pd, &pts, &numPts ) != CV_OK ) {
    printf("ERR: VtkUtils_GetPoints failed\n");
    return CV_ERROR;
  }

  for ( i = 0; i < numPts; i++ ) {
    if ( i == 0 ) {
      bbox[0] = bbox[1] = pts[3*i];
      bbox[2] = bbox[3] = pts[3*i+1];
      bbox[4] = bbox[5] = pts[3*i+2];
    }

    bbox[0] = minimum( bbox[0], pts[3*i] );
    bbox[1] = maximum( bbox[1], pts[3*i] );

    bbox[2] = minimum( bbox[2], pts[3*i+1] );
    bbox[3] = maximum( bbox[3], pts[3*i+1] );

    bbox[4] = minimum( bbox[4], pts[3*i+2] );
    bbox[5] = maximum( bbox[5], pts[3*i+2] );
  }

  delete [] pts;
  return CV_OK;
}


/* ---------------------- */
/* sys_geom_OrientProfile */
/* ---------------------- */

int sys_geom_OrientProfile( cvPolyData *src, double ppt[], double ptan[],
			    double xhat[], cvPolyData **dst )
{
  double yhat[3];
  vtkCellArray *lines;
  vtkPoints *pts = vtkPoints::New();
  vtkPolyData *srcPd = src->GetVtkPolyData();
  vtkPolyData *pd = vtkPolyData::New();
  int i, numPts;
  vtkFloatingPointType origpt[3];
  vtkFloatingPointType newpt[3];
  vtkFloatingPointType trans[2];
  cvPolyData *result;

  NormVector( &(ptan[0]), &(ptan[1]), &(ptan[2]) );
  NormVector( &(xhat[0]), &(xhat[1]), &(xhat[2]) );
  Cross( ptan[0], ptan[1], ptan[2], xhat[0], xhat[1], xhat[2],
	 &(yhat[0]), &(yhat[1]), &(yhat[2]) );
  NormVector( &(yhat[0]), &(yhat[1]), &(yhat[2]) );

  lines = VtkUtils_DeepCopyCells( srcPd->GetLines() );
  numPts = srcPd->GetNumberOfPoints();
  for ( i = 0; i < numPts; i++ ) {
    srcPd->GetPoint( i, origpt );
    trans[0] = origpt[0];
    trans[1] = origpt[1];
    newpt[0] = ppt[0] + trans[0] * xhat[0] + trans[1] * yhat[0];
    newpt[1] = ppt[1] + trans[0] * xhat[1] + trans[1] * yhat[1];
    newpt[2] = ppt[2] + trans[0] * xhat[2] + trans[1] * yhat[2];
    pts->InsertNextPoint( newpt );
  }

  pd->SetPoints( pts );
  pd->SetLines( lines );
  pts->Delete();
  lines->Delete();
  result = new cvPolyData( pd );
  pd->Delete();

  *dst = result;

  return CV_OK;
}


/* ------------------------- */
/* sys_geom_DisorientProfile */
/* ------------------------- */

int sys_geom_DisorientProfile( cvPolyData *src, double ppt[], double ptan[],
			       double xhat[], cvPolyData **dst )
{
  double yhat[3];
  double S[9], detS;
  double A[9];
  double B[9];
  vtkCellArray *lines;
  vtkPoints *pts = vtkPoints::New();
  vtkPolyData *srcPd = src->GetVtkPolyData();
  vtkPolyData *pd = vtkPolyData::New();
  int i, numPts;
  vtkFloatingPointType srcpt[3];
  vtkFloatingPointType dstpt[3];
  cvPolyData *result;

  double ep = 1e6 * FindMachineEpsilon();

  NormVector( &(ptan[0]), &(ptan[1]), &(ptan[2]) );
  NormVector( &(xhat[0]), &(xhat[1]), &(xhat[2]) );
  Cross( ptan[0], ptan[1], ptan[2], xhat[0], xhat[1], xhat[2],
	 &(yhat[0]), &(yhat[1]), &(yhat[2]) );
  NormVector( &(yhat[0]), &(yhat[1]), &(yhat[2]) );

  // Set up S, A and B for use with Cramer's Rule to find dstpt[0] and
  // dstpt[1], respectively:

  S[0] = xhat[0];
  S[1] = xhat[1];
  S[2] = xhat[2];

  S[3] = yhat[0];
  S[4] = yhat[1];
  S[5] = yhat[2];

  S[6] = ptan[0];
  S[7] = ptan[1];
  S[8] = ptan[2];

  detS = misc_Det3x3(S);

  A[3] = yhat[0];
  A[4] = yhat[1];
  A[5] = yhat[2];

  A[6] = ptan[0];
  A[7] = ptan[1];
  A[8] = ptan[2];

  B[0] = xhat[0];
  B[1] = xhat[1];
  B[2] = xhat[2];

  B[6] = ptan[0];
  B[7] = ptan[1];
  B[8] = ptan[2];

  /*
  a = xhat[0];
  b = yhat[0];
  c = xhat[1];
  d = yhat[1];
  if ( fabs(a*d - b*c) < ep ) {
    printf("ERR: singular disorientation matrix\n");
    pts->Delete();
    pd->Delete();
    *dst = NULL;
    return CV_ERROR;
  }
  coeff = 1.0 / ( a*d - b*c );

  lines = VtkUtils_DeepCopyCells( srcPd->GetLines() );
  numPts = srcPd->GetNumberOfPoints();
  for ( i = 0; i < numPts; i++ ) {
    srcPd->GetPoint( i, srcpt );
    dstpt[0] = coeff * ( d*(srcpt[0] - ppt[0]) - b*(srcpt[1] - ppt[1]) );
    dstpt[1] = coeff * ( -c*(srcpt[0] - ppt[0]) + a*(srcpt[1] - ppt[1]) );
    dstpt[2] = 0.0;
    pts->InsertNextPoint( dstpt );
  }
  */

  lines = VtkUtils_DeepCopyCells( srcPd->GetLines() );
  numPts = srcPd->GetNumberOfPoints();
  for ( i = 0; i < numPts; i++ ) {

    srcPd->GetPoint( i, srcpt );

    A[0] = srcpt[0] - ppt[0];
    A[1] = srcpt[1] - ppt[1];
    A[2] = srcpt[2] - ppt[2];

    B[3] = srcpt[0] - ppt[0];
    B[4] = srcpt[1] - ppt[1];
    B[5] = srcpt[2] - ppt[2];

    dstpt[0] = (1.0 / detS) * ( misc_Det3x3(A) );
    dstpt[1] = (1.0 / detS) * ( misc_Det3x3(B) );
    dstpt[2] = 0.0;
    pts->InsertNextPoint( dstpt );
  }

  pd->SetPoints( pts );
  pd->SetLines( lines );
  pts->Delete();
  lines->Delete();
  result = new cvPolyData( pd );
  pd->Delete();

  *dst = result;

  return CV_OK;
}


/* ------------------ */
/* sys_geom_Translate */
/* ------------------ */
/* Caller is responsible for cleaning up the result. */

int sys_geom_Translate( cvPolyData *src, double translate[], cvPolyData **dst )
{
  //  cvPolyData *result = new cvPolyData( src );
  cvPolyData *result = sys_geom_DeepCopy( src );
  int i, numPts;
  vtkFloatingPointType pt[3];
  
  vtkPoints *pts = result->GetVtkPolyData()->GetPoints();
  numPts = pts->GetNumberOfPoints();
  for ( i = 0; i < numPts; i++ ) {
    pts->GetPoint( i, pt );
    pt[0] += translate[0];
    pt[1] += translate[1];
    pt[2] += translate[2];
    pts->SetPoint( i, pt );
  }
  *dst = result;

  return CV_OK;
}


// -----------------
// sys_geom_ScaleAvg
// -----------------

int sys_geom_ScaleAvg( cvPolyData *src, double factor, cvPolyData **dst )
{
  cvPolyData *result = sys_geom_DeepCopy( src );
  int i, numPts;
  double avgPt[3];
  vtkFloatingPointType pt[3];
  vtkFloatingPointType vec[3];

  sys_geom_AvgPt( src, avgPt );
  vtkPoints *pts = result->GetVtkPolyData()->GetPoints();
  numPts = pts->GetNumberOfPoints();
  for ( i = 0; i < numPts; i++ ) {
    pts->GetPoint( i, pt );
    vec[0] = factor * (pt[0] - avgPt[0]);
    vec[1] = factor * (pt[1] - avgPt[1]);
    vec[2] = factor * (pt[2] - avgPt[2]);
    pt[0] = avgPt[0] + vec[0];
    pt[1] = avgPt[1] + vec[1];
    pt[2] = avgPt[2] + vec[2];
    pts->SetPoint( i, pt );
  }
  *dst = result;

  return CV_OK;
}


// --------------
// sys_geom_Align
// --------------

cvPolyData *sys_geom_Align( cvPolyData *ref, cvPolyData *src )
{
  double refNrm[3];
  double srcNrm[3];
  double *refPts, *srcPts;
  int numRefPts, numSrcPts;
  double refAvg[3];
  double srcAvg[3];
  double refStart[3];
  double radial[3];
  double refCross[3];
  double currCross[3];
  cvPolyData *dst;
  double currScore, maxScore;
  int posId;

  if ( sys_geom_PolygonNormal( ref, refNrm ) != CV_OK ) {
    printf( "ERR: normal calculation for reference polygon failed\n" );
    return NULL;
  }

  if ( sys_geom_GetOrderedPts( ref, &refPts, &numRefPts ) != CV_OK ) {
    printf( "ERR: get ref ordered points failed\n" );
    return NULL;
  }
  refStart[0] = refPts[0];
  refStart[1] = refPts[1];
  refStart[2] = refPts[2];
  delete [] refPts;

  // Compute the target vector ref norm cross ref radial vec at start
  // pos:
  sys_geom_AvgPt( ref, refAvg );
  radial[0] = refStart[0] - refAvg[0];
  radial[1] = refStart[1] - refAvg[1];
  radial[2] = refStart[2] - refAvg[2];
  Cross( refNrm[0], refNrm[1], refNrm[2],
	 radial[0], radial[1], radial[2],
	 &(refCross[0]), &(refCross[1]), &(refCross[2]) );
  NormVector( &(refCross[0]), &(refCross[1]), &(refCross[2]) );

  if ( sys_geom_PolygonNormal( src, srcNrm ) != CV_OK ) {
    printf( "ERR: normal calculation for source polygon failed\n" );
    return NULL;
  }

  // If src normal opposes ref normal, then invert src.  This is
  // reasonable because this alignment function is only meant for use
  // with neighboring curves which are changing direction gradually.
  if ( Dot( refNrm[0], refNrm[1], refNrm[2], srcNrm[0], srcNrm[1],
	    srcNrm[2] ) < 0.0 ) {
    VtkUtils_ReverseAllCells( src->GetVtkPolyData() );
  }

  if ( sys_geom_GetOrderedPts( src, &srcPts, &numSrcPts ) != CV_OK ) {
    printf( "ERR: get src ordered points failed\n" );
    return NULL;
  }
  sys_geom_AvgPt( src, srcAvg );

  // Foreach pos p in the src polygon, compute src norm cross radial
  // vec at p.  A score can then be computed as the dot between the
  // reference direction (i.e. refCross) and the current direction
  // (i.e. currCross).  Note that this strategy breaks down as refNrm
  // cross srcNrm --> refCross.  However, if we assume that adjacent
  // profiles won't change direction too abruptly, this is OK.  Note
  // also that this implies that we should apply sys_geom_Align in a
  // cascading fashion (i.e. align b to a, c to b, d to c, etc.) as
  // opposed to using a single base profile (i.e. align b to a, c to
  // a, d to a, etc.).

  for ( int i = 0; i < numSrcPts; i++ ) {
    radial[0] = srcPts[3*i] - srcAvg[0];
    radial[1] = srcPts[3*i+1] - srcAvg[1];
    radial[2] = srcPts[3*i+2] - srcAvg[2];
    Cross( srcNrm[0], srcNrm[1], srcNrm[2],
	   radial[0], radial[1], radial[2],
	   &(currCross[0]), &(currCross[1]), &(currCross[2]) );
    NormVector( &(currCross[0]), &(currCross[1]), &(currCross[2]) );
    currScore = Dot( refCross[0], refCross[1], refCross[2],
		     currCross[0], currCross[1], currCross[2] );
    if ( i == 0 ) {
      maxScore = currScore;
      posId = i;
    } else {
      if ( currScore > maxScore ) {
	posId = i;
      }
      maxScore = maximum( maxScore, currScore );
    }
  }
  delete [] srcPts;

  // No re-alignment:
  if ( posId == 0 ) {
    printf( "NOTE: no adjustment to alignment [%s]\n", src->GetName() );
    dst = new cvPolyData( src );
    return dst;
  }

  dst = sys_geom_ReorderPolygon( src, posId );
  return dst;
}


// -----------------------
// sys_geom_ReorderPolygon
// -----------------------

cvPolyData *sys_geom_ReorderPolygon( cvPolyData *src, int startIx )
{
  double *srcPts;
  int numSrcPts;
  vtkFloatingPointType *alignedPts;
  vtkIdType *cells;
  vtkPolyData *pd;
  cvPolyData *dst;
  int i, j;

  if ( sys_geom_GetOrderedPts( src, &srcPts, &numSrcPts ) != CV_OK ) {
    printf( "ERR: get src ordered points failed\n" );
    return NULL;
  }

  if ( ( startIx < 0 ) || ( startIx >= numSrcPts ) ) {
    printf("ERR: index %d out of range\n", startIx);
    delete [] srcPts;
    return NULL;
  }

  alignedPts = new vtkFloatingPointType [3 * numSrcPts];
  for ( i = 0; i < numSrcPts; i++ ) {
    j = (startIx + i) % numSrcPts;
    alignedPts[3*i] = srcPts[3*j];
    alignedPts[3*i+1] = srcPts[3*j+1];
    alignedPts[3*i+2] = srcPts[3*j+2];
  }
  cells = new vtkIdType [3 * numSrcPts];
  for ( i = 0; i < numSrcPts; i++ ) {
    cells[3*i] = 2;
    cells[3*i+1] = i;
    cells[3*i+2] = (i + 1) % numSrcPts;
  }
  delete [] srcPts;
  if ( VtkUtils_NewVtkPolyDataLines( &pd, numSrcPts, alignedPts, numSrcPts,
				     cells ) != CV_OK ) {
    printf( "ERR: poly data creation failed\n" );
    delete [] alignedPts;
    delete [] cells;
    return NULL;
  }
  dst = new cvPolyData( pd );
  pd->Delete();

  delete [] alignedPts;
  delete [] cells;

  return dst;
}


// --------------------
// sys_geom_AlignByDist
// --------------------

cvPolyData *sys_geom_AlignByDist( cvPolyData *ref, cvPolyData *src )
{
  double refNrm[3], srcNrm[3];
  double *refPts;
  int numRefPts;
  double *srcPts;
  int numSrcPts;
  int ix;
  cvPolyData *dst;

  // not sure this normal stuff makes sense? nw.
  if ( sys_geom_PolygonNormal( ref, refNrm ) != CV_OK ) {
    printf( "ERR: normal calculation for reference polygon failed\n" );
    return NULL;
  }
  if ( sys_geom_PolygonNormal( src, srcNrm ) != CV_OK ) {
    printf( "ERR: normal calculation for source polygon failed\n" );
    return NULL;
  }

  printf( "Aligning profile [%s]\n", src->GetName() );

  // If src normal opposes ref normal, then invert src.  This is
  // reasonable because this alignment function is only meant for use
  // with neighboring curves which are changing direction gradually.
  if ( Dot( refNrm[0], refNrm[1], refNrm[2], srcNrm[0], srcNrm[1],
	    srcNrm[2] ) < 0.0 ) {
      fprintf(stdout,"  Reversing src.\n");
      VtkUtils_ReverseAllCells( src->GetVtkPolyData() );
  }

  // Get ref and src points:
  if ( sys_geom_GetOrderedPts( ref, &refPts, &numRefPts ) != CV_OK ) {
    printf( "ERR: get ref ordered points failed\n" );
    return NULL;
  }
  if ( sys_geom_GetOrderedPts( src, &srcPts, &numSrcPts ) != CV_OK ) {
    delete [] refPts;
    printf( "ERR: get src ordered points failed\n" );
    return NULL;
  }

  // find the closest two points in 3-space between the two
  // profiles.  Note that it is possible two pts are the same
  // distance from a corresponding pt on another curve and the
  // one picked by this code is nearly random.  But this should
  // be okay.

  if (numRefPts != numSrcPts) {
      fprintf(stderr,"ERROR:  must have equal number of pts to align curves by distance~\n");
      delete [] srcPts;
      delete [] refPts;
      return CV_ERROR;
  }

  double d2 = 0;
  double d2min = 9999999999.99;
  int refPtId = -1;
  int dstPtId = -1;

  for (int ki = 0; ki < numRefPts; ki++) {
    for (int kj = 0; kj < numSrcPts; kj++) {
      d2 = 0;
      int pt1ix = ki;
      int pt2ix = kj;
      for (int i = 0; i < numRefPts; i++) {
          d2 += ((refPts[3*pt1ix+0]-srcPts[3*pt2ix+0]) * (refPts[3*pt1ix+0]-srcPts[3*pt2ix+0])) + 
               ((refPts[3*pt1ix+1]-srcPts[3*pt2ix+1]) * (refPts[3*pt1ix+1]-srcPts[3*pt2ix+1])) +
               ((refPts[3*pt1ix+2]-srcPts[3*pt2ix+2]) * (refPts[3*pt1ix+2]-srcPts[3*pt2ix+2]));
          pt2ix++;
          if (pt2ix == numSrcPts) pt2ix = 0;
          pt1ix++;
          if (pt1ix == numRefPts) pt1ix = 0;
      }
      if (d2 < d2min) {
          refPtId = ki;
          dstPtId = kj;
          d2min = d2;
      }
    }
  }

  fprintf(stdout,"  refPtId: %i  dstPtId: %i  d2min: %lf\n",refPtId,dstPtId,d2min);

  delete [] srcPts;
  delete [] refPts;

  // check for error condition
  if (refPtId < 0) {
      fprintf(stderr,"ERROR:  could not find min distance between curves?!\n");
      return CV_ERROR;
  }

  // No re-alignment:
  if ( refPtId == dstPtId ) {
    printf( "  NOTE: no adjustment to alignment [%s]\n", src->GetName() );
    dst = new cvPolyData( src );
    return dst;
  }

  if ( dstPtId > refPtId ) {
      ix = dstPtId - refPtId;
  } else {
      ix = dstPtId + (numRefPts -refPtId);
  }

  fprintf(stdout,"  ix: %i\n",ix);

  dst = sys_geom_ReorderPolygon(src, ix);

  return dst;

}


/* ----------------- */
/* sys_geom_Classify */
/* ----------------- */
/* Note that there is no deep copy of data here like there is
 * sys_geom_PtInPoly.  So hopefully we can get away without adding the
 * static object pointers we used in sys_geom_PtInPoly.
 */

int sys_geom_Classify( cvPolyData *obj, double pt[], int *result )
{
  vtkPolyData *pd;
  vtkCellArray *polys;
  int numPolys;
  vtkFloatingPointType tmp[3];
  vtkIdType *ptIds;
  vtkIdType npts;

  ggemsGeoPoint *verts;
  int maxVerts = 0;
  ggemsGeoPoint p;
  Rdouble Area = 0.0;

  pd = obj->GetVtkPolyData();
  polys = pd->GetPolys();
  numPolys = pd->GetNumberOfPolys();
  maxVerts = polys->GetMaxCellSize();

  verts = new ggemsGeoPoint [maxVerts];
  if ( verts == NULL ) {
    return CV_ERROR;
  }

  p.x = pt[0];
  p.y = pt[1];
  p.z = pt[2];

  // Foreach poly:
  polys->InitTraversal();
  while ( polys->GetNextCell( npts, ptIds ) ) {

    // Foreach pt in poly, set up verts array:
    for (int j = 0; j < npts; j++) {
      pd->GetPoint( ptIds[j], tmp );
      verts[j].x = tmp[0];
      verts[j].y = tmp[1];
      verts[j].z = tmp[2];
    }

    // Compute solid angle for this poly:
    Area += ggemsgeo_solid_angle ( npts, verts, &p ); 
  }

  // inside  <--> 1
  // outside <--> -1

  if ((Area > 2*PI) || (Area < -2*PI)) {
    *result = 1;
  } else {
    *result = -1;
  }

  delete [] verts;
  return CV_OK;
}


/* ----------------- */
/* sys_geom_PtInPoly */
/* ----------------- */
/* Input cvPolyData should be planar and lie in the xy plane.  Only the
 * x and y components of the given test point will be examined.  1 is
 * returned for points in the polygon, -1 for points outside.  
 */

static double *g_sys_geom_PtInPoly_pgon = NULL;
static int g_sys_geom_PtInPoly_num = 0;

int sys_geom_PtInPoly( cvPolyData *obj, double pt[], int usePrevPoly, int *result )
{

  // to speed access, let the user use the previous polygon.  It is up
  // to the user to ensure this makes sense!
  if (usePrevPoly != 0) {
     if ( ggems_CrossingsMultiplyTest(g_sys_geom_PtInPoly_pgon ,
                                 g_sys_geom_PtInPoly_num, pt ) ) {
      *result = 1;
      return CV_OK;
    } else {
      *result = -1;
      return CV_OK;
    }
  }

  // lazy free of the previous polygon
  if (g_sys_geom_PtInPoly_pgon != NULL) {
      delete [] g_sys_geom_PtInPoly_pgon;
      g_sys_geom_PtInPoly_pgon = NULL;
  }

  double *pgon = NULL;
  int num = 0;

  int havePolygon = 0;
  vtkFeatureEdges *edgeFilter;
  cvPolyData *tmppd;

  // check if we have a polygon in the cvPolyData.  If we do,
  // this obj is used to create line segments to be passed
  // to Ken's code which counts on line segments.
  vtkPolyData *pd = obj->GetVtkPolyData();

  if (pd->GetPolys()->GetNumberOfCells() > 0) {

      edgeFilter = vtkFeatureEdges::New();
      edgeFilter->BoundaryEdgesOn();
      edgeFilter->FeatureEdgesOff();
      edgeFilter->ManifoldEdgesOff();
      edgeFilter->NonManifoldEdgesOff();
      edgeFilter->SetInputData(pd);
      edgeFilter->Update();
      tmppd = new cvPolyData(edgeFilter->GetOutput());
      int status = sys_geom_Get2DPgon( tmppd, &pgon, &num );
      delete tmppd;
      edgeFilter->Delete();
      if (status != CV_OK ) {
        return CV_ERROR;
      }

  } else {
 
      if ( sys_geom_Get2DPgon( obj, &pgon, &num ) != CV_OK ) {
        return CV_ERROR;
      }

  }

  if (num == 0 || pgon == NULL) {
      return CV_ERROR;
  }

  if ( ggems_CrossingsMultiplyTest( pgon, num, pt ) ) {
    *result = 1;
  } else {
    *result = -1;
  }

  // delete on next call to function
  //delete [] pgon;
  g_sys_geom_PtInPoly_pgon = pgon;
  g_sys_geom_PtInPoly_num = num;

  return CV_OK;
}


// -------------------
// sys_geom_sampleLoop
// -------------------
    
cvPolyData *sys_geom_sampleLoop( cvPolyData *src, int targetNumPts )
{
  cvPolyData *merged_pd;
  vtkPolyData *pd;
  double *pts;
  int numPts;
  vtkIdType *lines;
  int numLines;
  int *startIxs;
  int numRegions;
  int i, j;
  vtkFloatingPointType *ptsOut;
  vtkIdType *linesOut;
  vtkPolyData *pdOut;
  cvPolyData *result;
  double tol = 1e10 * FindMachineEpsilon();

  if ( targetNumPts < 3 ) {
    printf("ERR: target # pts must be >= 3\n");
    return NULL;
  }

  merged_pd = sys_geom_MergePts( src );
  if ( merged_pd == NULL ) {
    return CV_ERROR;
  }
  pd = merged_pd->GetVtkPolyData();

  // First, we just want to count the number of closed loops:
  if ( VtkUtils_GetPoints( pd, &pts, &numPts ) != CV_OK ) {
    delete merged_pd;
    return NULL;
  }

  if ( VtkUtils_GetLines( pd, &lines, &numLines ) != CV_OK ) {
    delete merged_pd;
    delete [] pts;
    return NULL;
  }
  delete merged_pd;

  if ( VtkUtils_FindClosedLineRegions( lines, numLines, numPts,
				       &startIxs, &numRegions ) != CV_OK ) {
    delete [] pts;
    delete [] lines;
    return NULL;
  }
  delete [] startIxs;
  delete [] pts;
  delete [] lines;
  if ( numRegions != 1 ) {
    printf("ERR: sys_geom_sampleLoop requires input to contain exactly "
	   "1 closed loop\n");
    return NULL;
  }
 
  // Now get an ordered point list:
  if ( sys_geom_GetOrderedPts( src, &pts, &numPts ) != CV_OK ) {
    return NULL;
  }

  ptsOut = new vtkFloatingPointType [3*targetNumPts];
  linesOut = new vtkIdType [3*targetNumPts];

  // unfortunately I wrote my math code expecting 2-dimensional arrays 
  // instead of flat structures, so I need to convert here.
  cvMath *mathobj = new cvMath();
  double **nwpts = mathobj->createArray(numPts,3);
  for (i = 0; i < numPts; i++) {
      nwpts[i][0]=pts[3*i+0];
      nwpts[i][1]=pts[3*i+1];
      nwpts[i][2]=pts[3*i+2];
  } 

  double **outPts = NULL;
  int closed = 1;
  if ((mathobj->linearInterpolateCurve(nwpts, numPts, closed, targetNumPts, &outPts)) == CV_ERROR) {
      mathobj->deleteArray(nwpts,numPts,3);
      delete mathobj;
      fprintf(stderr,"ERROR:  problems sampling curve.\n");
      delete [] ptsOut;
      delete [] linesOut;
      return NULL;
  }

  for (i = 0; i < targetNumPts; i++) {
    ptsOut[3*i+0] = outPts[i][0];
    ptsOut[3*i+1] = outPts[i][1];
    ptsOut[3*i+2] = outPts[i][2];
  }

  mathobj->deleteArray(nwpts,numPts,3);
  mathobj->deleteArray(outPts,targetNumPts,3);
  delete mathobj;

  for ( i = 0; i < targetNumPts; i++ ) {
    if ( i == (targetNumPts-1) ) {
      j = 0;
    } else {
      j = i+1;
    }
    linesOut[3*i] = 2;
    linesOut[3*i+1] = i;
    linesOut[3*i+2] = j;
  }

  if ( VtkUtils_NewVtkPolyDataLines( &pdOut, targetNumPts, ptsOut,
				     targetNumPts, linesOut ) != CV_OK ) {
    delete [] ptsOut;
    delete [] linesOut;
    return NULL;
  }

  result = new cvPolyData( pdOut );
  pdOut->Delete();

  delete [] ptsOut;
  delete [] linesOut;
  return result;
}

/* -------------- */
/* sys_geom_loft_solid */
/* -------------- */

int sys_geom_loft_solid( cvPolyData **srcs,int numSrcs,int useLinearSampleAlongLength,
		int useFFT,int numOutPtsAlongLength, int numOutPtsInSegs,
		int numLinearPtsAlongLength,int numModes,int splineType,double bias, double tension,double continuity, cvPolyData **dst )
{
  cvPolyData *result = NULL;
  *dst = NULL;

  vtkNew(vtkLoftPolyDataSolid,lofter);
  for (int i=0;i<numSrcs;i++)
  {
    vtkPolyData *newPd = srcs[i]->GetVtkPolyData();
    lofter->AddInputData(newPd);
  }
  lofter->SetUseLinearSampleAlongLength(useLinearSampleAlongLength);
  lofter->SetUseFFT(useFFT);
  lofter->SetNumOutPtsAlongLength(numOutPtsAlongLength);
  lofter->SetNumOutPtsInSegs(numOutPtsInSegs);
  lofter->SetNumLinearPtsAlongLength(numLinearPtsAlongLength);
  lofter->SetNumModes(numModes);
  lofter->SetSplineType(splineType);
  lofter->SetBias(bias);
  lofter->SetTension(tension);
  lofter->SetContinuity(continuity);
  try {
    lofter->Update();

    result = new cvPolyData(lofter->GetOutput());
    *dst = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in boolean operation.\n");
    fflush(stderr);
    return CV_ERROR;
  }

  return CV_OK;
}


// ---------------------
// sys_geom_2DWindingNum
// ---------------------
// Assumes src is a 2D polygon in the xy plane.

int sys_geom_2DWindingNum( cvPolyData *pgn )
{
  cvPolyData *tmp;
  vtkPolyData *pd;
  vtkIdType *lines;
  int numLines;
  double *pts;
  int numPts;
  int *lineIds;
  int numLineIds;
  int lineId;
  int curr, next;
  int currI, currJ;
  int nextI, nextJ;
  double currVec[3], nextVec[3];
  double sign;
  double dtheta;
  double tot_theta = 0.0;
  int wnum;

  tmp = sys_geom_MergePts( pgn );
  pd = tmp->GetVtkPolyData();

  VtkUtils_GetLines( pd, &lines, &numLines );
  VtkUtils_GetPoints( pd, &pts, &numPts );

  if ( VtkUtils_GetClosedLineRegion( lines, numLines, 0,
				     &lineIds, &numLineIds ) != CV_OK ) {
    delete tmp;
    delete [] lines;
    delete [] pts;
    printf( "ERR: get closed line region failed\n" );
    return 0;
  }

  for ( curr = 0; curr < numLineIds; curr++ ) {
    lineId = lineIds[curr];
    currI = lines[2*lineId];
    currJ = lines[2*lineId+1];
    if ( curr == (numLineIds-1) ) {
      next = 0;
    } else {
      next = curr + 1;
    }
    lineId = lineIds[next];
    nextI = lines[2*lineId];
    nextJ = lines[2*lineId+1];

    currVec[0] = pts[3*currJ] - pts[3*currI];
    currVec[1] = pts[3*currJ+1] - pts[3*currI+1];
    currVec[2] = 0.0;
    NormVector( &(currVec[0]), &(currVec[1]), &(currVec[2]) );
    nextVec[0] = pts[3*nextJ] - pts[3*nextI];
    nextVec[1] = pts[3*nextJ+1] - pts[3*nextI+1];
    nextVec[2] = 0.0;
    NormVector( &(nextVec[0]), &(nextVec[1]), &(nextVec[2]) );

    // This is nothing more than the z-component of curr x next:
    sign = currVec[0] * nextVec[1] - currVec[1] * nextVec[0];

    if ( sign < 0.0 ) {
      dtheta = - acos( currVec[0] * nextVec[0] + currVec[1] * nextVec[1] );
    } else {
      dtheta = acos( currVec[0] * nextVec[0] + currVec[1] * nextVec[1] );
    }

    tot_theta += dtheta;
  }

  wnum = Round( tot_theta / (2 * CV_PI) );
  delete tmp;
  delete [] lines;
  delete [] pts;
  delete [] lineIds;
  return wnum;
}


// ----------------------
// sys_geom_PolygonNormal
// ----------------------

int sys_geom_PolygonNormal( cvPolyData *pgn, double n[] )
{
  double *pts;
  int numPts;
  int i, j;
  double v0[3], v1[3], v2[3];
  double ax, ay, az, bx, by, bz;

  if ( sys_geom_GetOrderedPts( pgn, &pts, &numPts ) != CV_OK ) {
    printf( "ERR: get ordered points failed\n" );
    return CV_ERROR;
  }

  // Adapted from vtk-3.1-beta/common/vtkPolygon.cxx:

  //  Because polygon may be concave, need to accumulate cross products to 
  //  determine true normal.
  v1[0] = pts[0];
  v1[1] = pts[1];
  v1[2] = pts[2];

  v2[0] = pts[3];
  v2[1] = pts[4];
  v2[2] = pts[5];

  n[0] = 0.0;
  n[1] = 0.0;
  n[2] = 0.0;

  for ( i = 0; i < numPts; i++ ) {

    v0[0] = v1[0];
    v0[1] = v1[1];
    v0[2] = v1[2];

    v1[0] = v2[0];
    v1[1] = v2[1];
    v1[2] = v2[2];

    j = (i + 2) % numPts;
    v2[0] = pts[3*j];
    v2[1] = pts[3*j+1];
    v2[2] = pts[3*j+2];

    // order is important!!! to maintain consistency with polygon vertex order 
    ax = v2[0] - v1[0];
    ay = v2[1] - v1[1];
    az = v2[2] - v1[2];

    bx = v0[0] - v1[0];
    by = v0[1] - v1[1];
    bz = v0[2] - v1[2];

    n[0] += (ay * bz - az * by);
    n[1] += (az * bx - ax * bz);
    n[2] += (ax * by - ay * bx);
  }

  NormVector( &(n[0]), &(n[1]), &(n[2]) );
  delete [] pts;
  return CV_OK;
}


// --------------
// sys_geom_AvgPt
// --------------
// This does NOT produce a centroid.

int sys_geom_AvgPt( cvPolyData *src, double pt[] )
{
  cvPolyData *tmp;
  vtkPolyData *pd;
  double *pts;
  int numPts;
  int i;

  tmp = sys_geom_MergePts( src );
  pd = tmp->GetVtkPolyData();
  if ( VtkUtils_GetPoints( pd, &pts, &numPts ) != CV_OK ) {
    printf( "ERR: get points failed\n" );
    delete tmp;
    return CV_ERROR;
  }

  pt[0] = pt[1] = pt[2] = 0.0;

  for ( i = 0; i < numPts; i++ ) {
    pt[0] += pts[3*i];
    pt[1] += pts[3*i+1];
    pt[2] += pts[3*i+2];
  }
  pt[0] /= numPts;
  pt[1] /= numPts;
  pt[2] /= numPts;

  delete tmp;
  delete [] pts;
  return CV_OK;
}


// --------------------------
// sys_geom_interpolateScalar
// --------------------------

int sys_geom_InterpolateScalar( cvPolyData *src, double pt[], double *scalar )
{
  // return value
  double s = 0.0;
  *scalar = s;

  vtkFloatingPointType x[3];
  vtkFloatingPointType closestPoint[3];
  vtkIdType cellId = 0;
  int subId = 0;
  vtkFloatingPointType dist2 = 0;
  vtkFloatingPointType pcoords[3];
  vtkFloatingPointType weights[10];
  vtkFloatingPointType *weightsPtr;
  vtkFloatingPointType *closestPointPtr;
  
  vtkPolyData *pd;
  pd = src->GetVtkPolyData();

  x[0]=pt[0];x[1]=pt[1];x[2]=pt[2];

  vtkCellLocator *locator = vtkCellLocator::New();
  vtkGenericCell *cell = vtkGenericCell::New();

  locator->SetDataSet(pd);
  locator->BuildLocator();

  locator->FindClosestPoint(x, closestPoint, cell, cellId, subId, dist2);
  closestPointPtr = closestPoint;
  weightsPtr = weights;
  if (cell->EvaluatePosition (x,closestPointPtr,subId,pcoords,dist2,weightsPtr) == 0) { 
      fprintf(stderr,"ERROR:  Point is not inside of generic cell!\n");
      locator->Delete();
      cell->Delete();
      return CV_ERROR;
  }
  
  //fprintf(stdout,"pcoords: %f %f %f cellId: %i subId; %i dist: %f\n",
  //        pcoords[0],pcoords[1],pcoords[2],cellId,subId,sqrt(dist2));

  vtkIdList *ids = vtkIdList::New();
  ids->Allocate(10,10);
  ids->Initialize();

  pd->GetCellPoints(cellId,ids);
 
  if (ids->GetNumberOfIds() == 0) {
      fprintf(stderr,"ERROR:  No id's found for cell %i.\n",cellId);
      ids->Delete();
      locator->Delete();
      cell->Delete();
      return CV_ERROR;
  }

  vtkDataArray *vScalars = pd->GetPointData()->GetScalars();

  vtkFloatingPointType nodeScalar = 0.0;
  int numIds = ids->GetNumberOfIds();

  for (int i = 0; i < numIds; i++) {
    nodeScalar = vScalars->GetTuple1(ids->GetId(i));
    s += weights[i]*nodeScalar;
    //fprintf(stdout,"%i: weight: %f value: %f\n", i,weights[i],nodeScalar);
  }

  ids->Delete();
  locator->Delete();
  cell->Delete();

  *scalar = s;

  return CV_OK;
}



// --------------------------
// sys_geom_InterpolateVector
// --------------------------

int sys_geom_InterpolateVector( cvPolyData *src, double pt[], double vect[] )
{
  // return value
  double vx = 0.0;
  double vy = 0.0;
  double vz = 0.0;

  vect[0] = vx;
  vect[1] = vy;
  vect[2] = vz;


  vtkFloatingPointType x[3];
  vtkFloatingPointType closestPoint[3];
  vtkIdType cellId = 0;
  int subId = 0;
  vtkFloatingPointType dist2 = 0;
  vtkFloatingPointType pcoords[3];
  vtkFloatingPointType weights[10];
  vtkFloatingPointType *weightsPtr;
  vtkFloatingPointType *closestPointPtr;
  
  vtkPolyData *pd;
  pd = src->GetVtkPolyData();

  x[0]=pt[0];x[1]=pt[1];x[2]=pt[2];

  vtkCellLocator *locator = vtkCellLocator::New();
  vtkGenericCell *cell = vtkGenericCell::New();

  locator->SetDataSet(pd);
  locator->BuildLocator();

  locator->FindClosestPoint(x, closestPoint, cell, cellId, subId, dist2);
  
  closestPointPtr = closestPoint;
  weightsPtr = weights;
  if (cell->EvaluatePosition (x,closestPointPtr,subId,pcoords,dist2,weightsPtr) == 0) { 
     fprintf(stderr,"ERROR:  Point is not inside of generic cell!\n");
     locator->Delete();
     cell->Delete();
     return CV_ERROR;
  }
  
  //  fprintf(stdout,"pcoords: %f %f %f cellId: %i subId; %i dist: %f\n",
  //      pcoords[0],pcoords[1],pcoords[2],cellId,subId,sqrt(dist2));
 
 
  vtkIdList *ids = vtkIdList::New();
  ids->Allocate(10,10);
  ids->Initialize();

  pd->GetCellPoints(cellId,ids);
 
  if (ids->GetNumberOfIds() == 0) {
      fprintf(stderr,"ERROR:  No id's found for cell %i.\n",cellId);
      ids->Delete();
      locator->Delete();
      cell->Delete();
      return CV_ERROR;
  }

  vtkDataArray *vVectors= pd->GetPointData()->GetVectors();

  vtkFloatingPointType *nodeVector;
  
  int numIds = ids->GetNumberOfIds();

  for (int i = 0; i < numIds; i++) {
    nodeVector = vVectors->GetTuple(ids->GetId(i));
    vx += weights[i]*nodeVector[0];
    vy += weights[i]*nodeVector[1];
    vz += weights[i]*nodeVector[2];
    //    fprintf(stdout,"%i: weight: %f value: %f %f %f\n", i,weights[i],nodeVector[0], nodeVector[1], nodeVector[2]);
  }

  ids->Delete();
  locator->Delete();
  cell->Delete();

  vect[0] = vx;
  vect[1] = vy;
  vect[2] = vz;
  return CV_OK;
}



// --------------------------
// sys_geom_IntersectWithLine
// --------------------------

int sys_geom_IntersectWithLine( cvPolyData *src, double p0[], double p1[], double intersect[] )
{
  // return value
  intersect[0] = 0.0;
  intersect[1] = 0.0;
  intersect[2] = 0.0;

  vtkFloatingPointType a0[3];  
  vtkFloatingPointType a1[3]; 
  vtkFloatingPointType tol = 0.001;  
  vtkFloatingPointType t = 0.0;  
  vtkFloatingPointType x[3];  
  vtkFloatingPointType pcoords[3];  
  int subId = 0;  
  vtkIdType cellId = 0;
 
  vtkPolyData *pd;
  pd = src->GetVtkPolyData();

  for (int i=0; i < 3; i++) {
      a0[i]=p0[i];
      a1[i]=p1[i];
  }

  vtkCellLocator *locator = vtkCellLocator::New();
  vtkGenericCell *cell = vtkGenericCell::New();

  locator->SetDataSet(pd);
  locator->BuildLocator();

  //fprintf(stdout,"a0: %f %f %f\n",a0[0],a0[1],a0[2]);
  //fprintf(stdout,"a1: %f %f %f\n",a1[0],a1[1],a1[2]);
  //fprintf(stdout,"tol: %f\n",tol);

  x[0]=0;x[1]=0;x[2]=0;
  //if (locator->IntersectWithLine(a0, a1, tol, t, x, pcoords, subId) == 0) {
  if (locator->IntersectWithLine(a0,a1,tol,t,x,pcoords,subId,cellId,cell) == 0) {
      fprintf(stderr,"ERROR:  Line does not intersect vtkPolyData!\n");
      locator->Delete();
      cell->Delete();
      return CV_ERROR;
  }

  locator->Delete();
  cell->Delete();

  intersect[0]=x[0];intersect[1]=x[1];intersect[2]=x[2];

  return CV_OK;
}


// -----------------
//   geom_warp3dPts
// -----------------

cvPolyData *sys_geom_warp3dPts(cvPolyData *src, double scale) {

    vtkPolyData *orgpd = src->GetVtkPolyData();
    int numPts = orgpd->GetNumberOfPoints();
    fprintf(stdout,"numPts: %i\n",numPts);
 
   // get the normals and vectors
    vtkDataArray* normals = orgpd->GetPointData()->GetNormals();
    vtkDataArray* vectors = orgpd->GetPointData()->GetVectors();
    vtkPoints* orgpts = orgpd->GetPoints();

    // create return vtk vector
    vtkPoints *newpts = vtkPoints::New();
    //newpts->SetNumberOfComponents(3);
    newpts->Allocate(numPts,10000);
    newpts->Initialize();

    vtkFloatArray *mags = vtkFloatArray::New();
    mags->SetNumberOfComponents(1);
    mags->Allocate(numPts,10000);
    mags->Initialize();

    vtkFloatingPointType nrm[3];
    vtkFloatingPointType v[3];
    vtkFloatingPointType newpt[3];
    vtkFloatingPointType pt[3];

    vtkFloatingPointType v_dot_n = 0.0;

    for (int i = 0; i < numPts; i++) {
           // get outward normal
           normals->GetTuple(i,nrm);
           vectors->GetTuple(i,v);
           orgpts->GetPoint(i,pt);

           // calculate normal component
           v_dot_n = v[0]*nrm[0]+v[1]*nrm[1]+v[2]*nrm[2];
           mags->InsertNextTuple1(v_dot_n);

           // scale by factor and add along normal vector
           newpt[0] = pt[0]+v_dot_n*scale*nrm[0];
           newpt[1] = pt[1]+v_dot_n*scale*nrm[1];
           newpt[2] = pt[2]+v_dot_n*scale*nrm[2];

           newpts->InsertNextPoint(newpt[0],newpt[1],newpt[2]);
    }

    // create cvPolyData object to return
    vtkPolyData* pd = vtkPolyData::New();
    pd->CopyStructure(orgpd);
    pd->SetPoints(newpts);
    pd->GetPointData()->SetScalars(mags);
    cvPolyData* reposobj = new cvPolyData(pd);
    return reposobj;

}


// ----------------------
//   geom_mathPointData
// ----------------------

int sys_geom_mathPointData( cvPolyData *srcA, cvPolyData *srcB, sys_geom_math_scalar scflag,
                            sys_geom_math_vector vflag, cvPolyData **dst ) {
    int i = 0;
    int j = 0;
    vtkFloatingPointType myvec[3];    
    vtkFloatingPointType s=0;
    vtkFloatingPointType tmpvec[3];    
    vtkFloatingPointType tmps=0;
    vtkFloatingPointArrayType *scalar = NULL;
    vtkFloatingPointArrayType *vec = NULL;

    // all of the pds must have the same num pts
    int numPtsA = srcA->GetVtkPolyData()->GetNumberOfPoints();
    int numPtsB = srcB->GetVtkPolyData()->GetNumberOfPoints();
    int numPts = numPtsA;
    if (numPtsA != numPtsB) {
        return CV_ERROR;
    }

    if (scflag == SYS_GEOM_NO_SCALAR && vflag == SYS_GEOM_NO_VECTOR) {
        return CV_ERROR;
    }

    // get pointers to data
    if (scflag != SYS_GEOM_NO_SCALAR) {
      vtkDataArray *scalarsA=srcA->GetVtkPolyData()->GetPointData()->GetScalars();
      vtkDataArray *scalarsB=srcB->GetVtkPolyData()->GetPointData()->GetScalars();
      // create return vtk scalar array
      scalar = vtkFloatingPointArrayType::New();
      scalar->SetNumberOfComponents(1);
      scalar->Allocate(numPts,1000);
      scalar->Initialize();
      for (i = 0; i < numPts; i++) {
        s = scalarsA->GetTuple1(i);
        tmps = scalarsB->GetTuple1(i);
        if (scflag == SYS_GEOM_ADD_SCALAR) {
           s = s + tmps; 
        } else if (scflag == SYS_GEOM_SUBTRACT_SCALAR) {
           s = s - tmps; 
        } else if (scflag == SYS_GEOM_MULTIPLY_SCALAR) {
           s = s * tmps; 
        } else if (scflag == SYS_GEOM_DIVIDE_SCALAR) {
           s = s / tmps; 
        } else {
          fprintf(stdout,"invalid flag!\n"); 
          return CV_ERROR;
        }
        scalar->InsertNextTuple1(s);
      }
    }

    if (vflag != SYS_GEOM_NO_VECTOR) {
      vtkDataArray *vectorsA=srcA->GetVtkPolyData()->GetPointData()->GetVectors();
      vtkDataArray *vectorsB=srcB->GetVtkPolyData()->GetPointData()->GetVectors();
      // create return vtk vector array
      vec = vtkFloatingPointArrayType::New();
      vec->SetNumberOfComponents(3);
      vec->Allocate(numPts,1000);
      vec->Initialize();

      for (i = 0; i < numPts; i++) {
        vectorsA->GetTuple(i,myvec);
        vectorsB->GetTuple(i,tmpvec);
        if (vflag == SYS_GEOM_ADD_VECTOR) {
          myvec[0] = myvec[0]+tmpvec[0];
          myvec[1] = myvec[1]+tmpvec[1];
          myvec[2] = myvec[2]+tmpvec[2];
        } else if (vflag == SYS_GEOM_SUBTRACT_VECTOR) {
          myvec[0] = myvec[0]-tmpvec[0];
          myvec[1] = myvec[1]-tmpvec[1];
          myvec[2] = myvec[2]-tmpvec[2];
        } else if (vflag == SYS_GEOM_MULTIPLY_VECTOR) {
          myvec[0] = myvec[0]*tmpvec[0];
          myvec[1] = myvec[1]*tmpvec[1];
          myvec[2] = myvec[2]*tmpvec[2];
        } else if (vflag == SYS_GEOM_DIVIDE_VECTOR) {
          myvec[0] = myvec[0]/tmpvec[0];
          myvec[1] = myvec[1]/tmpvec[1];
          myvec[2] = myvec[2]/tmpvec[2];
        } else {
          fprintf(stdout,"invalid flag!\n"); 
          return CV_ERROR;
        }
        vec->InsertNextTuple3(myvec[0],myvec[1],myvec[2]);
      }
    }

    // create cvPolyData object to return
    vtkPolyData* pd = vtkPolyData::New();
    pd->CopyStructure(srcA->GetVtkPolyData());
    if (scflag !=  SYS_GEOM_NO_SCALAR) {
      pd->GetPointData()->SetScalars(scalar);
    } 
    if (vflag != SYS_GEOM_NO_VECTOR) {
      pd->GetPointData()->SetVectors(vec);
    }

    cvPolyData* reposobj = new cvPolyData(pd);
    *dst =  reposobj;
    pd->Delete();

    return CV_OK;
}


// ----------------------
//   geom_Project
// ----------------------

int sys_geom_Project( cvPolyData *srcA, cvPolyData *srcB, sys_geom_math_scalar scflag,
                            sys_geom_math_vector vflag, cvPolyData **dst ) {

    int i = 0;
    int j = 0;

    vtkFloatingPointType s=0;
    vtkFloatingPointArrayType *scalar = NULL;
    vtkFloatingPointArrayType *vec = NULL;

    double vx = 0.0;
    double vy = 0.0;
    double vz = 0.0;

    vtkFloatingPointType x[3];
    vtkFloatingPointType closestPoint[3];
    vtkIdType cellId = 0;
    int subId = 0;
    vtkFloatingPointType dist2 = 0;
    vtkFloatingPointType pcoords[3];
    vtkFloatingPointType weights[10];
    vtkFloatingPointType *weightsPtr;
    vtkFloatingPointType *closestPointPtr;

    vtkPolyData *pdA = srcA->GetVtkPolyData();
    vtkPolyData *pdB = srcB->GetVtkPolyData();

    // all of the pds must have the same num pts
    int numPtsA = pdA->GetNumberOfPoints();
    int numPtsB = pdB->GetNumberOfPoints();

    if (scflag == SYS_GEOM_NO_SCALAR && vflag == SYS_GEOM_NO_VECTOR) {
        return CV_ERROR;
    }

    // build a locator to find closest points in A
    vtkCellLocator *locator = vtkCellLocator::New();
    vtkGenericCell *cell = vtkGenericCell::New();

    locator->SetDataSet(pdA);
    locator->BuildLocator();

    vtkDataArray *scalarsA;
    vtkDataArray *vectorsA;

    // get pointers to data and create return objects
    if (scflag != SYS_GEOM_NO_SCALAR) {
      scalarsA=srcA->GetVtkPolyData()->GetPointData()->GetScalars();
      // create return vtk scalar array
      scalar = vtkFloatingPointArrayType::New();
      scalar->SetNumberOfComponents(1);
      scalar->Allocate(numPtsB,1000);
      scalar->Initialize();
    }
    if (vflag != SYS_GEOM_NO_VECTOR) {
      vectorsA=srcA->GetVtkPolyData()->GetPointData()->GetVectors();
      // create return vtk vector array
      vec = vtkFloatingPointArrayType::New();
      vec->SetNumberOfComponents(3);
      vec->Allocate(numPtsB,1000);
      vec->Initialize();
    }

    vtkIdList *ids = vtkIdList::New();
    ids->Allocate(10,10);
    ids->Initialize();

    for (i = 0; i < numPtsB; i++) {

      pdB->GetPoint(i,x);

      locator->FindClosestPoint(x, closestPoint, cell, cellId, subId, dist2);
      closestPointPtr = closestPoint;
      weightsPtr = weights;

      if (cell->EvaluatePosition (x,closestPointPtr,subId,pcoords,dist2,weightsPtr) == 0) {
        fprintf(stderr,"Warning:  Point is not inside of generic cell!\n");
        fprintf(stderr,"          using average value for cell.\n");
        weights[0]=1.0/3.0;
        weights[1]=1.0/3.0;
        weights[2]=1.0/3.0;
      }

      pdA->GetCellPoints(cellId,ids);
      if (ids->GetNumberOfIds() == 0) {
        fprintf(stderr,"ERROR:  No id's found for cell %i.\n",cellId);
        ids->Delete();
        locator->Delete();
        cell->Delete();
        if (scalar != NULL) {
          scalar->Delete();
        }
        if (vec != NULL) {
          vec->Delete();
        }
        return CV_ERROR;
      }

      vtkFloatingPointType *nodeVector;
      vtkFloatingPointType nodeScalar;
      int numIds = ids->GetNumberOfIds();
      vx = 0.0; vy = 0.0; vz = 0.0; s = 0.0;
      for (int i = 0; i < numIds; i++) {
        if (scflag != SYS_GEOM_NO_SCALAR) {
          nodeScalar = scalarsA->GetTuple1(ids->GetId(i));
          s += weights[i]*nodeScalar;
        }
        if (vflag != SYS_GEOM_NO_VECTOR) {
          nodeVector = vectorsA->GetTuple(ids->GetId(i));
          vx += weights[i]*nodeVector[0];
          vy += weights[i]*nodeVector[1];
          vz += weights[i]*nodeVector[2];
          //  fprintf(stdout,"%i: weight: %f value: %f %f %f\n", i,weights[i],nodeVector[0], nodeVector[1], nodeVector[2]);
        }
      }
      if (scflag != SYS_GEOM_NO_SCALAR) scalar->InsertNextTuple1(s);
      if (vflag != SYS_GEOM_NO_VECTOR) vec->InsertNextTuple3(vx,vy,vz);
    }

    // create cvPolyData object to return
    vtkPolyData* pd = vtkPolyData::New();
    pd->CopyStructure(srcB->GetVtkPolyData());
    if (scflag !=  SYS_GEOM_NO_SCALAR) {
      pd->GetPointData()->SetScalars(scalar);
    } 
    if (vflag != SYS_GEOM_NO_VECTOR) {
      pd->GetPointData()->SetVectors(vec);
    }

    cvPolyData* reposobj = new cvPolyData(pd);
    *dst =  reposobj;
    pd->Delete();

    // clean up
    ids->Delete();
    locator->Delete();
    cell->Delete();
    if (scalar != NULL) scalar->Delete();
    if (vec != NULL) vec->Delete();

    return CV_OK;
}



// -------------------------
//   geom_ReplacePointData
// -------------------------

int sys_geom_ReplacePointData( cvPolyData *srcA, cvPolyData *srcB, sys_geom_math_scalar scflag,
                            sys_geom_math_vector vflag, cvPolyData **dst ) {

    int i = 0;
    int j = 0;

    vtkFloatingPointType s=0;
    vtkFloatingPointArrayType *scalar = NULL;
    vtkFloatingPointArrayType *vec = NULL;

    double vx = 0.0;
    double vy = 0.0;
    double vz = 0.0;

    vtkPolyData *pdA = srcA->GetVtkPolyData();
    vtkPolyData *pdB = srcB->GetVtkPolyData();

    // all of the pds must have the same num pts
    int numPtsA = pdA->GetNumberOfPoints();
    int numPtsB = pdB->GetNumberOfPoints();

    if (scflag == SYS_GEOM_NO_SCALAR && vflag == SYS_GEOM_NO_VECTOR) {
        return CV_ERROR;
    }

    // must have scalars on srcB
    vtkDataArray *scalarsB = NULL;
    scalarsB=srcB->GetVtkPolyData()->GetPointData()->GetScalars();
    if (scalarsB == NULL) {
        fprintf(stderr,"ERROR:  no scalars on srcB!\n");
        return CV_ERROR;
    }

    vtkDataArray *scalarsA;
    vtkDataArray *vectorsA;

    // get pointers to data and create return objects
    if (scflag != SYS_GEOM_NO_SCALAR) {
      scalarsA=srcA->GetVtkPolyData()->GetPointData()->GetScalars();
      // create return vtk scalar array
      scalar = vtkFloatingPointArrayType::New();
      scalar->SetNumberOfComponents(1);
      scalar->Allocate(numPtsB,1000);
      scalar->Initialize();
    }
    if (vflag != SYS_GEOM_NO_VECTOR) {
      vectorsA=srcA->GetVtkPolyData()->GetPointData()->GetVectors();
      // create return vtk vector array
      vec = vtkFloatingPointArrayType::New();
      vec->SetNumberOfComponents(3);
      vec->Allocate(numPtsB,1000);
      vec->Initialize();
    }

    for (i = 0; i < numPtsB; i++) {

      double sB = scalarsB->GetTuple1(i);
      int iB = (int)sB;
      //fprintf(stdout,"sB: %lf  iB: %i\n",sB,iB);

      vtkFloatingPointType *nodeVector;
      vtkFloatingPointType nodeScalar;
      s =0.0;
      vx = 0.0; vy = 0.0; vz = 0.0; s = 0.0;

      // need to offset by -1 here since node
      // numbers start at 1 but vtk starts refs
      // at 0
      iB = iB - 1;

      if (scflag != SYS_GEOM_NO_SCALAR) {
        nodeScalar = scalarsA->GetTuple1(iB);
        s = nodeScalar;
      }
      if (vflag != SYS_GEOM_NO_VECTOR) {
        nodeVector = vectorsA->GetTuple(iB);
        vx = nodeVector[0];
        vy = nodeVector[1];
        vz = nodeVector[2];
      }
      if (scflag != SYS_GEOM_NO_SCALAR) scalar->InsertNextTuple1(s);
      if (vflag != SYS_GEOM_NO_VECTOR) vec->InsertNextTuple3(vx,vy,vz);

    }

    // create cvPolyData object to return
    vtkPolyData* pd = vtkPolyData::New();
    pd->CopyStructure(srcB->GetVtkPolyData());
    if (scflag !=  SYS_GEOM_NO_SCALAR) {
      pd->GetPointData()->SetScalars(scalar);
    } 
    if (vflag != SYS_GEOM_NO_VECTOR) {
      pd->GetPointData()->SetVectors(vec);
    }

    cvPolyData* reposobj = new cvPolyData(pd);
    *dst =  reposobj;
    pd->Delete();

    // clean up
    if (scalar != NULL) scalar->Delete();
    if (vec != NULL) vec->Delete();

    return CV_OK;
}

/* -------------- */
/* sys_geom_set_array_for_local_op_sphere */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  
 *  @brief Function to set a boolean array on the surface for local mesh operations.
 *  Points are set based on cells or points in spherical region. If based on cells,
 *  cell is determine to be in sphere if the centroid of the cell is within the sphere.
 *  @param *pd The input polydata on which to set an array 
 *  @param **outpd polydata that contains the output surface with new array
 *  @param radius radius of the sphere
 *  @param *center center of the sphere
 *  @param *outarray This contains the arrayname holding the boolean array
 *  @param datatype This indicates whether the input array is point data (0) 
 *  or cell data (1)
 *  @return CV_OK if the function executes properly
 */

int sys_geom_set_array_for_local_op_sphere( cvPolyData *pd,cvPolyData **outpd,double radius,double *center,char *outarrayname,int datatype)
{
  vtkPolyData *geom = pd->GetVtkPolyData();
  cvPolyData *result = NULL;

  fprintf(stdout,"Adding array in sphere\n");
  fprintf(stdout,"Out Array Name: %s\n",outarrayname);
  fprintf(stdout,"Radius: %.3f\n",radius);
  fprintf(stdout,"Center: %.3f %.3f %.3f\n",center[0],center[1],center[2]);
  fprintf(stdout,"Datatype: %d\n",datatype);

  vtkNew(vtkPolyData,tmp);
  tmp->DeepCopy(geom);
  tmp->BuildLinks();
  vtkNew(vtkIntArray,newArray);
  newArray->SetName(outarrayname);
  if (datatype == 0)
  {
    double pt[3];
    int numPoints = tmp->GetNumberOfPoints();
    if (PlyDtaUtils_PDCheckArrayName(tmp,0,outarrayname) != CV_OK)
    {
      newArray->SetNumberOfTuples(numPoints);
      for (vtkIdType id=0;id < numPoints;id++)
	newArray->InsertValue(id,0);
    }
    else 
      newArray = vtkIntArray::SafeDownCast(tmp->GetPointData()->GetArray(outarrayname));
    for (vtkIdType id=0; id < numPoints;id++)
    {
      tmp->GetPoint(id,pt);
      double dist = sqrt(pow(pt[0] - center[0],2) + 
	                 pow(pt[1] - center[1],2) + 
			 pow(pt[2] - center[2],2));
      if (dist <= radius)
	newArray->InsertValue(id,1);
    }
    if (PlyDtaUtils_PDCheckArrayName(tmp,0,outarrayname) == CV_OK)
      tmp->GetPointData()->RemoveArray(outarrayname);
    tmp->GetPointData()->AddArray(newArray);
  } 
  else
  {
    double centroid[3];
    vtkIdType npts,*pts;
    int numCells = tmp->GetNumberOfCells();
    if (PlyDtaUtils_PDCheckArrayName(tmp,1,outarrayname) != CV_OK)
    {
      newArray->SetNumberOfTuples(numCells);
      for (vtkIdType id=0;id < numCells;id++)
	newArray->InsertValue(id,0);
    }
    else 
      newArray = vtkIntArray::SafeDownCast(tmp->GetCellData()->GetArray(outarrayname));
    for (vtkIdType id=0; id < numCells;id++)
    {
      tmp->GetCellPoints(id,npts,pts);
      vtkSmartPointer<vtkPoints> polyPts = vtkSmartPointer<vtkPoints>::New();
      vtkSmartPointer<vtkIdTypeArray> polyPtIds = vtkSmartPointer<vtkIdTypeArray>::New();
      for (int i=0;i<npts;i++)
      {
	polyPtIds->InsertValue(i,i);
	polyPts->InsertNextPoint(tmp->GetPoint(pts[i]));
      }
      vtkPolygon::ComputeCentroid(polyPtIds,polyPts,centroid);
      double dist = sqrt(pow(centroid[0] - center[0],2) + 
	                 pow(centroid[1] - center[1],2) + 
			 pow(centroid[2] - center[2],2));
      if (dist <= radius)
	newArray->InsertValue(id,1);
    }
    if (PlyDtaUtils_PDCheckArrayName(tmp,1,outarrayname) == CV_OK)
      tmp->GetCellData()->RemoveArray(outarrayname);
    tmp->GetCellData()->AddArray(newArray);
  }
  result = new cvPolyData(tmp);
  *outpd = result;

  return CV_OK;
}

/* -------------- */
/* sys_geom_set_array_for_local_op_face */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  
 *  @brief Function to set a boolean array on the surface for local mesh operations.
 *  Points are set based on an id of a given array
 *  @param *pd The input polydata on which to set an array 
 *  @param **outpd polydata that contains the output surface with new array
 *  @param *arrayname array on which to look for the given values
 *  @param *values ids to looks for in the given array name. Cells with this 
 *  id are given a value of 1
 *  @param *outarray This contains the arrayname holding the boolean array
 *  @param datatype This indicates whether the input array is point data (0) 
 *  or cell data (1)
 *  values indication with cells to decimate
 *  @return CV_OK if the function executes properly
 */

int sys_geom_set_array_for_local_op_face( cvPolyData *pd,cvPolyData **outpd,char *inarrayname,int *vals,int nvals,char *outarrayname,int datatype)
{
  vtkPolyData *geom = pd->GetVtkPolyData();
  cvPolyData *result = NULL;

  fprintf(stdout,"Adding array on face\n");
  fprintf(stdout,"Given Array Name: %s\n",inarrayname);
  fprintf(stdout,"Target Array Name: %s\n",outarrayname);
  fprintf(stdout,"Array Type: %d\n",datatype);
  fprintf(stdout,"Number of Ids: %d\n",nvals);

  double range[2];
  int max;
  int value=0;
  int *wantval;
  vtkNew(vtkPolyData,tmp);
  tmp->DeepCopy(geom);
  vtkNew(vtkIntArray,newArray);
  newArray->SetName(outarrayname);
  if (datatype == 0)
  {
    if (PlyDtaUtils_PDCheckArrayName(tmp,0,inarrayname) != CV_OK)
    {
      fprintf(stderr,"%s Array is not on the surface\n",inarrayname);
      return CV_ERROR;
    }
    int numPoints = tmp->GetNumberOfPoints();
    if (PlyDtaUtils_PDCheckArrayName(tmp,0,outarrayname) != CV_OK)
    {
      newArray->SetNumberOfTuples(numPoints);
      for (vtkIdType id=0;id< numPoints;id++)
	newArray->InsertValue(id,0);
    }
    else
      newArray = vtkIntArray::SafeDownCast(tmp->GetPointData()->GetArray(outarrayname));
    tmp->GetPointData()->GetArray(inarrayname)->GetRange(range);
    max = range[1];
    wantval =  new int[max];
    for (int i=0; i< max; i++)
      wantval[i] = 0;
    for (int i=0; i< nvals; i++)
      wantval[vals[i]-1] = 1;

    for (vtkIdType id=0;id < numPoints; id++)
    {
	value = (int)tmp->GetPointData()->GetArray(inarrayname)->GetTuple1(id);
      
      if (wantval[value-1])
	newArray->InsertValue(id,1);
    }
    if (PlyDtaUtils_PDCheckArrayName(tmp,0,outarrayname) == CV_OK)
      tmp->GetPointData()->RemoveArray(outarrayname);
    tmp->GetPointData()->AddArray(newArray);
    delete [] wantval;
  } 
  else
  {
    if (PlyDtaUtils_PDCheckArrayName(tmp,1,inarrayname) != CV_OK)
    {
      fprintf(stderr,"%s Array is not on the surface\n",inarrayname);
      return CV_ERROR;
    }
    int numCells = tmp->GetNumberOfCells();
    if (PlyDtaUtils_PDCheckArrayName(tmp,1,outarrayname) != CV_OK)
    {
      newArray->SetNumberOfTuples(numCells);
      for (vtkIdType id=0;id< numCells;id++)
	newArray->InsertValue(id,0);
    }
    else
      newArray = vtkIntArray::SafeDownCast(tmp->GetCellData()->GetArray(outarrayname));
    tmp->GetCellData()->GetArray(inarrayname)->GetRange(range);
    max = range[1];
    wantval =  new int[max];
    for (int i=0; i< max; i++)
      wantval[i] = 0;
    for (int i=0; i< nvals; i++)
      wantval[vals[i]-1] = 1;

    for (int id=0;id < numCells; id++)
    {
	value = (int)tmp->GetCellData()->GetArray(inarrayname)->GetTuple1(id);
      
      if (wantval[value-1])
	newArray->InsertValue(id,1);
    }
    if (PlyDtaUtils_PDCheckArrayName(tmp,1,outarrayname) == CV_OK)
      tmp->GetCellData()->RemoveArray(outarrayname);
    tmp->GetCellData()->AddArray(newArray);
    delete [] wantval;
  }
  result = new cvPolyData(tmp);
  *outpd = result;

  return CV_OK;
}

/* -------------- */
/* sys_geom_set_array_for_local_op_cells */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  
 *  @brief Function to set a boolean array on the surface for local mesh operations.
 *  Points are set based on an id of a given array
 *  @param *pd The input polydata on which to set an array 
 *  @param **outpd polydata that contains the output surface with new array
 *  @param *values ids of cells to change on PolyData1
 *  @param *outarray This contains the arrayname holding the boolean array
 *  @param datatype This indicates whether the input array is point data (0) 
 *  or cell data (1)
 *  values indication with cells to decimate
 *  @return CV_OK if the function executes properly
 */

int sys_geom_set_array_for_local_op_cells( cvPolyData *pd,cvPolyData **outpd,int *vals,int nvals,char *outarrayname,int datatype)
{
  vtkPolyData *geom = pd->GetVtkPolyData();
  cvPolyData *result = NULL;

  fprintf(stdout,"Adding array on cells\n");
  fprintf(stdout,"Target Array Name: %s\n",outarrayname);
  fprintf(stdout,"Array Type: %d\n",datatype);
  fprintf(stdout,"Number of Ids: %d\n",nvals);

  int *wantval;
  vtkNew(vtkPolyData,tmp);
  tmp->DeepCopy(geom);
  vtkNew(vtkIntArray,newArray);
  newArray->SetName(outarrayname);
  if (datatype == 0)
  {
    int numPoints = tmp->GetNumberOfPoints();
    if (PlyDtaUtils_PDCheckArrayName(tmp,0,outarrayname) != CV_OK)
    {
      newArray->SetNumberOfTuples(numPoints);
      for (vtkIdType id=0;id< numPoints;id++)
	newArray->InsertValue(id,0);
    }
    else
      newArray = vtkIntArray::SafeDownCast(tmp->GetPointData()->GetArray(outarrayname));
    wantval =  new int[numPoints];
    for (int i=0; i< numPoints; i++)
      wantval[i] = 0;
    for (int i=0; i< nvals; i++)
      wantval[vals[i]-1] = 1;

    for (vtkIdType id=0;id < numPoints; id++)
    {
      if (wantval[id-1])
	newArray->InsertValue(id,1);
    }
    if (PlyDtaUtils_PDCheckArrayName(tmp,0,outarrayname) == CV_OK)
      tmp->GetPointData()->RemoveArray(outarrayname);
    tmp->GetPointData()->AddArray(newArray);
    delete [] wantval;
  } 
  else
  {
    int numCells = tmp->GetNumberOfCells();
    if (PlyDtaUtils_PDCheckArrayName(tmp,1,outarrayname) != CV_OK)
    {
      newArray->SetNumberOfTuples(numCells);
      for (vtkIdType id=0;id< numCells;id++)
	newArray->InsertValue(id,0);
    }
    else
      newArray = vtkIntArray::SafeDownCast(tmp->GetCellData()->GetArray(outarrayname));
    wantval =  new int[numCells];
    for (int i=0; i< numCells; i++)
      wantval[i] = 0;
    for (int i=0; i< nvals; i++)
      wantval[vals[i]-1] = 1;

    for (int id=0;id < numCells; id++)
    {
      if (wantval[id-1])
	newArray->InsertValue(id,1);
    }
    if (PlyDtaUtils_PDCheckArrayName(tmp,1,outarrayname) == CV_OK)
      tmp->GetCellData()->RemoveArray(outarrayname);
    tmp->GetCellData()->AddArray(newArray);
    delete [] wantval;
  }
  result = new cvPolyData(tmp);
  *outpd = result;

  return CV_OK;
}

/* -------------- */
/* sys_geom_set_array_for_local_op_face_blend */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  
 *  @brief Function to set a boolean array on the surface for local mesh operations.
 *  Points are set based on an id of a given array
 *  @param *pd The input polydata on which to set an array 
 *  @param **outpd polydata that contains the output surface with new array
 *  @param *arrayname array on which to look for the given values
 *  @param *values ids to looks for in the given array name. Cells with this 
 *  id are given a value of 1
 *  @param *outarray This contains the arrayname holding the boolean array
 *  @param datatype This indicates whether the input array is point data (0) 
 *  or cell data (1)
 *  values indication with cells to decimate
 *  @return CV_OK if the function executes properly
 */

int sys_geom_set_array_for_local_op_face_blend( cvPolyData *pd,cvPolyData **outpd,char *inarrayname,int *vals,int nvals,double radius,char *outarrayname,int datatype)
{
  vtkPolyData *geom = pd->GetVtkPolyData();
  cvPolyData *result = NULL;

  fprintf(stdout,"Adding face blend\n");
  fprintf(stdout,"Given Array Name: %s\n",inarrayname);
  fprintf(stdout,"Target Array Name: %s\n",outarrayname);
  fprintf(stdout,"Array Type: %d\n",datatype);
  fprintf(stdout,"Number of Ids: %d\n",nvals);
  fprintf(stdout,"Radius: %.4f\n",radius);

  double range[2];
  int max;
  int value=0;
  int *wantval;
  vtkNew(vtkPolyData,tmp);
  tmp->DeepCopy(geom);
  vtkNew(vtkIntArray,newArray);
  newArray->SetName(outarrayname);
  if (datatype == 0)
  {
    fprintf(stderr,"Sorry, this functionality is not currently available");
    delete [] wantval;
  } 
  else
  {
    if (PlyDtaUtils_PDCheckArrayName(tmp,1,inarrayname) != CV_OK)
    {
      fprintf(stderr,"%s Array is not on the surface\n",inarrayname);
      return CV_ERROR;
    }
    vtkNew(vtkIdList,targetCells);
    for (int i=0; i< nvals; i++)
      targetCells->InsertNextId((vals[i]));
    vtkNew(vtkFindSeparateRegions,separator);
    separator->SetInputData(tmp);
    separator->SetOutPointArrayName("BoundaryPoints");
    separator->SetArrayName(inarrayname);
    separator->SetCellIds(targetCells);
    separator->Update();

    vtkNew(vtkGetSphereRegions,sphereSetter);
    sphereSetter->SetInputData(separator->GetOutput());
    sphereSetter->SetOutCellArrayName(outarrayname);
    sphereSetter->SetCellArrayName(inarrayname);
    sphereSetter->SetPointArrayName("BoundaryPoints");
    sphereSetter->SetSphereRadius(radius);
    sphereSetter->Update();

    result = new cvPolyData(sphereSetter->GetOutput());
    *outpd = result;
  }

  return CV_OK;
}


/* -------------- */
/* sys_geom_local_decimation */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  
 *  @brief Function to perform a decimation operation on only a portion 
 *  of a polydata
 *  @param *pd The input polydata on which to perform decimation 
 *  @param **outpd The output polydata surface
 *  @param *pointarrayname This contains the arrayname holding the boolean
 *  values indication with points to decimate
 *  @param *cellarrayname This contains the arrayname holding the boolean
 *  values indication with cells to decimate
 *  @return CV_OK if the function executes properly
 */

int sys_geom_local_quadric_decimation( cvPolyData *pd,cvPolyData **outpd, double target,
		char *pointarrayname, char *cellarrayname)
{
  vtkPolyData *geom = pd->GetVtkPolyData();
  cvPolyData *result = NULL;

  fprintf(stdout,"Running local decimation\n");
  fprintf(stdout,"Target Decimation: %.4f\n",target);
  fprintf(stdout,"Point Array Name: %s\n",pointarrayname);
  fprintf(stdout,"Cell Array Name: %s\n",cellarrayname);
  try {
    vtkNew(vtkLocalQuadricDecimation,decimator);
    decimator->SetInputData(geom);
    if (pointarrayname != 0) 
    {
      decimator->SetDecimatePointArrayName(pointarrayname);
      decimator->UsePointArrayOn();
    }
    if (cellarrayname != 0) 
    {
      decimator->SetDecimateCellArrayName(cellarrayname);
      decimator->UseCellArrayOn();
    }
    decimator->SetTargetReduction(target);
    decimator->Update();

    result = new cvPolyData( decimator->GetOutput());
    *outpd = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in local decimation.\n");
    fflush(stderr);
    return CV_ERROR;
  }

  return CV_OK;
}

/* -------------- */
/* sys_geom_local_laplacian_smooth */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  
 *  @brief Function to perform a smoothing operation on only a portion 
 *  of a polydata
 *  @param *pd The input polydata on which to perform decimation 
 *  @param **outpd The output polydata surface
 *  @param numiters number of iterations of smoothing to perform
 *  @param relax relaxation factor for smoothing
 *  @param *pointarrayname This contains the arrayname holding the boolean
 *  values indication with points to smooth
 *  @param *cellarrayname This contains the arrayname holding the boolean
 *  values indication with cells to smooth
 *  @return CV_OK if the function executes properly
 */

int sys_geom_local_laplacian_smooth( cvPolyData *pd,cvPolyData **outpd, int numiters,
		double relax,char *pointarrayname, char *cellarrayname)
{
  vtkPolyData *geom = pd->GetVtkPolyData();
  cvPolyData *result = NULL;

  fprintf(stdout,"Running local smoothing\n");
  fprintf(stdout,"Num Iters: %d\n",numiters);
  fprintf(stdout,"Relax: %.4f\n",relax);
  fprintf(stdout,"Point Array Name: %s\n",pointarrayname);
  fprintf(stdout,"Cell Array Name: %s\n",cellarrayname);
  try {
    vtkNew(vtkLocalSmoothPolyDataFilter,smoother);
    smoother->SetInputData(geom);
    if (pointarrayname != 0) 
    {
      smoother->SetSmoothPointArrayName(pointarrayname);
      smoother->UsePointArrayOn();
    }
    if (cellarrayname != 0) 
    {
      smoother->SetSmoothCellArrayName(cellarrayname);
      smoother->UseCellArrayOn();
    }
    smoother->SetNumberOfIterations(numiters);
    smoother->SetRelaxationFactor(relax);
    smoother->Update();

    vtkNew(vtkPolyDataNormals,normaler);
    normaler->SetInputData(smoother->GetOutput());
    normaler->SplittingOff();
    normaler->Update();

    result = new cvPolyData( normaler->GetOutput());
    *outpd = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in local smoothing.\n");
    fflush(stderr);
    return CV_ERROR;
  }

  return CV_OK;
}

/* -------------- */
/* sys_geom_local_constrain_smooth */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  
 *  @brief Function to perform a smoothing operation on only a portion 
 *  of a polydata
 *  @param *pd The input polydata on which to perform decimation 
 *  @param **outpd The output polydata surface
 *  @param numiters number of iterations of smoothing to perform
 *  @param constrainfactor Extent to which operation attempts to push surface
 *  back to original surface. 1.0 attempts to push it back 100%, 0.0
 *  attempts to push it back 0%.
 *  @param *pointarrayname This contains the arrayname holding the boolean
 *  values indication with points to smooth
 *  @param *cellarrayname This contains the arrayname holding the boolean
 *  values indication with cells to smooth
 *  @return CV_OK if the function executes properly
 */

int sys_geom_local_constrain_smooth( cvPolyData *pd,cvPolyData **outpd, int numiters,
		double constrainfactor,int numcgsolves, char *pointarrayname, char *cellarrayname)
{
  vtkPolyData *geom = pd->GetVtkPolyData();
  cvPolyData *result = NULL;

  fprintf(stdout,"Running local smoothing\n");
  fprintf(stdout,"Num Iters: %d\n",numiters);
  fprintf(stdout,"Constrain: %.4f\n",constrainfactor);
  fprintf(stdout,"Point Array Name: %s\n",pointarrayname);
  fprintf(stdout,"Cell Array Name: %s\n",cellarrayname);
  try {
    vtkNew(vtkCGSmooth,smoother);
    smoother->SetInputData(geom);
    if (pointarrayname != 0) 
    {
      smoother->SetPointArrayName(pointarrayname);
      smoother->UsePointArrayOn();
    }
    if (cellarrayname != 0) 
    {
      smoother->SetCellArrayName(cellarrayname);
      smoother->UseCellArrayOn();
    }
    smoother->SetNumGradientSolves(numcgsolves);
    smoother->SetNumSmoothOperations(numiters);
    smoother->SetWeight(constrainfactor);
    smoother->Update();

    vtkNew(vtkPolyDataNormals,normaler);
    normaler->SetInputData(smoother->GetOutput());
    normaler->SplittingOff();
    normaler->Update();

    result = new cvPolyData( normaler->GetOutput());
    *outpd = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in local smoothing.\n");
    fflush(stderr);
    return CV_ERROR;
  }

  return CV_OK;
}

/* -------------- */
/* sys_geom_local_subdivision */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  
 *  @brief Function to perform a subdivision operation on only a portion 
 *  of a polydata
 *  @param *pd The input polydata on which to perform decimation 
 *  @param **outpd The output polydata surface
 *  @param numiters number of iterations of subdivision to perform
 *  @param *pointarrayname This contains the arrayname holding the boolean
 *  values indication with points to subdivide
 *  @param *cellarrayname This contains the arrayname holding the boolean
 *  values indication with cells to subdivide
 *  @return CV_OK if the function executes properly
 */

int sys_geom_local_subdivision( cvPolyData *pd,cvPolyData **outpd, int numiters,
		char *pointarrayname, char *cellarrayname)
{
  vtkPolyData *geom = pd->GetVtkPolyData();
  cvPolyData *result = NULL;

  fprintf(stdout,"Running local subdivision\n");
  fprintf(stdout,"Num Iters: %d\n",numiters);
  fprintf(stdout,"Point Array Name: %s\n",pointarrayname);
  fprintf(stdout,"Cell Array Name: %s\n",cellarrayname);
  try {
    vtkNew(vtkLocalLinearSubdivisionFilter,subdivider);
    subdivider->SetInputData(geom);
    if (pointarrayname != 0) 
    {
      subdivider->SetSubdividePointArrayName(pointarrayname);
      subdivider->UsePointArrayOn();
    }
    if (cellarrayname != 0) 
    {
      subdivider->SetSubdivideCellArrayName(cellarrayname);
      subdivider->UseCellArrayOn();
    }
    subdivider->SetNumberOfSubdivisions(numiters);
    subdivider->Update();

    result = new cvPolyData( subdivider->GetOutput());
    *outpd = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in local subdivision.\n");
    fflush(stderr);
    return CV_ERROR;
  }

  return CV_OK;
}

#ifdef USE_VMTK

/* -------------- */
/* sys_geom_centerlines */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  
 *  @brief Function to extract centerlines from a vtkPolyData surface
 *  @brief VTMK is called to do this. Each cap on PolyData has an 
 *  @brief id and then each id is set as either inlet or outlet. 
 *  @param *sources list of source cap ids
 *  @param nsources number of source cap ids
 *  @param *targets list of target cap ids
 *  @param ntargets number of target cap ids
 *  @param **lines returned center lines as vtkPolyData
 *  @param ** voronoi returned voronoi diagram as vtkPolyData
 *  @return CV_OK if the VTMK function executes properly
 */

int sys_geom_centerlines( cvPolyData *polydata,int *sources,int nsources,
		int *targets,int ntargets,
		cvPolyData **lines, cvPolyData **voronoi)
{
  vtkPolyData *geom = polydata->GetVtkPolyData();
  cvPolyData *result1 = NULL;
  cvPolyData *result2 = NULL;
  *lines = NULL;
  *voronoi = NULL;

//  vtkSmartPointer<vtkvmtkPolyDataCenterlines> centerLiner = 
//    vtkSmartPointer<vtkvmtkPolyDataCenterlines>::New();
  vtkSmartPointer<vtkIdList> capInletIds = vtkSmartPointer<vtkIdList>::New();
  vtkSmartPointer<vtkIdList> capOutletIds = vtkSmartPointer<vtkIdList>::New();
  int pointId;

  fprintf(stderr,"NumSources %d\n",nsources);
  fprintf(stderr,"NumTargets %d\n",ntargets);
  for (pointId = 0;pointId<nsources;pointId++)
  {
    capInletIds->InsertNextId(*sources+pointId);
  }
  for (pointId = 0;pointId<ntargets;pointId++)
  {
    capOutletIds->InsertNextId(*targets+pointId);
  }

  vtkNew(vtkvmtkPolyDataCenterlines,centerLiner);
  try {
    std::cout<<"Getting Center Lines..."<<endl;
    centerLiner->SetInputData(geom);
    centerLiner->SetSourceSeedIds(capInletIds);
    centerLiner->SetTargetSeedIds(capOutletIds);
    centerLiner->SetRadiusArrayName("MaximumInscribedSphereRadius");
    centerLiner->SetCostFunction("1/R");
    centerLiner->SetFlipNormals(0);
    centerLiner->SetAppendEndPointsToCenterlines(1);
    centerLiner->SetSimplifyVoronoi(0);
    centerLiner->SetCenterlineResampling(0);
    centerLiner->SetResamplingStepLength(1);
    centerLiner->Update();

    result1 = new cvPolyData( centerLiner->GetOutput() );
    result2 = new cvPolyData( centerLiner->GetVoronoiDiagram() ); 
    *lines = result1;
    *voronoi = result2;
  }
  catch (...) {
    fprintf(stderr,"ERROR in centerline operation.\n");
    fflush(stderr);
    return CV_ERROR;
  }
  return CV_OK;
}

/* -------------- */
/* sys_geom_distancetocenterlines */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  
 *  @brief Function to calculate distance to surface from points on line
 *  @param *polydata The polydata to find distance form lines on
 *  @param *lines from centerline extraction, the centerlines to be used
 *  @param **distance vtkPolyData with distance functiona attached returned
 *  @return CV_OK if the VTMK function executes properly
 */

int sys_geom_distancetocenterlines( cvPolyData *polydata,cvPolyData *lines, 
		cvPolyData **distance)
{
  vtkPolyData *geom = polydata->GetVtkPolyData();
  vtkPolyData *centerlines = lines->GetVtkPolyData();
  cvPolyData *result = NULL;
  *distance = NULL;

  vtkNew(vtkvmtkPolyDataDistanceToCenterlines,distancer);
  try {
    std::cout<<"Getting Distance to Center Lines..."<<endl;
    distancer->SetInputData(geom);
    distancer->SetCenterlines(centerlines);
    distancer->SetUseRadiusInformation(1);
    distancer->SetEvaluateTubeFunction(0);
    distancer->SetEvaluateCenterlineRadius(0);
    distancer->SetProjectPointArrays(0);
    distancer->SetDistanceToCenterlinesArrayName("DistanceToCenterlines");
    distancer->SetCenterlineRadiusArrayName("MaximumInscribedSphereRadius");
    distancer->Update();
    
    result = new cvPolyData( distancer->GetOutput());
    *distance = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in centerline operation.\n");
    fflush(stderr);
    return CV_ERROR;
  }
  return CV_OK;
}

/* -------------- */
/* sys_geom_cap */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  
 *  @brief Function that calls VMTK method to cap which assigns ids t caps
 *  @param *polydata The polydata to cap
 *  @param *cappedpolydata the capped polydata is returned in this
 *  @param *numcenterids Number of caps or ids for the center of caps
 *  @param **centerids If type is 1, a cap with a centerpoint is added.
 *  Then this centerpoint id is return in this object
 *  @param type This determines whether to cap regularly or cap with a point
 *  in the center. 0 - regular, 1 - point in center
 *  @return CV_OK if the VTMK function executes properly
 */

int sys_geom_cap( cvPolyData *polydata,cvPolyData **cappedpolydata,int *numcenterids,int **centerids,int type)
{
  vtkPolyData *geom = polydata->GetVtkPolyData();
  cvPolyData *result = NULL;
  *cappedpolydata = NULL;
  vtkSmartPointer<vtkIdList> capCenterIds = vtkSmartPointer<vtkIdList>::New();
  vtkSmartPointer<vtkTriangleFilter> triangulate = 
	  vtkSmartPointer<vtkTriangleFilter>::New();
  int numids;
  int *allids;
  int i;
              
  try {
    if (type ==0)
    {
      vtkSmartPointer<vtkvmtkSimpleCapPolyData> capper = 
	      vtkSmartPointer<vtkvmtkSimpleCapPolyData>::New();
      capper->SetInputData(geom);
      capper->SetCellEntityIdsArrayName("CenterlineCapID");
      capper->SetCellEntityIdOffset(1);
      capper->Update();
      triangulate->SetInputData(capper->GetOutput());
      triangulate->Update();

      result = new cvPolyData( triangulate->GetOutput() );
      *cappedpolydata = result;
      capCenterIds->InsertNextId(0);
    }
    else if (type == 1)
    {
      vtkSmartPointer<vtkvmtkCapPolyData> capper = 
	      vtkSmartPointer<vtkvmtkCapPolyData>::New();
      capper->SetInputData(geom);
      capper->SetDisplacement(0);
      capper->SetInPlaneDisplacement(0);
      capper->Update();
      triangulate->SetInputData(capper->GetOutput());
      triangulate->Update();

      result = new cvPolyData( triangulate->GetOutput() );
      *cappedpolydata = result;
      capCenterIds->DeepCopy(capper->GetCapCenterIds());
  
    }

  }
  catch (...) {
    fprintf(stderr,"ERROR in capping operation.\n");
    fflush(stderr);
    return CV_ERROR;
  }

  numids = capCenterIds->GetNumberOfIds();
  allids = new int[numids];

  for (i=0;i<numids;i++)
  {
    allids[i] = capCenterIds->GetId(i);
  }
  *numcenterids = numids;
  *centerids = allids;

  return CV_OK;
}

/* -------------- */
/* sys_geom_cap_with_ids */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  
 *  @brief Function that calls customized vtk filter to cap with ids starting
 *  from the lowest number in the current "ModelFaceID" array 
 *  @param *polydata The polydata to cap
 *  @param fillid the id number to use for capping. Depends on filltype
 *  @param filledholes the number of holes filled in process
 *  @param filltype How to assign ids to caps on polydata. 0 - start from 1
 *  and each cap gets a new id. 1 - give every cap the same id (fillid).
 *  2 - start from fillid and give each new cap a value increasing from this
 *  @param type This determines whether to cap regularly or cap with a point
 *  in the center. 0 - regular, 1 - point in center
 *  @return CV_OK if the VTMK function executes properly
 */

int sys_geom_cap_with_ids( cvPolyData *polydata,cvPolyData **cappedpolydata,
    int fillId,int filledholes,int filltype)
{

  vtkPolyData *geom = polydata->GetVtkPolyData();
  cvPolyData *result = NULL;
  *cappedpolydata = NULL;
              
  try {

    std::cout<<"Capping Surface..."<<endl;
    vtkSmartPointer<vtkFillHolesFilterWithIds> capper = 
	    vtkSmartPointer<vtkFillHolesFilterWithIds>::New();
    capper->SetInputData(geom);
    capper->SetFillId(fillId);
    //Fill type, 0 for number of holes filled, 1 for a fillid, and 2 for 
    //increasing number id starting at given fillid number+1
    capper->SetFillType(filltype);
    capper->SetHoleSize(capper->GetHoleSizeMaxValue());
    capper->Update();

    filledholes = capper->GetNumberOfHolesFilled(); 
    vtkNew(vtkPolyData,capout);
    capout->DeepCopy(capper->GetOutput());

    if (filltype ==2)
    {
      vtkNew(vtkIntArray, currentCapArray);
      vtkNew(vtkIntArray, currentFaceArray);
      vtkNew(vtkIntArray, newFaceArray);
      if (PlyDtaUtils_PDCheckArrayName(capout,1,"CapID") != CV_OK)
      {
	fprintf(stderr,"CapID Array is not on the surface\n");
	return CV_ERROR;
      }
      if (PlyDtaUtils_PDCheckArrayName(geom,1,"ModelFaceID") != CV_OK)
      {
	fprintf(stderr,"ModelFaceID Array is not on the surface\n");
	return CV_ERROR;
      }
      currentCapArray = vtkIntArray::SafeDownCast(capout->GetCellData()->
	  GetArray("CapID"));
      currentFaceArray = vtkIntArray::SafeDownCast(geom->GetCellData()->
	  GetArray("ModelFaceID"));
      for (int i=0;i<capout->GetNumberOfCells();i++)
      {
	if (currentCapArray->GetValue(i) == -1)
	  newFaceArray->InsertValue(i,currentFaceArray->GetValue(i));
	else
	  newFaceArray->InsertValue(i,currentCapArray->GetValue(i));
      }
      newFaceArray->SetName("ModelFaceID");
      capout->GetCellData()->AddArray(newFaceArray);
      capout->GetCellData()->SetActiveScalars("ModelFaceID");
    }
    result = new cvPolyData(capout);
    *cappedpolydata = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in capping operation.\n");
    fflush(stderr);
    return CV_ERROR;
  }

  return CV_OK;
}

/* -------------- */
/* sys_geom_mapandcorrectids */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  
 *  @brief Function to map and correct ids after a surface has been thresholded
 *  then capped.
 *  @param *polydata The polydata map and correct ids on
 *  @param *originalpd The original polydata with correct ids
 *  @param *newpd The new polydata to correct the ids on
 *  @param **polydata The returned polydata with everything all set
 *  @param *originalarray original id array name on the surface to correct
 *  @param *newarray new id array name to give to the corrected ids
 *  @return CV_OK if the VTMK function executes properly
 */

int sys_geom_mapandcorrectids( cvPolyData *originalpd, cvPolyData *newpd, cvPolyData **polydata,char *originalarray, char *newarray)
{
  vtkPolyData *originalgeom = originalpd->GetVtkPolyData();
  vtkPolyData *newgeom = newpd->GetVtkPolyData();
  cvPolyData *result = NULL;
  *polydata = NULL;

  int i,j,k; 
  int subId;    
  int count;
  vtkIdType npts;
  vtkIdType *pts;
  double distance;
  double closestPt[3];
  double minmax[2];
  double centroid[3];
  long range; 
  vtkIdType closestCell;
  vtkIdType cellId;
  vtkIdType currentValue;
  vtkPolyData *newcopy = vtkPolyData::New();
  vtkSmartPointer<vtkCellLocator> locator = 
    vtkSmartPointer<vtkCellLocator>::New();
  vtkSmartPointer<vtkPointLocator> pointLocator = 
    vtkSmartPointer<vtkPointLocator>::New();
  vtkSmartPointer<vtkGenericCell> genericCell =
    vtkSmartPointer<vtkGenericCell>::New();
  vtkSmartPointer<vtkLongArray> currentRegionsLong = 
    vtkSmartPointer<vtkLongArray>::New();
  vtkSmartPointer<vtkIntArray> currentRegionsInt = 
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> realRegions = 
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIdList> closestCells = 
    vtkSmartPointer<vtkIdList>::New();

  newcopy->DeepCopy(newgeom);
  newcopy->BuildLinks();
  originalgeom->BuildLinks();
  locator->SetDataSet(originalgeom);
  locator->BuildLocator();
  pointLocator->SetDataSet(originalgeom);
  pointLocator->BuildLocator();

  currentRegionsLong = static_cast<vtkLongArray*>(newcopy->GetCellData()->GetScalars(newarray));
  currentRegionsLong->GetRange(minmax,0);

  realRegions = static_cast<vtkIntArray*>(originalgeom->GetCellData()->GetScalars(originalarray));
//  realRegions = vtkIntArray::SafeDownCast(originalgeom->GetCellData()->GetScalars(originalarray));

  range = minmax[1]-minmax[0];
  long *mapper;
  mapper = new long[1+range];
  
  for (i=0;i<range+1;i++)
  {
    mapper[i] = -1;
  }

  for (cellId=0;cellId<newcopy->GetNumberOfCells();cellId++)
  {
    currentValue = currentRegionsLong->GetValue(cellId);
    
    if (mapper[currentValue-1] == -1)
    {
      fprintf(stderr,"Getting Value: %d\n",currentValue-1);
      newcopy->GetCellPoints(cellId,npts,pts);

      vtkSmartPointer<vtkPoints> polyPts = vtkSmartPointer<vtkPoints>::New();
      vtkSmartPointer<vtkIdTypeArray> polyPtIds = vtkSmartPointer<vtkIdTypeArray>::New();
      for (i=0;i<npts;i++)
      {
	polyPtIds->InsertValue(i,i);
	polyPts->InsertNextPoint(newcopy->GetPoint(pts[i]));
      }

      vtkPolygon::ComputeCentroid(polyPtIds,polyPts,centroid);

      locator->FindClosestPoint(centroid,closestPt,genericCell,closestCell,
	  subId,distance);

      fprintf(stderr,"the value: %d\n",realRegions->GetValue(closestCell));
      mapper[currentValue-1] = realRegions->GetValue(closestCell);
    }
  }

  for (i=0;i<range+1;i++)
  {
    fprintf(stderr,"Want to see mapper vals: %d is %d\n",i,mapper[i]);
  }

  //Set original region values 
  for (cellId=0;cellId<newcopy->GetNumberOfCells();cellId++)
  {
    currentValue = static_cast<int>(currentRegionsLong->GetValue(cellId));
    currentRegionsInt->InsertValue(cellId,mapper[currentValue-1]);
  }

  newcopy->GetCellData()->RemoveArray(newarray);
  
  currentRegionsInt->SetName(originalarray);
  newcopy->GetCellData()->AddArray(currentRegionsInt);
  newcopy->GetCellData()->SetActiveScalars(originalarray);

  result = new cvPolyData( newcopy );
  *polydata = result;

  delete [] mapper;
  return CV_OK;
}

/* -------------- */
/* sys_geom_set_ids_for_caps */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  
 *  @brief Function to set ids in order to retain face names from Boolean 
 *  operation. Lots of sneaky tricks here
 *  @param *pd The polydata to set the ids on 
 *  @param **outpd The polydata with the full ModelFaceIDs on it
 *  @param **doublecaps This returns the faces that have two caps on it
 *  @param *numfaces This is the number of total faces on the object
 *  @return CV_OK if the VTMK function executes properly
 */

int sys_geom_set_ids_for_caps( cvPolyData *pd,cvPolyData **outpd,int **doublecaps,
		int *numfaces)
{
  vtkPolyData *geom = pd->GetVtkPolyData();
  cvPolyData *result = NULL;
  *outpd = NULL;

  int *capone;
  int *captwo;
  int facemax=0,capmax=0;
  double facerange[2],caprange[2];
  vtkNew(vtkIntArray,capids);
  vtkNew(vtkIntArray,faceids);
  if (PlyDtaUtils_PDCheckArrayName(geom,1,"CapID") != CV_OK)
  {
    fprintf(stderr,"First\n");
    fprintf(stderr,"CapID Array is not on the surface\n");
    return CV_ERROR;
  }
  if (PlyDtaUtils_PDCheckArrayName(geom,1,"ModelFaceID") != CV_OK)
  {
    fprintf(stderr,"ModelFaceID Array is not on the surface\n");
    return CV_ERROR;
  }

  capids = vtkIntArray::SafeDownCast(geom->GetCellData()->
      GetArray("CapID"));
  faceids = vtkIntArray::SafeDownCast(geom->GetCellData()->
      GetArray("ModelFaceID"));

  faceids->GetRange(facerange,0);
  facemax = facerange[1];
  capids->GetRange(caprange,0);
  capmax = caprange[1];

  capone = new int[facemax];
  captwo = new int[facemax];
  *doublecaps = new int[facemax];
  *numfaces = facemax;

  int numtwocaps=0;
  for (int i = 0; i < facemax; i++)
  {
    double facecaprange[2];
    vtkNew(vtkThreshold,threshold1); 
    threshold1->SetInputData(geom);
    threshold1->SetInputArrayToProcess(0,0,0,1,"ModelFaceID");
    threshold1->ThresholdBetween(i+1,i+1);
    threshold1->Update();

    vtkNew(vtkDataSetSurfaceFilter,surfacer);
    surfacer->SetInputData(threshold1->GetOutput());
    surfacer->Update();

    vtkNew(vtkIntArray,modelfacecaps);

    if (PlyDtaUtils_PDCheckArrayName(surfacer->GetOutput(),1,"CapID") != CV_OK)
    {
      fprintf(stderr,"Second\n");
      fprintf(stderr,"CapID Array is not on the surface\n");
      delete [] capone;
      delete [] captwo;
      return CV_ERROR;
    }
    modelfacecaps = vtkIntArray::SafeDownCast(surfacer->GetOutput()->
	GetCellData()->GetArray("CapID"));
    capone[i] = 0;
    captwo[i] = 0;
    (*doublecaps)[i] = 0;
    for (int j = 0; j < surfacer->GetOutput()->GetNumberOfCells();j++)
    {
      if (modelfacecaps->GetValue(j) == 1)
        capone[i] = 1;
      if (modelfacecaps->GetValue(j) == 2)
        captwo[i] = 1;
    }
    if (capone[i] && captwo[i])
    {
      numtwocaps++;
      (*doublecaps)[i] = numtwocaps;
    }
  }
  //New face ids are a function of the ModelFaceID, the CapID, whether or 
  //not the face has two caps assigned to it and total number of faces
  for (int i = 0; i < geom->GetNumberOfCells(); i++)
  {
    if (capids->GetValue(i) != -1)
    {
      int capval = capids->GetValue(i);
      int faceval = faceids->GetValue(i); 
      if ((*doublecaps)[faceval-1] != 0)
      {
	if (capval == 1)
	  faceids->SetValue(i,faceval+capval+facemax-1);
	else
	  faceids->SetValue(i,facemax*2+(*doublecaps)[faceval-1]);
      }
      else
	faceids->SetValue(i,faceval+capval+facemax-captwo[faceval-1]-1);
    }
  }

  geom->GetCellData()->RemoveArray("ModelFaceID");
  geom->GetCellData()->AddArray(faceids);
  geom->GetCellData()->SetActiveScalars("ModelFaceID");

  result = new cvPolyData(geom);
  *outpd = result;

  delete [] capone;
  delete [] captwo;
  return CV_OK;
}
#endif
