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
#include <string.h>
#include "cvRepository.h"
#include "cvRepositoryData.h"
#include "cvSolidModel.h"
#include "cvPolyData.h"
#include "cv_geom_init.h"
#include "cv_sys_geom.h"
#include "cv_integrate_surface.h"
#include "cv_arg.h"
#include "cv_misc_utils.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "cv_globals.h"


// Prototypes:
// -----------

int Geom_ReduceCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] );

int Geom_MakePolysConsistentCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

int Geom_ReverseAllCellsCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] );

int Geom_NumClosedLineRegionsCmd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] );

int Geom_GetClosedLineRegionCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

int Geom_PickCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] );

int Geom_OrientProfileCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int Geom_DisorientProfileCmd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] );

int Geom_AlignProfileCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] );

int Geom_TranslateCmd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] );

int Geom_ScaleAvgCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] );

int Geom_GetOrderedPtsCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int Geom_WriteOrderedPtsCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] );

int Geom_WriteLinesCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

int Geom_PolysClosedCmd( ClientData clientData, Tcl_Interp *interp,
			 int argc, CONST84 char *argv[] );

int Geom_SurfAreaCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] );

int Geom_GetPolyCentroidCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] );

int Geom_PrintTriStatsCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int Geom_PrintSmallPolysCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] );

int Geom_RmSmallPolysCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] );

int Geom_BBoxCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] );

int Geom_ClassifyCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] );

int Geom_PtInPolyCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] );

int Geom_MergePtsCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] );

int Geom_Warp3dPtsCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] );

int Geom_NumPtsCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] );

int Geom_sampleLoopCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int Geom_loftSolidCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int Geom_2dWindingNumCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] );

int Geom_PolygonNormCmd( ClientData clientData, Tcl_Interp *interp,
			 int argc, CONST84 char *argv[] );

int Geom_AvgPtCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

int Geom_CopyCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] );

int Geom_ProjectCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] );

int Geom_ReorderPgnCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

int Geom_SplinePtsToPathPlanCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

int Geom_IntegrateSurfaceCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

int Geom_IntegrateSurface2Cmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

int Geom_IntegrateEnergyCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

int Geom_FindDistanceCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] );

int Geom_InterpolateScalarCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] );

int Geom_InterpolateVectorCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] );

int Geom_IntersectWithLineCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_AddPointDataCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_SubtractPointDataCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_MultiplyPointDataCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_DividePointDataCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_IntegrateScalarSurfCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_IntegrateScalarThreshCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_ReplacePointDataCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_UnionCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_IntersectCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_SubtractCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_CheckSurfaceCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_CapIdSetCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_LocalDecimationCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_LocalLaplacianSmoothCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_LocalConstrainSmoothCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_LocalSubdivisionCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_SetArrayForLocalOp_SphereCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_SetArrayForLocalOp_FaceCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_SetArrayForLocalOp_CellsCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_SetArrayForLocalOp_BlendCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );
#ifdef USE_GTS
int Geom_Union_GTSCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_Intersect_GTSCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_Subtract_GTSCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );
#endif

#ifdef USE_VMTK
int Geom_CenterlinesCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int Geom_DistanceToCenterlinesCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int Geom_CapCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int Geom_CapWIdsCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int Geom_MapAndCorrectIdsCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );
#endif

// Helper functions
// ----------------


// ---------
// Geom_Init
// ---------

int Geom_Init( Tcl_Interp *interp )
{
  Tcl_CreateCommand( interp, "geom_reduce", Geom_ReduceCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_makePolysConsistent",
		     Geom_MakePolysConsistentCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_reverseAllCells",
		     Geom_ReverseAllCellsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_numClosedLineRegions",
		     Geom_NumClosedLineRegionsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_getClosedLineRegion",
		     Geom_GetClosedLineRegionCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_pick", Geom_PickCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_orientProfile", Geom_OrientProfileCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_disorientProfile", Geom_DisorientProfileCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_alignProfile", Geom_AlignProfileCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_translate", Geom_TranslateCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_scaleAvg", Geom_ScaleAvgCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_getOrderedPts", Geom_GetOrderedPtsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_writeOrderedPts",
		     Geom_WriteOrderedPtsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_writeLines", Geom_WriteLinesCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_polysClosed", Geom_PolysClosedCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_surfArea", Geom_SurfAreaCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_getPolyCentroid", Geom_GetPolyCentroidCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_printTriStats", Geom_PrintTriStatsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_printSmallPolys", Geom_PrintSmallPolysCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_rmSmallPolys", Geom_RmSmallPolysCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_bbox", Geom_BBoxCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_classify", Geom_ClassifyCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_ptInPoly", Geom_PtInPolyCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_mergePts", Geom_MergePtsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_warp3dPts", Geom_Warp3dPtsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_numPts", Geom_NumPtsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_sampleLoop", Geom_sampleLoopCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_loftSolid", Geom_loftSolidCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_2dWindingNum", Geom_2dWindingNumCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_polygonNorm", Geom_PolygonNormCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_avgPt", Geom_AvgPtCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_copy", Geom_CopyCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_project", Geom_ProjectCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_reorderPgn", Geom_ReorderPgnCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_splinePtsToPathPlan", Geom_SplinePtsToPathPlanCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_integrateSurfaceFlux", Geom_IntegrateSurfaceCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_integrateSurface2", Geom_IntegrateSurface2Cmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_integrateEnergy", Geom_IntegrateEnergyCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_findDistance", Geom_FindDistanceCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_interpolateScalar", Geom_InterpolateScalarCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_interpolateVector", Geom_InterpolateVectorCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_intersectWithLine", Geom_IntersectWithLineCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_addPointData", Geom_AddPointDataCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_subtractPointData", Geom_SubtractPointDataCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_multiplyPointData", Geom_MultiplyPointDataCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_dividePointData", Geom_DividePointDataCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_integrateScalarSurf", Geom_IntegrateScalarSurfCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_integrateScalarThresh", Geom_IntegrateScalarThreshCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_replacePointData", Geom_ReplacePointDataCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_union", Geom_UnionCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_intersect", Geom_IntersectCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_subtract", Geom_SubtractCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_checksurface", Geom_CheckSurfaceCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_set_ids_for_caps", Geom_CapIdSetCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_local_decimation", Geom_LocalDecimationCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_local_laplacian_smooth", Geom_LocalLaplacianSmoothCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_local_constrain_smooth", Geom_LocalConstrainSmoothCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_local_subdivision", Geom_LocalSubdivisionCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_set_array_for_local_op_sphere", Geom_SetArrayForLocalOp_SphereCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_set_array_for_local_op_face", Geom_SetArrayForLocalOp_FaceCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_set_array_for_local_op_cells", Geom_SetArrayForLocalOp_CellsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_set_array_for_local_op_face_blend", Geom_SetArrayForLocalOp_BlendCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
#ifdef USE_GTS 
  Tcl_CreateCommand( interp, "geom_union_gts", Geom_Union_GTSCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_intersect_gts", Geom_Intersect_GTSCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_subtract_gts", Geom_Subtract_GTSCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
#endif
#ifdef USE_VMTK 
  Tcl_CreateCommand( interp, "geom_centerlines", Geom_CenterlinesCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_distancetocenterlines", Geom_DistanceToCenterlinesCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_cap", Geom_CapCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_cap_with_ids", Geom_CapWIdsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_mapandcorrectids", Geom_MapAndCorrectIdsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
#endif
  return TCL_OK;
}


// --------------
// Geom_ReduceCmd
// --------------

int Geom_ReduceCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;
  double tol;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-tol", DOUBLE_Type, &tol, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_Reduce( (cvPolyData*)src, tol, (cvPolyData**)(&dst) ) != CV_OK ) {
    Tcl_SetResult( interp, "point merging error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}

// -------------
// Geom_UnionCmd
// -------------

int Geom_UnionCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *aName;
  char *bName;
  char *dstName;
  cvRepositoryData *srcA;
  cvRepositoryData *srcB;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;
  double tolerance = 1e-6;

  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-a", STRING_Type, &aName, NULL, REQUIRED, 0, { 0 } },
    { "-b", STRING_Type, &bName, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-tolerance", DOUBLE_Type, &tolerance, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( aName );
  if ( srcA == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", aName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  srcB = gRepository->GetObject( bName );
  if ( srcB == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", bName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, aName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, bName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_union( (cvPolyData*)srcA, (cvPolyData*)srcB, tolerance, (cvPolyData**)(&dst) )
       != CV_OK ) {
    Tcl_SetResult( interp, "poly manipulation error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


// -----------------
// Geom_IntersectCmd
// -----------------

int Geom_IntersectCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *aName;
  char *bName;
  char *dstName;
  cvRepositoryData *srcA;
  cvRepositoryData *srcB;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;
  double tolerance = 1e-6;

  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-a", STRING_Type, &aName, NULL, REQUIRED, 0, { 0 } },
    { "-b", STRING_Type, &bName, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-tolerance", DOUBLE_Type, &tolerance, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( aName );
  if ( srcA == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", aName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  srcB = gRepository->GetObject( bName );
  if ( srcB == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", bName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, aName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, bName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_intersect( (cvPolyData*)srcA, (cvPolyData*)srcB, tolerance,(cvPolyData**)(&dst) )
       != CV_OK ) {
    Tcl_SetResult( interp, "poly manipulation error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


// ----------------
// Geom_SubtractCmd
// ----------------

int Geom_SubtractCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *aName;
  char *bName;
  char *dstName;
  cvRepositoryData *srcA;
  cvRepositoryData *srcB;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;
  double tolerance = 1e-6;

  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-a", STRING_Type, &aName, NULL, REQUIRED, 0, { 0 } },
    { "-b", STRING_Type, &bName, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-tolerance", DOUBLE_Type, &tolerance, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( aName );
  if ( srcA == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", aName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  srcB = gRepository->GetObject( bName );
  if ( srcB == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", bName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, aName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, bName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_subtract( (cvPolyData*)srcA, (cvPolyData*)srcB, tolerance, (cvPolyData**)(&dst) )
       != CV_OK ) {
    Tcl_SetResult( interp, "poly manipulation error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}

// ----------------
// Geom_CheckSurfaceCmd
// ----------------

int Geom_CheckSurfaceCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *Name;
  cvRepositoryData *src;
  RepositoryDataT type;
  double tol = 1e-6;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &Name, NULL, REQUIRED, 0, { 0 } },
    { "-tolerance", DOUBLE_Type, &tol, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", Name,
		      (char *)NULL );
    return TCL_ERROR;
  }

  int stats[2];
  if ( sys_geom_checksurface( (cvPolyData*)src, stats,tol)
       != CV_OK ) {
    Tcl_SetResult( interp, "error checking surface", TCL_STATIC );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d %d",stats[0],stats[1]);
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE);

  return TCL_OK;
}

// ----------------
// Geom_CapIdSetCmd
// ----------------

int Geom_CapIdSetCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *Name;
  char *dstName;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &Name, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", Name,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }
  int *doublecaps;
  int numfaces=0;
  
  if ( sys_geom_set_ids_for_caps( (cvPolyData*)src, (cvPolyData**)(&dst),
			  &doublecaps,&numfaces)
       != CV_OK ) {
    Tcl_SetResult( interp, "error setting cap ids", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete [] doublecaps;
    delete dst;
    return TCL_ERROR;
  }

  char rtnstr[CV_STRLEN];
  for (int i = 0; i < numfaces; i++ ) {
    sprintf( rtnstr, "%d",doublecaps[i]);
    Tcl_AppendElement( interp, rtnstr );
  }

  delete [] doublecaps;
  return TCL_OK;
}

// ----------------
// Geom_SetArrayForLocalOp_FaceCmd
// ----------------

int Geom_SetArrayForLocalOp_FaceCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *Name;
  char *dstName;
  char *arrayName = 0; 
  char *outArray = "LocalOpsArray";
  int dataType = 1;
  ARG_List values;

  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 6;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &Name, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-array", STRING_Type, &arrayName, NULL, REQUIRED, 0, { 0 } },
    { "-values", LIST_Type, &values, NULL, REQUIRED, 0, { 0 } },
    { "-outarray", STRING_Type, &outArray, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-datatype", INT_Type, &dataType, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", Name,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  if (values.argc == 0) {
      ARG_FreeListArgvs( table_size, arg_table);
      return CV_OK;
  }

  int nvals = 0;
  int *vals = new int[values.argc];

  if ( ARG_ParseTclListStatic( interp, values, INT_Type, vals, values.argc, &nvals )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  // Do work of command:
  ARG_FreeListArgvs( table_size, arg_table );

  if ( sys_geom_set_array_for_local_op_face( (cvPolyData*)src,(cvPolyData**)(&dst),arrayName,vals,nvals,outArray,dataType)
       != CV_OK ) {
    Tcl_SetResult( interp, "error creating array on surface", TCL_STATIC );
    delete [] vals;
    return TCL_ERROR;
  }

  delete [] vals;

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// Geom_SetArrayForLocalOp_SphereCmd
// ----------------

int Geom_SetArrayForLocalOp_SphereCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *Name;
  char *dstName;
  double radius;
  char *outArray = "LocalOpsArray";
  int dataType = 1;
  double ctr[3];
  int nctr;
  ARG_List ctrList;

  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 6;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &Name, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-radius", DOUBLE_Type, &radius, NULL, REQUIRED, 0, { 0 } },
    { "-center", LIST_Type, &ctrList, NULL, REQUIRED, 0, { 0 } },
    { "-outarray", STRING_Type, &outArray, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-datatype", INT_Type, &dataType, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", Name,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  if ( ARG_ParseTclListStatic( interp, ctrList, DOUBLE_Type, ctr, 3, &nctr )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  if ( nctr != 3 ) {
    Tcl_SetResult( interp, "sphere requires a 3D center coordinate",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  // Do work of command:
  ARG_FreeListArgvs( table_size, arg_table );

  if ( sys_geom_set_array_for_local_op_sphere( (cvPolyData*)src,(cvPolyData**)(&dst),radius,ctr,outArray,dataType)
       != CV_OK ) {
    Tcl_SetResult( interp, "error creating array on surface", TCL_STATIC );
    return TCL_ERROR;
  }

  vtkPolyData *geom = ((cvPolyData*)(dst))->GetVtkPolyData();
  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// Geom_SetArrayForLocalOp_CellsCmd
// ----------------

int Geom_SetArrayForLocalOp_CellsCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *Name;
  char *dstName;
  char *outArray = "LocalOpsArray";
  int dataType = 1;
  ARG_List values;

  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &Name, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-values", LIST_Type, &values, NULL, REQUIRED, 0, { 0 } },
    { "-outarray", STRING_Type, &outArray, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-datatype", INT_Type, &dataType, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", Name,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  if (values.argc == 0) {
      ARG_FreeListArgvs( table_size, arg_table);
      return CV_OK;
  }

  int nvals = 0;
  int *vals = new int[values.argc];

  if ( ARG_ParseTclListStatic( interp, values, INT_Type, vals, values.argc, &nvals )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  // Do work of command:
  ARG_FreeListArgvs( table_size, arg_table );

  if ( sys_geom_set_array_for_local_op_cells( (cvPolyData*)src,(cvPolyData**)(&dst),vals,nvals,outArray,dataType)
       != CV_OK ) {
    Tcl_SetResult( interp, "error creating array on surface", TCL_STATIC );
    delete [] vals;
    return TCL_ERROR;
  }

  delete [] vals;

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// Geom_SetArrayForLocalOp_BlendCmd
// ----------------

int Geom_SetArrayForLocalOp_BlendCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *Name;
  char *dstName;
  char *arrayName = 0; 
  char *outArray = "LocalOpsArray";
  int dataType = 1;
  double radius;
  ARG_List values;

  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 7;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &Name, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-array", STRING_Type, &arrayName, NULL, REQUIRED, 0, { 0 } },
    { "-values", LIST_Type, &values, NULL, REQUIRED, 0, { 0 } },
    { "-radius", DOUBLE_Type, &radius, NULL, REQUIRED, 0, { 0 } },
    { "-outarray", STRING_Type, &outArray, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-datatype", INT_Type, &dataType, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", Name,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  if (values.argc == 0) {
      ARG_FreeListArgvs( table_size, arg_table);
      return CV_OK;
  }

  int nvals = 0;
  int *vals = new int[values.argc];

  if ( ARG_ParseTclListStatic( interp, values, INT_Type, vals, values.argc, &nvals )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  // Do work of command:
  ARG_FreeListArgvs( table_size, arg_table );

  if ( sys_geom_set_array_for_local_op_face_blend( (cvPolyData*)src,(cvPolyData**)(&dst),arrayName,vals,nvals,radius,outArray,dataType)
       != CV_OK ) {
    Tcl_SetResult( interp, "error creating array on surface", TCL_STATIC );
    delete [] vals;
    return TCL_ERROR;
  }

  delete [] vals;

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// Geom_LocalDecimationCmd
// ----------------

int Geom_LocalDecimationCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *Name;
  char *dstName;
  char *pointArrayName = 0; 
  char *cellArrayName = 0;
  double target = 0.25;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &Name, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-target", DOUBLE_Type, &target, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-pointarray", STRING_Type, &pointArrayName, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-cellarray", STRING_Type, &cellArrayName, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", Name,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }
  if ( sys_geom_local_quadric_decimation( (cvPolyData*)src, (cvPolyData**)(&dst),target,
			  pointArrayName,cellArrayName)
       != CV_OK ) {
    Tcl_SetResult( interp, "running local decimation operation", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// Geom_LocalLaplacianSmoothCmd
// ----------------

int Geom_LocalLaplacianSmoothCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *Name;
  char *dstName;
  char *pointArrayName = 0; 
  char *cellArrayName = 0;
  int numiters = 100;
  double relax = 0.01;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 6;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &Name, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-numiters", INT_Type, &numiters, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-relax", DOUBLE_Type, &relax, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-pointarray", STRING_Type, &pointArrayName, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-cellarray", STRING_Type, &cellArrayName, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", Name,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }
  if ( sys_geom_local_laplacian_smooth( (cvPolyData*)src, (cvPolyData**)(&dst),
			  numiters,relax,
			  pointArrayName,cellArrayName)
       != CV_OK ) {
    Tcl_SetResult( interp, "running local decimation operation", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// Geom_LocalConstrainSmoothCmd
// ----------------

int Geom_LocalConstrainSmoothCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *Name;
  char *dstName;
  char *pointArrayName = 0; 
  char *cellArrayName = 0;
  int numiters = 5;
  double constrainfactor = 0.7;
  int numcgsolves = 30;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 7;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &Name, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-numiters", INT_Type, &numiters, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-constrainfactor", DOUBLE_Type, &constrainfactor, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-numcgsolves", INT_Type, &numcgsolves, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-pointarray", STRING_Type, &pointArrayName, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-cellarray", STRING_Type, &cellArrayName, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", Name,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }
  if ( sys_geom_local_constrain_smooth( (cvPolyData*)src, (cvPolyData**)(&dst),
			  numiters,constrainfactor,numcgsolves,
			  pointArrayName,cellArrayName)
       != CV_OK ) {
    Tcl_SetResult( interp, "running local decimation operation", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// Geom_LocalSubdivisionCmd
// ----------------
//
int Geom_LocalSubdivisionCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *Name;
  char *dstName;
  char *pointArrayName = 0; 
  char *cellArrayName = 0;
  int numiters = 100;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &Name, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-numiters", INT_Type, &numiters, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-pointarray", STRING_Type, &pointArrayName, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-cellarray", STRING_Type, &cellArrayName, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", Name,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }
  if ( sys_geom_local_subdivision( (cvPolyData*)src, (cvPolyData**)(&dst),
			  numiters,pointArrayName,cellArrayName)
       != CV_OK ) {
    Tcl_SetResult( interp, "running local decimation operation", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  return TCL_OK;
}

#ifdef USE_GTS
// -------------
// Geom_Union_GTSCmd
// -------------

int Geom_Union_GTSCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *aName;
  char *bName;
  char *dstName;
  cvRepositoryData *srcA;
  cvRepositoryData *srcB;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-a", STRING_Type, &aName, NULL, REQUIRED, 0, { 0 } },
    { "-b", STRING_Type, &bName, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( aName );
  if ( srcA == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", aName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  srcB = gRepository->GetObject( bName );
  if ( srcB == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", bName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, aName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, bName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_union_gts( (cvPolyData*)srcA, (cvPolyData*)srcB, (cvPolyData**)(&dst) )
       != CV_OK ) {
    Tcl_SetResult( interp, "poly manipulation error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}

// -----------------
// Geom_Intersect_GTSCmd
// -----------------

int Geom_Intersect_GTSCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *aName;
  char *bName;
  char *dstName;
  cvRepositoryData *srcA;
  cvRepositoryData *srcB;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-a", STRING_Type, &aName, NULL, REQUIRED, 0, { 0 } },
    { "-b", STRING_Type, &bName, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( aName );
  if ( srcA == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", aName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  srcB = gRepository->GetObject( bName );
  if ( srcB == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", bName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, aName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, bName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_intersect_gts( (cvPolyData*)srcA, (cvPolyData*)srcB, (cvPolyData**)(&dst) )
       != CV_OK ) {
    Tcl_SetResult( interp, "poly manipulation error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


// ----------------
// Geom_Subtract_GTSCmd
// ----------------

int Geom_Subtract_GTSCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *aName;
  char *bName;
  char *dstName;
  cvRepositoryData *srcA;
  cvRepositoryData *srcB;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-a", STRING_Type, &aName, NULL, REQUIRED, 0, { 0 } },
    { "-b", STRING_Type, &bName, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( aName );
  if ( srcA == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", aName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  srcB = gRepository->GetObject( bName );
  if ( srcB == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", bName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, aName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, bName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_subtract_gts( (cvPolyData*)srcA, (cvPolyData*)srcB, (cvPolyData**)(&dst) )
       != CV_OK ) {
    Tcl_SetResult( interp, "poly manipulation error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}
#endif

// ---------------------------
// Geom_MakePolysConsistentCmd
// ---------------------------

int Geom_MakePolysConsistentCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_MakePolysConsistent( (cvPolyData*)src, (cvPolyData**)(&dst) )
       != CV_OK ) {
    Tcl_SetResult( interp, "poly manipulation error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


// -----------------------
// Geom_ReverseAllCellsCmd
// -----------------------

int Geom_ReverseAllCellsCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_ReverseAllCells( (cvPolyData*)src, (cvPolyData**)(&dst) )
       != CV_OK ) {
    Tcl_SetResult( interp, "cell reversal error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


// ----------------------------
// Geom_NumClosedLineRegionsCmd
// ----------------------------

int Geom_NumClosedLineRegionsCmd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  cvRepositoryData *src;
  RepositoryDataT type;
  int num;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } }
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_NumClosedLineRegions( (cvPolyData*)src, &num ) != CV_OK ) {
    Tcl_SetResult( interp, "region count failed", TCL_STATIC );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d", num );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE ); 

  return TCL_OK;
}


// ---------------------------
// Geom_GetClosedLineRegionCmd
// ---------------------------

int Geom_GetClosedLineRegionCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  int id;
  RepositoryDataT type;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-id", INT_Type, &id, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } }
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified destination does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_GetClosedLineRegion( (cvPolyData*)src, id, (cvPolyData**)(&dst) )
       != CV_OK ) {
    Tcl_SetResult( interp, "region retrieval failed", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// ------------
// Geom_PickCmd
// ------------

int Geom_PickCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  ARG_List posList;
  char *resultName;
  cvRepositoryData *obj;
  cvRepositoryData *result = NULL;
  RepositoryDataT type;
  double pos[3];

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-pos", LIST_Type, &posList, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &resultName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Convert given coordinate to double's:
  if ( posList.argc != 3 ) {
    Tcl_AppendResult( interp, "crd arg's must be Tcl lists with three ",
		      "elements each", (char*)NULL );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  if ( ( Tcl_GetDouble( interp, posList.argv[0], &(pos[0]) ) != TCL_OK ) ||
       ( Tcl_GetDouble( interp, posList.argv[1], &(pos[1]) ) != TCL_OK ) ||
       ( Tcl_GetDouble( interp, posList.argv[2], &(pos[2]) ) != TCL_OK ) ) {
    Tcl_SetResult( interp, "crd arg list elements must all be double's",
		   TCL_STATIC );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  ARG_FreeListArgvs( table_size, arg_table );

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified result does not exist:
  if ( gRepository->Exists( resultName ) ) {
    Tcl_AppendResult( interp, "object ", resultName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_Pick( (cvPolyData*)obj, pos, (cvPolyData**)(&result) )
       != CV_OK ) {
    Tcl_SetResult( interp, "no geometry containing given pos found",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( resultName, result ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete result;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, result->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// ---------------------
// Geom_OrientProfileCmd
// ---------------------

int Geom_OrientProfileCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;

  ARG_List pathPosList;
  ARG_List pathTanList;
  ARG_List pathXhatList;
  double ppt[3];
  double ptan[3];
  double xhat[3];
  int n;

  char *dstName;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-path_pos", LIST_Type, &pathPosList, NULL, REQUIRED, 0, { 0 } },
    { "-path_tan", LIST_Type, &pathTanList, NULL, REQUIRED, 0, { 0 } },
    { "-path_xhat", LIST_Type, &pathXhatList, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  // Parse lists
  // -----------
  if ( ARG_ParseTclListStatic( interp, pathPosList, DOUBLE_Type, ppt, 3, &n )
       != TCL_OK ) {
    Tcl_SetResult( interp, "path_pos: 3d coordinate", TCL_STATIC );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, pathTanList, DOUBLE_Type, ptan, 3, &n )
       != TCL_OK ) {
    Tcl_SetResult( interp, "path_tan: 3d vector", TCL_STATIC );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, pathXhatList, DOUBLE_Type, xhat, 3, &n )
       != TCL_OK ) {
    Tcl_SetResult( interp, "path_xhat: 3d vector", TCL_STATIC );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  ARG_FreeListArgvs( table_size, arg_table );


  // Do work of command
  // ------------------

  // Retrieve src object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Check src type:
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_OrientProfile( (cvPolyData*)src, ppt, ptan, xhat,
			       (cvPolyData**)(&dst) ) != CV_OK ) {
    Tcl_SetResult( interp, "orient error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// ------------------------
// Geom_DisorientProfileCmd
// ------------------------

int Geom_DisorientProfileCmd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;

  ARG_List pathPosList;
  ARG_List pathTanList;
  ARG_List pathXhatList;
  double ppt[3];
  double ptan[3];
  double xhat[3];
  int n;

  char *dstName;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-path_pos", LIST_Type, &pathPosList, NULL, REQUIRED, 0, { 0 } },
    { "-path_tan", LIST_Type, &pathTanList, NULL, REQUIRED, 0, { 0 } },
    { "-path_xhat", LIST_Type, &pathXhatList, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  // Parse lists
  // -----------
  if ( ARG_ParseTclListStatic( interp, pathPosList, DOUBLE_Type, ppt, 3, &n )
       != TCL_OK ) {
    Tcl_SetResult( interp, "path_pos: 3d coordinate", TCL_STATIC );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, pathTanList, DOUBLE_Type, ptan, 3, &n )
       != TCL_OK ) {
    Tcl_SetResult( interp, "path_tan: 3d vector", TCL_STATIC );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, pathXhatList, DOUBLE_Type, xhat, 3, &n )
       != TCL_OK ) {
    Tcl_SetResult( interp, "path_xhat: 3d vector", TCL_STATIC );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  ARG_FreeListArgvs( table_size, arg_table );


  // Do work of command
  // ------------------

  // Retrieve src object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Check src type:
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_DisorientProfile( (cvPolyData*)src, ppt, ptan, xhat,
				  (cvPolyData**)(&dst) ) != CV_OK ) {
    Tcl_SetResult( interp, "orient error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// --------------------
// Geom_AlignProfileCmd
// --------------------

int Geom_AlignProfileCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *refName;
  char *srcName;
  char *dstName;
  int vecMtd = 1;
  cvRepositoryData *ref;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-ref", STRING_Type, &refName, NULL, REQUIRED, 0, { 0 } },
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-vecMtd", BOOL_Type, &vecMtd, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  // Do work of command
  // ------------------

  // Retrieve ref object and check type:
  ref = gRepository->GetObject( refName );
  if ( ref == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", refName, (char *)NULL );
    return TCL_ERROR;
  }
  type = ref->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, refName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  // Retrieve src object and check type:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  if ( vecMtd ) {
    dst = sys_geom_Align( (cvPolyData*)ref, (cvPolyData*)src );
  } else {
    dst = sys_geom_AlignByDist( (cvPolyData*)ref, (cvPolyData*)src );
  }
  if ( dst == NULL ) {
    Tcl_SetResult( interp, "alignment error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// -----------------
// Geom_TranslateCmd
// -----------------

int Geom_TranslateCmd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;

  ARG_List vecList;
  double vec[3];
  int n;

  char *dstName;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-vec", LIST_Type, &vecList, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  // Parse lists
  // -----------
  if ( ARG_ParseTclListStatic( interp, vecList, DOUBLE_Type, vec, 3, &n )
       != TCL_OK ) {
    Tcl_SetResult( interp, "vec: 3d coordinate", TCL_STATIC );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  ARG_FreeListArgvs( table_size, arg_table );


  // Do work of command
  // ------------------

  // Retrieve src object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Check src type:
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_Translate( (cvPolyData*)src, vec, (cvPolyData**)(&dst) )
       != CV_OK ) {
    Tcl_SetResult( interp, "translate error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// ----------------
// Geom_ScaleAvgCmd
// ----------------

int Geom_ScaleAvgCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;
  double factor;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-factor", DOUBLE_Type, &factor, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }


  // Do work of command
  // ------------------

  // Retrieve src object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Check src type:
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_ScaleAvg( (cvPolyData*)src, factor, (cvPolyData**)(&dst) )
       != CV_OK ) {
    Tcl_SetResult( interp, "error scaling about average point", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// ---------------------
// Geom_GetOrderedPtsCmd
// ---------------------

int Geom_GetOrderedPtsCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  double *pts;
  int i, num;
  char dummy[CV_STRLEN];

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } }
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  // Check type:
  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_GetOrderedPts( (cvPolyData*)obj, &pts, &num ) != CV_OK ) {
    Tcl_SetResult( interp, "point query error", TCL_STATIC );
    return TCL_ERROR;
  }

  for ( i = 0; i < num; i++ ) {
    sprintf( dummy, "%f %f %f", pts[3*i], pts[3*i+1], pts[3*i+2] );
    Tcl_AppendElement( interp, dummy );
  }
  delete [] pts;

  return TCL_OK;
}


// -----------------------
// Geom_WriteOrderedPtsCmd
// -----------------------

int Geom_WriteOrderedPtsCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  char *fileName;
  cvRepositoryData *obj;
  RepositoryDataT type;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-file", STRING_Type, &fileName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  // Check type:
  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_WriteOrderedPts( (cvPolyData*)obj, fileName ) != CV_OK ) {
    Tcl_SetResult( interp, "geometry write error", TCL_STATIC );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// ------------------
// Geom_WriteLinesCmd
// ------------------

int Geom_WriteLinesCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  char *fileName;
  cvRepositoryData *obj;
  RepositoryDataT type;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-file", STRING_Type, &fileName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  // Check type:
  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_WriteLines( (cvPolyData*)obj, fileName ) != CV_OK ) {
    Tcl_SetResult( interp, "file write error", TCL_STATIC );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// -------------------
// Geom_PolysClosedCmd
// -------------------

int Geom_PolysClosedCmd( ClientData clientData, Tcl_Interp *interp,
			 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  cvRepositoryData *src;
  RepositoryDataT type;
  int closed;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_PolysClosed( (cvPolyData*)src, &closed ) != CV_OK ) {
    Tcl_SetResult( interp, "polygon traversal error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( closed ) {
    Tcl_SetResult( interp, "1", TCL_STATIC );
  } else {
    Tcl_SetResult( interp, "0", TCL_STATIC );
  }

  return TCL_OK;
}


// ----------------
// Geom_SurfAreaCmd
// ----------------

int Geom_SurfAreaCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  cvRepositoryData *src;
  RepositoryDataT type;
  double area;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_SurfArea( (cvPolyData*)src, &area ) != CV_OK ) {
    Tcl_SetResult( interp, "surface area computation error", TCL_STATIC );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%f", area );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE ); 

  return TCL_OK;
}


// -----------------------
// Geom_GetPolyCentroidCmd
// -----------------------

int Geom_GetPolyCentroidCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  cvRepositoryData *src;
  RepositoryDataT type;
  double centroid[3];

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_getPolyCentroid( (cvPolyData*)src, centroid) != CV_OK ) {
    Tcl_SetResult( interp, "polygon centroid computation error", TCL_STATIC );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%f %f %f", centroid[0], centroid[1], centroid[2] );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );
  
  return TCL_OK;
}


// ---------------------
// Geom_PrintTriStatsCmd
// ---------------------

int Geom_PrintTriStatsCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  char *usage;
  char *surfName;
  cvRepositoryData *surf;
  RepositoryDataT type;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-surf", STRING_Type, &surfName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  surf = gRepository->GetObject( surfName );
  if ( surf == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", surfName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = surf->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, surfName, " not of type cvPolyData",
		      (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_PrintTriStats( (cvPolyData*)surf ) != CV_OK ) {
    Tcl_SetResult( interp, "surface processing error", TCL_STATIC );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// -----------------------
// Geom_PrintSmallPolysCmd
// -----------------------

int Geom_PrintSmallPolysCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  double sideTol;
  cvRepositoryData *src;
  RepositoryDataT type;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-sideTol", DOUBLE_Type, &sideTol, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_PrintSmallPolys( (cvPolyData*)src, sideTol ) != CV_OK ) {
    Tcl_SetResult( interp, "polygon processing error", TCL_STATIC );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// --------------------
// Geom_RmSmallPolysCmd
// --------------------

int Geom_RmSmallPolysCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;
  double sideTol;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-sideTol", DOUBLE_Type, &sideTol, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_RmSmallPolys( (cvPolyData*)src, sideTol, (cvPolyData**)(&dst) )
       != CV_OK ) {
    Tcl_SetResult( interp, "polygon processing error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


// ------------
// Geom_BBoxCmd
// ------------

int Geom_BBoxCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  double bbox[6];
  char dummy[CV_STRLEN];
  int i;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_BBox( (cvPolyData*)obj, bbox ) != CV_OK ) {
    Tcl_SetResult( interp, "bounding box query error", TCL_STATIC );
    return TCL_ERROR;
  }

  for ( i = 0; i < 6; i++ ) {
    sprintf( dummy, "%f", bbox[i] );
    Tcl_AppendElement( interp, dummy );
  }

  return TCL_OK;
}


// ----------------
// Geom_ClassifyCmd
// ----------------

int Geom_ClassifyCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  ARG_List ptList;
  double pt[3];
  int npt;
  int ans;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-pt", LIST_Type, &ptList, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Parse coordinate list:
  if ( ARG_ParseTclListStatic( interp, ptList, DOUBLE_Type, pt, 3, &npt )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  ARG_FreeListArgvs( table_size, arg_table );
  if ( npt != 3 ) {
    Tcl_SetResult( interp, "only valid for 3d objects and queries",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_Classify( (cvPolyData*)obj, pt, &ans ) != CV_OK ) {
    Tcl_SetResult( interp, "classify error", TCL_STATIC );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d", ans );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );

  return TCL_OK;
}


// ----------------
// Geom_PtInPolyCmd
// ----------------

int Geom_PtInPolyCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  ARG_List ptList;
  double pt[3];
  int npt;
  int ans;
  int usePrevPoly = 0;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-pgn", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-pt", LIST_Type, &ptList, NULL, REQUIRED, 0, { 0 } },
    { "-usePrevPoly", INT_Type, &usePrevPoly, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Parse coordinate list:
  if ( ARG_ParseTclListStatic( interp, ptList, DOUBLE_Type, pt, 3, &npt )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  ARG_FreeListArgvs( table_size, arg_table );
  if ( npt != 2 ) {
    Tcl_SetResult( interp, "pt must be two-dimensional", TCL_STATIC );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_PtInPoly( (cvPolyData*)obj, pt,usePrevPoly, &ans ) != CV_OK ) {
    Tcl_SetResult( interp, "point-in-poly error", TCL_STATIC );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d", ans );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE ); 

  return TCL_OK;
}


// ----------------
// Geom_MergePtsCmd
// ----------------

int Geom_MergePtsCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  RepositoryDataT type;
  cvPolyData *dst;
  double tol = 1e10 * FindMachineEpsilon();

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-tol", DOUBLE_Type, &tol, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  dst = sys_geom_MergePts_tol( (cvPolyData*)src, tol );

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


// -----------------
// Geom_Warp3dPtsCmd
// -----------------

int Geom_Warp3dPtsCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  RepositoryDataT type;
  cvPolyData *dst;
  double scale = 1.0;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-scale", DOUBLE_Type, &scale, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  dst = sys_geom_warp3dPts( (cvPolyData*)src, scale );

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


// --------------
// Geom_NumPtsCmd
// --------------

int Geom_NumPtsCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  cvRepositoryData *src;
  RepositoryDataT type;
  int num;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  num = ((cvPolyData*)src)->GetVtkPolyData()->GetNumberOfPoints();

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d", num );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE ); 

  return TCL_OK;
}


// ---------------------
// Geom_sampleLoopCmd
// ---------------------

int Geom_sampleLoopCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  int targetNumPts;
  char *dstName;
  cvRepositoryData *src;
  RepositoryDataT type;
  cvPolyData *dst;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-num", INT_Type, &targetNumPts, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  dst = sys_geom_sampleLoop( (cvPolyData*)src, targetNumPts );

  if ( dst == NULL ) {
    Tcl_SetResult( interp, "subsample loop error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}

// ---------------------
// Geom_loftSolidCmd
// ---------------------

int Geom_loftSolidCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  char *usage;
  int numSrcs;
  ARG_List srcList;
  char *dstName;
  cvRepositoryData *src;
  cvPolyData *dst;
  RepositoryDataT type;
  cvPolyData **srcs;
  int numOutPtsInSegs;
  int numOutPtsAlongLength;
  int numModes;
  int useFFT;
  int useLinearSampleAlongLength;
  int numLinearPtsAlongLength;
  int splineType = 0;
  double continuity = 0;
  double bias = 0;
  double tension = 0;

  int table_size = 12;
  ARG_Entry arg_table[] = {
    { "-srclist", LIST_Type, &srcList, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-numOutInSegs", INT_Type, &numOutPtsInSegs, NULL, REQUIRED, 0, { 0 } },
    { "-numOutAlongLength", INT_Type, &numOutPtsAlongLength, NULL, REQUIRED, 0, { 0 } },
    { "-numLinearPtsAlongLength", INT_Type, &numLinearPtsAlongLength, NULL, REQUIRED, 0, { 0 } },
    { "-numModes", INT_Type, &numModes, NULL, REQUIRED, 0, { 0 } },
    { "-useFFT", INT_Type, &useFFT, NULL, REQUIRED, 0, { 0 } },
    { "-useLinearSampleAlongLength", INT_Type, &useLinearSampleAlongLength, NULL, REQUIRED, 0, { 0 } },
    { "-splineType", INT_Type, &splineType, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-bias", DOUBLE_Type, &bias, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-tension", DOUBLE_Type, &tension, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-continuity", DOUBLE_Type, &continuity, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:
  numSrcs = srcList.argc;

  // Foreach src obj, check that it is in the repository and of the
  // correct type (i.e. cvSolidModel).  Also build up the array of
  // cvSolidModel*'s to pass to cvSolidModel::MakeLoftedSurf.

  srcs = new cvPolyData * [numSrcs];

  for (int i = 0; i < numSrcs; i++ ) {
    src = gRepository->GetObject( srcList.argv[i] );
    if ( src == NULL ) {
      Tcl_AppendResult( interp, "couldn't find object ", srcList.argv[i],
			(char *)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      delete [] srcs;
      return TCL_ERROR;
    }
    type = src->GetType();
    if ( type != POLY_DATA_T ) {
      Tcl_AppendResult( interp, "object ", srcList.argv[i],
			" not of type cvPolyData", (char *)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      delete [] srcs;
      return TCL_ERROR;
    }
    srcs[i] = (cvPolyData *) src;
  }

  // We're done with the src object names:
  ARG_FreeListArgvs( table_size, arg_table );

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    delete [] srcs;
    return TCL_ERROR;
  }

  if ( sys_geom_loft_solid( srcs, numSrcs,useLinearSampleAlongLength,useFFT,
			  numOutPtsAlongLength,numOutPtsInSegs,
			  numLinearPtsAlongLength,numModes,splineType,bias,tension,continuity,
			  (cvPolyData**)(&dst) )
       != CV_OK ) {
    Tcl_SetResult( interp, "poly manipulation error", TCL_STATIC );
    delete dst;
    delete [] srcs;
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// --------------------
// Geom_2dWindingNumCmd
// --------------------

int Geom_2dWindingNumCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  int wnum;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  wnum = sys_geom_2DWindingNum( (cvPolyData*)obj );

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d", wnum );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE ); 

  return TCL_OK;
}


// -------------------
// Geom_PolygonNormCmd
// -------------------

int Geom_PolygonNormCmd( ClientData clientData, Tcl_Interp *interp,
			 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  double n[3];

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_PolygonNormal( (cvPolyData*)obj, n ) != CV_OK ) {
    Tcl_AppendResult( interp, "error computing normal for ",
		      objName, (char *)NULL );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%f %f %f", n[0], n[1], n[2] );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE ); 

  return TCL_OK;
}


// -------------
// Geom_AvgPtCmd
// -------------

int Geom_AvgPtCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  double pt[3];

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_AvgPt( (cvPolyData*)obj, pt ) != CV_OK ) {
    Tcl_AppendResult( interp, "error averaging points of ",
		      objName, (char *)NULL );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%f %f %f", pt[0], pt[1], pt[2] );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE ); 

  return TCL_OK;
}


// ------------
// Geom_CopyCmd
// ------------

int Geom_CopyCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  cvPolyData *dst;
  RepositoryDataT type;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  dst = sys_geom_DeepCopy( (cvPolyData*)src );
  if ( dst == NULL ) {
    Tcl_AppendResult( interp, "error copying ", srcName, (char *)NULL );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );
  return TCL_OK;
}

// ------------------
// Geom_ReorderPgnCmd
// ------------------

int Geom_ReorderPgnCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  cvPolyData *dst;
  RepositoryDataT type;
  int start;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-start", INT_Type, &start, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  dst = sys_geom_ReorderPolygon( (cvPolyData*)src, start );
  if ( dst == NULL ) {
    Tcl_AppendResult( interp, "error reordering ", srcName, (char *)NULL );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// ---------------------------
// Geom_SplinePtsToPathPlanCmd
// ---------------------------

int Geom_SplinePtsToPathPlanCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  int numOutputPts;
  char *filename = NULL;
  int flag;
  cvRepositoryData *src;
  RepositoryDataT type;


  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-file", STRING_Type, &filename, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-numOutputPts", INT_Type, &numOutputPts, NULL, REQUIRED, 0, { 0 } },
    { "-flag", INT_Type, &flag, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  // if no filename is specified, pass the interp to the spline code to create
  // a tcl string containing the path.  If a filename is specified, return string
  // will be left blank.
  int result;
  if (filename == NULL) {
    result = sys_geom_splinePtsToPathPlan( ((cvPolyData*)src)->GetVtkPolyData(),numOutputPts,
                                  filename, flag, interp);
  } else {
    result = sys_geom_splinePtsToPathPlan( ((cvPolyData*)src)->GetVtkPolyData(),numOutputPts,
                                  filename, flag, NULL);
  }

  if (result == CV_OK) {
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }

}


// ------------------------
// Geom_IntegrateSurfaceCmd
// ------------------------

int Geom_IntegrateSurfaceCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  ARG_List nrmList;
  cvRepositoryData *obj;
  RepositoryDataT type;
  double nrm[3];
  int tensorType;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-nrm", LIST_Type, &nrmList, NULL, REQUIRED, 0, { 0 } },
    { "-tensorType", INT_Type, &tensorType, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  
    if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
      Tcl_SetResult( interp, usage, TCL_VOLATILE );
      return TCL_ERROR;
    }

    // Convert given coordinate to double's:
    if ( nrmList.argc != 3 ) {
      Tcl_AppendResult( interp, "crd arg's must be Tcl lists with three ",
		      "elements each", (char*)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      return TCL_ERROR;
    }
    if ( ( Tcl_GetDouble( interp, nrmList.argv[0], &(nrm[0]) ) != TCL_OK ) ||
         ( Tcl_GetDouble( interp, nrmList.argv[1], &(nrm[1]) ) != TCL_OK ) ||
         ( Tcl_GetDouble( interp, nrmList.argv[2], &(nrm[2]) ) != TCL_OK ) ) {
      Tcl_SetResult( interp, "crd arg list elements must all be double's",
		   TCL_STATIC );
      ARG_FreeListArgvs( table_size, arg_table );
      return TCL_ERROR;
    }

  ARG_FreeListArgvs( table_size, arg_table );

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }
  
  double q = 0.0;
  if ( sys_geom_IntegrateSurface((cvPolyData*)obj, tensorType, nrm, &q) != CV_OK ) {
    Tcl_SetResult( interp, "error calculating surface integral",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  char qstring[2048];
  qstring[0]='\0';
  sprintf(qstring,"%lf",q);
  Tcl_SetResult( interp, qstring, TCL_VOLATILE );
  return TCL_OK;
}


// -------------------------
// Geom_IntegrateSurface2Cmd
// -------------------------

int Geom_IntegrateSurface2Cmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  ARG_List nrmList;
  cvRepositoryData *obj;
  RepositoryDataT type;
  int tensorType;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-tensorType", INT_Type, &tensorType, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  
    if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
      Tcl_SetResult( interp, usage, TCL_VOLATILE );
      return TCL_ERROR;
    }

  ARG_FreeListArgvs( table_size, arg_table );

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }
  
  double q = 0.0;
  double area = 0.0;
  if ( sys_geom_IntegrateSurface2((cvPolyData*)obj, tensorType, &q, &area) != CV_OK ) {
    Tcl_SetResult( interp, "error calculating surface integral",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  char qstring[2048];
  qstring[0]='\0';
  sprintf(qstring,"%lf %lf",q,area);
  Tcl_SetResult( interp, qstring, TCL_VOLATILE );
  return TCL_OK;
}


// -----------------------
// Geom_IntegrateEnergyCmd
// -----------------------

int Geom_IntegrateEnergyCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  ARG_List nrmList;
  cvRepositoryData *obj;
  RepositoryDataT type;
  double nrm[3];
  double rho = 0.0;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-nrm", LIST_Type, &nrmList, NULL, REQUIRED, 0, { 0 } },
    { "-rho", DOUBLE_Type, &rho, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  
    if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
      Tcl_SetResult( interp, usage, TCL_VOLATILE );
      return TCL_ERROR;
    }

    // Convert given coordinate to double's:
    if ( nrmList.argc != 3 ) {
      Tcl_AppendResult( interp, "crd arg's must be Tcl lists with three ",
		      "elements each", (char*)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      return TCL_ERROR;
    }
    if ( ( Tcl_GetDouble( interp, nrmList.argv[0], &(nrm[0]) ) != TCL_OK ) ||
         ( Tcl_GetDouble( interp, nrmList.argv[1], &(nrm[1]) ) != TCL_OK ) ||
         ( Tcl_GetDouble( interp, nrmList.argv[2], &(nrm[2]) ) != TCL_OK ) ) {
      Tcl_SetResult( interp, "crd arg list elements must all be double's",
		   TCL_STATIC );
      ARG_FreeListArgvs( table_size, arg_table );
      return TCL_ERROR;
    }

  ARG_FreeListArgvs( table_size, arg_table );

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }
  
  double energy = 0.0;
  if ( sys_geom_IntegrateEnergy((cvPolyData*)obj, rho, nrm, &energy) != CV_OK ) {
    Tcl_SetResult( interp, "error calculating surface integral",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  char estring[2048];
  estring[0]='\0';
  sprintf(estring,"%lf",energy);
  Tcl_SetResult( interp, estring, TCL_VOLATILE );
  return TCL_OK;
}


// --------------------
// Geom_FindDistanceCmd
// --------------------

int Geom_FindDistanceCmd( ClientData clientData, Tcl_Interp *interp,
		          int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  ARG_List ptList;
  double pt[3];
  int npt;
  double distance;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-pt", LIST_Type, &ptList, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Parse coordinate list:
  if ( ARG_ParseTclListStatic( interp, ptList, DOUBLE_Type, pt, 3, &npt )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  ARG_FreeListArgvs( table_size, arg_table );
  if ( npt != 3 ) {
    Tcl_SetResult( interp, "only valid for 3d objects and queries",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  distance = ((cvPolyData*)obj)->FindDistance( pt[0], pt[1], pt[2] );

  char result[2048];
  result[0]='\0';
  sprintf(result,"%lf",distance);
  Tcl_SetResult( interp, result, TCL_VOLATILE );

  return TCL_OK;
}


// -------------------------
// Geom_InterpolateScalarCmd
// -------------------------

int Geom_InterpolateScalarCmd( ClientData clientData, Tcl_Interp *interp,
		          int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  ARG_List ptList;
  double pt[3];
  int npt;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-pt", LIST_Type, &ptList, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Parse coordinate list:
  if ( ARG_ParseTclListStatic( interp, ptList, DOUBLE_Type, pt, 3, &npt )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  ARG_FreeListArgvs( table_size, arg_table );

  if ( npt != 3 ) {
    Tcl_SetResult( interp, "only valid for 3d objects and queries",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  double scalar = 0.0;
  if ( sys_geom_InterpolateScalar((cvPolyData*)obj, pt, &scalar) != CV_OK ) {
    Tcl_SetResult( interp, "error interpolating scalar",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  char result[2048];
  result[0]='\0';
  sprintf(result,"%lf",scalar);
  Tcl_SetResult( interp, result, TCL_VOLATILE );
  return TCL_OK;
}


// -------------------------
// Geom_InterpolateVectorCmd
// -------------------------

int Geom_InterpolateVectorCmd( ClientData clientData, Tcl_Interp *interp,
		          int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  ARG_List ptList;
  double pt[3];
  int npt;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-pt", LIST_Type, &ptList, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Parse coordinate list:
  if ( ARG_ParseTclListStatic( interp, ptList, DOUBLE_Type, pt, 3, &npt )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  ARG_FreeListArgvs( table_size, arg_table );

  if ( npt != 3 ) {
    Tcl_SetResult( interp, "only valid for 3d objects and queries",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  double vect[3];
  vect[0] = 0.0;
  vect[1] = 0.0;
  vect[2] = 0.0;
  if ( sys_geom_InterpolateVector((cvPolyData*)obj, pt, vect) != CV_OK ) {
    Tcl_SetResult( interp, "error interpolating vector",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  char result[2048];
  result[0]='\0';
  sprintf(result,"%lf %lf %lf",vect[0], vect[1], vect[2]);
  Tcl_SetResult( interp, result, TCL_VOLATILE );
  return TCL_OK;
}



// -------------------------
// Geom_IntersectWithLineCmd
// -------------------------

int Geom_IntersectWithLineCmd( ClientData clientData, Tcl_Interp *interp,
		          int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  ARG_List p0List,p1List;
  double p0[3],p1[3];
  int npt0,npt1;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-pt0", LIST_Type, &p0List, NULL, REQUIRED, 0, { 0 } },
    { "-pt1", LIST_Type, &p1List, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Parse coordinate list:
  if ( ARG_ParseTclListStatic( interp, p0List, DOUBLE_Type, p0, 3, &npt0 )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, p1List, DOUBLE_Type, p1, 3, &npt1 )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  ARG_FreeListArgvs( table_size, arg_table );

  if ( npt0 != 3 || npt1 != 3 ) {
    Tcl_SetResult( interp, "only valid for 3d objects and queries",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  double intersect[3];
  if ( sys_geom_IntersectWithLine((cvPolyData*)obj, p0, p1, intersect) != CV_OK ) {
    Tcl_SetResult( interp, "error intersecting vtkPolyData with line",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  char result[2048];
  result[0]='\0';
  sprintf(result,"%lf %lf %lf",intersect[0],intersect[1],intersect[2]);
  Tcl_SetResult( interp, result, TCL_VOLATILE );
  return TCL_OK;
}


// --------------------
// Geom_AddPointDataCmd
// --------------------

int Geom_AddPointDataCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcNameA;
  char *srcNameB;
  int scflag = FALSE;
  int vflag = FALSE; 
  char *dstName;
  
  cvRepositoryData *srcA = NULL;
  cvRepositoryData *srcB = NULL;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-srcA", STRING_Type, &srcNameA, NULL, REQUIRED, 0, { 0 } },
    { "-srcB", STRING_Type, &srcNameB, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-scalars", BOOL_Type, &scflag, NULL, REQUIRED, 0, { 0 } },
    { "-vectors", BOOL_Type, &vflag, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( srcNameA );
  if ( srcA == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcNameA,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Retrieve source object:
  srcB = gRepository->GetObject( srcNameB );
  if ( srcB == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcNameB,
		      (char *)NULL );
    return TCL_ERROR;
  }


  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcNameA, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcNameB, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;
  if (scflag) {
      sc = SYS_GEOM_ADD_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_ADD_VECTOR;
  }
 
  if ( sys_geom_mathPointData( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != CV_OK ) {
    Tcl_SetResult( interp, "point data math error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


// -------------------------
// Geom_SubtractPointDataCmd
// -------------------------

int Geom_SubtractPointDataCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcNameA;
  char *srcNameB;
  int scflag = FALSE;
  int vflag = FALSE; 
  char *dstName;
  
  cvRepositoryData *srcA = NULL;
  cvRepositoryData *srcB = NULL;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-srcA", STRING_Type, &srcNameA, NULL, REQUIRED, 0, { 0 } },
    { "-srcB", STRING_Type, &srcNameB, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-scalars", BOOL_Type, &scflag, NULL, REQUIRED, 0, { 0 } },
    { "-vectors", BOOL_Type, &vflag, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( srcNameA );
  if ( srcA == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcNameA,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Retrieve source object:
  srcB = gRepository->GetObject( srcNameB );
  if ( srcB == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcNameB,
		      (char *)NULL );
    return TCL_ERROR;
  }


  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcNameA, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcNameB, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;
  if (scflag) {
      sc = SYS_GEOM_SUBTRACT_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_SUBTRACT_VECTOR;
  }
 
  if ( sys_geom_mathPointData( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != CV_OK ) {
    Tcl_SetResult( interp, "point data math error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


// -------------------------
// Geom_MultiplyPointDataCmd
// -------------------------

int Geom_MultiplyPointDataCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcNameA;
  char *srcNameB;
  int scflag = FALSE;
  int vflag = FALSE; 
  char *dstName;
  
  cvRepositoryData *srcA = NULL;
  cvRepositoryData *srcB = NULL;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-srcA", STRING_Type, &srcNameA, NULL, REQUIRED, 0, { 0 } },
    { "-srcB", STRING_Type, &srcNameB, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-scalars", BOOL_Type, &scflag, NULL, REQUIRED, 0, { 0 } },
    { "-vectors", BOOL_Type, &vflag, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( srcNameA );
  if ( srcA == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcNameA,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Retrieve source object:
  srcB = gRepository->GetObject( srcNameB );
  if ( srcB == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcNameB,
		      (char *)NULL );
    return TCL_ERROR;
  }


  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcNameA, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcNameB, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;
  if (scflag) {
      sc = SYS_GEOM_MULTIPLY_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_MULTIPLY_VECTOR;
  }
 
  if ( sys_geom_mathPointData( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != CV_OK ) {
    Tcl_SetResult( interp, "point data math error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


// -----------------------
// Geom_DividePointDataCmd
// -----------------------

int Geom_DividePointDataCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcNameA;
  char *srcNameB;
  int scflag = FALSE;
  int vflag = FALSE; 
  char *dstName;
  
  cvRepositoryData *srcA = NULL;
  cvRepositoryData *srcB = NULL;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-srcA", STRING_Type, &srcNameA, NULL, REQUIRED, 0, { 0 } },
    { "-srcB", STRING_Type, &srcNameB, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-scalars", BOOL_Type, &scflag, NULL, REQUIRED, 0, { 0 } },
    { "-vectors", BOOL_Type, &vflag, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( srcNameA );
  if ( srcA == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcNameA,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Retrieve source object:
  srcB = gRepository->GetObject( srcNameB );
  if ( srcB == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcNameB,
		      (char *)NULL );
    return TCL_ERROR;
  }


  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcNameA, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcNameB, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;
  if (scflag) {
      sc = SYS_GEOM_DIVIDE_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_DIVIDE_VECTOR;
  }
 
  if ( sys_geom_mathPointData( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != CV_OK ) {
    Tcl_SetResult( interp, "point data math error", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


// ---------------
// Geom_ProjectCmd
// ---------------

int Geom_ProjectCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcNameA;
  char *srcNameB;
  int scflag = FALSE;
  int vflag = FALSE; 
  char *dstName;
  
  cvRepositoryData *srcA = NULL;
  cvRepositoryData *srcB = NULL;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-srcA", STRING_Type, &srcNameA, NULL, REQUIRED, 0, { 0 } },
    { "-srcB", STRING_Type, &srcNameB, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-scalars", BOOL_Type, &scflag, NULL, REQUIRED, 0, { 0 } },
    { "-vectors", BOOL_Type, &vflag, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( srcNameA );
  if ( srcA == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcNameA,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Retrieve source object:
  srcB = gRepository->GetObject( srcNameB );
  if ( srcB == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcNameB,
		      (char *)NULL );
    return TCL_ERROR;
  }


  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcNameA, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcNameB, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;
  if (scflag) {
      sc = SYS_GEOM_ADD_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_ADD_VECTOR;
  }
 
  if ( sys_geom_Project( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != CV_OK ) {
    Tcl_SetResult( interp, "error projecting polydata point data", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


// ---------------------------
// Geom_IntegrateScalarSurfCmd
// ---------------------------

int Geom_IntegrateScalarSurfCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  cvRepositoryData *src;
  RepositoryDataT type;
  double flux;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_IntegrateScalarSurf( (cvPolyData*)src, &flux ) != CV_OK ) {
    Tcl_SetResult( interp, "surface area computation error", TCL_STATIC );
    return TCL_ERROR;
  }

  char result[255];
  result[0]='\0';
  sprintf(result,"%lf",flux);

  Tcl_SetResult( interp, result, TCL_VOLATILE );

  return TCL_OK;
}


// -----------------------------
// Geom_IntegrateScalarThreshCmd
// -----------------------------

int Geom_IntegrateScalarThreshCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  cvRepositoryData *src;
  RepositoryDataT type;
  double wssthresh;
  double flux;
  double area;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-thr", DOUBLE_Type, &wssthresh, NULL, REQUIRED, 0, { 0 } }, 
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  if ( sys_geom_IntegrateScalarThresh( (cvPolyData*)src, wssthresh, &flux, &area) != CV_OK ) {
    Tcl_SetResult( interp, "surface area computation error", TCL_STATIC );
    return TCL_ERROR;
  }

  char result[255];
  result[0]='\0';
  sprintf(result,"%lf %1f", flux, area);

  Tcl_SetResult( interp, result, TCL_VOLATILE );

  return TCL_OK;
}


// -------------------------
// Geom_ReplacePointDataCmd
// ------------------------

int Geom_ReplacePointDataCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcNameA;
  char *srcNameB;
  int scflag = FALSE;
  int vflag = FALSE; 
  char *dstName;
  
  cvRepositoryData *srcA = NULL;
  cvRepositoryData *srcB = NULL;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-srcA", STRING_Type, &srcNameA, NULL, REQUIRED, 0, { 0 } },
    { "-srcB", STRING_Type, &srcNameB, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-scalars", BOOL_Type, &scflag, NULL, REQUIRED, 0, { 0 } },
    { "-vectors", BOOL_Type, &vflag, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( srcNameA );
  if ( srcA == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcNameA,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Retrieve source object:
  srcB = gRepository->GetObject( srcNameB );
  if ( srcB == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcNameB,
		      (char *)NULL );
    return TCL_ERROR;
  }


  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcNameA, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcNameB, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;
  if (scflag) {
      sc = SYS_GEOM_ADD_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_ADD_VECTOR;
  }
 
  if ( sys_geom_ReplacePointData( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != CV_OK ) {
    Tcl_SetResult( interp, "error replacing point data", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}

#ifdef USE_VMTK
int Geom_CenterlinesCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  ARG_List sourceList;
  ARG_List targetList;
  char *linesName;
  char *voronoiName;
  char *geomName;
  cvRepositoryData *geomSrc;
  cvRepositoryData *linesDst = NULL;
  cvRepositoryData *voronoiDst = NULL;
  RepositoryDataT type;

  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &geomName, NULL, REQUIRED, 0, { 0 } },
    { "-sourcelist", LIST_Type, &sourceList, NULL, REQUIRED, 0, { 0 } },
    { "-targetlist", LIST_Type, &targetList, NULL, REQUIRED, 0, { 0 } },
    { "-linesresult", STRING_Type, &linesName, NULL, REQUIRED, 0, { 0 } },
    { "-voronoiresult", STRING_Type, &voronoiName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  // Retrieve source object:
  geomSrc = gRepository->GetObject( geomName );
  if ( geomSrc == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", geomName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = geomSrc->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, geomName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( linesName ) ) {
    Tcl_AppendResult( interp, "object ", linesName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  if ( gRepository->Exists( voronoiName ) ) {
    Tcl_AppendResult( interp, "object ", voronoiName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  if (sourceList.argc == 0) {
      ARG_FreeListArgvs( table_size, arg_table);
      return CV_OK;
  }

  if (targetList.argc == 0) {
      ARG_FreeListArgvs( table_size, arg_table);
      return CV_OK;
  }

  int nsources = 0;
  int *sources = new int[sourceList.argc];
  int ntargets = 0;
  int *targets = new int[targetList.argc];

  if ( ARG_ParseTclListStatic( interp, sourceList, INT_Type, sources, sourceList.argc, &nsources )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  if ( ARG_ParseTclListStatic( interp, targetList, INT_Type, targets, targetList.argc, &ntargets )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  // Do work of command:
  ARG_FreeListArgvs( table_size, arg_table );

  if ( sys_geom_centerlines( (cvPolyData*)geomSrc, sources, nsources, targets, ntargets, (cvPolyData**)(&linesDst), (cvPolyData**)(&voronoiDst))
       != CV_OK ) {
    Tcl_SetResult( interp, "error creating centerlines", TCL_STATIC );
    return TCL_ERROR;
  }

  delete [] sources;
  delete [] targets;

  if ( !( gRepository->Register( linesName, linesDst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", linesName,
		      " in repository", (char *)NULL );
    delete linesDst;
    delete voronoiDst;
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( voronoiName, voronoiDst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", voronoiName,
		      " in repository", (char *)NULL );
    delete linesDst;
    delete voronoiDst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, linesDst->GetName(), TCL_VOLATILE );
//  Tcl_SetResult( interp, voronoiDst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}

int Geom_DistanceToCenterlinesCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *geomName;
  char *linesName;
  char *distanceName;
  cvRepositoryData *geomSrc;
  cvRepositoryData *linesSrc;
  cvRepositoryData *distanceDst = NULL;
  RepositoryDataT type;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &geomName, NULL, REQUIRED, 0, { 0 } },
    { "-lines", STRING_Type, &linesName, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &distanceName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  // Retrieve source object:
  geomSrc = gRepository->GetObject( geomName );
  if ( geomSrc == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", geomName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = geomSrc->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, geomName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  // Retrieve source object:
  linesSrc = gRepository->GetObject( linesName );
  if ( linesSrc == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", linesName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = linesSrc->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, linesName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  // Do work of command:
  ARG_FreeListArgvs( table_size, arg_table );

  if ( sys_geom_distancetocenterlines( (cvPolyData*)geomSrc, (cvPolyData*)linesSrc, (cvPolyData**)(&distanceDst) )
       != CV_OK ) {
    Tcl_SetResult( interp, "error getting distance to centerlines", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( distanceName, distanceDst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", distanceName,
		      " in repository", (char *)NULL );
    delete distanceDst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, distanceDst->GetName(), TCL_VOLATILE );
//  Tcl_SetResult( interp, voronoiDst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


int Geom_CapCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  int numIds;
  int *ids;      
  int captype;
  char *usage;
  char *cappedName;
  char *geomName;
  char idstring[256];
  cvRepositoryData *geomSrc;
  cvRepositoryData *cappedDst = NULL;
  RepositoryDataT type;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &geomName, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &cappedName, NULL, REQUIRED, 0, { 0 } },
    { "-captype", INT_Type, &captype, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  // Retrieve source object:
  geomSrc = gRepository->GetObject( geomName );
  if ( geomSrc == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", geomName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = geomSrc->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, geomName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( cappedName ) ) {
    Tcl_AppendResult( interp, "object ", cappedName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Do work of command:
  ARG_FreeListArgvs( table_size, arg_table );

  if ( sys_geom_cap( (cvPolyData*)geomSrc, (cvPolyData**)(&cappedDst), &numIds,&ids,captype )
       != CV_OK ) {
    Tcl_SetResult( interp, "error capping model", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( cappedName, cappedDst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", cappedName,
		      " in repository", (char *)NULL );
    delete cappedDst;
    return TCL_ERROR;
  }

//  Tcl_SetResult( interp, cappedDst->GetName(), TCL_VOLATILE );

  if (numIds == 0) 
  {
    Tcl_SetResult( interp, "No Ids Found", TCL_STATIC );
    return TCL_ERROR;
  }
  for (int i = 0; i < numIds; i++) {
	sprintf(idstring, "%i", ids[i]);
    Tcl_AppendElement ( interp, idstring);
	idstring[0]='\n';
  }
  delete [] ids;

  return TCL_OK;
}

int Geom_CapWIdsCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  int fillId;
  char *usage;
  char *cappedName;
  char *geomName;
  int num_filled = 0;
  int filltype = 0;
  cvRepositoryData *geomSrc;
  cvRepositoryData *cappedDst = NULL;
  RepositoryDataT type;

  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &geomName, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &cappedName, NULL, REQUIRED, 0, { 0 } },
    { "-fillnum", INT_Type, &fillId, NULL, REQUIRED, 0, { 0 } },
    { "-filltype", INT_Type, &filltype, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  // Retrieve source object:
  geomSrc = gRepository->GetObject( geomName );
  if ( geomSrc == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", geomName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = geomSrc->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, geomName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( cappedName ) ) {
    Tcl_AppendResult( interp, "object ", cappedName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Do work of command:
  ARG_FreeListArgvs( table_size, arg_table );

  if ( sys_geom_cap_with_ids( (cvPolyData*)geomSrc, (cvPolyData**)(&cappedDst)
	,fillId,num_filled,filltype)
       != CV_OK ) {
    Tcl_SetResult( interp, "error capping model", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( cappedName, cappedDst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", cappedName,
		      " in repository", (char *)NULL );
    delete cappedDst;
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d",num_filled);
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE);

  return TCL_OK;
}

int Geom_MapAndCorrectIdsCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *originalName;
  char *newName;
  char *resultName;
  char *originalArray;
  char *newArray;
  cvRepositoryData *geomSrc;
  cvRepositoryData *geomNew;
  cvRepositoryData *geomDst = NULL;
  RepositoryDataT type;

  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &originalName, NULL, REQUIRED, 0, { 0 } },
    { "-new", STRING_Type, &newName, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &resultName, NULL, REQUIRED, 0, { 0 } },
    { "-srcarrayname", STRING_Type, &originalArray, NULL, REQUIRED, 0, { 0 } },
    { "-newarrayname", STRING_Type, &newArray, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  // Retrieve source object:
  geomSrc = gRepository->GetObject( originalName );
  if ( geomSrc == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", originalName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Retrieve source object:
  geomNew = gRepository->GetObject( newName );
  if ( geomNew == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", newName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = geomSrc->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, originalName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  type = geomNew->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, newName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    Tcl_AppendResult( interp, "object ", resultName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Do work of command:
  ARG_FreeListArgvs( table_size, arg_table );

  if ( sys_geom_mapandcorrectids( (cvPolyData*)geomSrc, (cvPolyData*)geomNew, (cvPolyData**)(&geomDst), originalArray,newArray )
       != CV_OK ) {
    Tcl_SetResult( interp, "error correcting ids", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( resultName, geomDst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete geomDst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, geomDst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}
#endif
