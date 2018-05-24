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
#include <string.h>
#include "sv_Repository.h"
#include "sv_RepositoryData.h"
#include "sv_PolyData.h"
#include "sv_geom_init.h"
#include "sv_sys_geom.h"
#include "sv_SolidModel.h"
#include "sv_solid_init.h"
#include "sv_integrate_surface.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"
#include "sv_vtk_utils.h"
#include "sv_solid_init.h"
#include "vtkSmartPointer.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "sv2_globals.h"


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

int Geom_loftSolidWithNURBSCmd( ClientData clientData, Tcl_Interp *interp,
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

int Geom_CleanCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_CapIdSetCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_LocalDecimationCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_LocalLaplacianSmoothCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_LocalConstrainSmoothCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_LocalLinearSubdivisionCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_LocalButterflySubdivisionCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_LocalLoopSubdivisionCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_LocalBlendCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_SetArrayForLocalOp_SphereCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_SetArrayForLocalOp_FaceCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_SetArrayForLocalOp_CellsCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_SetArrayForLocalOp_BlendCmd( ClientData clientData, Tcl_Interp *interp,
                           int argc, CONST84 char *argv[] );

int Geom_All_UnionCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int Geom_Convert_NURBS_To_PolyCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

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
  Tcl_CreateCommand( interp, "geom_loftSolidWithNURBS", Geom_loftSolidWithNURBSCmd,
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
 Tcl_CreateCommand( interp, "geom_clean", Geom_CleanCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_set_ids_for_caps", Geom_CapIdSetCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_local_decimation", Geom_LocalDecimationCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_local_laplacian_smooth", Geom_LocalLaplacianSmoothCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_local_constrain_smooth", Geom_LocalConstrainSmoothCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_local_linear_subdivision", Geom_LocalLinearSubdivisionCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_local_butterfly_subdivision", Geom_LocalButterflySubdivisionCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_local_loop_subdivision", Geom_LocalLoopSubdivisionCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_local_blend", Geom_LocalBlendCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_set_array_for_local_op_sphere", Geom_SetArrayForLocalOp_SphereCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_set_array_for_local_op_face", Geom_SetArrayForLocalOp_FaceCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_set_array_for_local_op_cells", Geom_SetArrayForLocalOp_CellsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
 Tcl_CreateCommand( interp, "geom_set_array_for_local_op_face_blend", Geom_SetArrayForLocalOp_BlendCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_all_union", Geom_All_UnionCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "model_name_model_from_polydata_names", Geom_Convert_NURBS_To_PolyCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
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

  if ( sys_geom_Reduce( (cvPolyData*)src, tol, (cvPolyData**)(&dst) ) != SV_OK ) {
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
    { "-tolerance", DOUBLE_Type, &tolerance, NULL, SV_OPTIONAL, 0, { 0 } },
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
       != SV_OK ) {
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
    { "-tolerance", DOUBLE_Type, &tolerance, NULL, SV_OPTIONAL, 0, { 0 } },
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
       != SV_OK ) {
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
    { "-tolerance", DOUBLE_Type, &tolerance, NULL, SV_OPTIONAL, 0, { 0 } },
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
       != SV_OK ) {
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
    { "-tolerance", DOUBLE_Type, &tol, NULL, SV_OPTIONAL, 0, { 0 } },
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
       != SV_OK ) {
    Tcl_SetResult( interp, "error checking surface", TCL_STATIC );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d %d",stats[0],stats[1]);
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE);

  return TCL_OK;
}

// ------------
// Geom_CleanCmd
// ------------

int Geom_CleanCmd( ClientData clientData, Tcl_Interp *interp,
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

  dst = sys_geom_Clean( (cvPolyData*)src );
  if ( dst == NULL ) {
    Tcl_AppendResult( interp, "error cleaning ", srcName, (char *)NULL );
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
       != SV_OK ) {
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
    { "-outarray", STRING_Type, &outArray, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-datatype", INT_Type, &dataType, NULL, SV_OPTIONAL, 0, { 0 } },
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
      return SV_OK;
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
       != SV_OK ) {
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
    { "-outarray", STRING_Type, &outArray, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-datatype", INT_Type, &dataType, NULL, SV_OPTIONAL, 0, { 0 } },
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
       != SV_OK ) {
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
    { "-outarray", STRING_Type, &outArray, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-datatype", INT_Type, &dataType, NULL, SV_OPTIONAL, 0, { 0 } },
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
      return SV_OK;
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
       != SV_OK ) {
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
    { "-outarray", STRING_Type, &outArray, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-datatype", INT_Type, &dataType, NULL, SV_OPTIONAL, 0, { 0 } },
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
      return SV_OK;
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
       != SV_OK ) {
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
    { "-target", DOUBLE_Type, &target, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-pointarray", STRING_Type, &pointArrayName, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-cellarray", STRING_Type, &cellArrayName, NULL, SV_OPTIONAL, 0, { 0 } },
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
       != SV_OK ) {
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
    { "-numiters", INT_Type, &numiters, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-relax", DOUBLE_Type, &relax, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-pointarray", STRING_Type, &pointArrayName, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-cellarray", STRING_Type, &cellArrayName, NULL, SV_OPTIONAL, 0, { 0 } },
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
       != SV_OK ) {
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
    { "-numiters", INT_Type, &numiters, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-constrainfactor", DOUBLE_Type, &constrainfactor, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-numcgsolves", INT_Type, &numcgsolves, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-pointarray", STRING_Type, &pointArrayName, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-cellarray", STRING_Type, &cellArrayName, NULL, SV_OPTIONAL, 0, { 0 } },
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
       != SV_OK ) {
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
// Geom_LocalLinearSubdivisionCmd
// ----------------
//
int Geom_LocalLinearSubdivisionCmd( ClientData clientData, Tcl_Interp *interp,
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
    { "-numiters", INT_Type, &numiters, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-pointarray", STRING_Type, &pointArrayName, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-cellarray", STRING_Type, &cellArrayName, NULL, SV_OPTIONAL, 0, { 0 } },
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
  if ( sys_geom_local_linear_subdivision( (cvPolyData*)src, (cvPolyData**)(&dst),
			  numiters,pointArrayName,cellArrayName)
       != SV_OK ) {
    Tcl_SetResult( interp, "running local subdivision operation", TCL_STATIC );
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
// Geom_LocalButterflySubdivisionCmd
// ----------------
//
int Geom_LocalButterflySubdivisionCmd( ClientData clientData, Tcl_Interp *interp,
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
    { "-numiters", INT_Type, &numiters, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-pointarray", STRING_Type, &pointArrayName, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-cellarray", STRING_Type, &cellArrayName, NULL, SV_OPTIONAL, 0, { 0 } },
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
  if ( sys_geom_local_butterfly_subdivision( (cvPolyData*)src, (cvPolyData**)(&dst),
			  numiters,pointArrayName,cellArrayName)
       != SV_OK ) {
    Tcl_SetResult( interp, "running local subdivision operation", TCL_STATIC );
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
// Geom_LocalLoopSubdivisionCmd
// ----------------
//
int Geom_LocalLoopSubdivisionCmd( ClientData clientData, Tcl_Interp *interp,
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
    { "-numiters", INT_Type, &numiters, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-pointarray", STRING_Type, &pointArrayName, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-cellarray", STRING_Type, &cellArrayName, NULL, SV_OPTIONAL, 0, { 0 } },
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
  if ( sys_geom_local_loop_subdivision( (cvPolyData*)src, (cvPolyData**)(&dst),
			  numiters,pointArrayName,cellArrayName)
       != SV_OK ) {
    Tcl_SetResult( interp, "running local subdivision operation", TCL_STATIC );
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
// Geom_LocalBlendCmd
// ----------------
//
int Geom_LocalBlendCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *Name;
  char *dstName;
  char *pointArrayName = 0;
  char *cellArrayName = 0;
  int numblenditers = 2;
  int numsubblenditers = 2;
  int numsubdivisioniters = 1;
  int numcgsmoothiters = 3;
  int numlapsmoothiters = 50;
  double targetdecimation = 0.01;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 10;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &Name, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-numblenditers", INT_Type, &numblenditers, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-numsubblenditers", INT_Type, &numsubblenditers, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-numsubdivisioniters", INT_Type, &numsubdivisioniters, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-numcgsmoothiters", INT_Type, &numcgsmoothiters, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-numlapsmoothiters", INT_Type, &numlapsmoothiters, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-targetdecimation", DOUBLE_Type, &targetdecimation, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-pointarray", STRING_Type, &pointArrayName, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-cellarray", STRING_Type, &cellArrayName, NULL, SV_OPTIONAL, 0, { 0 } },
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
  if ( sys_geom_local_blend( (cvPolyData*)src, (cvPolyData**)(&dst),
			  numblenditers,numsubblenditers,
			  numsubdivisioniters, numcgsmoothiters,
			  numlapsmoothiters, targetdecimation,
			  pointArrayName,cellArrayName)
       != SV_OK ) {
    Tcl_SetResult( interp, "running local blend operation", TCL_STATIC );
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

// -------------
// Geom_All_UnionCmd
// -------------

int Geom_All_UnionCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  int numSrcs;
  int interT;
  ARG_List srcList;
  char *dstName;
  cvRepositoryData *src;
  cvPolyData *dst;
  RepositoryDataT type;
  cvPolyData **srcs;
  cvSolidModel *geom;
  double tolerance = 1e-5;

  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-srclist", LIST_Type, &srcList, NULL, REQUIRED, 0, { 0 } },
    { "-intertype", INT_Type, &interT, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-tolerance", DOUBLE_Type, &tolerance, NULL, SV_OPTIONAL, 0, { 0 } },
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

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    delete [] srcs;
    return TCL_ERROR;
  }

  if ( sys_geom_all_union( srcs, numSrcs,interT,tolerance,(cvPolyData**)(&dst) )
       != SV_OK ) {
    Tcl_SetResult( interp, "poly manipulation error", TCL_STATIC );
    delete dst;
    delete [] srcs;
    delete geom;
    return TCL_ERROR;
  }

  vtkPolyData *dstPd;
  dstPd = dst->GetVtkPolyData();
  geom->SetVtkPolyDataObject(dstPd);
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete geom;
    delete [] srcs;
    delete dst;
    return TCL_ERROR;
  }

  // Make a new Tcl command:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}

// -------------
// Geom_Convert_NURBS_To_PolyCmd
// -------------

int Geom_Convert_NURBS_To_PolyCmd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  char *usage;
  int numFaces;
  int numIds;
  int *allids;
  int interT;
  ARG_List faceList;
  ARG_List idList;
  char *srcName;
  char *dstName;
  cvRepositoryData *face;
  cvPolyData *dst;
  cvRepositoryData *model;
  RepositoryDataT type;
  cvPolyData **faces;
  cvSolidModel *geom;

  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-model", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-facelist", LIST_Type, &faceList, NULL, REQUIRED, 0, { 0 } },
    { "-ids", LIST_Type, &idList, NULL, REQUIRED, 0, { 0 } },
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
  numFaces = faceList.argc;
  numIds = idList.argc;

  if (numFaces != numIds)
  {
      Tcl_AppendResult( interp, "Number of Ids must equal number of faces!\n");
      ARG_FreeListArgvs( table_size, arg_table );
      return TCL_ERROR;
  }

  // Foreach src obj, check that it is in the repository and of the
  // correct type (i.e. cvSolidModel).  Also build up the array of
  // cvSolidModel*'s to pass to cvSolidModel::MakeLoftedSurf.

  faces = new cvPolyData * [numFaces];

  for (int i = 0; i < numFaces; i++ ) {
    face = gRepository->GetObject( faceList.argv[i] );
    if ( face == NULL ) {
      Tcl_AppendResult( interp, "couldn't find object ", faceList.argv[i],
			(char *)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      delete [] faces;
      return TCL_ERROR;
    }
    type = face->GetType();
    if ( type != POLY_DATA_T ) {
      Tcl_AppendResult( interp, "object ", faceList.argv[i],
			" not of type cvPolyData", (char *)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      delete [] faces;
      return TCL_ERROR;
    }
    faces[i] = (cvPolyData *) face;
  }

  // Create an array, and for each id insert it into the array
  allids = new int[numIds];
  if ( ARG_ParseTclListStatic( interp, idList, INT_Type, allids, idList.argc, &numIds )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    delete [] faces;
    delete [] allids;
    return TCL_ERROR;
  }

  // Retrieve cvPolyData source:
  model = gRepository->GetObject( srcName );
  if ( model == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    delete [] faces;
    delete [] allids;
    return TCL_ERROR;
  }
  type = model->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, "object ", srcName, " not of type cvPolyData",
		      (char *)NULL );
    delete [] faces;
    delete [] allids;
    return TCL_ERROR;
  }

  // We're done with the src object names:
  ARG_FreeListArgvs( table_size, arg_table );

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    delete [] faces;
    delete [] allids;
    return TCL_ERROR;
  }

  if ( sys_geom_assign_ids_based_on_faces((cvPolyData *)model,faces,numFaces,allids,(cvPolyData**)(&dst) )
       != SV_OK ) {
    Tcl_SetResult( interp, "poly manipulation error", TCL_STATIC );
    delete dst;
    delete [] faces;
    delete [] allids;
    return TCL_ERROR;
  }

  delete [] faces;
  delete [] allids;

  vtkPolyData *dstPd;
  dstPd = dst->GetVtkPolyData();
  geom->SetVtkPolyDataObject(dstPd);
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  // Make a new Tcl command:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}

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
       != SV_OK ) {
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
       != SV_OK ) {
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

  if ( sys_geom_NumClosedLineRegions( (cvPolyData*)src, &num ) != SV_OK ) {
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
       != SV_OK ) {
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
       != SV_OK ) {
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
			       (cvPolyData**)(&dst) ) != SV_OK ) {
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
				  (cvPolyData**)(&dst) ) != SV_OK ) {
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
    { "-vecMtd", BOOL_Type, &vecMtd, NULL, SV_OPTIONAL, 0, { 0 } },
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
       != SV_OK ) {
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
       != SV_OK ) {
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

  if ( sys_geom_GetOrderedPts( (cvPolyData*)obj, &pts, &num ) != SV_OK ) {
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

  if ( sys_geom_WriteOrderedPts( (cvPolyData*)obj, fileName ) != SV_OK ) {
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

  if ( sys_geom_WriteLines( (cvPolyData*)obj, fileName ) != SV_OK ) {
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

  if ( sys_geom_PolysClosed( (cvPolyData*)src, &closed ) != SV_OK ) {
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

  if ( sys_geom_SurfArea( (cvPolyData*)src, &area ) != SV_OK ) {
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

  if ( sys_geom_getPolyCentroid( (cvPolyData*)src, centroid) != SV_OK ) {
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

  if ( sys_geom_PrintTriStats( (cvPolyData*)surf ) != SV_OK ) {
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

  if ( sys_geom_PrintSmallPolys( (cvPolyData*)src, sideTol ) != SV_OK ) {
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
       != SV_OK ) {
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

  if ( sys_geom_BBox( (cvPolyData*)obj, bbox ) != SV_OK ) {
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

  if ( sys_geom_Classify( (cvPolyData*)obj, pt, &ans ) != SV_OK ) {
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

  if ( sys_geom_PtInPoly( (cvPolyData*)obj, pt,usePrevPoly, &ans ) != SV_OK ) {
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
    { "-tol", DOUBLE_Type, &tol, NULL, SV_OPTIONAL, 0, { 0 } },
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
    { "-scale", DOUBLE_Type, &scale, NULL, SV_OPTIONAL, 0, { 0 } },
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
    { "-splineType", INT_Type, &splineType, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-bias", DOUBLE_Type, &bias, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-tension", DOUBLE_Type, &tension, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-continuity", DOUBLE_Type, &continuity, NULL, SV_OPTIONAL, 0, { 0 } },
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
       != SV_OK ) {
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

// ---------------------
// Geom_loftSolidWithNURBSCmd
// ---------------------

int Geom_loftSolidWithNURBSCmd( ClientData clientData, Tcl_Interp *interp,
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
  int uDegree = 2;
  int vDegree = 2;
  double uSpacing = 0.01;
  double vSpacing = 0.01;
  char *uKnotSpanType;
  char *vKnotSpanType;
  char *uParametricSpanType;
  char *vParametricSpanType;

  int table_size = 10;
  ARG_Entry arg_table[] = {
    { "-srclist", LIST_Type, &srcList, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-uDegree", INT_Type, &uDegree, NULL, REQUIRED, 0, { 0 } },
    { "-vDegree", INT_Type, &vDegree, NULL, REQUIRED, 0, { 0 } },
    { "-uSpacing", DOUBLE_Type, &uSpacing, NULL, REQUIRED, 0, { 0 } },
    { "-vSpacing", DOUBLE_Type, &vSpacing, NULL, REQUIRED, 0, { 0 } },
    { "-uKnotSpanType", STRING_Type, &uKnotSpanType, NULL, REQUIRED, 0, { 0 } },
    { "-vKnotSpanType", STRING_Type, &vKnotSpanType, NULL, REQUIRED, 0, { 0 } },
    { "-uParametricSpanType", STRING_Type, &uParametricSpanType, NULL, REQUIRED, 0, { 0 } },
    { "-vParametricSpanType", STRING_Type, &vParametricSpanType, NULL, REQUIRED, 0, { 0 } },
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

  vtkSmartPointer<vtkSVNURBSSurface> NURBSSurface =
    vtkSmartPointer<vtkSVNURBSSurface>::New();
  if ( sys_geom_loft_solid_with_nurbs(srcs, numSrcs, uDegree, vDegree, uSpacing,
                                      vSpacing, uKnotSpanType, vKnotSpanType,
                                      uParametricSpanType, vParametricSpanType,
                                      NURBSSurface,
			                                (cvPolyData**)(&dst) ) != SV_OK ) {
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

  if ( sys_geom_PolygonNormal( (cvPolyData*)obj, n ) != SV_OK ) {
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

  if ( sys_geom_AvgPt( (cvPolyData*)obj, pt ) != SV_OK ) {
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
    { "-file", STRING_Type, &filename, NULL, SV_OPTIONAL, 0, { 0 } },
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

  if (result == SV_OK) {
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
  if ( sys_geom_IntegrateSurface((cvPolyData*)obj, tensorType, nrm, &q) != SV_OK ) {
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
  if ( sys_geom_IntegrateSurface2((cvPolyData*)obj, tensorType, &q, &area) != SV_OK ) {
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
  if ( sys_geom_IntegrateEnergy((cvPolyData*)obj, rho, nrm, &energy) != SV_OK ) {
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
  if ( sys_geom_InterpolateScalar((cvPolyData*)obj, pt, &scalar) != SV_OK ) {
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
  if ( sys_geom_InterpolateVector((cvPolyData*)obj, pt, vect) != SV_OK ) {
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
  if ( sys_geom_IntersectWithLine((cvPolyData*)obj, p0, p1, intersect) != SV_OK ) {
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

  if ( sys_geom_mathPointData( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != SV_OK ) {
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

  if ( sys_geom_mathPointData( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != SV_OK ) {
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

  if ( sys_geom_mathPointData( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != SV_OK ) {
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

  if ( sys_geom_mathPointData( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != SV_OK ) {
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

  if ( sys_geom_Project( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != SV_OK ) {
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

  if ( sys_geom_IntegrateScalarSurf( (cvPolyData*)src, &flux ) != SV_OK ) {
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

  if ( sys_geom_IntegrateScalarThresh( (cvPolyData*)src, wssthresh, &flux, &area) != SV_OK ) {
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

  if ( sys_geom_ReplacePointData( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != SV_OK ) {
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
