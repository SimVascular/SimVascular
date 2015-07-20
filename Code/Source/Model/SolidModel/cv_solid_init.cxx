/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
 *
 * Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
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
#include "cv_solid_init.h"
#include "cvSolidModel.h"
#include "cv_arg.h"
#include "cv_misc_utils.h"
#include "cv_vtk_utils.h"
#include "cvPolyData.h"
#include "cvPolyDataSolid.h"
#include "cv_sys_geom.h"

#include "cvFactoryRegistrar.h"

#ifdef USE_DISCRETE_MODEL
#include "cvMeshSimDiscreteSolidModel.h"
int DiscreteUtils_Init();
#endif

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "cv_globals.h"

// Prototypes:
// -----------

// Solid
// -----

int Solid_RegistrarsListCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );


int Solid_PolyCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

int Solid_PolyPtsCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] );

int Solid_CircleCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] );

int Solid_EllipseCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] );

int Solid_Box2dCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] );

int Solid_Box3dCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] );

int Solid_SphereCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] );

int Solid_EllipsoidCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

int Solid_CylinderCmd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] );

int Solid_TruncatedConeCmd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] );

int Solid_TorusCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] );

int Solid_Poly3dSolidCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] );

int Solid_Poly3dSurfaceCmd( ClientData clientData, Tcl_Interp *interp,
			    int argc, CONST84 char *argv[] );

int Solid_ExtrudeZCmd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] );

int Solid_ExtrudeCmd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] );

int Solid_MakeApproxCurveLoopCmd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] );

int Solid_MakeInterpCurveLoopCmd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] );

int Solid_MakeLoftedSurfCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] );

int Solid_CapSurfToSolidCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] );

int Solid_IntersectCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

int Solid_UnionCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] );

int Solid_SubtractCmd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] );

int Solid_ReadNativeCmd( ClientData clientData, Tcl_Interp *interp,
			 int argc, CONST84 char *argv[] );

int Solid_CopyCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

int Solid_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] );

int Solid_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] );

void DeleteSolid( ClientData clientData );

int Solid_SetKernelCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

int Solid_GetKernelCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

int Geom_All_UnionCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

int Model_Convert_Para_To_PolyCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

int Solid_PrintKernelInfoCmd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] );


// Solid object methods
// --------------------

static int Solid_FindExtentMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );

static int Solid_FindCentroidMtd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] );

static int Solid_GetTopoDimMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );

static int Solid_GetSpatialDimMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] );

static int Solid_ClassifyPtMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );

static int Solid_DistanceMtd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] );

static int Solid_GetFaceNormalMtd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] );

static int Solid_TranslateMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] );

static int Solid_RotateMtd( ClientData clientData, Tcl_Interp *interp,
			    int argc, CONST84 char *argv[] );

static int Solid_ScaleMtd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

static int Solid_ReflectMtd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] );

static int Solid_Apply4x4Mtd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] );

static int Solid_PrintMtd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

static int Solid_CheckMtd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

static int Solid_WriteNativeMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

static int Solid_WriteVtkPolyDataMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] );

static int Solid_GetFacePolyDataMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

static int Solid_GetPolyDataMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

static int Solid_SetVtkPolyDataMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

static int Solid_GetDiscontinuitiesMtd( ClientData clientData,
					Tcl_Interp *interp,
					int argc, CONST84 char *argv[] );

static int Solid_GetAxialIsoparametricCurveMtd( ClientData clientData,
						Tcl_Interp *interp,
						int argc, CONST84 char *argv[] );

static int Solid_GetKernelMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] );

static int Solid_GetFaceIdsMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] );

static int Solid_GetBoundaryFacesMtd( ClientData clientData, Tcl_Interp *interp,
		               int argc, CONST84 char *argv[] );

static int Solid_GetRegionIdsMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] );

static int Solid_GetFaceAttrMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] );

static int Solid_SetFaceAttrMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] );

static int Solid_GetRegionAttrMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] );

static int Solid_SetRegionAttrMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] );

  // Label-related methods
  // ---------------------

static int Solid_GetLabelKeysMtd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] );

static int Solid_GetLabelMtd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] );

static int Solid_SetLabelMtd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] );

static int Solid_ClearLabelMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );

static int Solid_DeleteFacesMtd( ClientData clientData, Tcl_Interp *interp,
                                 int argc, CONST84 char *argv[] );

static int Solid_DeleteRegionMtd( ClientData clientData, Tcl_Interp *interp,
                                 int argc, CONST84 char *argv[] );

static int Solid_CreateEdgeBlendMtd( ClientData clientData, Tcl_Interp *interp,
                                 int argc, CONST84 char *argv[] );

static int Solid_CombineFacesMtd( ClientData clientData, Tcl_Interp *interp,
                                 int argc, CONST84 char *argv[] );

static int Solid_RemeshFaceMtd( ClientData clientData, Tcl_Interp *interp,
                                 int argc, CONST84 char *argv[] );
// Helper functions
// ----------------

static void PrintMethods( Tcl_Interp *interp );


// ----------
// Solid_Init
// ----------

int Solid_Init( Tcl_Interp *interp )
{
  // Associate the solid model registrar with the Tcl interpreter so it can be
  // retrieved by the DLLs.
  Tcl_SetAssocData( interp, "SolidModelRegistrar", NULL, &cvSolidModel::gRegistrar );

  Tcl_CreateCommand( interp, "solid_registrars", Solid_RegistrarsListCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  // Initialize
  cvSolidModel::gCurrentKernel = SM_KT_INVALID;

#ifdef USE_PARASOLID
  cvSolidModel::gCurrentKernel = SM_KT_PARASOLID;
#endif

  Tcl_CreateCommand( interp, "solid_poly", Solid_PolyCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_polyPts", Solid_PolyPtsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_circle", Solid_CircleCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_ellipse", Solid_EllipseCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_box2d", Solid_Box2dCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  Tcl_CreateCommand( interp, "solid_box3d", Solid_Box3dCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_sphere", Solid_SphereCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_ellipsoid", Solid_EllipsoidCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_cylinder", Solid_CylinderCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_truncatedCone", Solid_TruncatedConeCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_torus", Solid_TorusCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_poly3dSolid", Solid_Poly3dSolidCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_poly3dSurface", Solid_Poly3dSurfaceCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_extrudeZ", Solid_ExtrudeZCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL ); 
  Tcl_CreateCommand( interp, "solid_extrude", Solid_ExtrudeCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_makeApproxCurveLoop",
		     Solid_MakeApproxCurveLoopCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_makeInterpCurveLoop",
		     Solid_MakeInterpCurveLoopCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_makeLoftedSurf", Solid_MakeLoftedSurfCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_capSurfToSolid", Solid_CapSurfToSolidCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  Tcl_CreateCommand( interp, "solid_intersect", Solid_IntersectCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_union", Solid_UnionCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_subtract", Solid_SubtractCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  Tcl_CreateCommand( interp, "solid_readNative", Solid_ReadNativeCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_copy", Solid_CopyCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  Tcl_CreateCommand( interp, "solid_methods", Solid_ListMethodsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  Tcl_CreateCommand( interp, "solid_setKernel", Solid_SetKernelCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "solid_getKernel", Solid_GetKernelCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  Tcl_CreateCommand( interp, "geom_all_union", Geom_All_UnionCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "model_name_model_from_polydata_names", Model_Convert_Para_To_PolyCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  return TCL_OK;
}

// This routine is used for debugging the registrar/factory system.
int Solid_RegistrarsListCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  if ( argc != 1 ) {
    Tcl_SetResult( interp, "usage: registrars_list", TCL_STATIC );
    return TCL_ERROR;
  }
  cvFactoryRegistrar *solidModelRegistrar = 
    (cvFactoryRegistrar *) Tcl_GetAssocData( interp, "SolidModelRegistrar", NULL);

  char result[255];
  sprintf( result, "Solid model registrar ptr -> %p\n", solidModelRegistrar );
  Tcl_AppendElement( interp, result );
  for (int i = 0; i < 5; i++) {
    sprintf( result, "GetFactoryMethodPtr(%i) = %p\n", 
      i, (solidModelRegistrar->GetFactoryMethodPtr(i)));
    Tcl_AppendElement( interp, result );
  }

  return TCL_OK;
}



// ----------------
// Solid_PolyPtsCmd
// ----------------

// Previously (in shapesPkg.cxx) we had maintained a Tcl_HashTable of
// all solid objects.  This table was used to look up other objects
// for object methods which required not only the "this" object (whose
// pointer is retured by the clientData mechanism), but also
// additional operands which were named by their Tcl names.  These
// objects were looked-up in the Tcl_HashTable by that name to
// retrieve additional object pointers.

// Now, since we're using the cvRepository mechanism (which is itself a
// Tcl_HashTable), we can use the cvRepository's lookup mechanisms to
// find those operands.  That is, we can call cvRepository's
// GetObject(name) method to get back object pointers for use inside
// Tcl object method functions.

int Solid_PolyPtsCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  char *srcName;
  char *dstName;
  char *usage;
  cvRepositoryData *pd;
  RepositoryDataT type;
  cvSolidModel *geom;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
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

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Retrieve cvPolyData source:
  pd = gRepository->GetObject( srcName );
  if ( pd == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }
  type = pd->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, "object ", srcName, " not of type cvPolyData",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  // Create the polygon solid:
  if ( geom->MakePoly2dPts( (cvPolyData *)pd ) != CV_OK ) {
    Tcl_SetResult( interp, "polygon solid creation error",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  // Register the solid:
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Make a new Tcl command:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// -------------
// Solid_PolyCmd
// -------------

int Solid_PolyCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char *srcName;
  char *dstName;
  char *usage;
  cvRepositoryData *pd;
  RepositoryDataT type;
  cvSolidModel *geom;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
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

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Retrieve cvPolyData source:
  pd = gRepository->GetObject( srcName );
  if ( pd == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }
  type = pd->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, "object ", srcName, " not of type cvPolyData",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  // Create the polygon solid:
  if ( geom->MakePoly2d( (cvPolyData *)pd ) != CV_OK ) {
    Tcl_SetResult( interp, "polygon solid creation error",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  // Register the solid:
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Make a new Tcl command:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// ---------------
// Solid_CircleCmd
// ---------------

// % solid_circle -result /some/obj/name -r <radius> -x <x_ctr> -y <y_ctr>

int Solid_CircleCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  double radius;
  double ctr[2];
  cvSolidModel *geom;
  
  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-r", DOUBLE_Type, &radius, NULL, REQUIRED, 0, { 0 } },
    { "-x", DOUBLE_Type, &(ctr[0]), NULL, REQUIRED, 0, { 0 } },
    { "-y", DOUBLE_Type, &(ctr[1]), NULL, REQUIRED, 0, { 0 } },
  };

  // Generate a usage string (see comments for ARG_GenSyntaxStr for
  // details):
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

  // If no add'l args are given, then return usage:
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  // Parse argc, argv based on the given syntax:
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:
  
  if ( radius <= 0.0 ) {
    Tcl_SetResult( interp, "radius must be positive", TCL_STATIC );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "object ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  if ( geom->MakeCircle( radius, ctr ) != CV_OK ) {
    Tcl_SetResult( interp, "circle solid creation error", TCL_STATIC );
    delete geom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// ----------------
// Solid_EllipseCmd
// ----------------

int Solid_EllipseCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  double xr, yr;
  double ctr[2];
  cvSolidModel *geom;
  
  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-xr", DOUBLE_Type, &xr, NULL, REQUIRED, 0, { 0 } },
    { "-yr", DOUBLE_Type, &yr, NULL, REQUIRED, 0, { 0 } },
    { "-ctrx", DOUBLE_Type, &(ctr[0]), NULL, REQUIRED, 0, { 0 } },
    { "-ctry", DOUBLE_Type, &(ctr[1]), NULL, REQUIRED, 0, { 0 } },
  };

  // Generate a usage string (see comments for ARG_GenSyntaxStr for
  // details):
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

  // If no add'l args are given, then return usage:
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  // Parse argc, argv based on the given syntax:
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:
  
  if ( ( xr <= 0.0 ) || ( yr <= 0.0 ) ) {
    Tcl_SetResult( interp, "radii must be positive", TCL_STATIC );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "object ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  if ( geom->MakeEllipse( xr, yr, ctr ) != CV_OK ) {
    Tcl_SetResult( interp, "ellipse solid creation error", TCL_STATIC );
    delete geom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// --------------
// Solid_Box2dCmd
// --------------

// % solid_box2d -result /some/obj/name -h <double> -w <double> \
//       -xctr <double> -yctr <double>

int Solid_Box2dCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  double boxDims[2];
  double ctr[2];
  cvSolidModel *geom;
  
  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-h", DOUBLE_Type, &(boxDims[0]), NULL, REQUIRED, 0, { 0 } },
    { "-w", DOUBLE_Type, &(boxDims[1]), NULL, REQUIRED, 0, { 0 } },
    { "-xctr", DOUBLE_Type, &(ctr[0]), NULL, REQUIRED, 0, { 0 } },
    { "-yctr", DOUBLE_Type, &(ctr[1]), NULL, REQUIRED, 0, { 0 } },
  };

  // Generate a usage string (see comments for ARG_GenSyntaxStr for
  // details):
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

  // If no add'l args are given, then return usage:
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  // Parse argc, argv based on the given syntax:
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:
  
  if ( ( boxDims[0] <= 0.0 ) || ( boxDims[1] <= 0.0 ) ) {
    Tcl_SetResult( interp, "height and width must be positive", TCL_STATIC );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "object ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  if ( geom->MakeBox2d( boxDims, ctr ) != CV_OK ) {
    Tcl_SetResult( interp, "box solid creation error", TCL_STATIC );
    delete geom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// --------------
// Solid_Box3dCmd
// --------------

int Solid_Box3dCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  ARG_List dimsList, ctrList;
  double dims[3];
  double ctr[3];
  int ndims, nctr;
  cvSolidModel *geom;
  
  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-dims", LIST_Type, &dimsList, NULL, REQUIRED, 0, { 0 } },
    { "-ctr", LIST_Type, &ctrList, NULL, REQUIRED, 0, { 0 } }
  };

  // Generate a usage string (see comments for ARG_GenSyntaxStr for
  // details):
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

  // If no add'l args are given, then return usage:
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  // Parse argc, argv based on the given syntax:
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Parse coordinate lists:
  if ( ARG_ParseTclListStatic( interp, dimsList, DOUBLE_Type, dims, 3, &ndims )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, ctrList, DOUBLE_Type, ctr, 3, &nctr )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  ARG_FreeListArgvs( table_size, arg_table );

  // Do work of command:
  
  if ( ( dims[0] <= 0.0 ) || ( dims[1] <= 0.0 ) || ( dims[2] <= 0.0 ) ) {
    Tcl_SetResult( interp, "all dims must be positive", TCL_STATIC );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "object ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  if ( geom->MakeBox3d( dims, ctr ) != CV_OK ) {
    Tcl_SetResult( interp, "box solid creation error", TCL_STATIC );
    delete geom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// ---------------
// Solid_SphereCmd
// ---------------

int Solid_SphereCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  ARG_List ctrList;
  double ctr[3];
  double r;
  int nctr;
  cvSolidModel *geom;
  
  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-r", DOUBLE_Type, &r, NULL, REQUIRED, 0, { 0 } },
    { "-ctr", LIST_Type, &ctrList, NULL, REQUIRED, 0, { 0 } },
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

  // Parse coordinate lists:
  if ( ARG_ParseTclListStatic( interp, ctrList, DOUBLE_Type, ctr, 3, &nctr )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    Tcl_Free( (char *) ctrList.argv );
    return TCL_ERROR;
  }

  // Do work of command:

  // We no longer need ctrList's argv:
  Tcl_Free( (char *) ctrList.argv );
  
  if ( nctr != 3 ) {
    Tcl_SetResult( interp, "sphere requires a 3D center coordinate",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "object ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  if ( geom->MakeSphere( r, ctr ) != CV_OK ) {
    Tcl_SetResult( interp, "sphere solid creation error", TCL_STATIC );
    delete geom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// ------------------
// Solid_EllipsoidCmd
// ------------------

int Solid_EllipsoidCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  ARG_List ctrList;
  ARG_List rList;
  double ctr[3];
  double r[3];
  int nctr, nr;
  cvSolidModel *geom;
  
  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-r", LIST_Type, &rList, NULL, REQUIRED, 0, { 0 } },
    { "-ctr", LIST_Type, &ctrList, NULL, REQUIRED, 0, { 0 } },
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

  // Parse coordinate lists:
  if ( ARG_ParseTclListStatic( interp, ctrList, DOUBLE_Type, ctr, 3, &nctr )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, rList, DOUBLE_Type, r, 3, &nr )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  ARG_FreeListArgvs( table_size, arg_table );

  // Do work of command:

  if ( nctr != 3 ) {
    Tcl_SetResult( interp, "ellipsoid requires a 3D center coordinate",
		   TCL_STATIC );
    return TCL_ERROR;
  }
  if ( nr != 3 ) {
    Tcl_SetResult( interp, "ellipsoid requires a 3D radius vector",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "object ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  if ( geom->MakeEllipsoid( r, ctr ) != CV_OK ) {
    Tcl_SetResult( interp, "sphere solid creation error", TCL_STATIC );
    delete geom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// -----------------
// Solid_CylinderCmd
// -----------------

int Solid_CylinderCmd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  ARG_List ctrList, axisList;
  double ctr[3];
  double axis[3];
  double r, l;
  int nctr, naxis;
  cvSolidModel *geom;
  
  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-radius", DOUBLE_Type, &r, NULL, REQUIRED, 0, { 0 } },
    { "-length", DOUBLE_Type, &l, NULL, REQUIRED, 0, { 0 } },
    { "-ctr", LIST_Type, &ctrList, NULL, REQUIRED, 0, { 0 } },
    { "-axis", LIST_Type, &axisList, NULL, REQUIRED, 0, { 0 } },
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

  // Parse lists:
  if ( ARG_ParseTclListStatic( interp, ctrList, DOUBLE_Type, ctr, 3, &nctr )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, axisList, DOUBLE_Type, axis, 3, &naxis )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  ARG_FreeListArgvs( table_size, arg_table );

  // Do work of command:

  if ( nctr != 3 ) {
    Tcl_SetResult( interp, "cylinder requires a 3D center coordinate",
		   TCL_STATIC );
    return TCL_ERROR;
  }
  if ( naxis != 3 ) {
    Tcl_SetResult( interp, "cylinder requires a 3D axis vector", TCL_STATIC );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "object ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  if ( geom->MakeCylinder( r, l, ctr, axis ) != CV_OK ) {
    Tcl_SetResult( interp, "cylinder solid creation error", TCL_STATIC );
    delete geom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// ----------------------
// Solid_TruncatedConeCmd
// ----------------------

int Solid_TruncatedConeCmd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  ARG_List ptList, dirList;
  double pt[3];
  double dir[3];
  double r1, r2;
  int npt, ndir;
  cvSolidModel *geom;
  
  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-r1", DOUBLE_Type, &r1, NULL, REQUIRED, 0, { 0 } },
    { "-r2", DOUBLE_Type, &r2, NULL, REQUIRED, 0, { 0 } },
    { "-pt", LIST_Type, &ptList, NULL, REQUIRED, 0, { 0 } },
    { "-dir", LIST_Type, &dirList, NULL, REQUIRED, 0, { 0 } },
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

  // Parse lists:
  if ( ARG_ParseTclListStatic( interp, ptList, DOUBLE_Type, pt, 3, &npt )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, dirList, DOUBLE_Type, dir, 3, &ndir )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  ARG_FreeListArgvs( table_size, arg_table );

  // Do work of command:

  if ( npt != 3 ) {
    Tcl_SetResult( interp, "truncatedCone requires a 3D coordinate",
		   TCL_STATIC );
    return TCL_ERROR;
  }
  if ( ndir != 3 ) {
    Tcl_SetResult( interp, "truncatedCone requires a 3D direction vector", TCL_STATIC );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "object ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  if ( geom->MakeTruncatedCone( pt, dir, r1, r2 ) != CV_OK ) {
    Tcl_SetResult( interp, "cylinder solid creation error", TCL_STATIC );
    delete geom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// --------------
// Solid_TorusCmd
// --------------

int Solid_TorusCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  ARG_List ctrList, axisList;
  double ctr[3];
  double axis[3];
  double rmaj, rmin;
  int nctr, naxis;
  cvSolidModel *geom;
  
  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-rmaj", DOUBLE_Type, &rmaj, NULL, REQUIRED, 0, { 0 } },
    { "-rmin", DOUBLE_Type, &rmin, NULL, REQUIRED, 0, { 0 } },
    { "-ctr", LIST_Type, &ctrList, NULL, REQUIRED, 0, { 0 } },
    { "-axis", LIST_Type, &axisList, NULL, REQUIRED, 0, { 0 } },
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

  // Parse lists:
  if ( ARG_ParseTclListStatic( interp, ctrList, DOUBLE_Type, ctr, 3, &nctr )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, axisList, DOUBLE_Type, axis, 3, &naxis )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  ARG_FreeListArgvs( table_size, arg_table );

  // Do work of command:

  if ( nctr != 3 ) {
    Tcl_SetResult( interp, "torus requires a 3D center coordinate",
		   TCL_STATIC );
    return TCL_ERROR;
  }
  if ( naxis != 3 ) {
    Tcl_SetResult( interp, "torus requires a 3D axis vector", TCL_STATIC );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "object ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  if ( geom->MakeTorus( rmaj, rmin, ctr, axis ) != CV_OK ) {
    Tcl_SetResult( interp, "torus solid creation error", TCL_STATIC );
    delete geom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// --------------------
// Solid_Poly3dSolidCmd
// --------------------

int Solid_Poly3dSolidCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  char *srcName;
  char *facetMethodName;
  char *facetStr;
  SolidModel_FacetT facetMethod;
  cvRepositoryData *pd;
  RepositoryDataT type;
  cvSolidModel *geom;
  double angle = 0.0;

  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-facet", STRING_Type, &facetMethodName, NULL, REQUIRED, 0, { 0 } },
    { "-angle", DOUBLE_Type, &angle, NULL, GDSC_OPTIONAL, 0, { 0 } },
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

  facetMethod = SolidModel_FacetT_StrToEnum( facetMethodName );
  if ( facetMethod == SM_Facet_Invalid ) {
    facetStr = SolidModel_FacetT_EnumToStr( SM_Facet_Invalid );
    Tcl_SetResult( interp, facetStr, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve cvPolyData source:
  pd = gRepository->GetObject( srcName );
  if ( pd == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }
  type = pd->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, "object ", srcName, " not of type cvPolyData",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "object ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  if ( geom->SetPoly3dFacetMethod( facetMethod ) != CV_OK ) {
    Tcl_AppendResult( interp, "error selecting facet method ",
		      facetMethodName, (char *) NULL );
    delete geom;
    return TCL_ERROR;
  }
  if ( geom->MakePoly3dSolid( (cvPolyData*)pd , angle ) != CV_OK ) {
    Tcl_SetResult( interp, "polygonal solid creation error", TCL_STATIC );
    delete geom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// ----------------------
// Solid_Poly3dSurfaceCmd
// ----------------------

int Solid_Poly3dSurfaceCmd( ClientData clientData, Tcl_Interp *interp,
			    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  char *srcName;
  char *facetMethodName;
  char *facetStr;
  SolidModel_FacetT facetMethod;
  cvRepositoryData *pd;
  RepositoryDataT type;
  cvSolidModel *geom;
  
  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-facet", STRING_Type, &facetMethodName, NULL, REQUIRED, 0, { 0 } },
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

  facetMethod = SolidModel_FacetT_StrToEnum( facetMethodName );
  if ( facetMethod == SM_Facet_Invalid ) {
    facetStr = SolidModel_FacetT_EnumToStr( SM_Facet_Invalid );
    Tcl_SetResult( interp, facetStr, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve cvPolyData source:
  pd = gRepository->GetObject( srcName );
  if ( pd == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }
  type = pd->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, "object ", srcName, " not of type cvPolyData",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "object ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  if ( geom->SetPoly3dFacetMethod( facetMethod ) != CV_OK ) {
    Tcl_AppendResult( interp, "error selecting facet method ",
		      facetMethodName, (char *) NULL );
    delete geom;
    return TCL_ERROR;
  }
  if ( geom->MakePoly3dSurface( (cvPolyData*)pd ) != CV_OK ) {
    Tcl_SetResult( interp, "solid polygonal surface creation error",
		   TCL_STATIC );
    delete geom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// -----------------
// Solid_ExtrudeZCmd
// -----------------

int Solid_ExtrudeZCmd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;
  double dist;
  cvRepositoryData *src;
  RepositoryDataT type;
  cvSolidModel *geom;
  
  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-dist", DOUBLE_Type, &dist, NULL, REQUIRED, 0, { 0 } }
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

  // Retrieve cvSolidModel source:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }
  type = src->GetType();
  if ( type != SOLID_MODEL_T ) {
    Tcl_AppendResult( interp, "object ", srcName, " not of type cvSolidModel",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  if ( geom->ExtrudeZ( (cvSolidModel *)src, dist ) != CV_OK ) {
    Tcl_SetResult( interp, "error in solid extrusion", TCL_STATIC );
    delete geom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// ----------------
// Solid_ExtrudeCmd
// ----------------

int Solid_ExtrudeCmd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;
  double pt1[3],pt2[3];
  double **dist;
  ARG_List pt1List,pt2List;
  cvRepositoryData *src;
  RepositoryDataT type;
  cvSolidModel *geom;
  
  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-pt1", LIST_Type, &pt1List, NULL, REQUIRED, 0, { 0 } },
    { "-pt2", LIST_Type, &pt2List, NULL, REQUIRED, 0, { 0 } },
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

  // Retrieve cvSolidModel source:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }
  type = src->GetType();
  if ( type != SOLID_MODEL_T ) {
    Tcl_AppendResult( interp, "object ", srcName, " not of type cvSolidModel",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Parse pt1 & pt2
  int npt1,npt2;
  if ( ARG_ParseTclListStatic( interp, pt1List, DOUBLE_Type, pt1, 3, &npt1 )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, pt2List, DOUBLE_Type, pt2, 3, &npt2 )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  dist = new double*[2];
  dist[0] = &pt1[0];
  dist[1] = &pt2[0];
 
  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    delete dist;
    return TCL_ERROR;
  }

  if ( geom->Extrude( (cvSolidModel *)src, dist ) != CV_OK ) {
    Tcl_SetResult( interp, "error in solid extrusion", TCL_STATIC );
    delete geom;
    delete dist;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete geom;
    delete dist;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  delete dist;
  return TCL_OK;
}


// ----------------------------
// Solid_MakeApproxCurveLoopCmd
// ----------------------------

int Solid_MakeApproxCurveLoopCmd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;
  double tol;
  cvRepositoryData *src;
  RepositoryDataT type;
  cvSolidModel *geom;
  int closed = 1;
  
  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-src_pd", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-fit_tol", DOUBLE_Type, &tol, NULL, REQUIRED, 0, { 0 } },
    { "-closed", INT_Type, &closed, NULL, GDSC_OPTIONAL, 0, { 0 } },
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

  // Retrieve cvPolyData source:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, "object ", srcName, " not of type cvPolyData",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  if ( geom->MakeApproxCurveLoop( (cvPolyData *)src, tol, closed ) != CV_OK ) {
    Tcl_SetResult( interp, "error in curve loop construction", TCL_STATIC );
    delete geom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// ----------------------------
// Solid_MakeInterpCurveLoopCmd
// ----------------------------

int Solid_MakeInterpCurveLoopCmd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;

  cvRepositoryData *src;
  RepositoryDataT type;
  cvSolidModel *geom;
  int closed = 1;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-src_pd", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-closed", INT_Type, &closed, NULL, GDSC_OPTIONAL, 0, { 0 } },
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

  // Retrieve cvPolyData source:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, "object ", srcName, " not of type cvPolyData",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  if ( geom->MakeInterpCurveLoop( (cvPolyData *)src, closed ) != CV_OK ) {
    Tcl_SetResult( interp, "error in curve loop construction", TCL_STATIC );
    delete geom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// -----------------------
// Solid_MakeLoftedSurfCmd
// -----------------------

int Solid_MakeLoftedSurfCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] )
{
  char *usage;
  ARG_List srcList;
  char *dstName;
  cvRepositoryData *src;
  RepositoryDataT type;
  int numSrcs;
  cvSolidModel **srcs;
  cvSolidModel *geom;
  int i;
  
  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-srcs", LIST_Type, &srcList, NULL, REQUIRED, 0, { 0 } },
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

  numSrcs = srcList.argc;

  if ( numSrcs < 2 ) {
    Tcl_SetResult( interp, "need >= 2 curve cvSolidModel's to loft surface",
		   TCL_STATIC );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  // Foreach src obj, check that it is in the repository and of the
  // correct type (i.e. cvSolidModel).  Also build up the array of
  // cvSolidModel*'s to pass to cvSolidModel::MakeLoftedSurf.

  srcs = new cvSolidModel * [numSrcs];

  for ( i = 0; i < numSrcs; i++ ) {
    src = gRepository->GetObject( srcList.argv[i] );
    if ( src == NULL ) {
      Tcl_AppendResult( interp, "couldn't find object ", srcList.argv[i],
			(char *)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      delete [] srcs;
      return TCL_ERROR;
    }
    type = src->GetType();
    if ( type != SOLID_MODEL_T ) {
      Tcl_AppendResult( interp, "object ", srcList.argv[i],
			" not of type cvSolidModel", (char *)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      delete [] srcs;
      return TCL_ERROR;
    }
    srcs[i] = (cvSolidModel *) src;
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

  if ( geom->MakeLoftedSurf( srcs, numSrcs , dstName ) != CV_OK ) {
    Tcl_SetResult( interp, "error in curve loop construction", TCL_STATIC );
    delete [] srcs;
    delete geom;
    return TCL_ERROR;
  }

  // We're done with the srcs array:
  delete [] srcs;

  // Register the new solid:
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// -----------------------
// Solid_CapSurfToSolidCmd
// -----------------------

int Solid_CapSurfToSolidCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  RepositoryDataT type;
  cvSolidModel *geom;
  
  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
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

  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != SOLID_MODEL_T ) {
    Tcl_AppendResult( interp, "object ", srcName, " not of type cvSolidModel",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  if ( geom->CapSurfToSolid( (cvSolidModel *)src ) != CV_OK ) {
    Tcl_SetResult( interp, "error in cap / bound operation", TCL_STATIC );
    delete geom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// -------------------
// Solid_ReadNativeCmd
// -------------------

int Solid_ReadNativeCmd( ClientData clientData, Tcl_Interp *interp,
			 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName, *fileName;

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

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "object ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  cvSolidModel *geom;
  if (cvSolidModel::gCurrentKernel == SM_KT_PARASOLID ||
      cvSolidModel::gCurrentKernel == SM_KT_DISCRETE ||
      cvSolidModel::gCurrentKernel == SM_KT_POLYDATA) {

	  geom = cvSolidModel::DefaultInstantiateSolidModel( interp);

	  if ( geom == NULL ) {
	    return TCL_ERROR;
	  }

	  if ( geom->ReadNative( fileName ) != CV_OK ) {
	    Tcl_SetResult( interp, "file read error", TCL_STATIC );
	    delete geom;
	    return TCL_ERROR;
	  }

	  // Register the new solid:
	  if ( !( gRepository->Register( objName, geom ) ) ) {
	    Tcl_AppendResult( interp, "error registering obj ", objName,
			      " in repository", (char *)NULL );
	    delete geom;
	    return TCL_ERROR;
	  }

  }

  else {
    fprintf( stdout, "current kernel is not valid (%i)\n",cvSolidModel::gCurrentKernel);
    //Tcl_SetResult( interp, "current kernel is not valid", TCL_STATIC );
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );
  return TCL_OK;
}


// -------------
// Solid_CopyCmd
// -------------

int Solid_CopyCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;
  cvRepositoryData *srcGeom;
  RepositoryDataT src_t;
  cvSolidModel *dstGeom;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
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

  // Retrieve source:
  srcGeom = gRepository->GetObject( srcName );
  if ( srcGeom == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }
  src_t = gRepository->GetType( srcName );
  if ( src_t != SOLID_MODEL_T ) {
    Tcl_AppendResult( interp, "object ", srcName, " not of type cvSolidModel",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified destination object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  dstGeom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( dstGeom == NULL ) {
    return TCL_ERROR;
  }
  if ( dstGeom->Copy( *((cvSolidModel *)srcGeom) ) != CV_OK ) {
    Tcl_SetResult( interp, "cvSolidModel copy error", TCL_STATIC );
    delete dstGeom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( dstName, dstGeom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dstGeom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, dstGeom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)dstGeom, DeleteSolid );

  return TCL_OK;
}


// ------------------
// Solid_IntersectCmd
// ------------------

int Solid_IntersectCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *usage;
  char *resultName;
  char *smpName;
  char *smpStr;
  SolidModel_SimplifyT smp = SM_Simplify_All;  // DEFAULT ARG VALUE
  char *aName;
  char *bName;
  RepositoryDataT aType, bType;
  cvRepositoryData *gmA;
  cvRepositoryData *gmB;
  cvSolidModel *geom;
  
  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &resultName, NULL, REQUIRED, 0, { 0 } },
    { "-smp", STRING_Type, &smpName, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-a", STRING_Type, &aName, NULL, REQUIRED, 0, { 0 } },
    { "-b", STRING_Type, &bName, NULL, REQUIRED, 0, { 0 } },
  };

  // Generate a usage string (see comments for ARG_GenSyntaxStr for
  // details):
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

  // If no add'l args are given, then return usage:
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  // Parse argc, argv based on the given syntax:
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Parse the simplification flag if given:
  if ( arg_table[1].valid ) {
    smp = SolidModel_SimplifyT_StrToEnum( smpName );
    if ( smp == SM_Simplify_Invalid ) {
      smpStr = SolidModel_SimplifyT_EnumToStr( SM_Simplify_Invalid );
      Tcl_SetResult( interp, smpStr, TCL_VOLATILE );
      return TCL_ERROR;
    }
  }    

  // Do work of command:

  // Retrieve cvSolidModel operands:
  gmA = gRepository->GetObject( aName );
  if ( gmA == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", aName, (char *)NULL );
    return TCL_ERROR;
  }
  aType = gRepository->GetType( aName );
  if ( aType != SOLID_MODEL_T ) {
    Tcl_AppendResult( interp, "object ", aName, " not of type cvSolidModel",
		      (char *)NULL );
    return TCL_ERROR;
  }

  gmB = gRepository->GetObject( bName );
  if ( gmB == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", bName, (char *)NULL );
    return TCL_ERROR;
  }
  bType = gRepository->GetType( bName );
  if ( bType != SOLID_MODEL_T ) {
    Tcl_AppendResult( interp, "object ", bName, " not of type cvSolidModel",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  if ( geom->Intersect( (cvSolidModel*)gmA, (cvSolidModel*)gmB, smp ) != CV_OK ) {
    Tcl_SetResult( interp, "intersection error", TCL_STATIC );
    delete geom;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( resultName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)geom, DeleteSolid );

  return TCL_OK;
}


// --------------
// Solid_UnionCmd
// --------------

int Solid_UnionCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *resultName;
  char *smpName;
  char *smpStr;
  SolidModel_SimplifyT smp = SM_Simplify_All;  // DEFAULT ARG VALUE
  char *aName;
  char *bName;
  RepositoryDataT aType, bType;
  cvRepositoryData *gmA;
  cvRepositoryData *gmB;
  cvSolidModel *result;
  
  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &resultName, NULL, REQUIRED, 0, { 0 } },
    { "-smp", STRING_Type, &smpName, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-a", STRING_Type, &aName, NULL, REQUIRED, 0, { 0 } },
    { "-b", STRING_Type, &bName, NULL, REQUIRED, 0, { 0 } },
  };

  // Generate a usage string (see comments for ARG_GenSyntaxStr for
  // details):
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

  // If no add'l args are given, then return usage:
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  // Parse argc, argv based on the given syntax:
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Parse the simplification flag if given:
  if ( arg_table[1].valid ) {
    smp = SolidModel_SimplifyT_StrToEnum( smpName );
    if ( smp == SM_Simplify_Invalid ) {
      smpStr = SolidModel_SimplifyT_EnumToStr( SM_Simplify_Invalid );
      Tcl_SetResult( interp, smpStr, TCL_VOLATILE );
      return TCL_ERROR;
    }
  }    

  // Do work of command:

  // Retrieve cvSolidModel operands:
  gmA = gRepository->GetObject( aName );
  if ( gmA == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", aName, (char *)NULL );
    return TCL_ERROR;
  }
  aType = gRepository->GetType( aName );
  if ( aType != SOLID_MODEL_T ) {
    Tcl_AppendResult( interp, "object ", aName, " not of type cvSolidModel",
		      (char *)NULL );
    return TCL_ERROR;
  }

  gmB = gRepository->GetObject( bName );
  if ( gmB == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", bName, (char *)NULL );
    return TCL_ERROR;
  }
  bType = gRepository->GetType( bName );
  if ( bType != SOLID_MODEL_T ) {
    Tcl_AppendResult( interp, "object ", bName, " not of type cvSolidModel",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  result = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( result == NULL ) {
    return TCL_ERROR;
  }

  if ( result->Union( (cvSolidModel*)gmA, (cvSolidModel*)gmB, smp ) != CV_OK ) {
    Tcl_SetResult( interp, "union error", TCL_STATIC );
    delete result;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( resultName, result ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete result;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, result->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)result, DeleteSolid );

  return TCL_OK;
}


// -----------------
// Solid_SubtractCmd
// -----------------

int Solid_SubtractCmd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] )
{
  char *usage;
  char *resultName;
  char *smpName;
  char *smpStr;
  SolidModel_SimplifyT smp = SM_Simplify_All;  // DEFAULT ARG VALUE
  char *aName;
  char *bName;
  RepositoryDataT aType, bType;
  cvRepositoryData *gmA;
  cvRepositoryData *gmB;
  cvSolidModel *result;
  
  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &resultName, NULL, REQUIRED, 0, { 0 } },
    { "-smp", STRING_Type, &smpName, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-a", STRING_Type, &aName, NULL, REQUIRED, 0, { 0 } },
    { "-b", STRING_Type, &bName, NULL, REQUIRED, 0, { 0 } },
  };

  // Generate a usage string (see comments for ARG_GenSyntaxStr for
  // details):
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

  // If no add'l args are given, then return usage:
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  // Parse argc, argv based on the given syntax:
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Parse the simplification flag if given:
  if ( arg_table[1].valid ) {
    smp = SolidModel_SimplifyT_StrToEnum( smpName );
    if ( smp == SM_Simplify_Invalid ) {
      smpStr = SolidModel_SimplifyT_EnumToStr( SM_Simplify_Invalid );
      Tcl_SetResult( interp, smpStr, TCL_VOLATILE );
      return TCL_ERROR;
    }
  }    

  // Do work of command:

  // Retrieve cvSolidModel operands:
  gmA = gRepository->GetObject( aName );
  if ( gmA == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", aName, (char *)NULL );
    return TCL_ERROR;
  }
  aType = gRepository->GetType( aName );
  if ( aType != SOLID_MODEL_T ) {
    Tcl_AppendResult( interp, "object ", aName, " not of type cvSolidModel",
		      (char *)NULL );
    return TCL_ERROR;
  }

  gmB = gRepository->GetObject( bName );
  if ( gmB == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", bName, (char *)NULL );
    return TCL_ERROR;
  }
  bType = gRepository->GetType( bName );
  if ( bType != SOLID_MODEL_T ) {
    Tcl_AppendResult( interp, "object ", bName, " not of type cvSolidModel",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Instantiate the new solid:
  result = cvSolidModel::DefaultInstantiateSolidModel( interp );
  if ( result == NULL ) {
    return TCL_ERROR;
  }

  if ( result->Subtract( (cvSolidModel*)gmA, (cvSolidModel*)gmB, smp )
       != CV_OK ) {
    Tcl_SetResult( interp, "subtract error", TCL_STATIC );
    delete result;
    return TCL_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( resultName, result ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete result;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, result->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)result, DeleteSolid );

  return TCL_OK;
}


// --------------------
// Solid_ListMethodsCmd
// --------------------

int Solid_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  if ( argc != 1 ) {
    Tcl_AppendResult( interp, "usage: ", argv[0], (char *)NULL );
    return TCL_ERROR;
  }

  PrintMethods( interp );
  return TCL_OK;
}


// ---------------
// Solid_ObjectCmd
// ---------------

int Solid_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] )
{
  if ( argc == 1 ) {
    PrintMethods( interp );
    return TCL_OK;
  }

  if ( Tcl_StringMatch( argv[1], "GetClassName" ) ) {
    Tcl_SetResult( interp, "SolidModel", TCL_STATIC );
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "FindExtent" ) ) {
    if ( Solid_FindExtentMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "FindCentroid" ) ) {
    if ( Solid_FindCentroidMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetTopoDim" ) ) {
    if ( Solid_GetTopoDimMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetSpatialDim" ) ) {
    if ( Solid_GetSpatialDimMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "ClassifyPt" ) ) {
    if ( Solid_ClassifyPtMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "DeleteFaces" ) ) {
    if ( Solid_DeleteFacesMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "DeleteRegion" ) ) {
    if ( Solid_DeleteRegionMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "CreateEdgeBlend" ) ) {
    if ( Solid_CreateEdgeBlendMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "CombineFaces" ) ) {
    if ( Solid_CombineFacesMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "RemeshFace" ) ) {
    if ( Solid_RemeshFaceMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "Distance" ) ) {
    if ( Solid_DistanceMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetFaceNormal" ) ) {
    if ( Solid_GetFaceNormalMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "Translate" ) ) {
    if ( Solid_TranslateMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "Rotate" ) ) {
    if ( Solid_RotateMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "Scale" ) ) {
    if ( Solid_ScaleMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "Reflect" ) ) {
    if ( Solid_ReflectMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "Apply4x4" ) ) {
    if ( Solid_Apply4x4Mtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "Print" ) ) {
    if ( Solid_PrintMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "Check" ) ) {
    if ( Solid_CheckMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "WriteNative" ) ) {
    if ( Solid_WriteNativeMtd( clientData, interp,
			       argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "WriteVtkPolyData" ) ) {
    if ( Solid_WriteVtkPolyDataMtd( clientData, interp,
				    argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetFacePolyData" ) ) {
    if ( Solid_GetFacePolyDataMtd( clientData, interp,
			       argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetPolyData" ) ) {
    if ( Solid_GetPolyDataMtd( clientData, interp,
			       argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "SetVtkPolyData" ) ) {
    if ( Solid_SetVtkPolyDataMtd( clientData, interp,
			       argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetDiscontinuities" ) ) {
    if ( Solid_GetDiscontinuitiesMtd( clientData, interp,
				      argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetAxialIsoparametricCurve" ) ) {
    if ( Solid_GetAxialIsoparametricCurveMtd( clientData, interp,
					      argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetKernel" ) ) {
    if ( Solid_GetKernelMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetLabelKeys" ) ) {
    if ( Solid_GetLabelKeysMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetLabel" ) ) {
    if ( Solid_GetLabelMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "SetLabel" ) ) {
    if ( Solid_SetLabelMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "ClearLabel" ) ) {
    if ( Solid_ClearLabelMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetFaceIds" ) ) {
    if ( Solid_GetFaceIdsMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetBoundaryFaces" ) ) {
    if ( Solid_GetBoundaryFacesMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetRegionIds" ) ) {
    if ( Solid_GetRegionIdsMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetFaceAttr" ) ) {
    if ( Solid_GetFaceAttrMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "SetFaceAttr" ) ) {
    if ( Solid_SetFaceAttrMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetRegionAttr" ) ) {
    if ( Solid_GetRegionAttrMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "SetRegionAttr" ) ) {
    if ( Solid_SetRegionAttrMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else {
    Tcl_AppendResult( interp, "\"", argv[1],
		      "\" not a recognized cvSolidModel method", (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// -----------
// DeleteSolid
// -----------
// This is the deletion call-back for cvSolidModel object commands.

void DeleteSolid( ClientData clientData )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;

  gRepository->UnRegister( geom->GetName() );
}



// ------------------
// Solid_SetKernelCmd
// ------------------

int Solid_SetKernelCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *kernelName;
  char *usage;
  SolidModel_KernelT kernel;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-name", STRING_Type, &kernelName, NULL, REQUIRED, 0, { 0 } }
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
  kernel = SolidModel_KernelT_StrToEnum( kernelName );
  if ( kernel != SM_KT_INVALID ) {
    cvSolidModel::gCurrentKernel = kernel;
    Tcl_SetResult( interp, kernelName, TCL_VOLATILE );
    return TCL_OK;
  } else {
    Tcl_SetResult( interp, SolidModel_KernelT_EnumToStr( SM_KT_INVALID ),
		   TCL_VOLATILE );
    return TCL_ERROR;
  }
}


// ------------------
// Solid_GetKernelCmd
// ------------------

int Solid_GetKernelCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *kernelName;

  if ( argc != 1 ) {
    Tcl_AppendResult( interp, "usage: ", argv[0], (char *)NULL );
    return TCL_ERROR;
  }

  kernelName = SolidModel_KernelT_EnumToStr( cvSolidModel::gCurrentKernel );
  Tcl_SetResult( interp, kernelName, TCL_VOLATILE );

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
       != CV_OK ) {
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
// Geom_All_UnionCmd
// -------------

int Model_Convert_Para_To_PolyCmd( ClientData clientData, Tcl_Interp *interp,
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
       != CV_OK ) {
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


// ------------
// PrintMethods
// ------------

static void PrintMethods( Tcl_Interp *interp )
{
  tcl_printstr(interp, "Apply4x4\n");
  tcl_printstr(interp, "Check\n");
  tcl_printstr(interp, "ClassifyPt\n");
  tcl_printstr(interp, "ClearLabel\n");
  tcl_printstr(interp, "DeleteFaces\n");
  tcl_printstr(interp, "DeleteRegion\n");
  tcl_printstr(interp, "CreateEdgeBlend\n");
  tcl_printstr(interp, "CombineFaces\n");
  tcl_printstr(interp, "RemeshFace\n");
  tcl_printstr(interp, "Distance\n");
  tcl_printstr(interp, "FindCentroid\n");
  tcl_printstr(interp, "FindExtent\n");
  tcl_printstr(interp, "GetClassName\n");
  tcl_printstr(interp, "GetDiscontinuities\n");
  tcl_printstr(interp, "GetAxialIsoparametricCurve\n");
  tcl_printstr(interp, "GetFaceAttr\n");  
  tcl_printstr(interp, "GetFaceIds\n");
  tcl_printstr(interp, "GetBoundaryFaces\n");
  tcl_printstr(interp, "GetFaceNormal\n");
  tcl_printstr(interp, "GetFacePolyData\n");
  tcl_printstr(interp, "GetKernel\n");
  tcl_printstr(interp, "GetLabel\n");
  tcl_printstr(interp, "GetLabelKeys\n");
  tcl_printstr(interp, "GetPolyData\n");
  tcl_printstr(interp, "SetVtkPolyData\n");
  tcl_printstr(interp, "GetRegionIds\n");
  tcl_printstr(interp, "GetSpatialDim\n");
  tcl_printstr(interp, "GetTopoDim\n");
  tcl_printstr(interp, "Print\n");
  tcl_printstr(interp, "Reflect\n");
  tcl_printstr(interp, "Rotate\n");
  tcl_printstr(interp, "Scale\n");
  tcl_printstr(interp, "SetFaceAttr\n");
  tcl_printstr(interp, "SetLabel\n");
  tcl_printstr(interp, "Translate\n");
  tcl_printstr(interp, "WriteNative\n");
  tcl_printstr(interp, "WriteVtkPolyData\n");
  return;
}


// -------------------
// Solid_FindExtentMtd
// -------------------

static int Solid_FindExtentMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  int status;
  double extent;

  status = geom->FindExtent( &extent);
  if ( status == CV_OK ) {
    char rtnstr[255];
    rtnstr[0]='\0';
    sprintf( rtnstr, "%f", extent );
    Tcl_SetResult( interp, rtnstr, TCL_VOLATILE ); 
    return TCL_OK;
  } else {
    Tcl_AppendResult( interp, "FindExtent: error on object ",
		      geom->GetName(), (char *)NULL );
    return TCL_ERROR;
  }
}


// ---------------------
// Solid_FindCentroidMtd
// ---------------------

static int Solid_FindCentroidMtd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  int status;
  double centroid[3];
  char tmp[CV_STRLEN];
  int tdim;

  if ( geom->GetSpatialDim( &tdim ) != CV_OK ) {
    Tcl_AppendResult( interp, "couldn't get spatial dim of object ",
		      geom->GetName(), (char *)NULL );
    return TCL_ERROR;
  }
  if ( ( tdim != 2 ) && ( tdim != 3 ) ) {
    sprintf( tmp, "%d", tdim );
    Tcl_AppendResult( interp, "spatial dim ", tmp, " not supported",
		      (char *)NULL );
    return TCL_ERROR;
  }
  status = geom->FindCentroid( centroid );
  if ( status == CV_OK ) {
    sprintf( tmp, "%f", centroid[0] );
    Tcl_AppendElement( interp, tmp );
    sprintf( tmp, "%f", centroid[1] );
    Tcl_AppendElement( interp, tmp );
    if ( tdim == 3 ) {
      sprintf( tmp, "%f", centroid[2] );
      Tcl_AppendElement( interp, tmp );
    }
    return TCL_OK;
  } else {
    Tcl_AppendResult( interp, "FindCentroid: error on object ",
		      geom->GetName(), (char *)NULL );
    return TCL_ERROR;
  }
}


// -------------------
// Solid_GetTopoDimMtd
// -------------------

static int Solid_GetTopoDimMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  int status;
  int tdim;

  status = geom->GetTopoDim( &tdim );
  if ( status == CV_OK ) {
    char rtnstr[255];
    rtnstr[0]='\0';
    sprintf( rtnstr, "%d", tdim );
    Tcl_SetResult( interp, rtnstr, TCL_VOLATILE ); 
    return TCL_OK;
  } else {
    Tcl_AppendResult( interp, "GetTopoDim: error on object ",
		      geom->GetName(), (char *)NULL );
    return TCL_ERROR;
  }
}


// ----------------------
// Solid_GetSpatialDimMtd
// ----------------------

static int Solid_GetSpatialDimMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  int status;
  int sdim;

  status = geom->GetSpatialDim( &sdim );
  if ( status == CV_OK ) {
    char rtnstr[255];
    rtnstr[0]='\0';
    sprintf( rtnstr, "%d", sdim );
    Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );
    return TCL_OK;
  } else {
    Tcl_AppendResult( interp, "GetSpatialDim: error on object ",
		      geom->GetName(), (char *)NULL );
    return TCL_ERROR;
  }
}


// -------------------
// Solid_ClassifyPtMtd
// -------------------

static int Solid_ClassifyPtMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] )
{
  char *usage;
  cvSolidModel *geom = (cvSolidModel *)clientData;
  double x, y, z;
  int v = 0;
  int ans;
  int status;
  int tdim, sdim;

  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-x", DOUBLE_Type, &x, NULL, REQUIRED, 0, { 0 } },
    { "-y", DOUBLE_Type, &y, NULL, REQUIRED, 0, { 0 } },
    { "-z", DOUBLE_Type, &z, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-verbose", INT_Type, &v, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );

  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  geom->GetTopoDim( &tdim );
  geom->GetSpatialDim( &sdim );

  if ( arg_table[2].valid ) {
    status = geom->ClassifyPt( x, y, z, v, &ans );

  } else {
    if ( ( tdim == 2 ) && ( sdim == 2 ) ) {
      status = geom->ClassifyPt( x, y, v, &ans );
    } else {
      Tcl_AppendResult( interp, "object ", geom->GetName(),
			" must be of topological and spatial dimension 2",
			(char *)NULL );
      return TCL_ERROR;
    }
  }

  if ( status == CV_OK ) {
    char rtnstr[255];
    rtnstr[0]='\0';
    sprintf( rtnstr, "%d", ans );
    Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );
    return TCL_OK;
  } else {
    Tcl_AppendResult( interp, "ClassifyPt: error on object ",
		      geom->GetName(), (char *)NULL );
    return TCL_ERROR;
  }
}


// -----------------
// Solid_DistanceMtd
// -----------------

static int Solid_DistanceMtd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] )
{
  char *usage;
  cvSolidModel *geom = (cvSolidModel *)clientData;
  ARG_List posList;
  double pos[3];
  int npos;
  double upperLimit, dist;
  int sdim;
  int status;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-pos", LIST_Type, &posList, NULL, REQUIRED, 0, { 0 } },
    { "-upperLimit", DOUBLE_Type, &upperLimit, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    Tcl_Free( (char *) posList.argv );
    return TCL_ERROR;
  }

  if ( ARG_ParseTclListStatic( interp, posList, DOUBLE_Type, pos, 3, &npos )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    Tcl_Free( (char *) posList.argv );
    return TCL_ERROR;
  }

  // Do work of command:

  // We no longer need posList's argv:
  Tcl_Free( (char *) posList.argv );

  // Check validity of given pos:
  status = geom->GetSpatialDim( &sdim );
  if ( status != CV_OK ) {
    Tcl_AppendResult( interp, "error retrieving spatial dim of obj ",
		      geom->GetName(), (char *) NULL );
    return TCL_ERROR;
  }
  if ( ( sdim == 3 ) && ( npos != 3 ) ) {
    Tcl_SetResult( interp, "objects in 3 spatial dims require a "
		   "3D position", TCL_STATIC );
    return TCL_ERROR;
  } else if ( ( sdim == 2 ) && ( npos != 2 ) ) {
    Tcl_SetResult( interp, "objects in 2 spatial dims require a "
		   "2D position", TCL_STATIC );
    return TCL_ERROR;
  }

  status = geom->Distance( pos, upperLimit, &dist );

  if ( status == CV_OK ) {
    char rtnstr[255];
    rtnstr[0]='\0';
    sprintf( rtnstr, "%f", dist );
    Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );
    return TCL_OK;
  } else {
    Tcl_AppendResult( interp, "Distance: error on object ",
		      geom->GetName(), (char *)NULL );
    return TCL_ERROR;
  }
}


// ------------------
// Solid_TranslateMtd
// ------------------

static int Solid_TranslateMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] )
{
  char *usage;
  cvSolidModel *geom = (cvSolidModel *)clientData;
  ARG_List vecList;
  double vec[3];
  int nvec;
  int status;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-vec", LIST_Type, &vecList, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    Tcl_Free( (char *) vecList.argv );
    return TCL_ERROR;
  }

  if ( ARG_ParseTclListStatic( interp, vecList, DOUBLE_Type, vec, 3, &nvec )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    Tcl_Free( (char *) vecList.argv );
    return TCL_ERROR;
  }

  // Do work of command:

  // We no longer need vecList's argv:
  Tcl_Free( (char *) vecList.argv );

  status = geom->Translate( vec, nvec );

  if ( status != CV_OK ) {
    Tcl_AppendResult( interp, "Translate: error on object ", geom->GetName(),
		      (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// ---------------
// Solid_RotateMtd
// ---------------

static int Solid_RotateMtd( ClientData clientData, Tcl_Interp *interp,
			    int argc, CONST84 char *argv[] )
{
  char *usage;
  cvSolidModel *geom = (cvSolidModel *)clientData;
  ARG_List axisList;
  double axis[3];
  int naxis;
  double rad;
  int status;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-axis", LIST_Type, &axisList, NULL, REQUIRED, 0, { 0 } },
    { "-rad", DOUBLE_Type, &rad, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    Tcl_Free( (char *) axisList.argv );
    return TCL_ERROR;
  }

  if ( ARG_ParseTclListStatic( interp, axisList, DOUBLE_Type, axis, 3, &naxis )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    Tcl_Free( (char *) axisList.argv );
    return TCL_ERROR;
  }

  // Do work of command:

  // We no longer need axisList's argv:
  Tcl_Free( (char *) axisList.argv );

  status = geom->Rotate( axis, naxis, rad );

  if ( status != CV_OK ) {
    Tcl_AppendResult( interp, "Rotate: error on object ", geom->GetName(),
		      (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// --------------
// Solid_ScaleMtd
// --------------

static int Solid_ScaleMtd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  char *usage;
  cvSolidModel *geom = (cvSolidModel *)clientData;
  double factor;
  int status;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-factor", DOUBLE_Type, &factor, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  status = geom->Scale( factor );

  if ( status != CV_OK ) {
    Tcl_AppendResult( interp, "Scale: error on object ", geom->GetName(),
		      (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// ----------------
// Solid_ReflectMtd
// ----------------

static int Solid_ReflectMtd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] )
{
  char *usage;
  cvSolidModel *geom = (cvSolidModel *)clientData;
  ARG_List posList;
  ARG_List nrmList;
  double pos[3];
  double nrm[3];
  int npos;
  int nnrm;
  int status;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-pos", LIST_Type, &posList, NULL, REQUIRED, 0, { 0 } },
    { "-nrm", LIST_Type, &nrmList, NULL, REQUIRED, 0, { 0 } }
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  if ( ARG_ParseTclListStatic( interp, posList, DOUBLE_Type, pos, 3, &npos )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, nrmList, DOUBLE_Type, nrm, 3, &nnrm )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  ARG_FreeListArgvs( table_size, arg_table );
  if ( (npos != 3) || (nnrm != 3) ) {
    Tcl_SetResult( interp, "pos and nrm must be 3D vectors", TCL_STATIC );
    return TCL_ERROR;
  }

  // Do work of command:

  status = geom->Reflect( pos, nrm );

  if ( status != CV_OK ) {
    Tcl_AppendResult( interp, "Reflect: error on object ", geom->GetName(),
		      (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// -----------------
// Solid_Apply4x4Mtd
// -----------------

static int Solid_Apply4x4Mtd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] )
{
  char *usage;
  cvSolidModel *geom = (cvSolidModel *)clientData;
  ARG_List matList;
  ARG_List rowList[4];
  double mat[4][4];
  int n, i;
  int status;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-mat", LIST_Type, &matList, NULL, REQUIRED, 0, { 0 } }
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  // Parsing a 4x4 input list of lists is ugly.  The argv's of
  // rowList[i] are not currently getting cleaned up properly here.

  if ( ARG_ParseTclListStatic( interp, matList, LIST_Type, rowList, 4, &n )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  ARG_FreeListArgvs( table_size, arg_table );
  if ( n != 4 ) {
    Tcl_SetResult( interp, "mat must be 4x4", TCL_STATIC );
    return TCL_ERROR;
  }
  for ( i = 0; i < 4; i++ ) {
    if ( ARG_ParseTclListStatic( interp, rowList[i], DOUBLE_Type, mat[i], 4,
				 &n ) != TCL_OK ) {
      Tcl_SetResult( interp, usage, TCL_VOLATILE );
      return TCL_ERROR;
    }
    if ( n != 4 ) {
      Tcl_SetResult( interp, "mat must be 4x4", TCL_STATIC );
      return TCL_ERROR;
    }
  }

  // Do work of command:

  status = geom->Apply4x4( mat );

  if ( status != CV_OK ) {
    Tcl_AppendResult( interp, "Apply4x4: error on object ", geom->GetName(),
		      (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// --------------
// Solid_PrintMtd
// --------------

static int Solid_PrintMtd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;

  // Do work of command:
  geom->Print();
  return TCL_OK;
}


// --------------
// Solid_CheckMtd
// --------------

static int Solid_CheckMtd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  int nerr;

  // Do work of command:
  geom->Check( &nerr );
  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d", nerr );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );
  return TCL_OK;
}


// --------------------
// Solid_WriteNativeMtd
// --------------------

// $solid WriteNative -file foo.gm

static int Solid_WriteNativeMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  char *fn;
  int status;
  int file_version = 0;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &fn, NULL, REQUIRED, 0, { 0 } },
    { "-version", INT_Type, &file_version, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:
  status = geom->WriteNative( file_version , fn );
  if ( status != CV_OK ) {
    Tcl_AppendResult( interp, "error writing object ", geom->GetName(),
		      " to file ", fn, (char *)NULL );
    return TCL_ERROR;
  } else {
    return TCL_OK;
  }
}


// -------------------------
// Solid_WriteVtkPolyDataMtd
// -------------------------

static int Solid_WriteVtkPolyDataMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  char *fn;
  int status;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &fn, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:
  status = geom->WriteVtkPolyData( fn );
  if ( status != CV_OK ) {
    Tcl_AppendResult( interp, "error writing object ", geom->GetName(),
		      " to file ", fn, (char *)NULL );
    return TCL_ERROR;
  } else {
    return TCL_OK;
  }
}


// --------------------
// Solid_GetPolyDataMtd
// --------------------

static int Solid_GetPolyDataMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  char *resultName;
  cvPolyData *pd;
  double max_dist = -1.0;
  int useMaxDist = 0;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &resultName, NULL, REQUIRED, 0, { 0 } },
    { "-max_edge_size", DOUBLE_Type, &max_dist, NULL, GDSC_OPTIONAL, 0, { 0 } },  
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  if (max_dist > 0) {
      useMaxDist = 1;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    Tcl_AppendResult( interp, "object ", resultName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Get the cvPolyData:
  pd = geom->GetPolyData(useMaxDist, max_dist);
  if ( pd == NULL ) {
    Tcl_AppendResult( interp, "error getting cvPolyData for ", geom->GetName(),
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Register the result:
  if ( !( gRepository->Register( resultName, pd ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete pd;
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------------
// Solid_SetVtkPolyDataMtd
// ----------------------

static int Solid_SetVtkPolyDataMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  char *objName;
  RepositoryDataT type;
  cvRepositoryData *obj;
  vtkPolyData *pd;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Do work of command:
  type = gRepository->GetType( objName );
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, "\"", objName,
		      "\" must be of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  obj = gRepository->GetObject( objName );
  switch (type) {
  case POLY_DATA_T:
    pd = ((cvPolyData *)obj)->GetVtkPolyData();
    break;
  default:
    Tcl_SetResult( interp, "error in SetVtkPolyData", TCL_STATIC );
    return TCL_ERROR;
    break;
  }

  // set the vtkPolyData:
  if(!geom->SetVtkPolyDataObject(pd))
  {
    return TCL_ERROR;
  }

  return TCL_OK;
}


// ------------------------
// Solid_GetFacePolyDataMtd
// ------------------------

static int Solid_GetFacePolyDataMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  char *resultName;
  cvPolyData *pd;
  int faceid;
  double max_dist = -1.0;
  int useMaxDist = 0;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &resultName, NULL, REQUIRED, 0, { 0 } },
    { "-face", INT_Type, &faceid, NULL, REQUIRED, 0, { 0 } },
    { "-max_edge_size", DOUBLE_Type, &max_dist, NULL, GDSC_OPTIONAL, 0, { 0 } }, 
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  if (max_dist > 0) {
      useMaxDist = 1;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    Tcl_AppendResult( interp, "object ", resultName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Get the cvPolyData:
  pd = geom->GetFacePolyData(faceid,useMaxDist,max_dist);
  if ( pd == NULL ) {
    Tcl_AppendResult( interp, "error getting cvPolyData for ", geom->GetName(),
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Register the result:
  if ( !( gRepository->Register( resultName, pd ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete pd;
    return TCL_ERROR;
  }

  return TCL_OK;
}


// ----------------------
// Solid_GetFaceNormalMtd
// ----------------------

static int Solid_GetFaceNormalMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  int faceid;
  double u,v;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-face", INT_Type, &faceid, NULL, REQUIRED, 0, { 0 } },
    { "-u", DOUBLE_Type, &u, NULL, REQUIRED, 0, { 0 } },
    { "-v", DOUBLE_Type, &v, NULL, REQUIRED, 0, {0}}, 
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  double normal[3];

  if ( geom->GetFaceNormal(faceid,u,v,normal) == CV_ERROR ) {
    Tcl_AppendResult( interp, "error getting Normal for face. ", (char *)NULL );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%lf %lf %lf",normal[0],normal[1],normal[2]);
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );

  return TCL_OK;
}


// ---------------------------
// Solid_GetDiscontinuitiesMtd
// ---------------------------

static int Solid_GetDiscontinuitiesMtd( ClientData clientData,
					Tcl_Interp *interp,
					int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  char *resultName;
  cvPolyData *pd;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &resultName, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    Tcl_AppendResult( interp, "object ", resultName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Get the cvPolyData:
  pd = geom->GetDiscontinuities();
  if ( pd == NULL ) {
    Tcl_AppendResult( interp, "error getting discontinuities for ",
		      geom->GetName(), (char *)NULL );
    return TCL_ERROR;
  }

  // Register the result:
  if ( !( gRepository->Register( resultName, pd ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete pd;
    return TCL_ERROR;
  }

  return TCL_OK;
}


// -----------------------------------
// Solid_GetAxialIsoparametricCurveMtd
// -----------------------------------

static int Solid_GetAxialIsoparametricCurveMtd( ClientData clientData,
						Tcl_Interp *interp,
						int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  char *resultName;
  double prm;
  cvSolidModel *curve;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &resultName, NULL, REQUIRED, 0, { 0 } },
    { "-prm", DOUBLE_Type, &prm, NULL, REQUIRED, 0, { 0 } }
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    Tcl_AppendResult( interp, "object ", resultName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Get the isoparametric curve on the given surface at the given
  // parameter value:
  if ( ( prm < 0.0 ) || ( prm > 1.0 ) ) {
    Tcl_SetResult( interp, "parameter value must be between 0.0 and 1.0",
		   TCL_STATIC );
    return TCL_ERROR;
  }
  curve = geom->GetAxialIsoparametricCurve( prm );
  if ( curve == NULL ) {
    Tcl_AppendResult( interp, "error getting isoparametric curve for ",
		      geom->GetName(), (char *)NULL );
    return TCL_ERROR;
  }

  // Register the result:
  if ( !( gRepository->Register( resultName, curve ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete curve;
    return TCL_ERROR;
  }

  // Create object command for new solid:
  Tcl_SetResult( interp, curve->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), Solid_ObjectCmd,
		     (ClientData)curve, DeleteSolid );

  return TCL_OK;
}


// ------------------
// Solid_GetKernelMtd
// ------------------

static int Solid_GetKernelMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  SolidModel_KernelT kernelType;
  char *kernelName;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:
  kernelType = geom->GetKernelT();
  kernelName = SolidModel_KernelT_EnumToStr( kernelType );

  Tcl_SetResult( interp, kernelName, TCL_VOLATILE );

  if ( kernelType == SM_KT_INVALID ) {
    return TCL_ERROR;
  } else {
    return TCL_OK;
  }
}


// ---------------------
// Solid_GetLabelKeysMtd
// ---------------------

static int Solid_GetLabelKeysMtd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  int numKeys, i;
  char **keys;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:
  geom->GetLabelKeys( &numKeys, &keys );
  for (i = 0; i < numKeys; i++) {
    Tcl_AppendElement( interp, keys[i] );
  }
  delete [] keys;

  return TCL_OK;
}


// -----------------
// Solid_GetLabelMtd
// -----------------

static int Solid_GetLabelMtd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  char *key, *value;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-key", STRING_Type, &key, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  if ( ! geom->GetLabel( key, &value ) ) {
    Tcl_AppendResult( interp, "key ", key, " not found", (char *) NULL );
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, value, TCL_VOLATILE );
  return TCL_OK;
}


// -----------------
// Solid_SetLabelMtd
// -----------------

static int Solid_SetLabelMtd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  char *key, *value;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-key", STRING_Type, &key, NULL, REQUIRED, 0, { 0 } },
    { "-value", STRING_Type, &value, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  if ( ! geom->SetLabel( key, value ) ) {
    if ( geom->IsLabelPresent( key ) ) {
      Tcl_AppendResult( interp, "key ", key, " already in use",
			(char *) NULL );
      return TCL_ERROR;
    } else {
      Tcl_AppendResult( interp, "error setting label ", key, (char *) NULL );
      return TCL_ERROR;
    }
  }

  return TCL_OK;
}


// -------------------
// Solid_ClearLabelMtd
// -------------------

static int Solid_ClearLabelMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  char *key;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-key", STRING_Type, &key, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  if ( ! geom->IsLabelPresent( key ) ) {
    Tcl_AppendResult( interp, "key ", key, " not found", (char *) NULL );
    return TCL_ERROR;
  }
    
  geom->ClearLabel( key );

  return TCL_OK;
}


// -------------------
// Solid_GetFaceIdsMtd
// -------------------

static int Solid_GetFaceIdsMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  int numFaces;
  int *faces;
  char facestring[256];

  int status = geom->GetFaceIds( &numFaces, &faces);
  if ( status == CV_OK ) {
    if (numFaces == 0) return TCL_OK;
    for (int i = 0; i < numFaces; i++) {
	  sprintf(facestring, "%i", faces[i]);
      Tcl_AppendElement ( interp, facestring);
	  facestring[0]='\n';
    }
    delete faces; 
    return TCL_OK;
  } else {
    Tcl_AppendResult( interp, "GetFaceIds: error on object ",
		      geom->GetName(), (char *)NULL );
    return TCL_ERROR;
  }
}

// -------------------
// Solid_GetBoundaryFacesMtd
// -------------------
//
int Solid_GetBoundaryFacesMtd( ClientData clientData, Tcl_Interp *interp,
		int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  double angle = 0.0;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-angle", DOUBLE_Type, &angle, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    fprintf(stderr,"As suspected\n");
    return TCL_ERROR;
  }

  int status = geom->GetBoundaryFaces(angle);
  if ( status == CV_OK ) {
    return TCL_OK;
  } else {
    Tcl_AppendResult( interp, "GetBoundaryFaces: error on object ",
		      geom->GetName(), (char *)NULL );
    return TCL_ERROR;
  }
}

// ---------------------
// Solid_GetRegionIdsMtd
// ---------------------

static int Solid_GetRegionIdsMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  int numRegions;
  int *regions;
  char regionstring[256];

  int status = geom->GetRegionIds( &numRegions, &regions);
  if ( status == CV_OK ) {
    if (numRegions == 0) return TCL_OK;
    for (int i = 0; i < numRegions; i++) {
	  sprintf(regionstring, "%i", regions[i]);
      Tcl_AppendElement ( interp, regionstring);
	  regionstring[0]='\n';
    }
    delete regions; 
    return TCL_OK;
  } else {
    Tcl_AppendResult( interp, "GetRegionIds: error on object ",
		      geom->GetName(), (char *)NULL );
    return TCL_ERROR;
  }
}


// --------------------
// Solid_GetFaceAttrMtd
// --------------------

static int Solid_GetFaceAttrMtd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  char *key, *value;
  int faceid;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-attr", STRING_Type, &key, NULL, REQUIRED, 0, { 0 } },
    { "-faceId", INT_Type, &faceid, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  if ( ! geom->GetFaceAttribute( key, faceid, &value ) ) {
    Tcl_AppendResult( interp, "attribute ", key, " not found", (char *) NULL );
    return TCL_ERROR;
  }

  Tcl_AppendResult( interp, value, (char *) NULL );

  return TCL_OK;
}


// --------------------
// Solid_SetFaceAttrMtd
// --------------------

static int Solid_SetFaceAttrMtd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  char *key, *value;
  int faceid;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-attr", STRING_Type, &key, NULL, REQUIRED, 0, { 0 } },
    { "-value", STRING_Type, &value, NULL, REQUIRED, 0, { 0 } },
    { "-faceId", INT_Type, &faceid, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  if ( ! geom->SetFaceAttribute( key, faceid, value ) ) {
    Tcl_AppendResult( interp, "attribute ", key, " could not be set", (char *) NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// ----------------------
// Solid_GetRegionAttrMtd
// ----------------------

static int Solid_GetRegionAttrMtd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  char *key, *value;
  int regionid;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-attr", STRING_Type, &key, NULL, REQUIRED, 0, { 0 } },
    { "-regionId", INT_Type, &regionid, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  if ( ! geom->GetRegionAttribute( key, regionid, &value ) ) {
    Tcl_AppendResult( interp, "attribute ", key, " not found", (char *) NULL );
    return TCL_ERROR;
  }

  Tcl_AppendResult( interp, value, (char *) NULL );

  return TCL_OK;
}


// ----------------------
// Solid_SetRegionAttrMtd
// ----------------------

static int Solid_SetRegionAttrMtd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] )
{
  cvSolidModel *geom = (cvSolidModel *)clientData;
  char *usage;
  char *key, *value;
  int regionid;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-attr", STRING_Type, &key, NULL, REQUIRED, 0, { 0 } },
    { "-value", STRING_Type, &value, NULL, REQUIRED, 0, { 0 } },
    { "-regionId", INT_Type, &regionid, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  if ( ! geom->SetRegionAttribute( key, regionid, value ) ) {
    Tcl_AppendResult( interp, "attribute ", key, " could not be set", (char *) NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// --------------------
// Solid_DeleteFacesMtd
// --------------------

static int Solid_DeleteFacesMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] )
{
  char *usage;
  cvSolidModel *geom = (cvSolidModel *)clientData;
  ARG_List faceList;
  int status;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-faces", LIST_Type, &faceList, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  if (faceList.argc == 0) {
      ARG_FreeListArgvs( table_size, arg_table);
      return CV_OK;
  }

  int nfaces = 0;
  int *faces = new int[faceList.argc];

  if ( ARG_ParseTclListStatic( interp, faceList, INT_Type, faces, faceList.argc, &nfaces )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  // Do work of command:
  ARG_FreeListArgvs( table_size, arg_table );

  status = geom->DeleteFaces( nfaces, faces );

  delete [] faces;

  if ( status != CV_OK ) {
    Tcl_AppendResult( interp, "DeleteFaces: error on object ", geom->GetName(),
		      (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}

// --------------------
// Solid_DeleteRegionMtd
// --------------------

static int Solid_DeleteRegionMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] )
{
  char *usage;
  cvSolidModel *geom = (cvSolidModel *)clientData;
  int regionid;
  int status;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-regionid", INT_Type, &regionid, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  // Do work of command:

  status = geom->DeleteRegion( regionid );

  if ( status != CV_OK ) {
    Tcl_AppendResult( interp, "DeleteRegion: error on object ", geom->GetName(),
		      (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// ------------------------
// Solid_CreateEdgeBlendMtd
// ------------------------

static int Solid_CreateEdgeBlendMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] )
{
  char *usage;
  cvSolidModel *geom = (cvSolidModel *)clientData;
  int status;
  int faceA;
  int faceB;
  double radius;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-faceA", INT_Type, &faceA, NULL, REQUIRED, 0, { 0 } },
    { "-faceB", INT_Type, &faceB, NULL, REQUIRED, 0, { 0 } },
    { "-radius", DOUBLE_Type, &radius, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  status = geom->CreateEdgeBlend( faceA, faceB, radius );

  if ( status != CV_OK ) {
    Tcl_AppendResult( interp, "CreateEdgeBlend: error on object ", geom->GetName(),
		      (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}

static int Solid_CombineFacesMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] )
{
  char *usage;
  cvSolidModel *geom = (cvSolidModel *)clientData;
  int status;
  int faceid1;
  int faceid2;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-targetface", INT_Type, &faceid1, NULL, REQUIRED, 0, { 0 } },
    { "-loseface", INT_Type, &faceid2, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  status = geom->CombineFaces( faceid1, faceid2);

  if ( status != CV_OK ) {
    Tcl_AppendResult( interp, "Combine Faces: Error", geom->GetName(),
		      (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}

static int Solid_RemeshFaceMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] )
{
  char *usage;
  cvSolidModel *geom = (cvSolidModel *)clientData;
  int status;
  ARG_List excludeList;
  double size;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-excludelist", LIST_Type, &excludeList, NULL, REQUIRED, 0, { 0 } },
    { "-size", DOUBLE_Type, &size, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  if (excludeList.argc == 0) {
      ARG_FreeListArgvs( table_size, arg_table);
      return CV_OK;
  }

  int nfaces = 0;
  int *faces = new int[excludeList.argc];

  if ( ARG_ParseTclListStatic( interp, excludeList, INT_Type, faces, excludeList.argc, &nfaces )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }

  status = geom->RemeshFace( nfaces, faces, size);

  delete [] faces;

  if ( status != CV_OK ) {
    Tcl_AppendResult( interp, "Remesh Face: Error", geom->GetName(),
		      (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}
