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
#include "sv_UnstructuredGrid.h"
#include "sv_PolyData.h"
#include "sv_UnstructuredGrid.h"
#include "sv2_post_init.h"
#include "sv_sys_geom.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"

#include "sv2_ConvertVisFiles.h"
#include "sv2_CalculateWallShearStress.h"
#include "sv2_CalculateTKE.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "sv2_globals.h"

// Prototypes:

int Post_readVisMeshCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );
int Post_readVisResCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );
int Post_calcWallShearCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );
int Post_calcWallShearMeanCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );
int Post_calcWallShearPulseCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );
int Post_calcOSICmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );
int Post_calcAvgPointDataCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );
int Post_calcTKECmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );


// -------------
// Post_Init
// -------------

int Post_Init( Tcl_Interp *interp )
{
  Tcl_CreateCommand( interp, "post_readVisMesh", Post_readVisMeshCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "post_readVisRes", Post_readVisResCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "post_calcWallShear", Post_calcWallShearCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "post_calcWallShearMean", Post_calcWallShearMeanCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "post_calcWallShearPulse", Post_calcWallShearPulseCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "post_calcOSI", Post_calcOSICmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "post_calcAvgPointData", Post_calcAvgPointDataCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "post_calcTKE", Post_calcTKECmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  return TCL_OK;
}

// -----------------------
// Post_readVisMeshCmd
// -----------------------

int Post_readVisMeshCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *fileName;
  char *objName;
  cvUnstructuredGrid *dst;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &fileName, NULL, REQUIRED, 0, { 0 } },
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

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "object ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  cvConvertVisFiles *myconverter = new cvConvertVisFiles();

  // read in the mesh file
  if (myconverter->ReadVisMesh(fileName) == SV_ERROR) {
      delete myconverter;
      Tcl_AppendResult( interp, "error reading ", fileName, (char *)NULL);
      return TCL_ERROR;
  }

  dst = myconverter->GetGridObj();
  delete myconverter;

  if (dst == NULL) {
    Tcl_AppendResult( interp, "error getting obj ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( objName , dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// -----------------------
// Post_readVisResCmd
// -----------------------

int Post_readVisResCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *fileName = NULL;
  char *gridName = NULL;
  char *resultName = NULL;
  char *stressName = NULL;
  char *transportName = NULL;
  char *tractionName = NULL;
  char *displacementName = NULL;
  char *wssName = NULL;
  ARG_List tractionNodesList;
  tractionNodesList.argc = 0;

  int table_size = 8;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &fileName, NULL, REQUIRED, 0, { 0 } },
    { "-grid", STRING_Type, &gridName, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &resultName, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-stress", STRING_Type, &stressName, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-transport", STRING_Type, &transportName, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-traction", STRING_Type, &tractionName, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-displacement", STRING_Type, &displacementName, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-wss", STRING_Type, &wssName, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-traction_nodes", LIST_Type, &tractionNodesList, NULL, SV_OPTIONAL, 0, { 0 } },
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

  // check to make sure we have a valid grid object
  if (! gRepository->Exists( gridName ) ) {
    Tcl_AppendResult( interp, "object ", gridName, " does not exist",
		      (char *)NULL );
    return TCL_ERROR;
  }

  cvRepositoryData *grid = gRepository->GetObject( gridName );
  RepositoryDataT type = grid->GetType();
  if ( type != UNSTRUCTURED_GRID_T ) {
    Tcl_AppendResult( interp, gridName, " not of type UnstructuredGrid", (char *)NULL );
    return TCL_ERROR;
  }

  // check to make sure the results objs don't exist
  if (resultName != NULL) {
    if ( gRepository->Exists( resultName ) ) {
      Tcl_AppendResult( interp, "object ", resultName, " exists",
	  	      (char *)NULL );
      return TCL_ERROR;
    }
  }
  if (stressName != NULL) {
    if ( gRepository->Exists( stressName ) ) {
      Tcl_AppendResult( interp, "object ", stressName, " exists",
		      (char *)NULL );
      return TCL_ERROR;
    }
  }
  if (transportName != NULL) {
    if ( gRepository->Exists( transportName ) ) {
      Tcl_AppendResult( interp, "object ", transportName, " exists",
		      (char *)NULL );
      return TCL_ERROR;
    }
  }
  if (tractionName != NULL) {
    if ( gRepository->Exists( tractionName ) ) {
      Tcl_AppendResult( interp, "object ", tractionName, " exists",
		      (char *)NULL );
      return TCL_ERROR;
    }
  }
  if (displacementName != NULL) {
    if ( gRepository->Exists( displacementName ) ) {
      Tcl_AppendResult( interp, "object ", displacementName, " exists",
		      (char *)NULL );
      return TCL_ERROR;
    }
  }
  if (wssName != NULL) {
    if ( gRepository->Exists( wssName ) ) {
      Tcl_AppendResult( interp, "object ", wssName, " exists",
		      (char *)NULL );
      return TCL_ERROR;
    }
  }

  // parse the traction node list if it was specified
  int numTractionNodes = 0;
  int *tractionNodes = NULL;
  if (tractionNodesList.argc != 0) {

    numTractionNodes = tractionNodesList.argc;

    // the +1 is just dummy so I don't do new's for
    // an array of size 1 (maybe that causes problems...)
    // note: we let cvConvertVisFiles free the memory
    // for the traction nodes list
    tractionNodes = new int[numTractionNodes + 1];
    int numTerms = 0;

    if ( ARG_ParseTclListStatic( interp, tractionNodesList, INT_Type, tractionNodes, numTractionNodes, &numTerms )
         != TCL_OK ) {
        Tcl_SetResult( interp, "error in traction nodes list", TCL_VOLATILE );
        delete [] tractionNodes;
        ARG_FreeListArgvs( table_size, arg_table );
        return TCL_ERROR;
    }
    ARG_FreeListArgvs( table_size, arg_table );
  }

  cvConvertVisFiles *myconverter = new cvConvertVisFiles();

  myconverter->SetGrid((cvUnstructuredGrid*)grid);

  if (numTractionNodes > 0) {
       myconverter->SetTractionNodes(numTractionNodes,tractionNodes);
  }

  // read in the results file
  if (myconverter->ReadVisRes(fileName) == SV_ERROR) {
      delete myconverter;
      Tcl_AppendResult( interp, "error reading ", fileName, (char *)NULL);
      return TCL_ERROR;
  }

  cvPolyData *dst;

  if (resultName != NULL) {
    dst = NULL;
    dst = myconverter->GetResObj();
    if (dst == NULL) {
      delete myconverter;
      Tcl_AppendResult( interp, "error getting obj ", resultName, (char *)NULL );
      return TCL_ERROR;
    }
    if ( !( gRepository->Register( resultName , dst ) ) ) {
      delete myconverter;
      Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
      delete dst;
      return TCL_ERROR;
    }
  }

  if (transportName != NULL) {
    dst = NULL;
    dst = myconverter->GetTransportObj();
    if (dst == NULL) {
      delete myconverter;
      Tcl_AppendResult( interp, "error getting obj ", transportName, (char *)NULL );
      return TCL_ERROR;
    }
    if ( !( gRepository->Register( transportName , dst ) ) ) {
      delete myconverter;
      Tcl_AppendResult( interp, "error registering obj ", transportName,
		      " in repository", (char *)NULL );
      delete dst;
      return TCL_ERROR;
    }
  }

  if (stressName != NULL) {
    dst = NULL;
    dst = myconverter->GetStressObj();
    if (dst == NULL) {
      delete myconverter;
      Tcl_AppendResult( interp, "error getting obj ", stressName, (char *)NULL );
      return TCL_ERROR;
    }
    if ( !( gRepository->Register( stressName , dst ) ) ) {
      delete myconverter;
      Tcl_AppendResult( interp, "error registering obj ", stressName,
		      " in repository", (char *)NULL );
      delete dst;
      return TCL_ERROR;
    }
  }

  if (tractionName != NULL) {
    dst = NULL;
    dst = myconverter->GetTractionObj();
    if (dst == NULL) {
      delete myconverter;
      Tcl_AppendResult( interp, "error getting obj ", tractionName, (char *)NULL );
      return TCL_ERROR;
    }
    if ( !( gRepository->Register( tractionName , dst ) ) ) {
      delete myconverter;
      Tcl_AppendResult( interp, "error registering obj ", tractionName,
		      " in repository", (char *)NULL );
      delete dst;
      return TCL_ERROR;
    }
  }

  if (displacementName != NULL) {
    dst = NULL;
    dst = myconverter->GetDisplacementObj();
    if (dst == NULL) {
      delete myconverter;
      Tcl_AppendResult( interp, "error getting obj ", displacementName, (char *)NULL );
      return TCL_ERROR;
    }
    if ( !( gRepository->Register( displacementName , dst ) ) ) {
      delete myconverter;
      Tcl_AppendResult( interp, "error registering obj ", displacementName,
		      " in repository", (char *)NULL );
      delete dst;
      return TCL_ERROR;
    }
  }

  if (wssName != NULL) {
    dst = NULL;
    dst = myconverter->GetWSSObj();
    if (dst == NULL) {
      delete myconverter;
      Tcl_AppendResult( interp, "error getting obj ", wssName, (char *)NULL );
      return TCL_ERROR;
    }
    if ( !( gRepository->Register( wssName , dst ) ) ) {
      delete myconverter;
      Tcl_AppendResult( interp, "error registering obj ", wssName,
		      " in repository", (char *)NULL );
      delete dst;
      return TCL_ERROR;
    }
  }

  delete myconverter;

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


// -------------------------
// Post_calcWallShearCmd
// -------------------------

int Post_calcWallShearCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *surfaceMeshName = NULL;
  char *tensorsName = NULL;
  char *tractionsName = NULL;
  char *resultName = NULL;

  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-surfaceMesh", STRING_Type, &surfaceMeshName, NULL, REQUIRED, 0, { 0 } },
    { "-tensors", STRING_Type, &tensorsName, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-tractions", STRING_Type, &tractionsName, NULL, SV_OPTIONAL, 0, { 0 } },
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

  // Do work of command:

  if (tensorsName == NULL && tractionsName == NULL) {
     Tcl_AppendResult( interp, "must specify either tensors or tractions",
                       (char *)NULL );
     return TCL_ERROR;
  }

  if (tensorsName != NULL && tractionsName != NULL) {
     Tcl_AppendResult( interp, "cannot specify both tensors or tractions",
                       (char *)NULL );
     return TCL_ERROR;
  }

  // check to make sure we have a valid exterior surface mesh object
  if (! gRepository->Exists( surfaceMeshName ) ) {
    Tcl_AppendResult( interp, "object ", surfaceMeshName, " does not exist",
		      (char *)NULL );
    return TCL_ERROR;
  }

  cvRepositoryData *surfaceMesh = gRepository->GetObject( surfaceMeshName );
  RepositoryDataT type = surfaceMesh->GetType();

  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, surfaceMeshName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  cvRepositoryData *tensors;
  cvRepositoryData *tractions;

  if (tensorsName != NULL) {
    if (! gRepository->Exists( tensorsName ) ) {
      Tcl_AppendResult( interp, "object ", tensorsName, " does not exist",
		      (char *)NULL );
      return TCL_ERROR;
    }

    tensors = gRepository->GetObject( tensorsName );
    type = tensors->GetType();

    if ( type != POLY_DATA_T ) {
      Tcl_AppendResult( interp, tensorsName, " not of type cvPolyData", (char *)NULL );
      return TCL_ERROR;
    }

  }

  if (tractionsName != NULL) {
    if (! gRepository->Exists( tractionsName ) ) {
      Tcl_AppendResult( interp, "object ", tractionsName, " does not exist",
		      (char *)NULL );
      return TCL_ERROR;
    }

    tractions = gRepository->GetObject( tractionsName );
    type = tractions->GetType();

    if ( type != POLY_DATA_T ) {
      Tcl_AppendResult( interp, tractionsName, " not of type cvPolyData", (char *)NULL );
      return TCL_ERROR;
    }

  }

  // check to make sure the results objs don't exist
  if (resultName != NULL) {
    if ( gRepository->Exists( resultName ) ) {
      Tcl_AppendResult( interp, "object ", resultName, " exists",
	  	      (char *)NULL );
      return TCL_ERROR;
    }
  }

  cvCalculateWallShearStress *wallshear = new cvCalculateWallShearStress();

  wallshear->SetSurfaceMesh((cvPolyData*)surfaceMesh);
  if (tensorsName != NULL) {
    wallshear->SetTensors((cvPolyData*)tensors);
  } else {
    wallshear->SetTractions((cvPolyData*)tractions);
  }

  // read in the results file
  if (tensorsName != NULL) {
    if (wallshear->CalcWallShearFromStresses() == SV_ERROR) {
      delete wallshear;
      Tcl_AppendResult( interp, "error calculating wall shear ", (char *)NULL);
      return TCL_ERROR;
    }
  } else {
    if (wallshear->CalcWallShearFromTractions() == SV_ERROR) {
      delete wallshear;
      Tcl_AppendResult( interp, "error calculating wall shear ", (char *)NULL);
      return TCL_ERROR;
    }
  }

  cvPolyData *dst;
  dst = NULL;
  dst = wallshear->GetWallShear();
  if (dst == NULL) {
    delete wallshear;
    Tcl_AppendResult( interp, "error getting obj ", resultName, (char *)NULL );
    return TCL_ERROR;
  }
  if ( !( gRepository->Register( resultName , dst ) ) ) {
    delete wallshear;
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  delete wallshear;

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


// -----------------------------
// Post_calcWallShearMeanCmd
// -----------------------------

int Post_calcWallShearMeanCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;
  ARG_List shearPdList;
  cvRepositoryData *src;
  RepositoryDataT type;
  char *resultName = NULL;
  char *surfaceMeshName = NULL;
  double period = 0.0;
  int numPds = 0;
  int i;
  cvRepositoryData *surfaceMesh = NULL;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-shearPdList", LIST_Type, &shearPdList, NULL, REQUIRED, 0, { 0 } },
    { "-surfaceMesh", STRING_Type, &surfaceMeshName, NULL, SV_OPTIONAL, 0, { 0 } },
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

  // Do work of command:

  numPds = shearPdList.argc;

  // Foreach src obj, check that it is in the repository and of the
  // correct type (i.e. cvPolyData).  Also build up the array of
  // cvPolyData*'s.

  cvPolyData **shearPds = new cvPolyData*[numPds];

  for ( i = 0; i < numPds; i++ ) {
    src = gRepository->GetObject( shearPdList.argv[i] );
    if ( src == NULL ) {
      Tcl_AppendResult( interp, "couldn't find object ", shearPdList.argv[i],
			(char *)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      delete [] shearPds;
      return TCL_ERROR;
    }
    type = src->GetType();
    if ( type != POLY_DATA_T ) {
      Tcl_AppendResult( interp, "object ", shearPdList.argv[i],
			" not of type cvPolyData", (char *)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      delete [] shearPds;
      return TCL_ERROR;
    }
    shearPds[i] = (cvPolyData*)src;
  }

  // We're done with the src object names:
  ARG_FreeListArgvs( table_size, arg_table );

  // check to make sure we have a valid exterior surface mesh object
  if (surfaceMeshName != NULL) {
    if (! gRepository->Exists( surfaceMeshName ) ) {
       Tcl_AppendResult( interp, "object ", surfaceMeshName, " does not exist",
		      (char *)NULL );
       return TCL_ERROR;
     }

     surfaceMesh = gRepository->GetObject( surfaceMeshName );
     type = surfaceMesh->GetType();

     if ( type != POLY_DATA_T ) {
       Tcl_AppendResult( interp, surfaceMeshName, " not of type cvPolyData", (char *)NULL );
       return TCL_ERROR;
     }
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    Tcl_AppendResult( interp, "object ", resultName, " already exists",
		      (char *)NULL );
    delete [] shearPds;
    return TCL_ERROR;
  }

  cvCalculateWallShearStress *wallshear = new cvCalculateWallShearStress();

  // optionally copy the structure from the surface mesh
  if (surfaceMesh != NULL) {
      wallshear->SetSurfaceMesh((cvPolyData*)surfaceMesh);
  }

  cvPolyData *dst = NULL;
  dst = wallshear->CalcWallShearMean(numPds, shearPds);

  if (dst == NULL) {
    delete wallshear;
    delete [] shearPds;
    Tcl_AppendResult( interp, "error getting obj ", resultName, (char *)NULL );
    return TCL_ERROR;
  }
  if ( !( gRepository->Register( resultName , dst ) ) ) {
    delete wallshear;
    delete [] shearPds;
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  delete wallshear;
  delete [] shearPds;

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;

}


// ------------------------------
// Post_calcWallShearPulseCmd
// ------------------------------

int Post_calcWallShearPulseCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{

  char *usage;
  ARG_List shearPdList;
  cvRepositoryData *src;
  RepositoryDataT type;
  char *resultName = NULL;
  char *surfaceMeshName = NULL;
  double period = 0.0;
  int numPds = 0;
  int i;
  cvRepositoryData *surfaceMesh = NULL;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-shearPdList", LIST_Type, &shearPdList, NULL, REQUIRED, 0, { 0 } },
    { "-surfaceMesh", STRING_Type, &surfaceMeshName, NULL, SV_OPTIONAL, 0, { 0 } },
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

  // Do work of command:

  numPds = shearPdList.argc;

  // Foreach src obj, check that it is in the repository and of the
  // correct type (i.e. cvPolyData).  Also build up the array of
  // cvPolyData*'s.

  cvPolyData **shearPds = new cvPolyData*[numPds];

  for ( i = 0; i < numPds; i++ ) {
    src = gRepository->GetObject( shearPdList.argv[i] );
    if ( src == NULL ) {
      Tcl_AppendResult( interp, "couldn't find object ", shearPdList.argv[i],
			(char *)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      delete [] shearPds;
      return TCL_ERROR;
    }
    type = src->GetType();
    if ( type != POLY_DATA_T ) {
      Tcl_AppendResult( interp, "object ", shearPdList.argv[i],
			" not of type cvPolyData", (char *)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      delete [] shearPds;
      return TCL_ERROR;
    }
    shearPds[i] = (cvPolyData*)src;
  }

  // We're done with the src object names:
  ARG_FreeListArgvs( table_size, arg_table );

  // check to make sure we have a valid exterior surface mesh object
  if (surfaceMeshName != NULL) {
    if (! gRepository->Exists( surfaceMeshName ) ) {
       Tcl_AppendResult( interp, "object ", surfaceMeshName, " does not exist",
		      (char *)NULL );
       return TCL_ERROR;
     }

     surfaceMesh = gRepository->GetObject( surfaceMeshName );
     type = surfaceMesh->GetType();

     if ( type != POLY_DATA_T ) {
       Tcl_AppendResult( interp, surfaceMeshName, " not of type cvPolyData", (char *)NULL );
       return TCL_ERROR;
     }
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    Tcl_AppendResult( interp, "object ", resultName, " already exists",
		      (char *)NULL );
    delete [] shearPds;
    return TCL_ERROR;
  }

  cvCalculateWallShearStress *wallshear = new cvCalculateWallShearStress();

  // optionally copy the structure from the surface mesh
  if (surfaceMesh != NULL) {
      wallshear->SetSurfaceMesh((cvPolyData*)surfaceMesh);
  }

  cvPolyData *dst = NULL;
  dst = wallshear->CalcWallShearPulse(numPds, shearPds);

  if (dst == NULL) {
    delete wallshear;
    delete [] shearPds;
    Tcl_AppendResult( interp, "error getting obj ", resultName, (char *)NULL );
    return TCL_ERROR;
  }
  if ( !( gRepository->Register( resultName , dst ) ) ) {
    delete wallshear;
    delete [] shearPds;
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  delete wallshear;
  delete [] shearPds;

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;

}


// -------------------
// Post_calcOSICmd
// -------------------

int Post_calcOSICmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *meanPdName = NULL;
  char *pulsePdName = NULL;
  char *surfaceMeshName = NULL;
  char *resultName = NULL;
  cvRepositoryData *src1;
  cvRepositoryData *src2;
  RepositoryDataT type;
  cvRepositoryData *surfaceMesh = NULL;

  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-meanPd", STRING_Type, &meanPdName, NULL, REQUIRED, 0, { 0 } },
    { "-pulsePd", STRING_Type, &pulsePdName, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &resultName, NULL, REQUIRED, 0, { 0 } },
    { "-surfaceMesh", STRING_Type, &surfaceMeshName, NULL, SV_OPTIONAL, 0, { 0 } },
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

  // Foreach shear obj, check that it is in the repository and of the
  // correct type (i.e. cvPolyData).

  src1 = gRepository->GetObject( meanPdName );
  if ( src1 == NULL ) {
      Tcl_AppendResult( interp, "couldn't find object ", meanPdName,
			(char *)NULL );
      return TCL_ERROR;
  }
  type = src1->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, "object ", meanPdName,
			" not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  src2 = gRepository->GetObject( pulsePdName );
  if ( src2 == NULL ) {
      Tcl_AppendResult( interp, "couldn't find object ", pulsePdName,
			(char *)NULL );
      return TCL_ERROR;
  }
  type = src2->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, "object ", pulsePdName,
			" not of type cvPolyData", (char *)NULL );
      return TCL_ERROR;
  }

  // check to make sure we have a valid exterior surface mesh object
  if (surfaceMeshName != NULL) {
    if (! gRepository->Exists( surfaceMeshName ) ) {
       Tcl_AppendResult( interp, "object ", surfaceMeshName, " does not exist",
		      (char *)NULL );
       return TCL_ERROR;
     }

     surfaceMesh = gRepository->GetObject( surfaceMeshName );
     type = surfaceMesh->GetType();

     if ( type != POLY_DATA_T ) {
       Tcl_AppendResult( interp, surfaceMeshName, " not of type cvPolyData", (char *)NULL );
       return TCL_ERROR;
     }
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    Tcl_AppendResult( interp, "object ", resultName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  cvCalculateWallShearStress *wallshear = new cvCalculateWallShearStress();

  // optionally copy the structure from the surface mesh
  if (surfaceMesh != NULL) {
      wallshear->SetSurfaceMesh((cvPolyData*)surfaceMesh);
  }

  cvPolyData *dst = NULL;
  dst = wallshear->CalcOSI((cvPolyData*)src1,(cvPolyData*)src2);

  if (dst == NULL) {
    delete wallshear;
    Tcl_AppendResult( interp, "error getting obj ", resultName, (char *)NULL );
    return TCL_ERROR;
  }
  if ( !( gRepository->Register( resultName , dst ) ) ) {
    delete wallshear;
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  delete wallshear;

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;

}


// ----------------------------
// Post_calcAvgPointDataCmd
// ----------------------------

int Post_calcAvgPointDataCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;
  ARG_List inputPdList;
  cvRepositoryData *src;
  RepositoryDataT type;
  char *resultName = NULL;
  char *surfaceMeshName = NULL;
  double period = 0.0;
  int numPds = 0;
  int i;
  cvRepositoryData *surfaceMesh = NULL;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-inputPdList", LIST_Type, &inputPdList, NULL, REQUIRED, 0, { 0 } },
    { "-surfaceMesh", STRING_Type, &surfaceMeshName, NULL, SV_OPTIONAL, 0, { 0 } },
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

  // Do work of command:

  numPds = inputPdList.argc;

  // Foreach src obj, check that it is in the repository and of the
  // correct type (i.e. cvPolyData).  Also build up the array of
  // cvPolyData*'s.

  cvPolyData **inputPds = new cvPolyData*[numPds];

  for ( i = 0; i < numPds; i++ ) {
    src = gRepository->GetObject( inputPdList.argv[i] );
    if ( src == NULL ) {
      Tcl_AppendResult( interp, "couldn't find object ", inputPdList.argv[i],
			(char *)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      delete [] inputPds;
      return TCL_ERROR;
    }
    type = src->GetType();
    if ( type != POLY_DATA_T ) {
      Tcl_AppendResult( interp, "object ", inputPdList.argv[i],
			" not of type cvPolyData", (char *)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      delete [] inputPds;
      return TCL_ERROR;
    }
    inputPds[i] = (cvPolyData*)src;
  }

  // We're done with the src object names:
  ARG_FreeListArgvs( table_size, arg_table );

  // check to make sure we have a valid exterior surface mesh object
  if (surfaceMeshName != NULL) {
    if (! gRepository->Exists( surfaceMeshName ) ) {
       Tcl_AppendResult( interp, "object ", surfaceMeshName, " does not exist",
		      (char *)NULL );
       delete [] inputPds;
       return TCL_ERROR;
     }

     surfaceMesh = gRepository->GetObject( surfaceMeshName );
     type = surfaceMesh->GetType();

     if ( type != POLY_DATA_T ) {
       Tcl_AppendResult( interp, surfaceMeshName, " not of type cvPolyData", (char *)NULL );
       delete [] inputPds;
       return TCL_ERROR;
     }
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    Tcl_AppendResult( interp, "object ", resultName, " already exists",
		      (char *)NULL );
    delete [] inputPds;
    return TCL_ERROR;
  }

  cvCalculateWallShearStress *wallshear = new cvCalculateWallShearStress();

  // optionally copy the structure from the surface mesh
  if (surfaceMesh != NULL) {
      wallshear->SetSurfaceMesh((cvPolyData*)surfaceMesh);
  }

  cvPolyData *dst = NULL;
  dst = wallshear->CalcAvgPointData(numPds, inputPds);

  if (dst == NULL) {
    delete wallshear;
    delete [] inputPds;
    Tcl_AppendResult( interp, "error getting obj ", resultName, (char *)NULL );
    return TCL_ERROR;
  }
  if ( !( gRepository->Register( resultName , dst ) ) ) {
    delete wallshear;
    delete [] inputPds;
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  delete wallshear;
  delete [] inputPds;

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;

}


// -------------------
// Post_calcTKECmd
// -------------------

int Post_calcTKECmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;
  ARG_List inputPdList;
  cvRepositoryData *src;
  RepositoryDataT type;
  char *resultName = NULL;
  double period = 0.0;
  int numPds = 0;
  int i;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-inputPdList", LIST_Type, &inputPdList, NULL, REQUIRED, 0, { 0 } },
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

  // Do work of command:

  numPds = inputPdList.argc;

  // Foreach src obj, check that it is in the repository and of the
  // correct type (i.e. cvPolyData).  Also build up the array of
  // cvPolyData*'s.

  cvPolyData **inputPds = new cvPolyData*[numPds];

  for ( i = 0; i < numPds; i++ ) {
    src = gRepository->GetObject( inputPdList.argv[i] );
    if ( src == NULL ) {
      Tcl_AppendResult( interp, "couldn't find object ", inputPdList.argv[i],
			(char *)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      delete [] inputPds;
      return TCL_ERROR;
    }
    type = src->GetType();
    if ( type != POLY_DATA_T ) {
      Tcl_AppendResult( interp, "object ", inputPdList.argv[i],
			" not of type cvPolyData", (char *)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      delete [] inputPds;
      return TCL_ERROR;
    }
    inputPds[i] = (cvPolyData*)src;
  }

  // We're done with the src object names:
  ARG_FreeListArgvs( table_size, arg_table );

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    Tcl_AppendResult( interp, "object ", resultName, " already exists",
		      (char *)NULL );
    delete [] inputPds;
    return TCL_ERROR;
  }

  cvCalculateTKE *tke = new cvCalculateTKE();

  cvPolyData *dst = NULL;
  tke->SetInputData(numPds, inputPds);
  dst = tke->GetTKEPolyData();

  delete [] inputPds;
  delete tke;

  if (dst == NULL) {
    Tcl_AppendResult( interp, "error getting obj ", resultName, (char *)NULL );
    return TCL_ERROR;
  }
  if ( !( gRepository->Register( resultName , dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;

}


