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
#include "sv_vmtk_utils_init.h"
#include "sv_vmtk_utils.h"
#include "sv_SolidModel.h"
#include "sv_vtk_utils.h"
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

#ifdef SV_USE_VMTK
int Geom_CenterlinesCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int Geom_DistanceToCenterlinesCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int Geom_GroupPolyDataCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int Geom_SeparateCenterlinesCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int Geom_MergeCenterlinesCmd( ClientData clientData, Tcl_Interp *interp,
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
// Vmtkutils_Init
// ---------

int Vmtkutils_Init( Tcl_Interp *interp )
{
#ifdef SV_USE_VMTK
  Tcl_CreateCommand( interp, "geom_centerlines", Geom_CenterlinesCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_grouppolydata", Geom_GroupPolyDataCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_distancetocenterlines", Geom_DistanceToCenterlinesCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_separatecenterlines", Geom_SeparateCenterlinesCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "geom_mergecenterlines", Geom_MergeCenterlinesCmd,
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

#ifdef SV_USE_VMTK
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
      return SV_OK;
  }

  if (targetList.argc == 0) {
      ARG_FreeListArgvs( table_size, arg_table);
      return SV_OK;
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
       != SV_OK ) {
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

int Geom_GroupPolyDataCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *geomName;
  char *linesName;
  char *groupedName;
  cvRepositoryData *geomSrc;
  cvRepositoryData *linesSrc;
  cvRepositoryData *groupedDst = NULL;
  RepositoryDataT type;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &geomName, NULL, REQUIRED, 0, { 0 } },
    { "-lines", STRING_Type, &linesName, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &groupedName, NULL, REQUIRED, 0, { 0 } },
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

  if ( sys_geom_grouppolydata( (cvPolyData*)geomSrc, (cvPolyData*)linesSrc, (cvPolyData**)(&groupedDst) )
       != SV_OK ) {
    Tcl_SetResult( interp, "error getting grouped polydata", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( groupedName, groupedDst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", groupedName,
		      " in repository", (char *)NULL );
    delete groupedDst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, groupedDst->GetName(), TCL_VOLATILE );
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
       != SV_OK ) {
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

int Geom_SeparateCenterlinesCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *linesName;
  char *separateName;
  cvRepositoryData *linesSrc;
  cvRepositoryData *separateDst = NULL;
  RepositoryDataT type;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-lines", STRING_Type, &linesName, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &separateName, NULL, REQUIRED, 0, { 0 } },
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

  if ( sys_geom_separatecenterlines( (cvPolyData*)linesSrc, (cvPolyData**)(&separateDst) )
       != SV_OK ) {
    Tcl_SetResult( interp, "error grouping centerlines", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( separateName, separateDst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", separateName,
		      " in repository", (char *)NULL );
    delete separateDst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, separateDst->GetName(), TCL_VOLATILE );
//  Tcl_SetResult( interp, voronoiDst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}

int Geom_MergeCenterlinesCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *linesName;
  char *mergeName;
  int mergeblanked = 1;
  cvRepositoryData *linesSrc;
  cvRepositoryData *mergeDst = NULL;
  RepositoryDataT type;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-lines", STRING_Type, &linesName, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &mergeName, NULL, REQUIRED, 0, { 0 } },
    { "-mergeblanked", INT_Type, &mergeblanked, NULL, SV_OPTIONAL, 0, { 0 } },
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

  if ( sys_geom_mergecenterlines( (cvPolyData*)linesSrc, mergeblanked, (cvPolyData**)(&mergeDst) )
       != SV_OK ) {
    Tcl_SetResult( interp, "error merging centerlines", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( mergeName, mergeDst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", mergeName,
		      " in repository", (char *)NULL );
    delete mergeDst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, mergeDst->GetName(), TCL_VOLATILE );
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

  if ( sys_geom_cap_for_centerlines( (cvPolyData*)geomSrc, (cvPolyData**)(&cappedDst), &numIds,&ids,captype )
       != SV_OK ) {
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
       != SV_OK ) {
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
       != SV_OK ) {
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
