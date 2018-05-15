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
#include "sv_adapt_init.h"
#include "sv_AdaptObject.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"
#include "sv_vtk_utils.h"
#include "sv_PolyData.h"
#include "sv_sys_geom.h"

#include "sv_FactoryRegistrar.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "sv2_globals.h"

// Prototypes:
// -----------

int cvAdapt_NewObjectCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
int cvAdapt_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] );

// Adapt
// -----
int Adapt_RegistrarsListCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

static int cvAdapt_CreateInternalMeshObjectMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_LoadModelMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_LoadMeshMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_LoadSolutionFromFileMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_LoadYbarFromFileMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_LoadAvgSpeedFromFileMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_LoadHessianFromFileMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_ReadSolutionFromMeshMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_ReadYbarFromMeshMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_ReadAvgSpeedFromMeshMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_SetAdaptOptionsMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_CheckOptionsMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_SetMetricMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_SetupMeshMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_RunAdaptorMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_PrintStatsMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_GetAdaptedMeshMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_TransferSolutionMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_TransferRegionsMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_WriteAdaptedModelMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_WriteAdaptedMeshMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
static int cvAdapt_WriteAdaptedSolutionMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

// Helper functions
// ----------------

static void AdaptPrintMethods( Tcl_Interp *interp );

void DeleteAdapt( ClientData clientData );


// ----------
// Adapt_Init
// ----------

int Adapt_Init( Tcl_Interp *interp )
{
  // Associate the adapt object registrar with the Tcl interpreter so it can be
  // retrieved by the DLLs.
  Tcl_SetAssocData( interp, "AdaptObjectRegistrar", NULL, &cvAdaptObject::gRegistrar );

  Tcl_CreateCommand( interp, "adapt_registrars", Adapt_RegistrarsListCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  //Not object tcl functions
  Tcl_CreateCommand( interp, "adapt_newObject", cvAdapt_NewObjectCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  // Initialize
  cvAdaptObject::gCurrentKernel = KERNEL_INVALID;

#ifdef USE_TETGEN_ADAPTOR
  cvAdaptObject::gCurrentKernel = KERNEL_TETGEN;
#endif

  return TCL_OK;
}

// This routine is used for debugging the registrar/factory system.
int Adapt_RegistrarsListCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  if ( argc != 1 ) {
    Tcl_SetResult( interp, "usage: registrars_list", TCL_STATIC );
    return TCL_ERROR;
  }
  cvFactoryRegistrar *adaptObjectRegistrar =
    (cvFactoryRegistrar *) Tcl_GetAssocData( interp, "AdaptObjectRegistrar", NULL);

  char result[255];
  sprintf( result, "Adapt object registrar ptr -> %p\n", adaptObjectRegistrar );
  Tcl_AppendElement( interp, result );
  for (int i = 0; i < 5; i++) {
    sprintf( result, "GetFactoryMethodPtr(%i) = %p\n",
      i, (adaptObjectRegistrar->GetFactoryMethodPtr(i)));
    Tcl_AppendElement( interp, result );
  }

  return TCL_OK;
}

// Now, since we're using the cvRepository mechanism (which is itself a
// Tcl_HashTable), we can use the cvRepository's lookup mechanisms to
// find those operands.  That is, we can call cvRepository's
// GetObject(name) method to get back object pointers for use inside
// Tcl object method functions.

int cvAdapt_NewObjectCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  char *resultName = NULL;

  char *usage;
  char *kernelName;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &resultName, NULL, REQUIRED, 0, { 0 } },
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
  if ( gRepository->Exists( resultName ) ) {
    Tcl_AppendResult( interp, "object ", resultName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  KernelType meshType = KERNEL_INVALID;
  kernelName = cvMeshSystem::GetCurrentKernelName();
  // Instantiate the new mesh:
  cvAdaptObject *adaptor = NULL;
  if (!strcmp(kernelName,"TetGen")) {
    meshType = KERNEL_TETGEN;
    cvAdaptObject::gCurrentKernel = KERNEL_TETGEN;
  } else if (!strcmp(kernelName,"MeshSim")) {
    meshType = KERNEL_MESHSIM;
    cvAdaptObject::gCurrentKernel = KERNEL_MESHSIM;
  } else {
    Tcl_SetResult (interp, "invalid kernel name", TCL_VOLATILE);
  }
  Tcl_SetResult( interp, kernelName, TCL_VOLATILE );
  adaptor = cvAdaptObject::DefaultInstantiateAdaptObject( interp, meshType);

  if ( adaptor == NULL ) {
    return TCL_ERROR;
  }

  // Register the solid:
  if ( !( gRepository->Register( resultName, adaptor ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete adaptor;
    return TCL_ERROR;
  }

  // Make a new Tcl command:
  Tcl_SetResult( interp, adaptor->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), cvAdapt_ObjectCmd,
		     (ClientData)adaptor, DeleteAdapt );

  return TCL_OK;
}

// -------------
// Adapt_Object
// -------------
int cvAdapt_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  if ( argc == 1 ) {
    AdaptPrintMethods( interp );
    return TCL_OK;
  }

  if ( Tcl_StringMatch( argv[1], "CreateInternalMeshObject" ) ) {
    if ( cvAdapt_CreateInternalMeshObjectMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "LoadModel" ) ) {
    if ( cvAdapt_LoadModelMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "LoadMesh" ) ) {
    if ( cvAdapt_LoadMeshMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "LoadSolutionFromFile" ) ) {
    if ( cvAdapt_LoadSolutionFromFileMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "LoadYbarFromFile" ) ) {
    if ( cvAdapt_LoadYbarFromFileMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "LoadAvgSpeedFromFile" ) ) {
    if ( cvAdapt_LoadAvgSpeedFromFileMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "LoadHessianFromFile" ) ) {
    if ( cvAdapt_LoadHessianFromFileMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "ReadSolutionFromMesh" ) ) {
    if ( cvAdapt_ReadSolutionFromMeshMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "ReadYbarFromMesh" ) ) {
    if ( cvAdapt_ReadYbarFromMeshMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "ReadAvgSpeedFromMesh" ) ) {
    if ( cvAdapt_ReadAvgSpeedFromMeshMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "SetAdaptOptions" ) ) {
    if ( cvAdapt_SetAdaptOptionsMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "CheckOptions" ) ) {
    if ( cvAdapt_CheckOptionsMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "SetMetric" ) ) {
    if ( cvAdapt_SetMetricMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "SetupMesh" ) ) {
    if ( cvAdapt_SetupMeshMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "RunAdaptor" ) ) {
    if ( cvAdapt_RunAdaptorMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "PrintStats" ) ) {
    if ( cvAdapt_PrintStatsMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetAdaptedMesh" ) ) {
    if ( cvAdapt_GetAdaptedMeshMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "TransferSolution" ) ) {
    if ( cvAdapt_TransferSolutionMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "TransferRegions" ) ) {
    if ( cvAdapt_TransferRegionsMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "WriteAdaptedModel" ) ) {
    if ( cvAdapt_WriteAdaptedModelMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "WriteAdaptedMesh" ) ) {
    if ( cvAdapt_WriteAdaptedMeshMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "WriteAdaptedSolution" ) ) {
    if ( cvAdapt_WriteAdaptedSolutionMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else {
    Tcl_AppendResult( interp, "\"", argv[1],
		      "\" not a recognized cvAdaptObject method", (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}

// -------------
// DeleteAdapt
// -------------
// This is the deletion call-back for cvAdaptObject object commands.

void DeleteAdapt( ClientData clientData ) {
    cvAdaptObject *geom = (cvAdaptObject *)clientData;

    gRepository->UnRegister( geom->GetName() );
}

// ------------
// AdaptPrintMethods
// ------------

static void AdaptPrintMethods( Tcl_Interp *interp )
{

  tcl_printstr(interp, "CreateInternalMeshObject\n");
  tcl_printstr(interp, "LoadModel\n");
  tcl_printstr(interp, "LoadMesh\n");
  tcl_printstr(interp, "LoadSolutionFromFile\n");
  tcl_printstr(interp, "LoadYbarFromFile\n");
  tcl_printstr(interp, "LoadAvgSpeedFromFile\n");
  tcl_printstr(interp, "LoadHessianFromFile\n");
  tcl_printstr(interp, "ReadSolutionFromMesh\n");
  tcl_printstr(interp, "ReadYbarFromMesh\n");
  tcl_printstr(interp, "ReadAvgSpeedFromMesh\n");
  tcl_printstr(interp, "SetAdaptOptions\n");
  tcl_printstr(interp, "CheckOptions\n");
  tcl_printstr(interp, "SetMetric\n");
  tcl_printstr(interp, "SetupMesh\n");
  tcl_printstr(interp, "RunAdaptor\n");
  tcl_printstr(interp, "PrintStats\n");
  tcl_printstr(interp, "TransferSolution\n");
  tcl_printstr(interp, "TransferRegions\n");
  tcl_printstr(interp, "WriteAdaptedModel\n");
  tcl_printstr(interp, "WriteAdaptedMesh\n");
  tcl_printstr(interp, "WriteAdaptedSolution\n");

  return;
}

// ----------------
// cvAdapt_CreateInternalMeshObjectMtd
// ----------------
static int cvAdapt_CreateInternalMeshObjectMtd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  char *meshFileName = NULL;
  char *solidFileName = NULL;

  char *usage;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-meshfile", STRING_Type, &meshFileName, NULL, REQUIRED, 0, { 0 } },
    { "-solidfile", STRING_Type, &solidFileName, NULL, SV_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_sz, arg_table );
  if (argc != 2)
  {
    if ( ARG_ParseTclStr( interp, argc, argv, 2,
         		 table_sz, arg_table ) != TCL_OK ) {
      Tcl_SetResult( interp, usage, TCL_VOLATILE );
      return TCL_ERROR;
    }
  }

  // Do work of command:

  cvAdaptObject *geom = (cvAdaptObject *)clientData;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return TCL_ERROR;
  }
  if (geom->CreateInternalMeshObject(interp,meshFileName,solidFileName) != SV_OK)
  {
    fprintf(stderr,"Error in creation of internal mesh\n");
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// cvAdapt_LoadModelMtd
// ----------------
static int cvAdapt_LoadModelMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char *solidFileName = NULL;

  char *usage;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &solidFileName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_sz, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  cvAdaptObject *geom = (cvAdaptObject *)clientData;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return TCL_ERROR;
  }
  if (geom->LoadModel(solidFileName) != SV_OK)
  {
    fprintf(stderr,"Error in loading of model\n");
    return TCL_ERROR;
  }//, meshFileName, solidFileName );

  return TCL_OK;
}

// ----------------
// cvAdapt_LoadMeshMtd
// ----------------
static int cvAdapt_LoadMeshMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char *meshFileName = NULL;

  char *usage;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &meshFileName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_sz, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  cvAdaptObject *geom = (cvAdaptObject *)clientData;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return TCL_ERROR;
  }
  if (geom->LoadMesh(meshFileName) != SV_OK)
  {
    fprintf(stderr,"Error in loading of mesh\n");
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// cvAdapt_LoadSolutionFromFileMtd
// ----------------
static int cvAdapt_LoadSolutionFromFileMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char *fileName = NULL;

  char *usage;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &fileName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_sz, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  cvAdaptObject *geom = (cvAdaptObject *)clientData;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return TCL_ERROR;
  }
  if (geom->LoadSolutionFromFile(fileName) != SV_OK)
  {
    fprintf(stderr,"Error in loading of solution\n");
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// cvAdapt_LoadYbarFromFileMtd
// ----------------
static int cvAdapt_LoadYbarFromFileMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char *fileName = NULL;

  char *usage;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &fileName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_sz, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  cvAdaptObject *geom = (cvAdaptObject *)clientData;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return TCL_ERROR;
  }
  if (geom->LoadYbarFromFile(fileName) != SV_OK)
  {
    fprintf(stderr,"Error in loading of average speed\n");
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// cvAdapt_LoadAvgSpeedFromFileMtd
// ----------------
static int cvAdapt_LoadAvgSpeedFromFileMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char *fileName = NULL;

  char *usage;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &fileName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_sz, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  cvAdaptObject *geom = (cvAdaptObject *)clientData;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return TCL_ERROR;
  }
  if (geom->LoadAvgSpeedFromFile(fileName) != SV_OK)
  {
    fprintf(stderr,"Error in loading of average speed\n");
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// cvAdapt_LoadHessianFromFileMtd
// ----------------
static int cvAdapt_LoadHessianFromFileMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char *fileName = NULL;

  char *usage;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &fileName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_sz, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  cvAdaptObject *geom = (cvAdaptObject *)clientData;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return TCL_ERROR;
  }
  if (geom->LoadHessianFromFile(fileName) != SV_OK)
  {
    fprintf(stderr,"Error in loading of hessian\n");
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// cvAdapt_ReadSolutionFromMeshMtd
// ----------------
static int cvAdapt_ReadSolutionFromMeshMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  cvAdaptObject *geom = (cvAdaptObject *)clientData;

  if (geom->ReadSolutionFromMesh() == SV_OK) {
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }
  return TCL_OK;
}

// ----------------
// cvAdapt_ReadYbarFromMeshMtd
// ----------------
static int cvAdapt_ReadYbarFromMeshMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  cvAdaptObject *geom = (cvAdaptObject *)clientData;

  if (geom->ReadYbarFromMesh() == SV_OK) {
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }
  return TCL_OK;
}

// ----------------
// cvAdapt_ReadAvgSpeedFromMeshMtd
// ----------------
static int cvAdapt_ReadAvgSpeedFromMeshMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  cvAdaptObject *geom = (cvAdaptObject *)clientData;

  if (geom->ReadAvgSpeedFromMesh() == SV_OK) {
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }
  return TCL_OK;
}

// ----------------
// cvAdapt_SetAdaptOptionsMtd
// ----------------
static int cvAdapt_SetAdaptOptionsMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  double value=0;
  char *flag = NULL;

  char *usage;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-flag", STRING_Type, &flag, NULL, REQUIRED, 0, { 0 } },
    { "-value", DOUBLE_Type, &value, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_sz, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  cvAdaptObject *geom = (cvAdaptObject *)clientData;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return TCL_ERROR;
  }
  if (geom->SetAdaptOptions(flag,value) != SV_OK)
  {
    fprintf(stderr,"Error in options setting\n");
    fprintf(stderr,"%s is not a valid option flag\n",flag);
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// cvAdapt_CheckOptionsMtd
// ----------------
static int cvAdapt_CheckOptionsMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  cvAdaptObject *geom = (cvAdaptObject *)clientData;

  if (geom->CheckOptions() == SV_OK) {
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// cvAdapt_SetMetricMtd
// ----------------
static int cvAdapt_SetMetricMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char *fileName = NULL;
  int option = -1;
  int strategy = -1;

  char *usage;

  int table_sz = 3;
  ARG_Entry arg_table[] = {
    { "-input", STRING_Type, &fileName, NULL, REQUIRED, 0, { 0 } },
    { "-option", INT_Type, &option, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-strategy", INT_Type, &strategy, NULL, SV_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_sz, arg_table );
  if (argc != 2)
  {
    if ( ARG_ParseTclStr( interp, argc, argv, 2,
			  table_sz, arg_table ) != TCL_OK ) {
      Tcl_SetResult( interp, usage, TCL_VOLATILE );
      return TCL_ERROR;
    }
  }
  cvAdaptObject *geom = (cvAdaptObject *)clientData;

  if (geom->SetMetric(fileName,option,strategy) == SV_OK) {
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// cvAdapt_SetupMeshMtd
// ----------------
static int cvAdapt_SetupMeshMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  cvAdaptObject *geom = (cvAdaptObject *)clientData;

  if (geom->SetupMesh() == SV_OK) {
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// cvAdapt_RunAdaptorMtd
// ----------------
static int cvAdapt_RunAdaptorMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  cvAdaptObject *geom = (cvAdaptObject *)clientData;

  if (geom->RunAdaptor() == SV_OK) {
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// cvAdapt_PrintStatsMtd
// ----------------
static int cvAdapt_PrintStatsMtd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  cvAdaptObject *geom = (cvAdaptObject *)clientData;

  if (geom->PrintStats() == SV_OK) {
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }
}

// ----------------
// cvAdapt_GetAdaptedMeshMtd
// ----------------
int cvAdapt_GetAdaptedMeshMtd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  cvAdaptObject *geom = (cvAdaptObject *)clientData;

  if (geom->GetAdaptedMesh() == SV_OK) {
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }
}

// ----------------
// cvAdapt_TransferSolutionMtd
// ----------------
int cvAdapt_TransferSolutionMtd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  cvAdaptObject *geom = (cvAdaptObject *)clientData;

  if (geom->TransferSolution() == SV_OK) {
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }
}

// ----------------
// cvAdapt_TransferRegionsMtd
// ----------------
int cvAdapt_TransferRegionsMtd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  cvAdaptObject *geom = (cvAdaptObject *)clientData;

  if (geom->TransferRegions() == SV_OK) {
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }
}

// ----------------
// cvAdapt_WriteAdaptedModelMtd
// ----------------
static int cvAdapt_WriteAdaptedModelMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char *fileName = NULL;

  char *usage;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &fileName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_sz, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  cvAdaptObject *geom = (cvAdaptObject *)clientData;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return TCL_ERROR;
  }
  if (geom->WriteAdaptedModel(fileName) != SV_OK)
  {
    fprintf(stderr,"Error in writing of model\n");
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// cvAdapt_WriteAdaptedMeshMtd
// ----------------
static int cvAdapt_WriteAdaptedMeshMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char *fileName = NULL;

  char *usage;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &fileName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_sz, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  cvAdaptObject *geom = (cvAdaptObject *)clientData;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return TCL_ERROR;
  }
  if (geom->WriteAdaptedMesh(fileName) != SV_OK)
  {
    fprintf(stderr,"Error in writing of mesh\n");
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------
// cvAdapt_WriteAdaptedSolutionMtd
// ----------------
static int cvAdapt_WriteAdaptedSolutionMtd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char *fileName = NULL;

  char *usage;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &fileName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_sz, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  cvAdaptObject *geom = (cvAdaptObject *)clientData;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return TCL_ERROR;
  }
  if (geom->WriteAdaptedSolution(fileName) != SV_OK)
  {
    fprintf(stderr,"Error in writing of solution\n");
    return TCL_ERROR;
  }

  return TCL_OK;
}
