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
#include "cvMeshSystem.h"
#include "cvMeshObject.h"
#include "cv_mesh_init.h"

#include <stdio.h>
#include <string.h>
#include "cvRepository.h"
#include "cvRepositoryData.h"
#include "cvPolyData.h"
#include "cv_arg.h"
#include "cvVTK.h"
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

int cvMesh_SetMeshKernelCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] );


int cvMesh_NewObjectCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
int cvMesh_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] );
int cvMesh_LogonCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] );
int cvMesh_LogoffCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] );


// gdscMesh methods
// --------------------
static int cvMesh_GetKernelMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );
static int cvMesh_PrintMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );
static int cvMesh_UpdateMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );
static int cvMesh_WriteMetisAdjacencyMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );

static int cvMesh_GetPolyDataMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

static int cvMesh_GetSolidMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

static int cvMesh_SetVtkPolyDataMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

static int cvMesh_GetUnstructuredGridMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

static int cvMesh_GetFacePolyDataMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

static int cvMesh_GetModelFaceInfoMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
  
static int cvMesh_GetBoundaryFacesMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
/*
#ifdef USE_DISCRETE_MODEL
static int cvMesh_LoadDiscreteModelMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
#endif
*/

static int cvMesh_LoadModelMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
static int cvMesh_LoadMeshMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
static int cvMesh_NewMeshMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
static int cvMesh_GenerateMeshMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
static int cvMesh_WriteMeshMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
static int cvMesh_WriteStatsMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
static int cvMesh_SetMeshOptionsMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
static int cvMesh_SetCylinderRefinementMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
static int cvMesh_SetSphereRefinementMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
static int cvMesh_SetBoundaryLayerMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
static int cvMesh_SetWallsMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
static int cvMesh_SetSolidKernelMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
static int cvMesh_SetSizeFunctionBasedMeshMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
static int cvMesh_AdaptMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );
  

// Helper functions
// ----------------

static void gdscMeshPrintMethods( Tcl_Interp *interp );


// ----------
// cvMesh_Init
// ----------

int Gdscmesh_Init( Tcl_Interp *interp )
{
  // Associate the mesh registrar with the Tcl interpreter so it can be
  // retrieved by the DLLs.
  Tcl_SetAssocData( interp, "MeshSystemRegistrar", NULL, ((ClientData*)&cvMeshSystem::RegisterKernel ));

  // Initialize
  cvMeshSystem::SetCurrentKernel( cvMeshObject::KERNEL_INVALID );

  Tcl_CreateCommand( interp, "mesh_newObject", cvMesh_NewObjectCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "mesh_listMethods", cvMesh_ListMethodsCmd,
  		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );  
  Tcl_CreateCommand( interp, "mesh_setKernel", cvMesh_SetMeshKernelCmd,
  		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL ); 
  Tcl_CreateCommand( interp, "mesh_logon", cvMesh_LogonCmd,
  		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL ); 
  Tcl_CreateCommand( interp, "mesh_logoff", cvMesh_LogoffCmd,
  		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL ); 

  return TCL_OK;
}


//
//
//

int cvMesh_NewObjectCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  char *resultName;
  char *meshFileName = NULL;  
  char *solidFileName = NULL;

  char *usage;

  int table_sz = 3;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &resultName, NULL, REQUIRED, 0, { 0 } },
    { "-meshfile", STRING_Type, &meshFileName, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-solidfile", STRING_Type, &solidFileName, NULL, GDSC_OPTIONAL, 0, { 0 } }, 
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

  // Instantiate the new mesh:
  cvMeshObject *geom;
  geom = cvMeshSystem::DefaultInstantiateMeshObject( interp, meshFileName, solidFileName );
  if ( geom == NULL ) {
    return TCL_ERROR;
  }

  // Register the solid:
  if ( !( gRepository->Register( resultName, geom ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete geom;
    return TCL_ERROR;
  }

  // Make a new Tcl command:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), cvMesh_ObjectCmd,
		     (ClientData)geom, DeletegdscMesh );

  return TCL_OK;
}


// ----------------------
// cvMesh_ListMethodsCmd
// ----------------------

int cvMesh_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  if ( argc != 1 ) {
    Tcl_AppendResult( interp, "usage: ", argv[0], (char *)NULL );
    return TCL_ERROR;
  }

  gdscMeshPrintMethods( interp );
  return TCL_OK;
}

int cvMesh_SetMeshKernelCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *kernelName;
  char *usage;

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
  cvMeshObject::KernelType kernelType = cvMeshObject::GetKernelType( kernelName );

  if ( kernelType != cvMeshObject::KERNEL_INVALID && cvMeshSystem::SetCurrentKernel(kernelType) ) {
    Tcl_SetResult( interp, kernelName, TCL_VOLATILE );
    return TCL_OK;
  } else {
    Tcl_SetResult( interp, "Mesh kernel is not available", TCL_STATIC );
    return TCL_ERROR;
  }
}

// -----------------
// cvMesh_ObjectCmd
// -----------------

int cvMesh_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] )
{
  if ( argc == 1 ) {
    gdscMeshPrintMethods( interp );
    return TCL_OK;
  }

  if ( Tcl_StringMatch( argv[1], "LoadModel" ) ) {
    if ( cvMesh_LoadModelMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "GetBoundaryFaces" ) ) {
    if ( cvMesh_GetBoundaryFacesMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "LoadMesh" ) ) {
    if ( cvMesh_LoadMeshMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "NewMesh" ) ) {
    if ( cvMesh_NewMeshMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "SetMeshOptions" ) ) {
    if ( cvMesh_SetMeshOptionsMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "SetCylinderRefinement" ) ) {
    if ( cvMesh_SetCylinderRefinementMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "SetSphereRefinement" ) ) {
    if ( cvMesh_SetSphereRefinementMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "SetSizeFunctionBasedMesh" ) ) {
    if ( cvMesh_SetSizeFunctionBasedMeshMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "GenerateMesh" ) ) {
    if ( cvMesh_GenerateMeshMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "SetBoundaryLayer" ) ) {
    if ( cvMesh_SetBoundaryLayerMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "SetWalls" ) ) {
    if ( cvMesh_SetWallsMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "SetSolidKernel" ) ) {
    if ( cvMesh_SetSolidKernelMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "GetModelFaceInfo" ) ) {
    if ( cvMesh_GetModelFaceInfoMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
    return TCL_OK;
  }

  // The method "Update" must be called before any of the other  
  // methods since it loads the mesh.  To avoid confusion, we
  // call this method directly prior to any other.
  cvMeshObject* geom = (cvMeshObject *)clientData;
  if (geom->GetMeshLoaded() == 0) {
    if (geom->Update() == CV_ERROR) {
      return TCL_ERROR;
    }
  }
 
  if ( Tcl_StringMatch( argv[1], "Update" ) ) {
    // ignore this call now, it is done implicitly (see above)
    //if ( cvMesh_UpdateMtd( clientData, interp, argc, argv ) != TCL_OK ) {
    //  return TCL_ERROR;
    //}
  } else if ( Tcl_StringMatch( argv[1], "Print" ) ) {
    if ( cvMesh_PrintMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetKernel" ) ) {
    if ( cvMesh_GetKernelMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "WriteMetisAdjacency" ) ) {
    if ( cvMesh_WriteMetisAdjacencyMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    } 
  } else if ( Tcl_StringMatch( argv[1], "GetPolyData" ) ) {
    if ( cvMesh_GetPolyDataMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetSolid" ) ) {
    if ( cvMesh_GetSolidMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "SetVtkPolyData" ) ) {
    if ( cvMesh_SetVtkPolyDataMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetUnstructuredGrid" ) ) {
    if ( cvMesh_GetUnstructuredGridMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetFacePolyData" ) ) {
    if ( cvMesh_GetFacePolyDataMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "WriteMesh" ) ) {
    if ( cvMesh_WriteMeshMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "WriteStats" ) ) {
    if ( cvMesh_WriteStatsMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "Adapt" ) ) {
    if ( cvMesh_AdaptMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else {
    Tcl_AppendResult( interp, "\"", argv[1],
		      "\" not a recognized cvMeshObject method", (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
  
}


// -------------
// DeletegdscMesh
// -------------
// This is the deletion call-back for cvMeshObject object commands.

void DeletegdscMesh( ClientData clientData ) {
    cvMeshObject *geom = (cvMeshObject *)clientData;
  
    gRepository->UnRegister( geom->GetName() );
}

// -------------
// fakeDeletegdscMesh
// -------------
// This is the deletion call-back for cvMeshObject object commands.

void fakeDeletegdscMesh( ClientData clientData ) {
  ;
}

// ------------
// gdscMeshPrintMethods
// ------------

static void gdscMeshPrintMethods( Tcl_Interp *interp )
{

  // Note:  I've commented out some of the currently
  // unimplemented methods in the mesh object.  Since I may
  // want these in the future, instead of removing the methods
  // from the object I just hide them from the user.  This
  // way all I have to do is bind in code in the MegaMeshObject
  // and these commands are ready to go.

  tcl_printstr(interp, "GetFacePolyData\n");
  tcl_printstr(interp, "GetKernel\n");
  tcl_printstr(interp, "GetPolyData\n");
  tcl_printstr(interp, "GetSolid\n");
  tcl_printstr(interp, "SetVtkPolyData\n");
  tcl_printstr(interp, "GetUnstructuredGrid\n");
  tcl_printstr(interp, "Print\n");
  tcl_printstr(interp, "Update\n");
  tcl_printstr(interp, "WriteMetisAdjacency\n");
  tcl_printstr(interp, "*** methods to generate meshes ***\n");
  tcl_printstr(interp, "LoadModel\n");
  /*
#ifdef USE_DISCRETE_MODEL
  tcl_printstr(interp, "LoadDiscreteModel\n");
#endif
  */
  tcl_printstr(interp, "LoadMesh\n");
  tcl_printstr(interp, "NewMesh\n");
  tcl_printstr(interp, "SetBoundaryLayer\n");
  tcl_printstr(interp, "SetWalls\n");
  tcl_printstr(interp, "SetMeshOptions\n");
  tcl_printstr(interp, "SetCylinderRefinement\n");
  tcl_printstr(interp, "SetSphereRefinement\n");
  tcl_printstr(interp, "SetSizeFunctionBasedMesh\n");
  tcl_printstr(interp, "GenerateMesh\n");
  tcl_printstr(interp, "WriteMesh\n");
  tcl_printstr(interp, "WriteStats\n");
  tcl_printstr(interp, "Adapt\n");
  tcl_printstr(interp, "SetSolidKernel\n");
  tcl_printstr(interp, "GetModelFaceInfo\n");
  
  return;
}

// ----------------
// cvMesh_GetKernelMtd
// ----------------

static int cvMesh_GetKernelMtd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  char *usage;
  cvMeshObject::KernelType kernelType;
  char *kernelName;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  kernelType = geom->GetMeshKernel();
  kernelName = cvMeshObject::GetKernelName( kernelType );
  Tcl_SetResult( interp, kernelName, TCL_VOLATILE );

  if ( kernelType == SM_KT_INVALID ) {
    fprintf(stderr,"Invalid kernel type\n");
    return TCL_ERROR;
  } else {
    return TCL_OK;
  }
}

// ----------------
// cvMesh_PrintMtd
// ----------------

static int cvMesh_PrintMtd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;

  if (geom->Print() == CV_OK) {
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }
 
}


// -----------------
// cvMesh_UpdateMtd
// -----------------

static int cvMesh_UpdateMtd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;

  if (geom->Update() == CV_OK) {
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }
}


// ------------------------
// cvMesh-SetSolidKernelMtd
// ------------------------

int cvMesh_SetSolidKernelMtd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *kernelName;
  char *usage;
  SolidModel_KernelT kernel;
  cvMeshObject *geom = (cvMeshObject *)clientData;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-name", STRING_Type, &kernelName, NULL, REQUIRED, 0, { 0 } }
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:
  kernel = SolidModel_KernelT_StrToEnum( kernelName );
  if ( kernel != SM_KT_INVALID ) {
    geom->SetSolidModelKernel(kernel);
    Tcl_SetResult( interp, kernelName, TCL_VOLATILE );
    return TCL_OK;
  } else {
    Tcl_SetResult( interp, SolidModel_KernelT_EnumToStr( SM_KT_INVALID ),
		   TCL_VOLATILE );
    return TCL_ERROR;
  }
}

// -------------------------------
// cvMesh_WriteMetisAdjacencyMtd
// -------------------------------

static int cvMesh_WriteMetisAdjacencyMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] ) {
  cvMeshObject *geom = (cvMeshObject *)clientData;
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
  status = geom->WriteMetisAdjacency( fn );

  if ( status != CV_OK ) {
    Tcl_AppendResult( interp, "error writing object ", geom->GetName(),
		      " to file ", fn, (char *)NULL );
    return TCL_ERROR;
  } else {
    return TCL_OK;
  }
}

// ----------------------
// cvMesh_GetPolyDataMtd
// ----------------------

static int cvMesh_GetPolyDataMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
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
  pd = geom->GetPolyData();
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
// cvMesh_GetSolidMtd
// ----------------------

static int cvMesh_GetSolidMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
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
  pd = geom->GetSolid();
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
// cvMesh_SetVtkPolyDataMtd
// ----------------------

static int cvMesh_SetVtkPolyDataMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
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

// -------------------------------
// cvMesh_GetUnstructuredGridMtd
// -------------------------------

static int cvMesh_GetUnstructuredGridMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  char *usage;
  char *resultName;
  cvUnstructuredGrid *ug;

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

  // Get the cvUnstructuredGrid:
  ug = geom->GetUnstructuredGrid();
  if ( ug == NULL ) {
    Tcl_AppendResult( interp, "error getting cvPolyData for ", geom->GetName(),
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Register the result:
  if ( !( gRepository->Register( resultName, ug ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", resultName,
		      " in repository", (char *)NULL );
    delete ug;
    return TCL_ERROR;
  }

  return TCL_OK;
}


// --------------------------
// cvMesh_GetFacePolyDataMtd
// --------------------------

static int cvMesh_GetFacePolyDataMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  char *usage;
  char *resultName;
  cvPolyData *pd;
  int face;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &resultName, NULL, REQUIRED, 0, { 0 } },
    { "-face", INT_Type   , &face, NULL, REQUIRED, 0 , { 0 }},
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
  pd = geom->GetFacePolyData(face);
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

//
// LogOn
//

int cvMesh_LogonCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  
  char *logFileName;

  char *usage;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &logFileName, NULL, REQUIRED, 0, { 0 } }, 
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

  cvMeshSystem* meshKernel = cvMeshSystem::GetCurrentKernel();

  // read in the results file
  if (meshKernel == NULL || meshKernel->LogOn(logFileName) == CV_ERROR) {
      Tcl_AppendResult( interp, "error opening logfile ", (char *)NULL);
      return TCL_ERROR;
  }

  return TCL_OK;
}


// ------------------
// cvMesh_LogoffCmd
// ------------------

int cvMesh_LogoffCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  if ( argc != 1 ) {
    Tcl_AppendResult( interp, "usage: ", argv[0], (char *)NULL );
    return TCL_ERROR;
  }

  cvMeshSystem* meshKernel = cvMeshSystem::GetCurrentKernel();

  if (meshKernel == NULL || meshKernel->LogOff() == CV_ERROR) {
      Tcl_AppendResult( interp, "error turning off logfile ", (char *)NULL);
      return TCL_ERROR;
  }

  return TCL_OK;
}

// -------------------------
// cvMesh_SetMeshOptionsMtd
// -------------------------

static int cvMesh_SetMeshOptionsMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  char *usage;
  char *flags;
  ARG_List valueList;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-options", STRING_Type   , &flags, NULL, REQUIRED, 0 , { 0 }},
    { "-values", LIST_Type   , &valueList, NULL, GDSC_OPTIONAL, 0 , { 0 }},
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

  double *values = new double [valueList.argc];
  int numValues = 0;
  if ( ARG_ParseTclListStatic( interp, valueList, DOUBLE_Type, values, valueList.argc, &numValues )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    delete [] values;
    return TCL_ERROR;
  }

  if (numValues != valueList.argc) {
     Tcl_AppendResult( interp, "error in values list", (char *)NULL );
     delete [] values;
     return TCL_ERROR;
  }

  // Do work of command:

  // Get the cvPolyData:
  if ( geom->SetMeshOptions(flags,numValues,values) == CV_ERROR ) {
    Tcl_AppendResult( interp, "error in method ",
		      (char *)NULL );
    delete [] values;
    return TCL_ERROR;
  }

  delete [] values;

  return TCL_OK;
}

//
// LoadModel
//

int cvMesh_LoadModelMtd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  char *FileName;
  char *usage;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &FileName, NULL, REQUIRED, 0, { 0 } }, 
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

  // read in the results file
  fprintf(stderr,"Filename: %s\n",FileName);
  if (geom->LoadModel(FileName) == CV_ERROR) {
      Tcl_AppendResult( interp, "error loading solid model", (char *)NULL);
      return TCL_ERROR;
  }

  return TCL_OK;
}

// -------------------
// Solid_GetBoundaryFacesMtd
// -------------------
//
int cvMesh_GetBoundaryFacesMtd( ClientData clientData, Tcl_Interp *interp,
		int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
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

#ifdef USE_DISCRETE_MODEL

/*
//
// LoadDiscreteModel
//

int cvMesh_LoadDiscreteModelMtd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  char *FileName;
  char *usage;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &FileName, NULL, REQUIRED, 0, { 0 } }, 
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

  // read in the results file
  if (geom->LoadDiscreteModel(FileName) == CV_ERROR) {
      Tcl_AppendResult( interp, "error loading solid model", (char *)NULL);
      return TCL_ERROR;
  }

  return TCL_OK;
}

*/

#endif

int cvMesh_LoadMeshMtd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  char *FileName;
  char *SurfFileName = 0;
  char *usage;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &FileName, NULL, REQUIRED, 0, { 0 } }, 
    { "-surfile", STRING_Type, &SurfFileName, NULL, GDSC_OPTIONAL, 0, { 0 } }, 
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

  // read in the results file
  if (geom->LoadMesh(FileName,SurfFileName) == CV_ERROR) {
      Tcl_AppendResult( interp, "error opening logfile ", (char *)NULL);
      return TCL_ERROR;
  }

  return TCL_OK;
}

int cvMesh_WriteStatsMtd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  char *FileName;
  char *usage;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &FileName, NULL, REQUIRED, 0, { 0 } }, 
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

  // read in the results file
  if (geom->WriteStats(FileName) == CV_ERROR) {
      Tcl_AppendResult( interp, "error opening logfile ", (char *)NULL);
      return TCL_ERROR;
  }

  return TCL_OK;
}

int cvMesh_AdaptMtd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;

  // Do work of command:
  if (geom->Adapt() == CV_OK) {
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }
} 

int cvMesh_WriteMeshMtd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  char *FileName;
  char *usage;
  int smsver = 0;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &FileName, NULL, REQUIRED, 0, { 0 } }, 
    { "-version", INT_Type, &smsver, NULL, GDSC_OPTIONAL, 0, { 0 } }, 

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

  // read in the results file
  if (geom->WriteMesh(FileName,smsver) == CV_ERROR) {
      Tcl_AppendResult( interp, "error opening logfile ", (char *)NULL);
      return TCL_ERROR;
  }

  return TCL_OK;
}


// -------------------
// cvMesh_NewMeshMtd
// -------------------

int cvMesh_NewMeshMtd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  if ( argc != 2 ) {
    Tcl_AppendResult( interp, "usage: ", argv[0], (char *)NULL );
    return TCL_ERROR;
  }

  if (geom->NewMesh() == CV_ERROR) {
      Tcl_AppendResult( interp, "error creating new mesh ", (char *)NULL);
      return TCL_ERROR;
  }

  return TCL_OK;
}


// ------------------------
// cvMesh_GenerateMeshMtd
// ------------------------

int cvMesh_GenerateMeshMtd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  if ( argc != 2 ) {
    Tcl_AppendResult( interp, "usage: ", argv[0], (char *)NULL );
    return TCL_ERROR;
  }

  if (geom->GenerateMesh() == CV_ERROR) {
      Tcl_AppendResult( interp, "Error generating mesh ", (char *)NULL);
      return TCL_ERROR;
  }

  return TCL_OK;
}


// -------------------------------
// cvMesh_SetSphereRefinementMtd
// -------------------------------

static int cvMesh_SetSphereRefinementMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  char *usage;
  double size;
  ARG_List ctrList;
  double ctr[3];
  double r;
  int nctr;
  
  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-size", DOUBLE_Type, &size, NULL, REQUIRED, 0, { 0 } },
    { "-r", DOUBLE_Type, &r, NULL, REQUIRED, 0, { 0 } },
    { "-ctr", LIST_Type, &ctrList, NULL, REQUIRED, 0, { 0 } },
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

  // Do work of command:

  if ( geom->SetSphereRefinement(size,r,ctr) == CV_ERROR ) {
    Tcl_AppendResult( interp, "error in method ",
		      (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}

// -------------------------------
// cvMesh_SetSizeFunctionBasedMeshMtd
// -------------------------------

static int cvMesh_SetSizeFunctionBasedMeshMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  char *usage;
  char *functionName;
  double size;
  
  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-size", DOUBLE_Type, &size, NULL, REQUIRED, 0, { 0 } },
    { "-functionname", STRING_Type, &functionName, NULL, REQUIRED, 0, { 0 } },
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

  if ( geom->SetSizeFunctionBasedMesh(size,functionName) == CV_ERROR ) {
    Tcl_AppendResult( interp, "error in setting size function ",
		      (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// ---------------------------------
// cvMesh_SetCylinderRefinementMtd
// ---------------------------------

static int cvMesh_SetCylinderRefinementMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  char *usage;
  double size;
  ARG_List ctrList;
  ARG_List nrmList;
  double ctr[3];
  double nrm[3];
  double r;
  int nctr;
  int nnrm;
  double length;
  
  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-size", DOUBLE_Type, &size, NULL, REQUIRED, 0, { 0 } },
    { "-r", DOUBLE_Type, &r, NULL, REQUIRED, 0, { 0 } },
    { "-length", DOUBLE_Type, &length, NULL, REQUIRED, 0, { 0 } },
    { "-ctr", LIST_Type, &ctrList, NULL, REQUIRED, 0, { 0 } },
    { "-nrm", LIST_Type, &nrmList, NULL, REQUIRED, 0, { 0 } },
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

  // Parse coordinate lists:
  if ( ARG_ParseTclListStatic( interp, ctrList, DOUBLE_Type, ctr, 3, &nctr )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    Tcl_Free( (char *) ctrList.argv );
    Tcl_Free( (char *) nrmList.argv );
    return TCL_ERROR;
  }
  // Parse coordinate lists:
  if ( ARG_ParseTclListStatic( interp, nrmList, DOUBLE_Type, nrm, 3, &nnrm )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    Tcl_Free( (char *) ctrList.argv );
    Tcl_Free( (char *) nrmList.argv );
    return TCL_ERROR;
  }

  // Do work of command:

  // We no longer need ctrList's argv:
  Tcl_Free( (char *) ctrList.argv );
  Tcl_Free( (char *) nrmList.argv );
  
  if ( nctr != 3 ) {
    Tcl_SetResult( interp, "sphere requires a 3D center coordinate",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  if ( nnrm != 3 ) {
    Tcl_SetResult( interp, "norm must be 3D",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  // Do work of command:

  if ( geom->SetCylinderRefinement(size,r,length,ctr,nrm) == CV_ERROR ) {
    Tcl_AppendResult( interp, "error in method ",
		      (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// ----------------------------
// cvMesh_SetBoundaryLayerMtd
// ----------------------------

static int cvMesh_SetBoundaryLayerMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  char *usage;
  int type = 0;
  int id = 0;
  int side = 0;
  int nL = 0;
  double *H = NULL;
  ARG_List Hlist;

  int table_size = 5;
  ARG_Entry arg_table[] = {
    { "-type", INT_Type   , &type, NULL, REQUIRED, 0 , { 0 }},
    { "-id", INT_Type, &id, NULL, REQUIRED, 0 , { 0 }},
    { "-side", INT_Type   , &side, NULL, REQUIRED, 0 , { 0 }},
    { "-nL", INT_Type   , &nL, NULL, REQUIRED, 0 , { 0 }},
    { "-H", LIST_Type   , &Hlist, NULL, REQUIRED, 0 , { 0 }},
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

  // Parse coordinate lists:

  H = new double [Hlist.argc];
  int numH = 0;
  if ( ARG_ParseTclListStatic( interp, Hlist, DOUBLE_Type, H, Hlist.argc, &numH )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    delete [] H;
    return TCL_ERROR;
  }

  if (numH != Hlist.argc) {
     Tcl_AppendResult( interp, "error in H list", (char *)NULL );
     delete [] H;
     return TCL_ERROR;
  }

  // Do work of command:

  if ( geom->SetBoundaryLayer(type,id,side,nL,H) == CV_ERROR ) {
    Tcl_AppendResult( interp, "error in method ",
		      (char *)NULL );
    delete [] H;
    return TCL_ERROR;
  }

  return TCL_OK;
}

// ----------------------------
// cvMesh_SetWallsMtd
// ----------------------------

static int cvMesh_SetWallsMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  char *usage;
  ARG_List wallsList;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-walls", LIST_Type   , &wallsList, NULL, REQUIRED, 0 , { 0 }},
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

  // Parse coordinate lists:
  if (wallsList.argc == 0) {
      ARG_FreeListArgvs( table_size, arg_table);
      return CV_OK;
  }

  int *walls = new int [wallsList.argc];
  int numWalls = 0;
  if ( ARG_ParseTclListStatic( interp, wallsList, INT_Type, walls, wallsList.argc, &numWalls )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_size, arg_table );
    delete [] walls;
    return TCL_ERROR;
  }

  if (numWalls != wallsList.argc) {
     Tcl_AppendResult( interp, "error in H list", (char *)NULL );
     delete [] walls;
     return TCL_ERROR;
  }

  // Do work of command:

  if ( geom->SetWalls(numWalls,walls) == CV_ERROR ) {
    Tcl_AppendResult( interp, "error in method ",
		      (char *)NULL );
    delete [] walls;
    return TCL_ERROR;
  }

  return TCL_OK;
}



// --------------------------
// cvMesh_GetModelFaceInfoMtd
// --------------------------

static int cvMesh_GetModelFaceInfoMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] )
{
  cvMeshObject *geom = (cvMeshObject *)clientData;
  char *usage;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  char info[99999];
  geom->GetModelFaceInfo(info);

  Tcl_SetResult( interp, info, TCL_VOLATILE );

  return TCL_OK;
}
