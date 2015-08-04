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
#include "cv_adapt_init.h"
#include "cvAdaptObject.h"
#include "cv_arg.h"
#include "cv_misc_utils.h"
#include "cv_vtk_utils.h"
#include "cvPolyData.h"
#include "cv_sys_geom.h"

#include "cvFactoryRegistrar.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "cv_globals.h"

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

static int cvAdapt_PrintStatsMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );
static int cvAdapt_CreateInternalMeshObjectMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );

// Helper functions
// ----------------

static void gdscAdaptPrintMethods( Tcl_Interp *interp );

void DeletegdscAdapt( ClientData clientData );


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

  //Callable tcl functions
  Tcl_CreateCommand( interp, "adapt_newObject", cvAdapt_NewObjectCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "PrintStats", cvAdapt_PrintStatsMtd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "CreateInternalMeshObject", cvAdapt_CreateInternalMeshObjectMtd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  // Initialize
  cvAdaptObject::gCurrentKernel = KERNEL_INVALID;

#ifdef USE_TET_ADAPTOR
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
  char *resultName;

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
  cvAdaptObject *adaptor;
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
		     (ClientData)adaptor, DeletegdscAdapt );

  return TCL_OK;
}

// -------------
// Adapt_Object
// -------------
int cvAdapt_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  if ( argc == 1 ) {
    gdscAdaptPrintMethods( interp );
    return TCL_OK;
  }

  if ( Tcl_StringMatch( argv[1], "PrintStats" ) ) {
    if ( cvAdapt_PrintStatsMtd( clientData, interp, argc, argv ) != CV_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "CreateInternalMeshObject" ) ) {
    if ( cvAdapt_CreateInternalMeshObjectMtd( clientData, interp, argc, argv ) != CV_OK ) {
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
// DeletegdscAdapt
// -------------
// This is the deletion call-back for cvAdaptObject object commands.

void DeletegdscAdapt( ClientData clientData ) {
    cvAdaptObject *geom = (cvAdaptObject *)clientData;
  
    gRepository->UnRegister( geom->GetName() );
}

// ------------
// gdscAdaptPrintMethods
// ------------

static void gdscAdaptPrintMethods( Tcl_Interp *interp )
{

  tcl_printstr(interp, "PrintStats\n");
  tcl_printstr(interp, "CreateInternalMeshObject\n");
  
  return;
}

// ----------------
// cvAdapt_PrintStatsMtd
// ----------------

static int cvAdapt_PrintStatsMtd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  cvAdaptObject *geom = (cvAdaptObject *)clientData;

  if (geom->PrintStats() == CV_OK) {
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }
}

// ----------------
// cvAdapt_CreateInternalMeshObjectMtd
// ----------------
int cvAdapt_CreateInternalMeshObjectMtd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
// ----------------
  char *resultName;
  char *meshFileName = NULL;  
  char *solidFileName = NULL;

  char *usage;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-meshfile", STRING_Type, &meshFileName, NULL, REQUIRED, 0, { 0 } },
    { "-solidfile", STRING_Type, &solidFileName, NULL, GDSC_OPTIONAL, 0, { 0 } }, 
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
  geom->CreateInternalMeshObject(interp);//, meshFileName, solidFileName );

  // Make a new Tcl command:
  Tcl_SetResult( interp, geom->GetName(), TCL_VOLATILE );

  return TCL_OK;
}
