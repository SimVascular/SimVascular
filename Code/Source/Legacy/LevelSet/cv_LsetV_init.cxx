/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
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
 */

#include "SimVascular.h" 

#include <stdio.h>
#include <string.h>
#include "cv_LsetV_init.h"
#include "cvLevelSetVelocity.h"
#include "cvLevelSetVelocityImage.h"
#include "cvLevelSetVelocityKGI.h"
#include "cvLevelSetVelocityConstant.h"
#include "cvRepository.h"
#include "cvLevelSetVelocityThreshold.h"
#include "cvLevelSetVelocityPotential.h"
#include "cvLevelSetVelocityExponentialDecay.h"
#include "cvLevelSetVelocitySmooth.h"
#include "cv_arg.h"
#include "cv_misc_utils.h"
#include "cv_vtk_utils.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "cv_globals.h"

// Prototypes:
// -----------

  // -------------------------
  // Constructors / destructor
  // -------------------------

int LsetVKGI_NewCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] );

int LsetVConst_NewCmd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] );

int LsetVThr_NewCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] );

int LsetVPotential_NewCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int LsetVExpDecay_NewCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] );

int LsetVSmooth_NewCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

void DeleteLsetV( ClientData clientData );

  // --------------
  // Query commands
  // --------------

int LsetV_ListInstancesCmd( ClientData clientData, Tcl_Interp *interp,
			    int argc, CONST84 char *argv[] );

int LsetVKGI_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] );

int LsetVConst_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] );

int LsetVThr_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] );

int LsetVPotential_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] );

int LsetVExpDecay_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] );

int LsetVSmooth_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );

  // -----------------------------------------
  // Object commands (i.e. method dispatchers)
  // -----------------------------------------

int LsetV_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] );

int LsetVImage_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] );

int LsetVKGI_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

int LsetVConst_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] );

int LsetVThr_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

int LsetVPotential_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] );

int LsetVExpDecay_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] );

int LsetVSmooth_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

  // ---------------
  // Method commands
  // ---------------

static int LsetV_SetStopVMtd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] );

static int LsetV_GetStopVMtd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] );

static int LsetV_GetMemoryUsageMtd( ClientData clientData, Tcl_Interp *interp,
				    int argc, CONST84 char *argv[] );

static int LsetVImage_SetImageObjMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] );

static int LsetVImage_GetImageObjMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] );

static int LsetVImage_SetImageMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] );

static int LsetVImage_GetImageMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] );

static int LsetVImage_GetMagGradRangeMtd( ClientData clientData,
					  Tcl_Interp *interp,
					  int argc, CONST84 char *argv[] );

static int LsetVImage_GetXYMagGradRangeMtd( ClientData clientData,
					    Tcl_Interp *interp,
					    int argc, CONST84 char *argv[] );

static int LsetVImage_GetZMagGradRangeMtd( ClientData clientData,
					   Tcl_Interp *interp,
					   int argc, CONST84 char *argv[] );

static int LsetVImage_GetIntensityRangeMtd( ClientData clientData,
					    Tcl_Interp *interp,
					    int argc, CONST84 char *argv[] );

    // ----
    // cvLevelSetVelocityKGI
    // ----

static int LsetVKGI_SetConstMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

static int LsetVKGI_GetConstMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

    // ------
    // cvLevelSetVelocityConstant
    // ------

static int LsetVConst_SetConstMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] );

static int LsetVConst_GetConstMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] );

    // ----
    // cvLevelSetVelocityThreshold
    // ----

static int LsetVThr_SetConstMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

static int LsetVThr_GetConstMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

    // ----------
    // cvLevelSetVelocityPotential
    // ----------

static int LsetVPotential_SetConstMtd( ClientData clientData,
				       Tcl_Interp *interp,
				       int argc, CONST84 char *argv[] );

static int LsetVPotential_GetConstMtd( ClientData clientData,
				       Tcl_Interp *interp,
				       int argc, CONST84 char *argv[] );

    // ---------
    // cvLevelSetVelocityExponentialDecay
    // ---------

static int LsetVExpDecay_SetConstMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] );

static int LsetVExpDecay_GetConstMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] );

    // -------
    // cvLevelSetVelocitySmooth
    // -------

static int LsetVSmooth_SetConstMtd( ClientData clientData,
				    Tcl_Interp *interp,
				    int argc, CONST84 char *argv[] );

static int LsetVSmooth_GetConstMtd( ClientData clientData,
				    Tcl_Interp *interp,
				    int argc, CONST84 char *argv[] );


// Helper fns
// ----------

static int NewName( CONST84 char *name, Tcl_Interp *interp );
static void PrintMethods_V();
static void PrintMethods_VImage();
static void PrintMethods_VKGI();
static void PrintMethods_VConst();
static void PrintMethods_VThr();
static void PrintMethods_VPotential();
static void PrintMethods_VExpDecay();
static void PrintMethods_VSmooth();


// -------
// NewName
// -------

static int NewName( CONST84 char *name, Tcl_Interp *interp )
{
  int code;

  code = Tcl_VarEval( interp, "info commands ", name, (char *)NULL );
  if ( code != TCL_OK ) {
    return 0;
  }
  if ( strlen( Tcl_GetStringResult(interp) ) == 0 ) {
    return 1;
  } else {
    return 0;
  }
}


// ----------
// LsetV_Init
// ----------
// Initialize Tcl hash table for level set velocity objects, and bind
// action-oriented commands to Tcl interp.

// Commands:
//   - lsetVKGI
//   - { lsetVFoo }
//   - lsetV_instances
//   - lsetV_methods

int Lsetv_Init( Tcl_Interp *interp )
{
  Tcl_InitHashTable( &gLsetVTable, TCL_STRING_KEYS );

  Tcl_CreateCommand( interp, "lsetVKGI", LsetVKGI_NewCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  Tcl_CreateCommand( interp, "lsetVConst", LsetVConst_NewCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  Tcl_CreateCommand( interp, "lsetVThr", LsetVThr_NewCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  Tcl_CreateCommand( interp, "lsetVPotential", LsetVPotential_NewCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  Tcl_CreateCommand( interp, "lsetVExpDecay", LsetVExpDecay_NewCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  Tcl_CreateCommand( interp, "lsetVSmooth", LsetVSmooth_NewCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  Tcl_CreateCommand( interp, "lsetV_instances", LsetV_ListInstancesCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  Tcl_CreateCommand( interp, "lsetV_methods", LsetVKGI_ListMethodsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  return TCL_OK;
}


// ---------------
// LsetVKGI_NewCmd
// ---------------

int LsetVKGI_NewCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] )
{
  CONST84 char* vName;
  cvLevelSetVelocityKGI *v;
  Tcl_HashEntry *entryPtr;
  int newEntry = 0;

  // Check syntax:
  if (argc != 2) {
    Tcl_AppendResult( interp, "usage: ", argv[0], " <objName>",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure this is a new object name:
  vName = argv[1];
  if ( !NewName( vName, interp ) ) {
    Tcl_AppendResult( interp, "object \"", vName,
		      "\" already exists", (char *)NULL );
    return TCL_ERROR;
  }

  // Allocate new cvLevelSetVelocity object (of class cvLevelSetVelocityKGI):
  v = new cvLevelSetVelocityKGI;
  if ( v == NULL ) {
    Tcl_AppendResult( interp, "error allocating object \"", vName,
		      "\"", (char *)NULL );
    return TCL_ERROR;
  }

  strcpy( v->tclName_, vName );
  entryPtr = Tcl_CreateHashEntry( &gLsetVTable, vName, &newEntry );
  if ( !newEntry ) {
    Tcl_SetResult( interp, "error updating LsetV hash table",
		   TCL_STATIC );
    delete v;
    return TCL_ERROR;
  }
  Tcl_SetHashValue( entryPtr, (ClientData)v );
  char vNameStr[2048];
  vNameStr[0]='\0';
  sprintf(vNameStr,"%s",vName);
  Tcl_SetResult( interp, vNameStr, TCL_VOLATILE );
  Tcl_CreateCommand( interp,  Tcl_GetStringResult(interp), LsetVKGI_ObjectCmd,
		     (ClientData)v, DeleteLsetV );
  return TCL_OK;
}


// -----------------
// LsetVConst_NewCmd
// -----------------

// lsetVConst <objName>

int LsetVConst_NewCmd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] )
{
  CONST84 char *vName;
  cvLevelSetVelocityConstant *v;
  Tcl_HashEntry *entryPtr;
  int newEntry = 0;

  // Check syntax:
  if (argc != 2) {
    Tcl_AppendResult( interp, "usage: ", argv[0], " <objName>",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure this is a new object name:
  vName = argv[1];
  if ( !NewName( vName, interp ) ) {
    Tcl_AppendResult( interp, "object \"", vName,
		      "\" already exists", (char *)NULL );
    return TCL_ERROR;
  }

  // Allocate new cvLevelSetVelocity object (of class cvLevelSetVelocityConstant):
  v = new cvLevelSetVelocityConstant;
  if ( v == NULL ) {
    Tcl_AppendResult( interp, "error allocating object \"", vName,
		      "\"", (char *)NULL );
    return TCL_ERROR;
  }

  strcpy( v->tclName_, vName );
  entryPtr = Tcl_CreateHashEntry( &gLsetVTable, vName, &newEntry );
  if ( !newEntry ) {
    Tcl_SetResult( interp, "error updating LsetV hash table",
		   TCL_STATIC );
    delete v;
    return TCL_ERROR;
  }
  Tcl_SetHashValue( entryPtr, (ClientData)v );
  char vNameStr[2048];
  vNameStr[0]='\0';
  sprintf(vNameStr,"%s",vName);
  Tcl_SetResult( interp, vNameStr, TCL_VOLATILE );
  Tcl_CreateCommand( interp,  Tcl_GetStringResult(interp), LsetVConst_ObjectCmd,
		     (ClientData)v, DeleteLsetV );
  return TCL_OK;
}


// ---------------
// LsetVThr_NewCmd
// ---------------

int LsetVThr_NewCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] )
{
  CONST84 char *vName;
  cvLevelSetVelocityThreshold *v;
  Tcl_HashEntry *entryPtr;
  int newEntry = 0;

  // Check syntax:
  if (argc != 2) {
    Tcl_AppendResult( interp, "usage: ", argv[0], " <objName>",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure this is a new object name:
  vName = argv[1];
  if ( !NewName( vName, interp ) ) {
    Tcl_AppendResult( interp, "object \"", vName,
		      "\" already exists", (char *)NULL );
    return TCL_ERROR;
  }

  // Allocate new cvLevelSetVelocity object (of class cvLevelSetVelocityThreshold):
  v = new cvLevelSetVelocityThreshold;
  if ( v == NULL ) {
    Tcl_AppendResult( interp, "error allocating object \"", vName,
		      "\"", (char *)NULL );
    return TCL_ERROR;
  }

  strcpy( v->tclName_, vName );
  entryPtr = Tcl_CreateHashEntry( &gLsetVTable, vName, &newEntry );
  if ( !newEntry ) {
    Tcl_SetResult( interp, "error updating LsetV hash table",
		   TCL_STATIC );
    delete v;
    return TCL_ERROR;
  }
  Tcl_SetHashValue( entryPtr, (ClientData)v );
  char vNameStr[2048];
  vNameStr[0]='\0';
  sprintf(vNameStr,"%s",vName);
  Tcl_SetResult( interp, vNameStr, TCL_VOLATILE );
  Tcl_CreateCommand( interp,  Tcl_GetStringResult(interp), LsetVThr_ObjectCmd,
		     (ClientData)v, DeleteLsetV );
  return TCL_OK;
}


// ---------------------
// LsetVPotential_NewCmd
// ---------------------

int LsetVPotential_NewCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  CONST84 char *vName;
  cvLevelSetVelocityPotential *v;
  Tcl_HashEntry *entryPtr;
  int newEntry = 0;

  // Check syntax:
  if (argc != 2) {
    Tcl_AppendResult( interp, "usage: ", argv[0], " <objName>",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure this is a new object name:
  vName = argv[1];
  if ( !NewName( vName, interp ) ) {
    Tcl_AppendResult( interp, "object \"", vName,
		      "\" already exists", (char *)NULL );
    return TCL_ERROR;
  }

  // Allocate new cvLevelSetVelocity object (of class cvLevelSetVelocityPotential):
  v = new cvLevelSetVelocityPotential;
  if ( v == NULL ) {
    Tcl_AppendResult( interp, "error allocating object \"", vName,
		      "\"", (char *)NULL );
    return TCL_ERROR;
  }

  strcpy( v->tclName_, vName );
  entryPtr = Tcl_CreateHashEntry( &gLsetVTable, vName, &newEntry );
  if ( !newEntry ) {
    Tcl_SetResult( interp, "error updating LsetV hash table",
		   TCL_STATIC );
    delete v;
    return TCL_ERROR;
  }
  Tcl_SetHashValue( entryPtr, (ClientData)v );
  char vNameStr[2048];
  vNameStr[0]='\0';
  sprintf(vNameStr,"%s",vName);
  Tcl_SetResult( interp, vNameStr, TCL_VOLATILE );
  Tcl_CreateCommand( interp,  Tcl_GetStringResult(interp), LsetVPotential_ObjectCmd,
		     (ClientData)v, DeleteLsetV );
  return TCL_OK;
}


// --------------------
// LsetVExpDecay_NewCmd
// --------------------

int LsetVExpDecay_NewCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  CONST84 char *vName;
  cvLevelSetVelocityExponentialDecay *v;
  Tcl_HashEntry *entryPtr;
  int newEntry = 0;

  // Check syntax:
  if (argc != 2) {
    Tcl_AppendResult( interp, "usage: ", argv[0], " <objName>",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure this is a new object name:
  vName = argv[1];
  if ( !NewName( vName, interp ) ) {
    Tcl_AppendResult( interp, "object \"", vName,
		      "\" already exists", (char *)NULL );
    return TCL_ERROR;
  }

  // Allocate new cvLevelSetVelocity object (of class cvLevelSetVelocityExponentialDecay):
  v = new cvLevelSetVelocityExponentialDecay;
  if ( v == NULL ) {
    Tcl_AppendResult( interp, "error allocating object \"", vName,
		      "\"", (char *)NULL );
    return TCL_ERROR;
  }

  strcpy( v->tclName_, vName );
  entryPtr = Tcl_CreateHashEntry( &gLsetVTable, vName, &newEntry );
  if ( !newEntry ) {
    Tcl_SetResult( interp, "error updating LsetV hash table",
		   TCL_STATIC );
    delete v;
    return TCL_ERROR;
  }
  Tcl_SetHashValue( entryPtr, (ClientData)v );
  char vNameStr[2048];
  vNameStr[0]='\0';
  sprintf(vNameStr,"%s",vName);
  Tcl_SetResult( interp, vNameStr, TCL_VOLATILE );
  Tcl_CreateCommand( interp,  Tcl_GetStringResult(interp), LsetVExpDecay_ObjectCmd,
		     (ClientData)v, DeleteLsetV );
  return TCL_OK;
}


// ------------------
// LsetVSmooth_NewCmd
// ------------------

int LsetVSmooth_NewCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  CONST84 char *vName;
  cvLevelSetVelocitySmooth *v;
  Tcl_HashEntry *entryPtr;
  int newEntry = 0;

  // Check syntax:
  if (argc != 2) {
    Tcl_AppendResult( interp, "usage: ", argv[0], " <objName>",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure this is a new object name:
  vName = argv[1];
  if ( !NewName( vName, interp ) ) {
    Tcl_AppendResult( interp, "object \"", vName,
		      "\" already exists", (char *)NULL );
    return TCL_ERROR;
  }

  // Allocate new cvLevelSetVelocity object (of class cvLevelSetVelocitySmooth):
  v = new cvLevelSetVelocitySmooth;
  if ( v == NULL ) {
    Tcl_AppendResult( interp, "error allocating object \"", vName,
		      "\"", (char *)NULL );
    return TCL_ERROR;
  }

  strcpy( v->tclName_, vName );
  entryPtr = Tcl_CreateHashEntry( &gLsetVTable, vName, &newEntry );
  if ( !newEntry ) {
    Tcl_SetResult( interp, "error updating LsetV hash table",
		   TCL_STATIC );
    delete v;
    return TCL_ERROR;
  }
  Tcl_SetHashValue( entryPtr, (ClientData)v );
  char vNameStr[2048];
  vNameStr[0]='\0';
  sprintf(vNameStr,"%s",vName);
  Tcl_SetResult( interp, vNameStr, TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), LsetVSmooth_ObjectCmd,
		     (ClientData)v, DeleteLsetV );
  return TCL_OK;
}


// -----------
// DeleteLsetV
// -----------
// Deletion callback invoked when the Tcl object is deleted.  Delete
// Tcl hash table entry as well as the cvLevelSetVelocity object itself.

void DeleteLsetV( ClientData clientData )
{
  cvLevelSetVelocity *v = (cvLevelSetVelocity *)clientData;
  Tcl_HashEntry *entryPtr;

  entryPtr = Tcl_FindHashEntry( &gLsetVTable, v->tclName_ );
  if ( entryPtr == NULL ) {
    printf("Error looking up lsetV object %s for deletion.\n",
	   v->tclName_);
  } else {
    Tcl_DeleteHashEntry( entryPtr );
  }
  delete v;
}


// -----------------------
// LsetVKGI_ListMethodsCmd
// -----------------------

int LsetVKGI_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  if ( argc != 1 ) {
    Tcl_AppendResult( interp, "usage: ", argv[0], (char *)NULL );
    return TCL_ERROR;
  }

  PrintMethods_V();
  PrintMethods_VImage();
  PrintMethods_VKGI();
  return TCL_OK;
}


// -------------------------
// LsetVConst_ListMethodsCmd
// -------------------------

int LsetVConst_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] )
{
  if ( argc != 1 ) {
    Tcl_AppendResult( interp, "usage: ", argv[0], (char *)NULL );
    return TCL_ERROR;
  }

  PrintMethods_V();
  PrintMethods_VConst();
  return TCL_OK;
}


// -----------------------
// LsetVThr_ListMethodsCmd
// -----------------------

int LsetVThr_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] )
{
  if ( argc != 1 ) {
    Tcl_AppendResult( interp, "usage: ", argv[0], (char *)NULL );
    return TCL_ERROR;
  }

  PrintMethods_V();
  PrintMethods_VImage();
  PrintMethods_VThr();
  return TCL_OK;
}


// -----------------------------
// LsetVPotential_ListMethodsCmd
// -----------------------------

int LsetVPotential_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] )
{
  if ( argc != 1 ) {
    Tcl_AppendResult( interp, "usage: ", argv[0], (char *)NULL );
    return TCL_ERROR;
  }

  PrintMethods_V();
  PrintMethods_VImage();
  PrintMethods_VPotential();
  return TCL_OK;
}


// ----------------------------
// LsetVExpDecay_ListMethodsCmd
// ----------------------------

int LsetVExpDecay_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] )
{
  if ( argc != 1 ) {
    Tcl_AppendResult( interp, "usage: ", argv[0], (char *)NULL );
    return TCL_ERROR;
  }

  PrintMethods_V();
  PrintMethods_VImage();
  PrintMethods_VExpDecay();
  return TCL_OK;
}


// --------------------------
// LsetVSmooth_ListMethodsCmd
// --------------------------

int LsetVSmooth_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] )
{
  if ( argc != 1 ) {
    Tcl_AppendResult( interp, "usage: ", argv[0], (char *)NULL );
    return TCL_ERROR;
  }

  PrintMethods_V();
  PrintMethods_VSmooth();
  return TCL_OK;
}


// ----------------------
// LsetV_ListInstancesCmd
// ----------------------

int LsetV_ListInstancesCmd( ClientData clientData, Tcl_Interp *interp,
			    int argc, CONST84 char *argv[] )
{
  Tcl_HashEntry *entryPtr;
  Tcl_HashSearch search;

  if ( argc != 1 ) {
    Tcl_AppendResult( interp, "usage: ", argv[0], (char *)NULL );
    return TCL_ERROR;
  }

  for ( entryPtr = Tcl_FirstHashEntry( &gLsetVTable, &search );
	entryPtr != NULL;
	entryPtr = Tcl_NextHashEntry( &search ) ) {
    Tcl_AppendElement( interp, (char*)(Tcl_GetHashKey( &gLsetVTable, entryPtr )) );
  }
  return TCL_OK;
}


// --------------
// PrintMethods_V
// --------------

static void PrintMethods_V()
{
  printf("cvLevelSetVelocity\n");
  printf("  GetClassName\n");
  printf("  SetStopV\n");
  printf("  GetStopV\n");

  return;
}


// -------------------
// PrintMethods_VImage
// -------------------

static void PrintMethods_VImage()
{
  printf("cvLevelSetVelocityImage\n");
  printf("  SetImage\n");
  printf("  GetImage\n");
  printf("  SetImageObj\n");
  printf("  GetImageObj\n");
  printf("  GetMagGradRange\n");
  printf("  GetIntensityRange\n");

  return;
}


// -----------------
// PrintMethods_VKGI
// -----------------

static void PrintMethods_VKGI()
{
  printf("cvLevelSetVelocityKGI\n");
  printf("  SetConst\n");
  printf("  GetConst\n");

  return;
}


// -------------------
// PrintMethods_VConst
// -------------------

static void PrintMethods_VConst()
{
  printf("cvLevelSetVelocityConstant\n");
  printf("  SetConst\n");
  printf("  GetConst\n");

  return;
}


// -----------------
// PrintMethods_VThr
// -----------------

static void PrintMethods_VThr()
{
  printf("cvLevelSetVelocityThreshold\n");
  printf("  SetConst\n");
  printf("  GetConst\n");

  return;
}


// -----------------------
// PrintMethods_VPotential
// -----------------------

static void PrintMethods_VPotential()
{
  printf("cvLevelSetVelocityPotential\n");
  printf("  SetConst\n");
  printf("  GetConst\n");

  return;
}


// ----------------------
// PrintMethods_VExpDecay
// ----------------------

static void PrintMethods_VExpDecay()
{
  printf("cvLevelSetVelocityExponentialDecay\n");
  printf("  SetConst\n");
  printf("  GetConst\n");

  return;
}


// --------------------
// PrintMethods_VSmooth
// --------------------

static void PrintMethods_VSmooth()
{
  printf("cvLevelSetVelocitySmooth\n");
  printf("  SetConst\n");
  printf("  GetConst\n");

  return;
}


// ---------------
// LsetV_ObjectCmd
// ---------------

int LsetV_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] )
{
  // This PrintMethods is un-reachable:
  if ( argc == 1 ) {
    PrintMethods_V();
    return TCL_OK;
  }

  // GetClassName should also be un-reachable:
  if ( Tcl_StringMatch( argv[1], "GetClassName" ) ) {
    Tcl_SetResult( interp, "lsetV", TCL_STATIC );
    return TCL_OK;
  }

  // These, however, should get "inherited":
  else if ( Tcl_StringMatch( argv[1], "SetStopV" ) ) {
    if ( LsetV_SetStopVMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetStopV" ) ) {
    if ( LsetV_GetStopVMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetMemoryUsage" ) ) {
    if ( LsetV_GetMemoryUsageMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }
  } else {
    Tcl_AppendResult( interp, "\"", argv[1],
		      "\" not a recognized lsetV method", (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// -----------------
// LsetV_SetStopVMtd
// -----------------

int LsetV_SetStopVMtd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] )
{
  char *usage;
  cvLevelSetVelocity *vel = (cvLevelSetVelocity *)clientData;
  double stopV;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-value", DOUBLE_Type, &stopV, NULL, REQUIRED, 0, { 0 } }
  };
  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2, table_size, arg_table )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  vel->SetStopV( stopV );

  return TCL_OK;
}


// -----------------
// LsetV_GetStopVMtd
// -----------------
// Since this returns a SINGLE value, we do NOT use the flag/value
// pair format which is used with some other GetFooMtd functions.
// Those results are designed to be compatible with Tcl's array set /
// get commands.  Here, we just return the single value by itself
// s.t. the cmd result can be set directly to a Tcl var.

int LsetV_GetStopVMtd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocity *vel = (cvLevelSetVelocity *)clientData;
  char *usage;
  double stopV;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  stopV = vel->GetStopV();

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%f", stopV );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );

  return TCL_OK;
}


// -----------------------
// LsetV_GetMemoryUsageMtd
// -----------------------

int LsetV_GetMemoryUsageMtd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocity *vel = (cvLevelSetVelocity *)clientData;
  char *usage;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d", vel->GetMemoryUsage() );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );

  return TCL_OK;
}


// -------------------------
// LsetVImage_SetImageObjMtd
// -------------------------
// Client data is in fact a concrete velocity object.  We've
// introduced the intermediate class cvLevelSetVelocityImage to serve as an abstraction
// to all image-dependent velocities.  Since we cast clientData to
// cvLevelSetVelocityImage * here, this function can only be called as a method
// implementation for classes derived from cvLevelSetVelocityImage.

int LsetVImage_SetImageObjMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityImage *v = (cvLevelSetVelocityImage *)clientData;
  char *usage;
  RepositoryDataT type;
  cvRepositoryData *img;
  char *imgName;
  int closed = 0;
  vtkFloatingPointType pixDimsF[3];
  double pixDimsD[3];
  vtkFloatingPointType originF[3];
  double originD[3];
  int imgDims[3];
  short *imgData = NULL;
  float *fimgData = NULL;
  int numImgData;
  vtkStructuredPoints *vtksp;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &imgName, NULL, REQUIRED, 0, { 0 } },
    { "-closed", INT_Type, &closed, NULL, GDSC_OPTIONAL, 0, { 0 } },
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

  // Look up given image object:
  img = gRepository->GetObject( imgName );
  if ( img == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", imgName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure image is of type STRUCTURED_PTS_T:
  type = img->GetType();
  if ( type != STRUCTURED_PTS_T ) {
    Tcl_AppendResult( interp, "error: object ", imgName,
		      "not of type StructuredPts", (char *)NULL );
    return TCL_ERROR;
  }

  // Retrive geometric information:
  vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();
  vtksp->GetDimensions( imgDims );
  vtksp->GetSpacing( pixDimsF );
  vtksp->GetOrigin( originF );

  originF[0] -= pixDimsF[0] / 2;
  originF[1] -= pixDimsF[1] / 2;
  originF[2] -= pixDimsF[2] / 2;

  originD[0] = originF[0];
  originD[1] = originF[1];
  originD[2] = originF[2];

  pixDimsD[0] = pixDimsF[0];
  pixDimsD[1] = pixDimsF[1];
  pixDimsD[2] = pixDimsF[2];

  switch ( vtksp->GetScalarType() ) {
  case VTK_SHORT:
    if ( VtkUtils_MakeShortArray( vtksp->GetPointData()->GetScalars(),
				  &numImgData, &imgData ) != CV_OK ) {
      Tcl_AppendResult( interp, "error accessing short data in object ",
			imgName, (char *)NULL );
      return TCL_ERROR;
    }
    if ( (imgDims[0] * imgDims[1] * imgDims[2]) != numImgData ) {
      Tcl_AppendResult( interp, "image size mismatch in object ",
			imgName, (char *)NULL );
      delete [] imgData;
      return TCL_ERROR;
    }
    if ( v->SetImage( imgData, numImgData, imgDims, pixDimsD, originD, closed )
	 != CV_OK ) {
      Tcl_SetResult( interp, "error setting image", TCL_STATIC );
      delete [] imgData;
      return TCL_ERROR;
    }
    break;

  case VTK_FLOAT:
    if ( VtkUtils_MakeFloatArray( vtksp->GetPointData()->GetScalars(),
				  &numImgData, &fimgData ) != CV_OK ) {
      Tcl_AppendResult( interp, "error accessing float data in object ",
			imgName, (char *)NULL );
      return TCL_ERROR;
    }
    if ( (imgDims[0] * imgDims[1] * imgDims[2]) != numImgData ) {
      Tcl_AppendResult( interp, "image size mismatch in object ",
			imgName, (char *)NULL );
      delete [] fimgData;
      return TCL_ERROR;
    }
    if ( v->SetImage( fimgData, numImgData, imgDims, pixDimsD, originD,
		      closed ) != CV_OK ) {
      Tcl_SetResult( interp, "error setting image", TCL_STATIC );
      delete [] fimgData;
      return TCL_ERROR;
    }
    break;

  default:
    Tcl_SetResult( interp, "only short and float scalar types can be "
		   "used to set image obj", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( imgData != NULL ) delete [] imgData;
  if ( fimgData != NULL ) delete [] fimgData;
  return TCL_OK;
}


// -------------------------
// LsetVImage_GetImageObjMtd
// -------------------------
// See comments at LsetVImage_SetImageObjMtd.

int LsetVImage_GetImageObjMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityImage *v = (cvLevelSetVelocityKGI *)clientData;
  char *usage;
  char *objName;
  Image_T *image;
  cvStrPts *sp;
  int numData, i;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-dst", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
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

  // Check for given object name:
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "object ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Retrieve image struct:
  if ( !(v->GetImage( &image ) ) ) {
    Tcl_SetResult( interp, "error getting image", TCL_STATIC );
    return TCL_ERROR;
  }

  // Copy intensity data over to vtkDataArray:
  numData = image->imgDims[0] * image->imgDims[1] * image->imgDims[2];
  vtkShortArray *vtkdata = vtkShortArray::New();
  //vtkdata->SetDataTypeToShort();
  for (i = 0; i < numData; i++) {
    vtkdata->InsertTuple1( i, (short)image->pixels[i].intensity );
  }

  // Build vtkStructuredPoints:
  vtkStructuredPoints *vtksp = vtkStructuredPoints::New();
  // not allowed in vtk-6.0.0  vtksp->SetScalarType( VTK_SHORT );
  vtksp->GetPointData()->SetScalars( vtkdata );
  vtksp->SetDimensions( image->imgDims[0], image->imgDims[1],
			image->imgDims[2] );
  vtksp->SetSpacing( image->pixelDims[0], image->pixelDims[1],
		     image->pixelDims[2] );
  vtksp->SetOrigin( image->pixelDims[0] / 2, image->pixelDims[1] / 2,
		    image->pixelDims[2] / 2 );

  // Instantiate cvRepositoryData object and register:
  sp = new cvStrPts( vtksp );
  vtkdata->Delete();
  vtksp->Delete();

  sp->SetName( objName );
  if ( ! ( gRepository->Register( sp->GetName(), sp ) ) ) {
    Tcl_AppendResult( interp, "error registering object ", objName,
		      " in repository", (char *)NULL );
    delete sp;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, sp->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// --------------------
// LsetVImage_ObjectCmd
// --------------------

int LsetVImage_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  if ( Tcl_StringMatch( argv[1], "SetImage" ) ) {
    if ( LsetVImage_SetImageMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetImage" ) ) {
    if ( LsetVImage_GetImageMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "SetImageObj" ) ) {
    if ( LsetVImage_SetImageObjMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetImageObj" ) ) {
    if ( LsetVImage_GetImageObjMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetMagGradRange" ) ) {
    if ( LsetVImage_GetMagGradRangeMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetXYMagGradRange" ) ) {
    if ( LsetVImage_GetXYMagGradRangeMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetZMagGradRange" ) ) {
    if ( LsetVImage_GetZMagGradRangeMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetIntensityRange" ) ) {
    if ( LsetVImage_GetIntensityRangeMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }
  } else {

    // "Inherit" methods from LsetV...
    if ( LsetV_ObjectCmd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  }

  return TCL_OK;
}


// ------------------
// LsetVKGI_ObjectCmd
// ------------------

int LsetVKGI_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  if ( argc == 1 ) {
    PrintMethods_V();
    PrintMethods_VImage();
    PrintMethods_VKGI();
    return TCL_OK;
  }

  if ( Tcl_StringMatch( argv[1], "GetClassName" ) ) {
    Tcl_SetResult( interp, "lsetVKGI", TCL_STATIC );
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "SetConst" ) ) {
    if ( LsetVKGI_SetConstMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetConst" ) ) {
    if ( LsetVKGI_GetConstMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else {

    // "Inherit" methods from LsetVImage...
    if ( LsetVImage_ObjectCmd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  }

  return TCL_OK;
}


// --------------------
// LsetVKGI_SetConstMtd
// --------------------

static int LsetVKGI_SetConstMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityKGI *v = (cvLevelSetVelocityKGI *)clientData;
  char *usage;
  double eK, eI;
  double balloon = 1.0;   // cvLevelSetVelocityKGI requires some value
  double gradIPow = 1.0;  // cvLevelSetVelocityKGI requires some value
  double beta = 0.0;      // cvLevelSetVelocityKGI requires some value
  int localStop;          // don't need to supply a default to cvLevelSetVelocityKGI

  int table_size = 6;
  ARG_Entry arg_table[] = {
    { "-eK", DOUBLE_Type, &eK, NULL, REQUIRED, 0, { 0 } },
    { "-eI", DOUBLE_Type, &eI, NULL, REQUIRED, 0, { 0 } },
    { "-balloon", DOUBLE_Type, &balloon, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-gradIPow", DOUBLE_Type, &gradIPow, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-beta", DOUBLE_Type, &beta, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-localStop", INT_Type, &localStop, NULL, GDSC_OPTIONAL, 0, { 0 } },
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
  if ( v->SetEK( eK ) != CV_OK ) {
    Tcl_SetResult( interp, "error setting eK", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( v->SetEI( eI ) != CV_OK ) {
    Tcl_SetResult( interp, "error setting eI", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( v->SetBalloonF( balloon ) != CV_OK ) {
    Tcl_SetResult( interp, "error setting balloon force", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( v->SetGradIPow( gradIPow ) != CV_OK ) {
    Tcl_SetResult( interp, "error setting grad(I) power", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( v->SetBeta( beta ) != CV_OK ) {
    Tcl_SetResult( interp, "error setting beta (geodesic control)",
		   TCL_STATIC );
    return TCL_ERROR;
  }
  if ( arg_table[5].valid ) {
    if ( v->SetApplyLocalStop( localStop ) != CV_OK ) {
      Tcl_SetResult( interp, "error setting local stop flag", TCL_STATIC );
      return TCL_ERROR;
    }
  }

  return TCL_OK;
}


// --------------------
// LsetVKGI_GetConstMtd
// --------------------

static int LsetVKGI_GetConstMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityKGI *v = (cvLevelSetVelocityKGI *)clientData;
  char *usage;
  double eK, eI, balloon, gradIPow, beta;
  int localStop;
  char dummyStr[CV_STRLEN];

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );

  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( v->GetEK( &eK ) == CV_OK ) {
    sprintf( dummyStr, "%f", eK );
    Tcl_AppendElement( interp, "-eK" );
    Tcl_AppendElement( interp, dummyStr );
  }

  if ( v->GetEI( &eI ) == CV_OK ) {
    sprintf( dummyStr, "%f", eI );
    Tcl_AppendElement( interp, "-eI" );
    Tcl_AppendElement( interp, dummyStr );
  }

  if ( v->GetBalloonF( &balloon ) == CV_OK ) {
    sprintf( dummyStr, "%f", balloon );
    Tcl_AppendElement( interp, "-balloon" );
    Tcl_AppendElement( interp, dummyStr );
  }

  if ( v->GetGradIPow( &gradIPow ) == CV_OK ) {
    sprintf( dummyStr, "%f", gradIPow );
    Tcl_AppendElement( interp, "-gradIPow" );
    Tcl_AppendElement( interp, dummyStr );
  }

  if ( v->GetBeta( &beta ) == CV_OK ) {
    sprintf( dummyStr, "%f", beta );
    Tcl_AppendElement( interp, "-beta" );
    Tcl_AppendElement( interp, dummyStr );
  }

  if ( v->GetApplyLocalStop( &localStop ) == CV_OK ) {
    sprintf( dummyStr, "%d", localStop );
    Tcl_AppendElement( interp, "-localStop" );
    Tcl_AppendElement( interp, dummyStr );
  }

  return TCL_OK;
}


// -----------------------------
// LsetVImage_GetMagGradRangeMtd
// -----------------------------

static int LsetVImage_GetMagGradRangeMtd( ClientData clientData,
					  Tcl_Interp *interp,
					  int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityImage *v = (cvLevelSetVelocityImage *)clientData;
  char *usage;
  double rng[2];

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( (v->GetMagGradRange( rng )) != CV_OK ) {
    Tcl_SetResult( interp, "error querying image gradient", TCL_STATIC );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%f %f", rng[0], rng[1] );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );

  return TCL_OK;
}


// -------------------------------
// LsetVImage_GetXYMagGradRangeMtd
// -------------------------------

static int LsetVImage_GetXYMagGradRangeMtd( ClientData clientData,
					    Tcl_Interp *interp,
					  int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityImage *v = (cvLevelSetVelocityImage *)clientData;
  char *usage;
  double rng[2];

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( (v->GetXYMagGradRange( rng )) != CV_OK ) {
    Tcl_SetResult( interp, "error querying image gradient", TCL_STATIC );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%f %f", rng[0], rng[1] );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );

  return TCL_OK;
}


// ------------------------------
// LsetVImage_GetZMagGradRangeMtd
// ------------------------------

static int LsetVImage_GetZMagGradRangeMtd( ClientData clientData,
					   Tcl_Interp *interp,
					   int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityImage *v = (cvLevelSetVelocityImage *)clientData;
  char *usage;
  double rng[2];

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( (v->GetZMagGradRange( rng )) != CV_OK ) {
    Tcl_SetResult( interp, "error querying image gradient", TCL_STATIC );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%f %f", rng[0], rng[1] );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );

  return TCL_OK;
}


// -------------------------------
// LsetVImage_GetIntensityRangeMtd
// -------------------------------

static int LsetVImage_GetIntensityRangeMtd( ClientData clientData,
					    Tcl_Interp *interp,
					    int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityImage *v = (cvLevelSetVelocityImage *)clientData;
  char *usage;
  double rng[2];

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( (v->GetIntensityRange( rng )) != CV_OK ) {
    Tcl_SetResult( interp, "error querying image data", TCL_STATIC );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%f %f", rng[0], rng[1] );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );

  return TCL_OK;
}


// --------------------
// LsetVConst_ObjectCmd
// --------------------

// Methods:
//   - SetConst, GetConst

int LsetVConst_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  if ( argc == 1 ) {
    PrintMethods_V();
    PrintMethods_VConst();
    return TCL_OK;
  }

  if ( Tcl_StringMatch( argv[1], "GetClassName" ) ) {
    Tcl_SetResult( interp, "lsetVConst", TCL_STATIC );
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "SetConst" ) ) {
    if ( LsetVConst_SetConstMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetConst" ) ) {
    if ( LsetVConst_GetConstMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else {

    // "Inherit" methods from LsetV...
    if ( LsetV_ObjectCmd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  }

  return TCL_OK;
}


// ----------------------
// LsetVConst_SetConstMtd
// ----------------------

static int LsetVConst_SetConstMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityConstant *v = (cvLevelSetVelocityConstant *)clientData;
  char *usage;
  double vel;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-v", DOUBLE_Type, &vel, NULL, REQUIRED, 0, { 0 } },
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
  if ( v->SetV( vel ) != CV_OK ) {
    Tcl_SetResult( interp, "error setting v", TCL_STATIC );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// ----------------------
// LsetVConst_GetConstMtd
// ----------------------

static int LsetVConst_GetConstMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityConstant *v = (cvLevelSetVelocityConstant *)clientData;
  char *usage;
  double vel;
  char tmpStr[CV_STRLEN];

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( (v->GetV( &vel )) != CV_OK ) {
    Tcl_SetResult( interp, "error getting v", TCL_STATIC );
    return TCL_ERROR;
  }

  Tcl_AppendElement( interp, "-v" );
  sprintf( tmpStr, "%f", vel );
  Tcl_AppendElement( interp, tmpStr );

  return TCL_OK;
}


// ------------------
// LsetVThr_ObjectCmd
// ------------------

int LsetVThr_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  if ( argc == 1 ) {
    PrintMethods_V();
    PrintMethods_VImage();
    PrintMethods_VThr();
    return TCL_OK;
  }

  if ( Tcl_StringMatch( argv[1], "GetClassName" ) ) {
    Tcl_SetResult( interp, "lsetVThr", TCL_STATIC );
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "SetConst" ) ) {
    if ( LsetVThr_SetConstMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetConst" ) ) {
    if ( LsetVThr_GetConstMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  } else {

    // "Inherit" methods from LsetVImage...
    if ( LsetVImage_ObjectCmd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  }

  return TCL_OK;
}


// --------------------
// LsetVThr_SetConstMtd
// --------------------

static int LsetVThr_SetConstMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityThreshold *v = (cvLevelSetVelocityThreshold *)clientData;
  char *usage;
  double balloon;
  double thr;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-balloon", DOUBLE_Type, &balloon, NULL, REQUIRED, 0, { 0 } },
    { "-threshold", DOUBLE_Type, &thr, NULL, REQUIRED, 0, { 0 } },
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
  if ( v->SetBalloonF( balloon ) != CV_OK ) {
    Tcl_SetResult( interp, "error setting balloon force", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( v->SetThreshold( thr ) != CV_OK ) {
    Tcl_SetResult( interp, "error setting threshold", TCL_STATIC );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// --------------------
// LsetVThr_GetConstMtd
// --------------------

static int LsetVThr_GetConstMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityThreshold *v = (cvLevelSetVelocityThreshold *)clientData;
  char *usage;
  double balloon, thr;
  char dummyStr[CV_STRLEN];

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );

  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( v->GetBalloonF( &balloon ) == CV_OK ) {
    sprintf( dummyStr, "%f", balloon );
    Tcl_AppendElement( interp, "-balloon" );
    Tcl_AppendElement( interp, dummyStr );
  }

  if ( v->GetThreshold( &thr ) == CV_OK ) {
    sprintf( dummyStr, "%f", thr );
    Tcl_AppendElement( interp, "-threshold" );
    Tcl_AppendElement( interp, dummyStr );
  }

  return TCL_OK;
}


// ------------------------
// LsetVPotential_ObjectCmd
// ------------------------

int LsetVPotential_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] )
{
  if ( argc == 1 ) {
    PrintMethods_V();
    PrintMethods_VImage();
    PrintMethods_VPotential();
    return TCL_OK;
  }

  if ( Tcl_StringMatch( argv[1], "GetClassName" ) ) {
    Tcl_SetResult( interp, "lsetVPotential", TCL_STATIC );
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "SetConst" ) ) {
    if ( LsetVPotential_SetConstMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetConst" ) ) {
    if ( LsetVPotential_GetConstMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }
  } else {

    // "Inherit" methods from LsetVImage...
    if ( LsetVImage_ObjectCmd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  }

  return TCL_OK;
}


// --------------------------
// LsetVPotential_SetConstMtd
// --------------------------

static int LsetVPotential_SetConstMtd( ClientData clientData,
				       Tcl_Interp *interp,
				       int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityPotential *v = (cvLevelSetVelocityPotential *)clientData;
  char *usage;
  double eP;
  double eK, Klow, Kupp;
  //  double balloon;

  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-eP", DOUBLE_Type, &eP, NULL, REQUIRED, 0, { 0 } },
    { "-eK", DOUBLE_Type, &eK, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-Klow", DOUBLE_Type, &Klow, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-Kupp", DOUBLE_Type, &Kupp, NULL, GDSC_OPTIONAL, 0, { 0 } },
    //    { "-balloon", DOUBLE_Type, &balloon, NULL, GDSC_OPTIONAL, 0, { 0 } },
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
  if ( v->SetEP( eP ) != CV_OK ) {
    Tcl_SetResult( interp, "error setting eP", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( arg_table[1].valid ) {
    if ( v->SetEK( eK ) != CV_OK ) {
      Tcl_SetResult( interp, "error setting eK", TCL_STATIC );
      return TCL_ERROR;
    }
  }
  if ( arg_table[2].valid ) {
    if ( v->SetKlow( Klow ) != CV_OK ) {
      Tcl_SetResult( interp, "error setting Klow", TCL_STATIC );
      return TCL_ERROR;
    }
  }
  if ( arg_table[3].valid ) {
    if ( v->SetKupp( Kupp ) != CV_OK ) {
      Tcl_SetResult( interp, "error setting Kupp", TCL_STATIC );
      return TCL_ERROR;
    }
  }
  /*
  if ( arg_table[4].valid ) {
    if ( v->SetBalloonF( balloon ) != CV_OK ) {
      Tcl_SetResult( interp, "error setting balloon force", TCL_STATIC );
      return TCL_ERROR;
    }
  }
  */

  return TCL_OK;
}


// --------------------------
// LsetVPotential_GetConstMtd
// --------------------------

static int LsetVPotential_GetConstMtd( ClientData clientData,
				       Tcl_Interp *interp,
				       int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityPotential *v = (cvLevelSetVelocityPotential *)clientData;
  char *usage;
  char dummyStr[CV_STRLEN];

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );

  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  sprintf( dummyStr, "%f", v->GetEP() );
  Tcl_AppendElement( interp, "-eP" );
  Tcl_AppendElement( interp, dummyStr );

  sprintf( dummyStr, "%f", v->GetEK() );
  Tcl_AppendElement( interp, "-eK" );
  Tcl_AppendElement( interp, dummyStr );

  sprintf( dummyStr, "%f", v->GetKlow() );
  Tcl_AppendElement( interp, "-Klow" );
  Tcl_AppendElement( interp, dummyStr );

  sprintf( dummyStr, "%f", v->GetKupp() );
  Tcl_AppendElement( interp, "-Kupp" );
  Tcl_AppendElement( interp, dummyStr );

  sprintf( dummyStr, "%f", v->GetBalloonF() );
  Tcl_AppendElement( interp, "-balloon" );
  Tcl_AppendElement( interp, dummyStr );

  return TCL_OK;
}


// -----------------------
// LsetVExpDecay_ObjectCmd
// -----------------------

int LsetVExpDecay_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] )
{
  if ( argc == 1 ) {
    PrintMethods_V();
    PrintMethods_VImage();
    PrintMethods_VExpDecay();
    return TCL_OK;
  }

  if ( Tcl_StringMatch( argv[1], "GetClassName" ) ) {
    Tcl_SetResult( interp, "lsetVExpDecay", TCL_STATIC );
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "SetConst" ) ) {
    if ( LsetVExpDecay_SetConstMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetConst" ) ) {
    if ( LsetVExpDecay_GetConstMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }
  } else {

    // "Inherit" methods from LsetVImage...
    if ( LsetVImage_ObjectCmd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  }

  return TCL_OK;
}


// -------------------------
// LsetVExpDecay_SetConstMtd
// -------------------------

static int LsetVExpDecay_SetConstMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityExponentialDecay *v = (cvLevelSetVelocityExponentialDecay *)clientData;
  char *usage;
  double eI, eIneg, Kt;
  int clamp, expand;
  char *kt_name;

  int table_size = 6;
  ARG_Entry arg_table[] = {
    { "-eI", DOUBLE_Type, &eI, NULL, REQUIRED, 0, { 0 } },
    { "-Kt", DOUBLE_Type, &Kt, NULL, REQUIRED, 0, { 0 } },
    { "-clamp", BOOL_Type, &clamp, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-expand", BOOL_Type, &expand, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-eIneg", DOUBLE_Type, &eIneg, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-3d_curv", STRING_Type, &kt_name, NULL, GDSC_OPTIONAL, 0, { 0 } },
    //    { "-monotonic", BOOL_Type, &monotonic, NULL, GDSC_OPTIONAL, 0, { 0 } },
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
  if ( v->SetEI( eI ) != CV_OK ) {
    Tcl_SetResult( interp, "error setting eI", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( v->SetKt( Kt ) != CV_OK ) {
    Tcl_SetResult( interp, "error setting Kt", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( arg_table[2].valid ) {
    v->SetClamp( clamp );
  }
  if ( arg_table[3].valid ) {
    v->SetExpand( expand );
  }
  if ( arg_table[4].valid ) {
    v->SetEIneg( eIneg );
  }
  if ( arg_table[5].valid ) {
    if ( v->Set3DKType( kt_name ) != CV_OK ) {
      Tcl_SetResult( interp, "error setting 3d curvature type", TCL_STATIC );
      return TCL_ERROR;
    }
  }
  /*
  if ( arg_table[6].valid ) {
    v->SetMonotonic( monotonic );
  }
  */

  return TCL_OK;
}


// -------------------------
// LsetVExpDecay_GetConstMtd
// -------------------------

static int LsetVExpDecay_GetConstMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityExponentialDecay *v = (cvLevelSetVelocityExponentialDecay *)clientData;
  char *usage;
  char dummyStr[CV_STRLEN];
  double eIneg;
  cvLevelSetVelocityExponentialDecay3DKT ktype;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );

  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  sprintf( dummyStr, "%f", v->GetEI() );
  Tcl_AppendElement( interp, "-eI" );
  Tcl_AppendElement( interp, dummyStr );

  sprintf( dummyStr, "%f", v->GetKt() );
  Tcl_AppendElement( interp, "-Kt" );
  Tcl_AppendElement( interp, dummyStr );

  Tcl_AppendElement( interp, "-clamp" );
  if ( v->GetClamp() ) {
    Tcl_AppendElement( interp, "true" );
  } else {
    Tcl_AppendElement( interp, "false" );
  }

  Tcl_AppendElement( interp, "-monotonic" );
  if ( v->GetMonotonic() ) {
    Tcl_AppendElement( interp, "true" );
  } else {
    Tcl_AppendElement( interp, "false" );
  }

  Tcl_AppendElement( interp, "-expand" );
  if ( v->GetExpand() ) {
    Tcl_AppendElement( interp, "true" );
  } else {
    Tcl_AppendElement( interp, "false" );
  }

  if ( v->GetEIneg( &eIneg ) == CV_OK ) {
    Tcl_AppendElement( interp, "-eIneg" );
    sprintf( dummyStr, "%f", eIneg );
    Tcl_AppendElement( interp, dummyStr );
  }

  if ( v->Get3DKType( &ktype ) == CV_OK ) {
    sprintf( dummyStr, "%s", DKT_EnumToStr( ktype ) );
    Tcl_AppendElement( interp, "-3d_curv" );
    Tcl_AppendElement( interp, dummyStr );
  }

  return TCL_OK;
}


// ----------------------
// LsetVImage_SetImageMtd
// ----------------------
// SetImage and GetImage can be dealt with abstractly at the
// intermediate cvLevelSetVelocityImage level in the cvLevelSetVelocity class hierarchy.  We are
// finally updating the Tcl bindings to use single functions to bind
// to cvLevelSetVelocityImage::{SetImage, GetImage}.

static int LsetVImage_SetImageMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityImage *v = (cvLevelSetVelocityImage *)clientData;
  char *usage;
  char *fb, *expFn;
  ARG_List numRangeList, pixDimsList, imgDimsList;
  int nrange, npix, ndims;
  int numRange[2];
  double pixDims[3];
  int imgDims[3];
  Tcl_DString buffer;

  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-base", STRING_Type, &fb, NULL, REQUIRED, 0, { 0 } },
    { "-numRange", LIST_Type, &numRangeList, NULL, REQUIRED, 0, { 0 } },
    { "-pix", LIST_Type, &pixDimsList, NULL, REQUIRED, 0, { 0 } },
    { "-img", LIST_Type, &imgDimsList, NULL, REQUIRED, 0, { 0 } },
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

  // Parse lists:
  if ( ARG_ParseTclListStatic( interp, numRangeList, INT_Type, numRange, 2,
			       &nrange ) != TCL_OK ) {
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, pixDimsList, DOUBLE_Type, pixDims, 3,
			       &npix ) != TCL_OK ) {
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, imgDimsList, INT_Type, imgDims, 3,
			       &ndims ) != TCL_OK ) {
    ARG_FreeListArgvs( table_size, arg_table );
    return TCL_ERROR;
  }
  ARG_FreeListArgvs( table_size, arg_table );

  // Do work of command:

  // Check lengths of input lists:
  if ( nrange != 2 ) {
    Tcl_SetResult( interp, "num range must be a list of 2 integer elements",
		   TCL_STATIC );
    return TCL_ERROR;
  }
  if ( npix != 3 ) {
    Tcl_SetResult( interp, "pix dims must be a list of 3 double elements",
		   TCL_STATIC );
    return TCL_ERROR;
  }
  if ( ndims != 3 ) {
    Tcl_SetResult( interp, "image dims must be a list of 3 double elements",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  // Do file name expansion:
  expFn = Tcl_TildeSubst( interp, fb, &buffer );
  if ( expFn == NULL ) {
    Tcl_AppendResult( interp, "error expanding filebase: ", fb, (char *)NULL );
    return TCL_ERROR;
  }

  if ( v->SetImage( expFn, numRange, imgDims, pixDims ) != CV_OK ) {
    Tcl_AppendResult( interp, "error reading image ", expFn, (char *)NULL );
    return TCL_ERROR;
  }

  Tcl_DStringFree( &buffer );

  return TCL_OK;
}


// ----------------------
// LsetVImage_GetImageMtd
// ----------------------

static int LsetVImage_GetImageMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocityImage *v = (cvLevelSetVelocityImage *)clientData;
  char *usage;
  Image_T *image;
  char tmpStr[CV_STRLEN];

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );

  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( !(v->GetImage( &image ) ) ) {
    Tcl_SetResult( interp, "error getting image", TCL_STATIC );
    return TCL_ERROR;
  }

  Tcl_AppendElement( interp, "-filebase" );
  Tcl_AppendElement( interp, image->filebase );

  sprintf( tmpStr, "%d %d", image->fileNumRange[0], image->fileNumRange[1] );
  Tcl_AppendElement( interp, "-numRange" );
  Tcl_AppendElement( interp, tmpStr );

  sprintf( tmpStr, "%f %f %f", image->pixelDims[0], image->pixelDims[1],
	   image->pixelDims[2] );
  Tcl_AppendElement( interp, "-pix" );
  Tcl_AppendElement( interp, tmpStr );

  sprintf( tmpStr, "%d %d %d", image->imgDims[0], image->imgDims[1],
	   image->imgDims[2] );
  Tcl_AppendElement( interp, "-img" );
  Tcl_AppendElement( interp, tmpStr );

  return TCL_OK;
}


// ---------------------
// LsetVSmooth_ObjectCmd
// ---------------------

int LsetVSmooth_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  if ( argc == 1 ) {
    PrintMethods_V();
    PrintMethods_VSmooth();
    return TCL_OK;
  }

  if ( Tcl_StringMatch( argv[1], "GetClassName" ) ) {
    Tcl_SetResult( interp, "lsetVSmooth", TCL_STATIC );
    return TCL_OK;
  } else if ( Tcl_StringMatch( argv[1], "SetConst" ) ) {
    if ( LsetVSmooth_SetConstMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }
  } else if ( Tcl_StringMatch( argv[1], "GetConst" ) ) {
    if ( LsetVSmooth_GetConstMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }
  } else {

    // "Inherit" methods from LsetV...
    if ( LsetV_ObjectCmd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }
  }

  return TCL_OK;
}


// -----------------------
// LsetVSmooth_SetConstMtd
// -----------------------

static int LsetVSmooth_SetConstMtd( ClientData clientData, Tcl_Interp *interp,
				    int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocitySmooth *v = (cvLevelSetVelocitySmooth *)clientData;
  char *usage;
  double lower_kt, upper_kt;
  char *kt_name;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-lower_Kt", DOUBLE_Type, &lower_kt, NULL, REQUIRED, 0, { 0 } },
    { "-upper_Kt", DOUBLE_Type, &upper_kt, NULL, REQUIRED, 0, { 0 } },
    { "-3d_curv", STRING_Type, &kt_name, NULL, GDSC_OPTIONAL, 0, { 0 } },
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
  if ( v->SetKts( upper_kt, lower_kt ) != CV_OK ) {
    Tcl_SetResult( interp, "error setting curvature bounds", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( arg_table[2].valid ) {
    if ( v->Set3DKType( kt_name ) != CV_OK ) {
      Tcl_SetResult( interp, "error setting 3d curvature type", TCL_STATIC );
      return TCL_ERROR;
    }
  }

  return TCL_OK;
}


// -----------------------
// LsetVSmooth_GetConstMtd
// -----------------------

static int LsetVSmooth_GetConstMtd( ClientData clientData, Tcl_Interp *interp,
				    int argc, CONST84 char *argv[] )
{
  cvLevelSetVelocitySmooth *v = (cvLevelSetVelocitySmooth *)clientData;
  char *usage;
  double lower_kt, upper_kt;
  cvLevelSetVelocitySmooth3DKT ktype;
  char dummyStr[CV_STRLEN];

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );

  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( v->GetKts( &upper_kt, &lower_kt ) == CV_OK ) {
    sprintf( dummyStr, "%f", lower_kt );
    Tcl_AppendElement( interp, "-lower_Kt" );
    Tcl_AppendElement( interp, dummyStr );
    sprintf( dummyStr, "%f", upper_kt );
    Tcl_AppendElement( interp, "-upper_Kt" );
    Tcl_AppendElement( interp, dummyStr );
  }
  if ( v->Get3DKType( &ktype ) == CV_OK ) {
    sprintf( dummyStr, "%s", KT_EnumToStr( ktype ) );
    Tcl_AppendElement( interp, "-3d_curv" );
    Tcl_AppendElement( interp, dummyStr );
  }

  return TCL_OK;
}
