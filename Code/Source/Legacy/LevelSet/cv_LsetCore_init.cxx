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

#include "cv_LsetCore_init.h"
#include "cvLevelSet.h"
#include "cvLevelSetVelocity.h"
#include "cvRepository.h"
#include "cvSolidModel.h"
#include "cv_misc_utils.h"
#include "cv_arg.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "cv_globals.h"

// Prototypes:
// -----------

// LsetCore
// --------

int LsetCore_NewCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] );

void DeleteLsetCore( ClientData clientData );

int LsetCore_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] );

int LsetCore_ListInstancesCmd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] );

int LsetCore_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

// LsetCore methods
// ----------------

static int LsetCore_SetTimeMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );

static int LsetCore_GetTimeMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );

static int LsetCore_GetTimeStepMtd( ClientData clientData, Tcl_Interp *interp,
				    int argc, CONST84 char *argv[] );

static int LsetCore_ResetTimeStepMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] );

static int LsetCore_SetGridMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );

static int LsetCore_GetGridMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );

static int LsetCore_SetCircleSeedMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] );

static int LsetCore_GetCircleSeedMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] );

static int LsetCore_SetPdSeedMtd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] );

static int LsetCore_GetPdSeedMtd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] );

static int LsetCore_SetSolidSeedMtd( ClientData clientData, Tcl_Interp *interp,
				     int argc, CONST84 char *argv[] );

static int LsetCore_GetSolidSeedMtd( ClientData clientData, Tcl_Interp *interp,
				     int argc, CONST84 char *argv[] );

static int LsetCore_SetImageSeedMtd( ClientData clientData, Tcl_Interp *interp,
				     int argc, CONST84 char *argv[] );

static int LsetCore_GetImageSeedMtd( ClientData clientData, Tcl_Interp *interp,
				     int argc, CONST84 char *argv[] );

static int LsetCore_SetVelocityMtd( ClientData clientData, Tcl_Interp *interp,
				    int argc, CONST84 char *argv[] );

static int LsetCore_GetVelocityMtd( ClientData clientData, Tcl_Interp *interp,
				    int argc, CONST84 char *argv[] );

static int LsetCore_SetTimersMtd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] );

static int LsetCore_GetTimersMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );

static int LsetCore_GetTimerGranularityMtd( ClientData clientData,
					    Tcl_Interp *interp,
					    int argc, CONST84 char *argv[] );

static int LsetCore_SetVExtensionMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] );

static int LsetCore_GetVExtensionMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] );

static int LsetCore_InitMtd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] );

static int LsetCore_IsInitMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] );

static int LsetCore_EvolveOneTimeStepMtd( ClientData clientData,
					  Tcl_Interp *interp,
					  int argc, CONST84 char *argv[] );

static int LsetCore_GridModifiedMtd( ClientData clientData, Tcl_Interp *interp,
				     int argc, CONST84 char *argv[] );

static int LsetCore_ExtractFrontMtd( ClientData clientData, Tcl_Interp *interp,
				     int argc, CONST84 char *argv[] );

static int LsetCore_RebuildPhiMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] );

static int LsetCore_ExtractPhiMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] );

static int LsetCore_GetBoundaryDataMtd( ClientData clientData,
					Tcl_Interp *interp,
					int argc, CONST84 char *argv[] );

static int LsetCore_ExtractCurvatureMtd( ClientData clientData,
					 Tcl_Interp *interp,
					 int argc, CONST84 char *argv[] );

static int LsetCore_ExtractVelMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] );

static int LsetCore_FindMaxVMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

static int LsetCore_VanishedMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

static int LsetCore_ExtractActiveNodesMtd( ClientData clientData,
					   Tcl_Interp *interp,
					   int argc, CONST84 char *argv[] );

static int LsetCore_ExtractForceMinVNodesMtd( ClientData clientData,
					      Tcl_Interp *interp,
					      int argc, CONST84 char *argv[] );

static int LsetCore_ExtractProjectionSetsMtd( ClientData clientData,
					      Tcl_Interp *interp,
					      int argc, CONST84 char *argv[] );

static int LsetCore_ExtractCoveredNodesMtd( ClientData clientData,
					    Tcl_Interp *interp,
					    int argc, CONST84 char *argv[] );

static int LsetCore_ExtractUncoveredNodesMtd( ClientData clientData,
					      Tcl_Interp *interp,
					      int argc, CONST84 char *argv[] );

static int LsetCore_ExtractMineNodesMtd( ClientData clientData,
					 Tcl_Interp *interp,
					 int argc, CONST84 char *argv[] );

static int LsetCore_SetSaveProjectionSetsMtd( ClientData clientData,
					      Tcl_Interp *interp,
					      int argc, CONST84 char *argv[] );

static int LsetCore_GetSaveProjectionSetsMtd( ClientData clientData,
					      Tcl_Interp *interp,
					      int argc, CONST84 char *argv[] );

static int LsetCore_SaveGridMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] );

static int LsetCore_GetGridStatsMtd( ClientData clientData, Tcl_Interp *interp,
				     int argc, CONST84 char *argv[] );

static int LsetCore_GetMemoryUsageMtd( ClientData clientData,
				       Tcl_Interp *interp,
				       int argc, CONST84 char *argv[] );

// Helper fns
// ----------

static int NewName( CONST84 char *name );
static int NewName( CONST84 char *name, Tcl_Interp *interp );
static void PrintMethods();
static void CleanUpNodeSets( cvPolyData *nodeSets[], int numSets );


// -------
// NewName
// -------

static int NewName( CONST84 char *name )
{
  Tcl_HashEntry *entryPtr;
  entryPtr = Tcl_FindHashEntry( &gLsetCoreTable, name );
  if ( entryPtr != NULL ) {
    return 0;
  }
  return 1;
}


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


// -------------
// LsetCore_Init
// -------------
// Initialize Tcl hash table for level set objects, and bind
// action-oriented commands to Tcl interp.

// Commands:
//   - lsetCore
//   - lsetCore_instances
//   - lsetCore_methods

int Lsetcore_Init( Tcl_Interp *interp )
{
  Tcl_InitHashTable( &gLsetCoreTable, TCL_STRING_KEYS );
  Tcl_CreateCommand( interp, "lsetCore", LsetCore_NewCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "lsetCore_instances", LsetCore_ListInstancesCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "lsetCore_methods", LsetCore_ListMethodsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  strcpy( projectionSetBase_, "" );
  return TCL_OK;
}


// ---------------
// LsetCore_NewCmd
// ---------------

// lsetCore <objName>

int LsetCore_NewCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] )
{
  CONST84 char *lsName;
  cvLevelSet *ls;
  Tcl_HashEntry *entryPtr;
  int newEntry = 0;

  // Check syntax:
  if (argc != 2) {
    Tcl_AppendResult( interp, "usage: ", argv[0], " <objName>",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure this is a new object name:
  lsName = argv[1];
  if ( !NewName( lsName ) ) {
    Tcl_AppendResult( interp, "lsetCore object \"", lsName,
		      "\" already exists", (char *)NULL );
    return TCL_ERROR;
  }

  // Allocate new cvLevelSet object:
  ls = new cvLevelSet;
  if ( ls == NULL ) {
    Tcl_AppendResult( interp, "error allocating object \"", lsName,
		      "\"", (char *)NULL );
    return TCL_ERROR;
  }

  strcpy( ls->tclName_, lsName );
  entryPtr = Tcl_CreateHashEntry( &gLsetCoreTable, lsName, &newEntry );
  if ( !newEntry ) {
    Tcl_SetResult( interp, "error updating cvLevelSet hash table",
		   TCL_STATIC );
    delete ls;
    return TCL_ERROR;
  }
  Tcl_SetHashValue( entryPtr, (ClientData)ls );
  char lsNameStr[2048];
  lsNameStr[0]='\0';
  sprintf(lsNameStr,"%s",lsName);
  Tcl_SetResult( interp, lsNameStr, TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), LsetCore_ObjectCmd,
		     (ClientData)ls, DeleteLsetCore );
  return TCL_OK;
}


// --------------
// DeleteLsetCore
// --------------
// Deletion callback invoked when the Tcl object is deleted.  Delete
// Tcl hash table entry as well as the cvLevelSet object itself.

void DeleteLsetCore( ClientData clientData )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  Tcl_HashEntry *entryPtr;

  entryPtr = Tcl_FindHashEntry( &gLsetCoreTable, ls->tclName_ );
  if ( entryPtr == NULL ) {
    printf("Error looking up LsetCore object %s for deletion.\n",
	   ls->tclName_);
  } else {
    Tcl_DeleteHashEntry( entryPtr );
  }
  delete ls;
}


// -----------------------
// LsetCore_ListMethodsCmd
// -----------------------
// An action-oriented cmd related to the LsetCore class which lists the
// methods provided by this class.  Does not return a Tcl result, but
// prints out each method and its usage syntax.

int LsetCore_ListMethodsCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  if ( argc != 1 ) {
    Tcl_AppendResult( interp, "usage: ", argv[0], (char *)NULL );
    return TCL_ERROR;
  }

  PrintMethods();
  return TCL_OK;
}


// -------------------------
// LsetCore_ListInstancesCmd
// -------------------------
// An action-oriented cmd related to the LsetCore class which lists all
// current instances of LsetCore.  Returns a Tcl list containing names
// of those instances.

int LsetCore_ListInstancesCmd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] )
{
  Tcl_HashEntry *entryPtr;
  Tcl_HashSearch search;

  if ( argc != 1 ) {
    Tcl_AppendResult( interp, "usage: ", argv[0], (char *)NULL );
    return TCL_ERROR;
  }

  for ( entryPtr = Tcl_FirstHashEntry( &gLsetCoreTable, &search );
	entryPtr != NULL;
	entryPtr = Tcl_NextHashEntry( &search ) ) {
    Tcl_AppendElement( interp, (char*)(Tcl_GetHashKey( &gLsetCoreTable, entryPtr )) );
  }
  return TCL_OK;
}


// ------------
// PrintMethods
// ------------

static void PrintMethods()
{
  printf("GetClassName\n");
  printf("SetTime\n");
  printf("GetTime\n");
  printf("GetTimeStep\n");
  printf("ResetTimeStep\n");
  printf("SetGrid\n");
  printf("GetGrid\n");
  printf("SetCircleSeed\n");
  printf("GetCircleSeed\n");
  printf("SetPdSeed\n");
  printf("GetPdSeed\n");
  printf("SetSolidSeed\n");
  printf("GetSolidSeed\n");
  printf("SetImageSeed\n");
  printf("GetImageSeed\n");
  printf("SetVelocity\n");
  printf("GetVelocity\n");
  printf("SetTimers\n");
  printf("GetTimers\n");
  printf("GetTimerGranularity\n");
  printf("SetVExtension\n");
  printf("GetVExtension\n");
  printf("Init\n");
  printf("IsInit\n");
  printf("EvolveOneTimeStep\n");
  printf("GridModified\n");
  printf("ExtractFront\n");
  printf("RebuildPhi\n");
  printf("ExtractPhi\n");
  printf("GetBoundaryData\n");
  printf("ExtractCurvature\n");
  printf("ExtractVel\n");
  printf("FindMaxV\n");
  printf("Vanished\n");
  printf("ExtractActiveNodes\n");
  printf("ExtractForceMinVNodes\n");
  printf("ExtractProjectionSets\n");
  printf("ExtractCoveredNodes\n");
  printf("ExtractUncoveredNodes\n");
  printf("ExtractMineNodes\n");
  printf("SetSaveProjectionSets\n");
  printf("GetSaveProjectionSets\n");
  printf("SaveGrid\n");
  printf("GetGridStats\n");
  printf("GetMemoryUsage\n");

  return;
}


// ------------------
// LsetCore_ObjectCmd
// ------------------

// Note: All methods must also be invoked alone (i.e. as the only
// method in any particular command).

int LsetCore_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  if ( argc == 1 ) {
    PrintMethods();
    return TCL_OK;
  }

  if ( Tcl_StringMatch( argv[1], "GetClassName" ) ) {
    Tcl_SetResult( interp, "LsetCore", TCL_STATIC );
    return TCL_OK;

  } else if ( Tcl_StringMatch( argv[1], "SetTime" ) ) {
    if ( LsetCore_SetTimeMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "GetTime" ) ) {
    if ( LsetCore_GetTimeMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "GetTimeStep" ) ) {
    if ( LsetCore_GetTimeStepMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "ResetTimeStep" ) ) {
    if ( LsetCore_ResetTimeStepMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "SetGrid" ) ) {
    if ( LsetCore_SetGridMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "GetGrid" ) ) {
    if ( LsetCore_GetGridMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "SetCircleSeed" ) ) {
    if ( LsetCore_SetCircleSeedMtd(clientData, interp, argc, argv)
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "GetCircleSeed" ) ) {
    if ( LsetCore_GetCircleSeedMtd(clientData, interp, argc, argv)
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "SetPdSeed" ) ) {
    if ( LsetCore_SetPdSeedMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "GetPdSeed" ) ) {
    if ( LsetCore_GetPdSeedMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "SetSolidSeed" ) ) {
    if ( LsetCore_SetSolidSeedMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "GetSolidSeed" ) ) {
    if ( LsetCore_GetSolidSeedMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "SetImageSeed" ) ) {
    if ( LsetCore_SetImageSeedMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "GetImageSeed" ) ) {
    if ( LsetCore_GetImageSeedMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "SetVelocity" ) ) {
    if ( LsetCore_SetVelocityMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "GetVelocity" ) ) {
    if ( LsetCore_GetVelocityMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "SetTimers" ) ) {
    if ( LsetCore_SetTimersMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "GetTimers" ) ) {
    if ( LsetCore_GetTimersMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "GetTimerGranularity" ) ) {
    if ( LsetCore_GetTimerGranularityMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "SetVExtension" ) ) {
    if ( LsetCore_SetVExtensionMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "GetVExtension" ) ) {
    if ( LsetCore_GetVExtensionMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "Init" ) ) {
    if ( LsetCore_InitMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "IsInit" ) ) {
    if ( LsetCore_IsInitMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "EvolveOneTimeStep" ) ) {
    if ( LsetCore_EvolveOneTimeStepMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "GridModified" ) ) {
    if ( LsetCore_GridModifiedMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "ExtractFront" ) ) {
    if ( LsetCore_ExtractFrontMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "RebuildPhi" ) ) {
    if ( LsetCore_RebuildPhiMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "ExtractPhi" ) ) {
    if ( LsetCore_ExtractPhiMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "GetBoundaryData" ) ) {
    if ( LsetCore_GetBoundaryDataMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "ExtractCurvature" ) ) {
    if ( LsetCore_ExtractCurvatureMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "ExtractVel" ) ) {
    if ( LsetCore_ExtractVelMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "FindMaxV" ) ) {
    if ( LsetCore_FindMaxVMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "Vanished" ) ) {
    if ( LsetCore_VanishedMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "ExtractActiveNodes" ) ) {
    if ( LsetCore_ExtractActiveNodesMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "ExtractForceMinVNodes" ) ) {
    if ( LsetCore_ExtractForceMinVNodesMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "ExtractProjectionSets" ) ) {
    if ( LsetCore_ExtractProjectionSetsMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "ExtractCoveredNodes" ) ) {
    if ( LsetCore_ExtractCoveredNodesMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "ExtractUncoveredNodes" ) ) {
    if ( LsetCore_ExtractUncoveredNodesMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "ExtractMineNodes" ) ) {
    if ( LsetCore_ExtractMineNodesMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "SetSaveProjectionSets" ) ) {
    if ( LsetCore_SetSaveProjectionSetsMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "GetSaveProjectionSets" ) ) {
    if ( LsetCore_GetSaveProjectionSetsMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "SaveGrid" ) ) {
    if ( LsetCore_SaveGridMtd( clientData, interp, argc, argv ) != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "GetGridStats" ) ) {
    if ( LsetCore_GetGridStatsMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else if ( Tcl_StringMatch( argv[1], "GetMemoryUsage" ) ) {
    if ( LsetCore_GetMemoryUsageMtd( clientData, interp, argc, argv )
	 != TCL_OK ) {
      return TCL_ERROR;
    }

  } else {
    Tcl_AppendResult( interp, "\"", argv[1],
		      "\" not a recognized LsetCore method", (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// -------------------
// LsetCore_SetTimeMtd
// -------------------

static int LsetCore_SetTimeMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  double simTime, cflFactor;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-simTime", DOUBLE_Type, &simTime, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-cflFactor", DOUBLE_Type, &cflFactor, NULL, GDSC_OPTIONAL, 0, { 0 } },
    //    { "-dt", DOUBLE_Type, &dt, NULL, GDSC_OPTIONAL, 0, { 0 } },
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
  if ( arg_table[0].valid ) {
    if ( !(ls->SetSimTime( simTime )) ) {
      Tcl_SetResult( interp, "error setting simTime", TCL_STATIC );
      return TCL_ERROR;
    }
  }

  if ( arg_table[1].valid ) {
    if ( !( ls->SetDtFactor( cflFactor ) ) ) {
      Tcl_SetResult( interp, "error setting CFL factor", TCL_STATIC );
      return TCL_ERROR;
    }
  }

  /*
  if ( arg_table[2].valid ) {
    if ( !( ls->SetDt( dt ) ) ) {
      Tcl_SetResult( interp, "error setting dt", TCL_STATIC );
      return TCL_ERROR;
    }
  }
  */

  return TCL_OK;
}


// -------------------
// LsetCore_GetTimeMtd
// -------------------

// $object GetTime

// Sidenote on returning Tcl arrays:
// -----
//   comp.lang.tcl   11/16/98
//   message ID      <72ort4$cob$1@m1.cs.man.ac.uk>
//   poster          Donal K. Fellows (fellowsd@cs.man.ac.uk)
// -----
// You can't return Tcl arrays as simple variables.  Three possible
// options include:
//   1. Always set array element values using the same dedicated array
//      variable.
//   2. Take in the name of an array variable to use for setting
//      element values.
//   3. Return a list of name/value pairs, which can then be used by
//      the caller to build an array using "array set <arrayName>
//      <name/value pair list>".

// I like option (3) for its simplicity.  But if we return a Tcl list
// from GetTime, then it would be most symmetric to take in a Tcl list
// in SetTime.  Which would be fine, but it complicates the command
// line a bit.

static int LsetCore_GetTimeMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  double simTime, t, cflFactor;
  char dummyStr[CV_STRLEN];

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );

  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Specified sim time:
  if ( !(ls->GetSimTime( &simTime ) ) ) {
    Tcl_SetResult( interp, "error getting simTime", TCL_STATIC );
    return TCL_ERROR;
  }
  sprintf( dummyStr, "%f", simTime );
  Tcl_AppendElement( interp, "-simTime" );
  Tcl_AppendElement( interp, dummyStr );

  // Current sim time:
  if ( ls->GetCurrTime( &t ) ) {
    sprintf( dummyStr, "%f", t );
    Tcl_AppendElement( interp, "-currTime" );
    Tcl_AppendElement( interp, dummyStr );
  }

  // CFL factor (optional):
  if ( ls->GetDtFactor( &cflFactor ) ) {
    sprintf( dummyStr, "%f", cflFactor );
    Tcl_AppendElement( interp, "-cflFactor" );
    Tcl_AppendElement( interp, dummyStr );
  }

  /*
  // dt (not sure what to do about this yet):
  if ( ls->GetDt( &dt ) ) {
    sprintf( dummyStr, "%f", dt );
    Tcl_AppendElement( interp, "-dt" );
    Tcl_AppendElement( interp, dummyStr );
  }
  */

  return TCL_OK;
}


// -----------------------
// LsetCore_GetTimeStepMtd
// -----------------------

static int LsetCore_GetTimeStepMtd( ClientData clientData, Tcl_Interp *interp,
				    int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  int ts;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  ts = ls->GetTimeStep();

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d", ts );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE ); 

  return TCL_OK;
}


// -------------------------
// LsetCore_ResetTimeStepMtd
// -------------------------

static int LsetCore_ResetTimeStepMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  ls->ResetTimeStep();
  return TCL_OK;
}


// -------------------
// LsetCore_SetGridMtd
// -------------------

static int LsetCore_SetGridMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  double hv[3] = { 1.0, 1.0, 1.0 };
  int gv[3] = { 1, 1, 1 };
  double oriv[3] = { 0.0, 0.0, 0.0 };
  double bandv[2];
  double mineWd = 0.0;  // cvLevelSetSparseGrid has a default mine width behavior
  ARG_List hlist, glist, olist, blist;
  int nh, ng, nori, nband;
  GridT gridType = Dense_GridT;
  char *gridTypeStr;
  char *usage;

  int table_size = 6;
  ARG_Entry arg_table[] = {
    { "-h", LIST_Type, &hlist, NULL, REQUIRED, 0, { 0 } },
    { "-dim", LIST_Type, &glist, NULL, REQUIRED, 0, { 0 } },
    { "-origin", LIST_Type, &olist, NULL, REQUIRED, 0, { 0 } },
    { "-type", STRING_Type, &gridTypeStr, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-bandExt", LIST_Type, &blist, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-mineWd", DOUBLE_Type, &mineWd, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    if ( arg_table[0].valid ) {
      Tcl_Free( (char *) hlist.argv );
    }
    if ( arg_table[1].valid ) {
      Tcl_Free( (char *) glist.argv );
    }
    if ( arg_table[2].valid ) {
      Tcl_Free( (char *) olist.argv );
    }
    if ( arg_table[4].valid ) {
      Tcl_Free( (char *) blist.argv );
    }
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, hlist, DOUBLE_Type, hv, 3, &nh )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    Tcl_Free( (char *) hlist.argv );
    Tcl_Free( (char *) glist.argv );
    Tcl_Free( (char *) olist.argv );
    if ( arg_table[4].valid ) {
      Tcl_Free( (char *) blist.argv );
    }
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, glist, INT_Type, gv, 3, &ng )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    Tcl_Free( (char *) hlist.argv );
    Tcl_Free( (char *) glist.argv );
    Tcl_Free( (char *) olist.argv );
    if ( arg_table[4].valid ) {
      Tcl_Free( (char *) blist.argv );
    }
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, olist, DOUBLE_Type, oriv, 3, &nori )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    Tcl_Free( (char *) hlist.argv );
    Tcl_Free( (char *) glist.argv );
    Tcl_Free( (char *) olist.argv );
    if ( arg_table[4].valid ) {
      Tcl_Free( (char *) blist.argv );
    }
    return TCL_ERROR;
  }
  if ( arg_table[4].valid ) {
    if ( ARG_ParseTclListStatic( interp, blist, DOUBLE_Type, bandv,
				 2, &nband ) != TCL_OK ) {
      Tcl_SetResult( interp, usage, TCL_VOLATILE );
      Tcl_Free( (char *) hlist.argv );
      Tcl_Free( (char *) glist.argv );
      Tcl_Free( (char *) olist.argv );
      Tcl_Free( (char *) blist.argv );
      return TCL_ERROR;
    }
    if ( nband != 2 ) {
      Tcl_SetResult( interp, "band extent is defined by 2 double's",
		     TCL_STATIC );
      Tcl_Free( (char *) hlist.argv );
      Tcl_Free( (char *) glist.argv );
      Tcl_Free( (char *) olist.argv );
      Tcl_Free( (char *) blist.argv );
      return TCL_ERROR;
    }
  }

  // Now we're done parsing all the list arguments, so we can free the
  // string space used by those lists:
  Tcl_Free( (char *) hlist.argv );
  Tcl_Free( (char *) glist.argv );
  Tcl_Free( (char *) olist.argv );
  if ( arg_table[4].valid ) {
    Tcl_Free( (char *) blist.argv );
  }

  // Parse the grid type parameter:
  if ( arg_table[3].valid ) {
    gridType = GridT_StrToEnum( gridTypeStr );
  }
  if ( gridType != Invalid_GridT ) {
    if ( !( ls->SetGridType( gridType ) ) ) {
      Tcl_AppendResult( interp, "error setting grid type ", gridTypeStr,
			(char *)NULL );
      return TCL_ERROR;
    }
  } else {
    Tcl_SetResult( interp, GridT_EnumToStr( gridType ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Now ensure consistency of parameter set:
  if ( gridType != Sparse_GridT ) {
    if ( arg_table[4].valid ) {
      Tcl_AppendResult( interp, arg_table[4].name, " is only valid for "
			"sparse grids", (char *)NULL );
      return TCL_ERROR;
    }
    if ( arg_table[5].valid ) {
      Tcl_AppendResult( interp, arg_table[5].name, " is only valid for "
			"sparse grids", (char *)NULL );
      return TCL_ERROR;
    }
  } else {
    if ( ! arg_table[4].valid ) {
      Tcl_AppendResult( interp, arg_table[4].name, " required for "
			"sparse grids", (char *)NULL );
      return TCL_ERROR;
    }
  }

  // Do work of command:
  if ( !(ls->SetGridSpacing( hv )) ) {
    Tcl_SetResult( interp, "error setting physical pixel dims", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !(ls->SetGridSize( gv )) ) {
    Tcl_SetResult( interp, "error setting logical grid dims", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !(ls->SetGridOrigin( oriv )) ) {
    Tcl_SetResult( interp, "error setting grid origin", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( gridType == Sparse_GridT ) {
    if ( !(ls->SetGridBandParams( bandv, mineWd ) ) ) {
      Tcl_SetResult( interp, "error setting sparse grid band parameters",
		     TCL_STATIC );
      return TCL_ERROR;
    }
  }

  return TCL_OK;
}


// -------------------
// LsetCore_GetGridMtd
// -------------------

// $object GetGrid

static int LsetCore_GetGridMtd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  double hv[3];
  int gv[3];
  double oriv[3];
  double bandv[2];
  double mineWd;
  char tmpStr[CV_STRLEN];

  // If any add'l arg's given, return usage string:
  if ( argc != 2 ) {
    Tcl_AppendResult( interp, "usage: $LsetCoreObject ", argv[1],
		      (char *)NULL );
    return TCL_ERROR;
  }

  if ( !(ls->GetGridSpacing( hv ) ) ) {
    Tcl_SetResult( interp, "error getting grid spacings", TCL_STATIC );
    return TCL_ERROR;
  }

  sprintf( tmpStr, "%f", hv[0] );
  Tcl_AppendElement( interp, "-hx" );
  Tcl_AppendElement( interp, tmpStr );

  sprintf( tmpStr, "%f", hv[1] );
  Tcl_AppendElement( interp, "-hy" );
  Tcl_AppendElement( interp, tmpStr );

  sprintf( tmpStr, "%f", hv[2] );
  Tcl_AppendElement( interp, "-hz" );
  Tcl_AppendElement( interp, tmpStr );

  if ( !(ls->GetGridSize( gv ) ) ) {
    Tcl_SetResult( interp, "error getting logical grid dims", TCL_STATIC );
    return TCL_ERROR;
  }

  sprintf( tmpStr, "%d", gv[0] );
  Tcl_AppendElement( interp, "-gridx" );
  Tcl_AppendElement( interp, tmpStr );

  sprintf( tmpStr, "%d", gv[1] );
  Tcl_AppendElement( interp, "-gridy" );
  Tcl_AppendElement( interp, tmpStr );

  sprintf( tmpStr, "%d", gv[2] );
  Tcl_AppendElement( interp, "-gridz" );
  Tcl_AppendElement( interp, tmpStr );

  if ( !(ls->GetGridOrigin( oriv ) ) ) {
    Tcl_SetResult( interp, "error getting grid origin", TCL_STATIC );
    return TCL_ERROR;
  }

  sprintf( tmpStr, "%f", oriv[0] );
  Tcl_AppendElement( interp, "-orix" );
  Tcl_AppendElement( interp, tmpStr );

  sprintf( tmpStr, "%f", oriv[1] );
  Tcl_AppendElement( interp, "-oriy" );
  Tcl_AppendElement( interp, tmpStr );

  sprintf( tmpStr, "%f", oriv[2] );
  Tcl_AppendElement( interp, "-oriz" );
  Tcl_AppendElement( interp, tmpStr );

  Tcl_AppendElement( interp, "-type" );
  Tcl_AppendElement( interp, GridT_EnumToStr( ls->GetGridType() ) );

  if ( ls->GetGridBandParams( bandv, &mineWd ) ) {
    sprintf( tmpStr, "%f %f", bandv[0], bandv[1] );
    Tcl_AppendElement( interp, "-bandExt" );
    Tcl_AppendElement( interp, tmpStr );

    sprintf( tmpStr, "%f", mineWd );
    Tcl_AppendElement( interp, "-mineWd" );
    Tcl_AppendElement( interp, tmpStr );
  }

  return TCL_OK;
}


// -------------------------
// LsetCore_SetCircleSeedMtd
// -------------------------

// $object SetCircleSeed -r <val> -x <val> -y <val> -z <val>

// Value set for seed z coordinate needs to be matched with z
// coordinate set for Grid in Grid::Init.

static int LsetCore_SetCircleSeedMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  double r, x, y, z;
  Seed_T seed;

  int table_size = 4;
  ARG_Entry arg_table[] = {
    { "-r", DOUBLE_Type, &r, NULL, REQUIRED, 0, { 0 } },
    { "-x", DOUBLE_Type, &x, NULL, REQUIRED, 0, { 0 } },
    { "-y", DOUBLE_Type, &y, NULL, REQUIRED, 0, { 0 } },
    { "-z", DOUBLE_Type, &z, NULL, REQUIRED, 0, { 0 } },
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

  seed.r = r;
  seed.pos[0] = x;
  seed.pos[1] = y;
  seed.pos[2] = z;

  if ( !(ls->SetSeed( &seed )) ) {
    Tcl_SetResult( interp, "error setting circular seed", TCL_STATIC );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// -------------------------
// LsetCore_GetCircleSeedMtd
// -------------------------

static int LsetCore_GetCircleSeedMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  Seed_T seed;
  char tmpStr[CV_STRLEN];

  // If any add'l arg's given, return usage string:
  if ( argc != 2 ) {
    Tcl_AppendResult( interp, "usage: $LsetCoreObject ", argv[1], (char *)NULL );
    return TCL_ERROR;
  }

  if ( !(ls->GetSeed( &seed ) ) ) {
    Tcl_SetResult( interp, "error getting circular seed", TCL_STATIC );
    return TCL_ERROR;
  }

  sprintf( tmpStr, "%f", seed.r );
  Tcl_AppendElement( interp, "-r" );
  Tcl_AppendElement( interp, tmpStr );

  sprintf( tmpStr, "%f", seed.pos[0] );
  Tcl_AppendElement( interp, "-x" );
  Tcl_AppendElement( interp, tmpStr );

  sprintf( tmpStr, "%f", seed.pos[1] );
  Tcl_AppendElement( interp, "-y" );
  Tcl_AppendElement( interp, tmpStr );

  sprintf( tmpStr, "%f", seed.pos[2] );
  Tcl_AppendElement( interp, "-z" );
  Tcl_AppendElement( interp, tmpStr );

  return TCL_OK;
}


// ---------------------
// LsetCore_SetPdSeedMtd
// ---------------------

// % $object SetPdSeed -polydata <objName>

static int LsetCore_SetPdSeedMtd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  char *objName;
  cvRepositoryData *pd;
  RepositoryDataT type;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-polydata", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } }
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

  pd = gRepository->GetObject( objName );
  if ( pd == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName,
                      (char *)NULL );
    return TCL_ERROR;
  }

  type = pd->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, objName, " not a poly data object",
                      (char *)NULL );
    return TCL_ERROR;
  }

  if ( !(ls->SetSeed( (cvPolyData*)pd ) ) ) {
    Tcl_SetResult( interp, "error setting cvPolyData seed", TCL_STATIC );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// ---------------------
// LsetCore_GetPdSeedMtd
// ---------------------

// % $object GetPdSeed

static int LsetCore_GetPdSeedMtd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  cvPolyData *seed;

  // Do work of command:

  if ( !(ls->GetSeed( &seed ) ) ) {
    Tcl_SetResult( interp, "error getting cvPolyData seed", TCL_STATIC );
    return TCL_ERROR;
  }

  printf( "Use the result with caution, object name may be obsolete.\n" );

  char rtnstr[1024];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%s", seed->GetName() );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );

  return TCL_OK;
}


// ------------------------
// LsetCore_SetSolidSeedMtd
// ------------------------

static int LsetCore_SetSolidSeedMtd( ClientData clientData, Tcl_Interp *interp,
				     int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  char *objName;
  cvRepositoryData *sm;
  RepositoryDataT type;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-solid", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
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

  sm = gRepository->GetObject( objName );
  if ( sm == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName,
                      (char *)NULL );
    return TCL_ERROR;
  }

  type = sm->GetType();
  if ( (type != SOLID_MODEL_T) ) {
    Tcl_AppendResult( interp, objName, " not a solid model object",
                      (char *)NULL );
    return TCL_ERROR;
  }

  if ( !(ls->SetSeed( (cvSolidModel*)sm ) ) ) {
    Tcl_SetResult( interp, "error setting solid model seed", TCL_STATIC );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// ------------------------
// LsetCore_GetSolidSeedMtd
// ------------------------

static int LsetCore_GetSolidSeedMtd( ClientData clientData, Tcl_Interp *interp,
				     int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  cvSolidModel *seed;

  // Do work of command:

  if ( !(ls->GetSeed( &seed ) ) ) {
    Tcl_SetResult( interp, "error getting cvSolidModel seed", TCL_STATIC );
    return TCL_ERROR;
  }

  printf( "Use the result with caution, object name may be obsolete.\n" );

  char rtnstr[1024];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%s", seed->GetName() );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE ); 

  return TCL_OK;
}


// ------------------------
// LsetCore_SetImageSeedMtd
// ------------------------

static int LsetCore_SetImageSeedMtd( ClientData clientData, Tcl_Interp *interp,
				     int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  char *objName;
  double thr;
  cvRepositoryData *img;
  RepositoryDataT type;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-img", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-thr", DOUBLE_Type, &thr, NULL, REQUIRED, 0, { 0 } }
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

  img = gRepository->GetObject( objName );
  if ( img == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName,
                      (char *)NULL );
    return TCL_ERROR;
  }

  type = img->GetType();
  if ( (type != STRUCTURED_PTS_T) ) {
    Tcl_AppendResult( interp, objName, " not an image object",
                      (char *)NULL );
    return TCL_ERROR;
  }

  if ( !(ls->SetSeed( (cvStrPts*)img, thr ) ) ) {
    Tcl_SetResult( interp, "error setting image seed", TCL_STATIC );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// ------------------------
// LsetCore_GetImageSeedMtd
// ------------------------
// This is misleading, since class cvLevelSet makes its own private
// copies of repository objects when SetSeed is called.  As a result,
// the object we retrieve with GetSeed is not a repository object, and
// as such its name means little in the context of the repository!
// Still, I've put in name copies in cvLevelSet's SetSeed methods so
// that the retrieved names will match the names of objects used at
// the time of the call to SetSeed, even though those objects may not
// exist by the time this function is called.

static int LsetCore_GetImageSeedMtd( ClientData clientData, Tcl_Interp *interp,
				     int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  cvStrPts *seed;
  double thr;
  char buffer[100];

  // Do work of command:

  if ( !(ls->GetSeed( &seed, &thr ) ) ) {
    Tcl_SetResult( interp, "error getting image seed", TCL_STATIC );
    return TCL_ERROR;
  }

  printf( "Use the result with caution, object name may be obsolete.\n" );
  Tcl_AppendElement( interp, "-img" );
  Tcl_AppendElement( interp, seed->GetName() );
  Tcl_AppendElement( interp, "-thr" );
  sprintf( buffer, "%f", thr );
  Tcl_AppendElement( interp, buffer );
  return TCL_OK;
}


// -----------------------
// LsetCore_SetVelocityMtd
// -----------------------

// $object SetVelocity -vobj <name>

static int LsetCore_SetVelocityMtd( ClientData clientData, Tcl_Interp *interp,
				    int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *vName;
  char *usage;
  Tcl_HashEntry *entryPtr;
  cvLevelSetVelocity *v;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-vobj", STRING_Type, &vName, NULL, REQUIRED, 0, { 0 } },
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

  // Look up the named velocity object in the velocity table:
  entryPtr = Tcl_FindHashEntry( &gLsetVTable, vName );
  if ( entryPtr == NULL ) {
    Tcl_AppendResult( interp, "velocity object ", vName, " not found",
		      (char *)NULL );
    return TCL_ERROR;
  }
  v = (cvLevelSetVelocity *)Tcl_GetHashValue( entryPtr );

  if ( ls->LinkVelocity( v ) != CV_OK ) {
    Tcl_SetResult( interp, "error setting velocity", TCL_STATIC );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// -----------------------
// LsetCore_GetVelocityMtd
// -----------------------

// $object GetVelocity

static int LsetCore_GetVelocityMtd( ClientData clientData, Tcl_Interp *interp,
				    int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  cvLevelSetVelocity *v;
  char *usage;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );

  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( ls->GetVelocity( &v ) != CV_OK ) {
    Tcl_SetResult( interp, "error getting velocity", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( v == NULL ) {
    Tcl_SetResult( interp, "error getting velocity", TCL_STATIC );
    return TCL_ERROR;
  }

  char rtnstr[1024];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%s", v->tclName_ );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );

  return TCL_OK;
}


// ---------------------
// LsetCore_SetTimersMtd
// ---------------------

// $object SetTimers -flag <int>

static int LsetCore_SetTimersMtd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  int i;
  int value;
  int flagFlag;
  char usage[CV_STRLEN];

  sprintf( usage, "usage: $LsetCoreObject %s -flag <int>", argv[1] );

  // If no add'l arg's given, return usage string:
  if ( argc == 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  // If we don't have an even number of arg's, then usage is incorrect:
  if ( argc != 4 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  flagFlag = 0;

  // Now, for each arg/val pair:
  for ( i = 2; i < argc; i += 2 ) {

    // Set dt:
    if ( Tcl_StringMatch( argv[i], "-flag" ) ) {
      if ( Tcl_GetInt( interp, argv[i+1], &value ) != TCL_OK ) {
	Tcl_AppendResult( interp, "invalid value for flag: ",
			  argv[i+1], (char *)NULL );
	return TCL_ERROR;
      }
      flagFlag = 1;
    }

    // Syntax errors:
    else if ( argv[i][0] == '-' ) {
      Tcl_AppendResult( interp, "\"", argv[i],
			"\" not a recognized flag", (char *)NULL );
      return TCL_ERROR;
    } else {
      Tcl_AppendResult( interp, "expecting a flag, but found \"", argv[i],
			"\" instead", (char *)NULL );
      return TCL_ERROR;
    }
  }

  if ( !flagFlag ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  ls->SetTimers( value );

  return TCL_OK;
}


// ---------------------
// LsetCore_GetTimersMtd
// ---------------------

static int LsetCore_GetTimersMtd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  int value;

  // If any add'l arg's given, return usage string:
  if ( argc != 2 ) {
    Tcl_AppendResult( interp, "usage: $LsetCoreObject ",
		      argv[1], (char *)NULL );
    return TCL_ERROR;
  }

  ls->GetTimers( &value );

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d", value );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );

  return TCL_OK;
}


// -------------------------------
// LsetCore_GetTimerGranularityMtd
// -------------------------------

static int LsetCore_GetTimerGranularityMtd( ClientData clientData,
					    Tcl_Interp *interp,
					    int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  double g;
  char *usage;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  g = ls->GetTimerGranularity();

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%f", g );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );

  return TCL_OK;
}


// ----------------------
// LsetCore_SetVExtension
// ----------------------

static int LsetCore_SetVExtensionMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  char *fnName;
  ExtensionT etype;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-fn", STRING_Type, &fnName, NULL, REQUIRED, 0, { 0 } },
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
  etype = ExtensionT_StrToEnum( fnName );
  if ( ls->SetVExtension( etype ) != CV_OK ) {
    Tcl_SetResult( interp, "error setting extension", TCL_STATIC );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// -------------------------
// LsetCore_GetVExtensionMtd
// -------------------------

static int LsetCore_GetVExtensionMtd( ClientData clientData,
				      Tcl_Interp *interp,
				      int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  ExtensionT etype;
  char *dummy;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );

  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( !(ls->GetVExtension( &etype ) ) ) {
    Tcl_SetResult( interp, "error getting velociy extension", TCL_STATIC );
    return TCL_ERROR;
  }

  dummy = ExtensionT_EnumToStr( etype );
  Tcl_AppendElement( interp, "-fn" );
  Tcl_AppendElement( interp, dummy );

  return TCL_OK;
}

// ----------------
// LsetCore_InitMtd
// ----------------

// $object Init

// Allocates data structures.  Off-loading this responsiblity from
// EvolveOneTimeStep.

static int LsetCore_InitMtd( ClientData clientData, Tcl_Interp *interp,
			     int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char usage[CV_STRLEN];

  sprintf( usage, "usage: $LsetCoreObject %s", argv[1] );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  //  if ( !(ls->Status()) ) {
    if ( !(ls->Init()) ) {
      Tcl_SetResult( interp, "error initializing level set", TCL_STATIC );
      return TCL_ERROR;
    }
    //  }

  return TCL_OK;
}


// ------------------
// LsetCore_IsInitMtd
// ------------------

static int LsetCore_IsInitMtd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d", ls->Status() );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );

  return TCL_OK;
}


// --------------------------
// LsetCore_EvolveOneTimeStep
// --------------------------

// Result should be true until stopping criterion is reached.  This
// will facilitate a script-level construct such as:
//   % while [$object EvolveOneTimeStep] {
//       ...
//     }

// Adding the responsibility for management of potentially-created
// sets of projection nodes to this function.

static int LsetCore_EvolveOneTimeStepMtd( ClientData clientData,
					  Tcl_Interp *interp,
					  int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char usage[CV_STRLEN];
  cvPolyData **projectionSets = NULL;
  int numSets = 0;

  sprintf( usage, "usage: $LsetCoreObject %s", argv[1] );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( !(ls->Status()) ) {
    Tcl_SetResult( interp, "incorrect status... did you call Init?",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  if ( ls->EvolveOneTimeStep() ) {
    Tcl_SetResult( interp, "1", TCL_STATIC );
  } else {
    Tcl_SetResult( interp, "0", TCL_STATIC );
  }

  return TCL_OK;
}


// ------------------------
// LsetCore_GridModifiedMtd
// ------------------------

static int LsetCore_GridModifiedMtd( ClientData clientData, Tcl_Interp *interp,
				     int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;

  if ( argc != 2 ) {
    usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( ls->GetGrid()->GridModified() ) {
    Tcl_SetResult( interp, "1", TCL_STATIC );
  } else {
    Tcl_SetResult( interp, "0", TCL_STATIC );
  }

  return TCL_OK;
}


// ------------------------
// LsetCore_ExtractFrontMtd
// ------------------------
// Updated 1/26/00.

static int LsetCore_ExtractFrontMtd( ClientData clientData, Tcl_Interp *interp,
				     int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  char *objName;
  int closed = 0;     // default
  cvPolyData *front;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-out", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-closed", INT_Type, &closed, NULL, GDSC_OPTIONAL, 0, { 0 } }
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

  front = ls->ExtractFront( closed );
  if ( front == NULL ) {
    Tcl_SetResult( interp, "error on front extraction", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !(gRepository->Register( objName, front )) ) {
    Tcl_SetResult( interp, "error registering obj in repository",
		   TCL_STATIC );
    delete front;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, front->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// ----------------------
// LsetCore_RebuildPhiMtd
// ----------------------

// $object RebuildPhi

static int LsetCore_RebuildPhiMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char usage[CV_STRLEN];

  sprintf( usage, "usage: $LsetCoreObject %s", argv[1] );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( ls->RebuildPhi() ) {
    Tcl_SetResult( interp, "1", TCL_STATIC );
    return TCL_OK;
  } else {
    Tcl_SetResult( interp, "0", TCL_STATIC );
    return TCL_ERROR;
  }
}


// ----------------------
// LsetCore_ExtractPhiMtd
// ----------------------

static int LsetCore_ExtractPhiMtd( ClientData clientData, Tcl_Interp *interp,
				   int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  char *objName;
  cvDataObject *phi;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-out", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
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

  // Do work of command:
  phi = ls->ExtractPhi();
  if ( phi == NULL ) {
    Tcl_SetResult( interp, "error on phi extraction", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !(gRepository->Register( objName, phi )) ) {
    Tcl_SetResult( interp, "error registering obj in repository",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, phi->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// ---------------------------
// LsetCore_GetBoundaryDataMtd
// ---------------------------

static int LsetCore_GetBoundaryDataMtd( ClientData clientData,
					Tcl_Interp *interp,
					int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  char *fieldName, *fieldStr;
  GridScalarT fieldType;
  char *resultName;
  cvPolyData *dataPd;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-field", STRING_Type, &fieldName, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &resultName, NULL, REQUIRED, 0, { 0 } },
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

  // Do work of command:

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    Tcl_AppendResult( interp, "object ", resultName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Translate the given field name:
  fieldType = GridScalarT_StrToEnum( fieldName );
  if ( fieldType == GS_INVALID ) {
    fieldStr = GridScalarT_EnumToStr( GS_INVALID );
    Tcl_SetResult( interp, fieldStr, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Now, go get the data:
  dataPd = ls->GetGrid()->GetBoundaryData( fieldType );
  if ( dataPd == NULL ) {
    Tcl_AppendResult( interp, "error on extraction of boundary data ",
		      fieldName, (char *)NULL );
    return TCL_ERROR;
  }

  // And register the result in the repository:
  if ( !(gRepository->Register( resultName, dataPd )) ) {
    Tcl_SetResult( interp, "error registering obj in repository",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dataPd->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// ----------------------------
// LsetCore_ExtractCurvatureMtd
// ----------------------------

// $object ExtractCurvature -out <str>

static int LsetCore_ExtractCurvatureMtd( ClientData clientData,
					 Tcl_Interp *interp,
					 int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  char *objName;
  cvStrPts *k;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-out", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
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
  k = ls->ExtractCurvature();
  if ( k == NULL ) {
    Tcl_SetResult( interp, "error on curvature extraction", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !(gRepository->Register( objName, k )) ) {
    Tcl_SetResult( interp, "error registering obj in repository",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, k->GetName(), TCL_VOLATILE );

  return TCL_OK;
}

// ----------------------
// LsetCore_ExtractVelMtd
// ----------------------

static int LsetCore_ExtractVelMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  char *objName;
  cvPolyData *vectors;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-out", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } }
  };

  usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );

  if ( argc == 2 ) {
    Tcl_AppendResult( interp, usage, (char*)NULL );
    return TCL_OK;
  }

  if ( ARG_ParseTclStr( interp, argc, argv, 2,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_AppendResult( interp, usage, (char*)NULL );
    return TCL_ERROR;
  }

  vectors = ls->ExtractVectors();
  if ( vectors == NULL ) {
    Tcl_SetResult( interp, "error on velocity extraction", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !(gRepository->Register( objName, vectors )) ) {
    Tcl_SetResult( interp, "error registering obj in repository",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  // KCW [5/6/99]: Moved this inside cvRepository::Register.
  //  vectors->SetName( objName );

  Tcl_SetResult( interp, vectors->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// --------------------
// LsetCore_FindMaxVMtd
// --------------------

static int LsetCore_FindMaxVMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  double maxV;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_AppendResult( interp, usage, (char*)NULL );
    return TCL_ERROR;
  }

  maxV = ls->GetMaxV();

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%f", maxV );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE ); 

  return TCL_OK;
}


// --------------------
// LsetCore_VanishedMtd
// --------------------

static int LsetCore_VanishedMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_AppendResult( interp, usage, (char*)NULL );
    return TCL_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d", ls->Vanished() );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );

  return TCL_OK;
}


// ---------------
// CleanUpNodeSets
// ---------------

void CleanUpNodeSets( cvPolyData *nodeSets[], int numSets )
{
  int i;

  for ( i = 0; i < numSets; i++ ) {
    delete nodeSets[i];
  }
  delete [] nodeSets;
  return;
}


// ------------------------------
// LsetCore_ExtractActiveNodesMtd
// ------------------------------

static int LsetCore_ExtractActiveNodesMtd( ClientData clientData,
					   Tcl_Interp *interp,
					   int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  char *objName;
  cvPolyData *nodes;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-out", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
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
  //  nodes = ls->ExtractActiveNodes();
  nodes = ls->GetGrid()->GetActiveNodes();
  if ( nodes == NULL ) {
    Tcl_SetResult( interp, "error on active node extraction", TCL_STATIC );
    return TCL_ERROR;
  }

  if ( !(gRepository->Register( objName, nodes )) ) {
    Tcl_SetResult( interp, "error registering obj in repository",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, nodes->GetName(), TCL_VOLATILE );

  return TCL_OK;
}


// ---------------------------------
// LsetCore_ExtractForceMinVNodesMtd
// ---------------------------------

static int LsetCore_ExtractForceMinVNodesMtd( ClientData clientData,
					      Tcl_Interp *interp,
					      int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  char *objName;
  cvPolyData **nodeSets;
  int numSets;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-out", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
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
  if ( ls->GetGrid()->SaveForceMinVNodes() != CV_OK ) {
    Tcl_SetResult( interp, "error querying nodes", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( ls->GetGrid()->GetNodeSets( &nodeSets, &numSets ) != CV_OK ) {
    Tcl_SetResult( interp, "error on node extraction", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( numSets == 0 ) {
    Tcl_SetResult( interp, "zero nodes found; no output created", TCL_STATIC );
    return TCL_OK;
  }
  if ( numSets != 1 ) {
    Tcl_SetResult( interp, "error on node extraction", TCL_STATIC );
    CleanUpNodeSets( nodeSets, numSets );
    return TCL_ERROR;
  }

  if ( !(gRepository->Register( objName, nodeSets[0] ) ) ) {
      Tcl_AppendResult( interp, "error registering obj ", objName,
			" in repository", (char *)NULL );
      CleanUpNodeSets( nodeSets, numSets );
      return TCL_ERROR;
  }

  Tcl_SetResult( interp, (nodeSets[0])->GetName(), TCL_VOLATILE );
  delete [] nodeSets;
  return TCL_OK;
}


// ---------------------------------
// LsetCore_ExtractProjectionSetsMtd
// ---------------------------------

static int LsetCore_ExtractProjectionSetsMtd( ClientData clientData,
					      Tcl_Interp *interp,
					      int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  char *baseName;
  char objName[CV_STRLEN];
  cvPolyData **nodeSets = NULL;
  int i, j, numSets;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-base", STRING_Type, &baseName, NULL, REQUIRED, 0, { 0 } },
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
  if ( ! ( ls->GetGrid()->PSetsValid() ) ) {
    Tcl_SetResult( interp, "error: projection sets not valid\n"
		   "  projection set extraction requires:\n"
		   "    - SetSaveProjectionSets -flag 1\n"
		   "    - EvolveOneTimeStep\n"
		   "    - ExtractProjectionSets before any other node-querying"
		   " method (e.g. ExtractActiveNodes)\n", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( ls->GetGrid()->GetNodeSets( &nodeSets, &numSets ) != CV_OK ) {
    Tcl_SetResult( interp, "error on projection sets extraction", TCL_STATIC );
    return TCL_ERROR;
  }

  for (i = 0; i < numSets; i++) {
    sprintf(objName, "%s%d", baseName, i);
    if ( !(gRepository->Register( objName, nodeSets[i] ) ) ) {
      Tcl_AppendResult( interp, "error registering obj ", objName,
			" in repository", (char *)NULL );
      for ( j = i; j < numSets; j++ ) {
	delete nodeSets[j];
      }
      delete [] nodeSets;
      return TCL_ERROR;
    }
  }

  // Delete the array of cvPolyData pointers.  (Responsibility for mem
  // mgmt of the objects themselves has now been assumed by the
  // cvRepository.)
  if ( nodeSets != NULL ) {
    delete [] nodeSets;
  }

  return TCL_OK;
}


// -------------------------------
// LsetCore_ExtractCoveredNodesMtd
// -------------------------------

static int LsetCore_ExtractCoveredNodesMtd( ClientData clientData,
					    Tcl_Interp *interp,
					    int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  char *objName;
  cvPolyData **nodeSets;
  int numSets;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-out", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
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
  if ( ls->GetGrid()->SaveCoveredNodes() != CV_OK ) {
    Tcl_SetResult( interp, "error on querying covered nodes", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( ls->GetGrid()->GetNodeSets( &nodeSets, &numSets ) != CV_OK ) {
    Tcl_SetResult( interp, "error on covered nodes extraction", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( numSets != 1 ) {
    Tcl_SetResult( interp, "error on covered nodes extraction", TCL_STATIC );
    CleanUpNodeSets( nodeSets, numSets );
    return TCL_ERROR;
  }

  if ( !(gRepository->Register( objName, nodeSets[0] ) ) ) {
      Tcl_AppendResult( interp, "error registering obj ", objName,
			" in repository", (char *)NULL );
      CleanUpNodeSets( nodeSets, numSets );
      return TCL_ERROR;
  }

  Tcl_SetResult( interp, (nodeSets[0])->GetName(), TCL_VOLATILE );
  delete [] nodeSets;
  return TCL_OK;
}


// ---------------------------------
// LsetCore_ExtractUncoveredNodesMtd
// ---------------------------------

static int LsetCore_ExtractUncoveredNodesMtd( ClientData clientData,
					      Tcl_Interp *interp,
					      int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  char *objName;
  cvPolyData **nodeSets;
  int numSets;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-out", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
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
  if ( ls->GetGrid()->SaveUncoveredNodes() != CV_OK ) {
    Tcl_SetResult( interp, "error on querying uncovered nodes", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( ls->GetGrid()->GetNodeSets( &nodeSets, &numSets ) != CV_OK ) {
    Tcl_SetResult( interp, "error on uncovered nodes extraction", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( numSets == 0 ) {
    Tcl_SetResult( interp, "no uncovered nodes found", TCL_STATIC );
    return TCL_OK;
  }
  if ( numSets != 1 ) {
    Tcl_SetResult( interp, "error on uncovered nodes extraction", TCL_STATIC );
    CleanUpNodeSets( nodeSets, numSets );
    return TCL_ERROR;
  }

  if ( !(gRepository->Register( objName, nodeSets[0] ) ) ) {
      Tcl_AppendResult( interp, "error registering obj ", objName,
			" in repository", (char *)NULL );
      CleanUpNodeSets( nodeSets, numSets );
      return TCL_ERROR;
  }

  Tcl_SetResult( interp, (nodeSets[0])->GetName(), TCL_VOLATILE );
  delete [] nodeSets;
  return TCL_OK;
}


// ----------------------------
// LsetCore_ExtractMineNodesMtd
// ----------------------------

static int LsetCore_ExtractMineNodesMtd( ClientData clientData,
					 Tcl_Interp *interp,
					 int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  char *objName;
  cvPolyData **nodeSets;
  int numSets;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-out", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
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
  if ( ls->GetGrid()->SaveMineNodes() != CV_OK ) {
    Tcl_SetResult( interp, "error on querying mine nodes", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( ls->GetGrid()->GetNodeSets( &nodeSets, &numSets ) != CV_OK ) {
    Tcl_SetResult( interp, "error on mine nodes extraction", TCL_STATIC );
    return TCL_ERROR;
  }
  if ( numSets == 0 ) {
    Tcl_SetResult( interp, "no mine nodes found", TCL_STATIC );
    return TCL_OK;
  }
  if ( numSets != 1 ) {
    Tcl_SetResult( interp, "error on mine nodes extraction", TCL_STATIC );
    CleanUpNodeSets( nodeSets, numSets );
    return TCL_ERROR;
  }

  if ( !(gRepository->Register( objName, nodeSets[0] ) ) ) {
      Tcl_AppendResult( interp, "error registering obj ", objName,
			" in repository", (char *)NULL );
      CleanUpNodeSets( nodeSets, numSets );
      return TCL_ERROR;
  }

  Tcl_SetResult( interp, (nodeSets[0])->GetName(), TCL_VOLATILE );
  delete [] nodeSets;
  return TCL_OK;
}


// ---------------------------------
// LsetCore_SetSaveProjectionSetsMtd
// ---------------------------------

static int LsetCore_SetSaveProjectionSetsMtd( ClientData clientData,
					      Tcl_Interp *interp,
					      int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  int flag;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-flag", INT_Type, &flag, NULL, REQUIRED, 0, { 0 } },
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
  if (flag) {
    ls->SetSaveProjectionSets( 1 );
  } else {
    ls->SetSaveProjectionSets( 0 );
  }

  return TCL_OK;
}


// ---------------------------------
// LsetCore_GetSaveProjectionSetsMtd
// ---------------------------------

static int LsetCore_GetSaveProjectionSetsMtd( ClientData clientData,
					      Tcl_Interp *interp,
					      int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  int flag;
  char dummy[CV_STRLEN];

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:
  ls->GetSaveProjectionSets( &flag );

  sprintf( dummy, "%d", flag );
  Tcl_AppendElement( interp, "-flag" );
  Tcl_AppendElement( interp, dummy );

  return TCL_OK;
}


// --------------------
// LsetCore_SaveGridMtd
// --------------------
// Create a cvPolyData object which describes the nodes and
// connectivities of the created grid.

static int LsetCore_SaveGridMtd( ClientData clientData, Tcl_Interp *interp,
				 int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;
  char *resultName;
  cvPolyData *grid;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-result", STRING_Type, &resultName, NULL, REQUIRED, 0, { 0 } },
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

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    Tcl_AppendResult( interp, "object ", resultName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Invoke grid cvPolyData creation:
  grid = ls->GetGrid()->CreateGridPolyData();
  if ( grid == NULL ) {
    Tcl_SetResult( interp, "error saving grid... has the grid been "
		   "initialized?", TCL_STATIC );
    return TCL_ERROR;
  }

  // And register the result in the repository:
  if ( !(gRepository->Register( resultName, grid )) ) {
    Tcl_SetResult( interp, "error registering obj in repository",
		   TCL_STATIC );
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, grid->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// ------------------------
// LsetCore_GetGridStatsMtd
// ------------------------
// Retrieve statistics about a cvLevelSetSparseGrid in the form of a string.
// Since the stats of relevance to particular concrete subclasses of
// cvLevelSetStructuredGrid may be completely different, we will make no attempt
// to itemize this string, but will simply directly return the string
// generated by the grid object.

static int LsetCore_GetGridStatsMtd( ClientData clientData, Tcl_Interp *interp,
				     int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:
  Tcl_SetResult( interp, ls->GetGridStatString(), TCL_VOLATILE );

  return TCL_OK;
}


// --------------------------
// LsetCore_GetMemoryUsageMtd
// --------------------------

static int LsetCore_GetMemoryUsageMtd( ClientData clientData,
				       Tcl_Interp *interp,
				       int argc, CONST84 char *argv[] )
{
  cvLevelSet *ls = (cvLevelSet *)clientData;
  char *usage;

  usage = ARG_GenSyntaxStr( 2, argv, 0, NULL );
  if ( argc != 2 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d", ls->GetMemoryUsage() );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE );

  return TCL_OK;
}

