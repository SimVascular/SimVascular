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
#include "cvMeshSimDiscreteSolidModel.h"
#include "cv_discrete_init.h"
#include "cv_discrete_utils.h"


// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Prototypes:
// -----------

int MeshSimDiscrete_AvailableCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

// Globals:
// --------

#include "cv_globals.h"


cvSolidModel* CreateMeshSimDiscreteSolidModel()
{
	return new cvMeshSimDiscreteSolidModel();
}

int Meshsimdiscretesolid_Init( Tcl_Interp *interp )
{

  // Initialize discrete_utils
  if (DiscreteUtils_Init() != CV_OK) {
    return TCL_OK;
  }

	// Get the solid model factory registrar from the main app.
	cvFactoryRegistrar* solidModelRegistrar = 
    (cvFactoryRegistrar *) Tcl_GetAssocData( interp, "SolidModelRegistrar", NULL);
	
  if (solidModelRegistrar != NULL) {
	  // Register this particular factory method with the main app.
	  solidModelRegistrar->SetFactoryMethodPtr( SM_KT_DISCRETE, 
      (FactoryMethodPtr) &CreateMeshSimDiscreteSolidModel );

    Tcl_CreateCommand( interp, "discrete_available", MeshSimDiscrete_AvailableCmd,
		       (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  }
  else {
    return TCL_ERROR;
  }

  return TCL_OK;
}

int MeshSimDiscrete_AvailableCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  // Make a new Tcl command:
  Tcl_SetResult( interp, "Discrete Solid Model Module Available", TCL_VOLATILE );

  return TCL_OK;
}

