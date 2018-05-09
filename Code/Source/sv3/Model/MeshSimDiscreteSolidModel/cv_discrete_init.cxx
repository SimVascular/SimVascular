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
  if (DiscreteUtils_Init() != SV_OK) {
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

