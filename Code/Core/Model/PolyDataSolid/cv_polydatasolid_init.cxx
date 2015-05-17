/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
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
 *
 *=========================================================================*/


/** @file cv_polydatasolid_init.cxx
 *  @brief Ipmlements function to register PolyDataSolid as a solid type
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#include "SimVascular.h" 

#include <stdio.h>
#include <string.h>
#include "cvRepository.h"
#include "cv_solid_init.h"
#include "cv_polydatasolid_init.h"
#include "cv_polydatasolid_utils.h"
#include "cvSolidModel.h"
#include "cv_arg.h"
#include "cv_misc_utils.h"
#include "cv_vtk_utils.h"
#include "cvPolyData.h"
#include "cvPolyDataSolid.h"
#include "vtkPolyData.h"

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

cvPolyDataSolid* CreatePolyDataSolid()
{
	return new cvPolyDataSolid();
}

// -----
// Solid
// -----
//
int PolyDataSolid_AvailableCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

int PolyDataSolid_RegistrarsListCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );


int PolyDataSolid_Init( Tcl_Interp *interp )
{
  cvFactoryRegistrar* solidModelRegistrar = 
    (cvFactoryRegistrar *) Tcl_GetAssocData( interp, "SolidModelRegistrar", NULL);

  if (solidModelRegistrar != NULL) {
          // Register this particular factory method with the main app.
          solidModelRegistrar->SetFactoryMethodPtr( SM_KT_POLYDATA, 
      (FactoryMethodPtr) &CreatePolyDataSolid );

    Tcl_CreateCommand( interp, "polydata_available", PolyDataSolid_AvailableCmd,
		       (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  }
  else {
    return TCL_ERROR;
  }

  Tcl_CreateCommand( interp, "polydatasolid_registrars", PolyDataSolid_RegistrarsListCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  // Initialize parasolid_utils
  if (PlyDtaUtils_Init() != CV_OK) {
    return TCL_ERROR;
  }

  return TCL_OK;
}

int PolyDataSolid_AvailableCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  Tcl_SetResult( interp, "PolyData Solid Module Available", TCL_VOLATILE );

  return TCL_OK;
}

int PolyDataSolid_RegistrarsListCmd( ClientData clientData, Tcl_Interp *interp,
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

