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


/** @file cv_occtsolid_init.cxx
 *  @brief Ipmlements function to register OCCTSolidModel as a solid type
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
#include "cv_occt_init.h"
#include "cvSolidModel.h"
#include "cv_arg.h"
#include "cv_misc_utils.h"
#include "cv_vtk_utils.h"
#include "cvOCCTSolidModel.h"

#include "cvFactoryRegistrar.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "cv_globals.h"
#include <TDF_Data.hxx>
#include <TDF_Label.hxx>
#include <TDocStd_Application.hxx>
#include <AppStd_Application.hxx>
#include <TDocStd_Document.hxx>
#include <TDocStd_XLinkTool.hxx>
#include <CDF_Session.hxx>

// Prototypes:
// -----------

cvOCCTSolidModel* CreateOCCTSolidModel()
{
	return new cvOCCTSolidModel();
}

// -----
// Solid
// -----
//
int OCCTSolidModel_AvailableCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

int OCCTSolidModel_RegistrarsListCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );


int Occt_Init( Tcl_Interp *interp )
{
  //if (!CDF_Session::Exists()) {
  //  Handle(CDF_Session) S = CDF_Session::CurrentSession();
  //  if (!S->HasCurrentApplication())
  //  Standard_DomainError::Raise("DDocStd::Find no applicative session");
  //  gOCCTManager = Handle(TDocStd_Application)::DownCast(S->CurrentApplication());
  //}
  //else {
  //  fprintf(stderr,"No active application\n");
  //  // none active application
  //}
  gOCCTManager = new AppStd_Application;
  if ( gOCCTManager == NULL ) {
    fprintf( stderr, "error allocating gOCCTManager\n" );
    return TCL_ERROR;
  }
  Handle(TDocStd_Document) doc;
  gOCCTManager->NewDocument("Standard",doc);

  printf("  %-12s %s\n","OpenCASCADE:", "6.9.1");
  cvFactoryRegistrar* solidModelRegistrar =
    (cvFactoryRegistrar *) Tcl_GetAssocData( interp, "SolidModelRegistrar", NULL);

  if (solidModelRegistrar != NULL) {
          // Register this particular factory method with the main app.
          solidModelRegistrar->SetFactoryMethodPtr( SM_KT_OCCT,
      (FactoryMethodPtr) &CreateOCCTSolidModel );

    Tcl_CreateCommand( interp, "opencascade_available", OCCTSolidModel_AvailableCmd,
		       (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  }
  else {
    return TCL_ERROR;
  }

  Tcl_CreateCommand( interp, "opencascadesolidmodel_registrars", OCCTSolidModel_RegistrarsListCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  return TCL_OK;
}

int OCCTSolidModel_AvailableCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  Tcl_SetResult( interp, "OpenCASCADE Solid Module Available", TCL_VOLATILE );

  return TCL_OK;
}

int OCCTSolidModel_RegistrarsListCmd( ClientData clientData, Tcl_Interp *interp,
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
