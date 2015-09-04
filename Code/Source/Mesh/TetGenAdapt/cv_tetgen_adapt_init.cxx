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

/** @file cv_tetgen_adapt_init.cxx
 *  @brief Ipmlements functions to register TetGenAdapt as an adaptor type
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#include "SimVascular.h"
#include "cv_misc_utils.h"
#include "cv_tetgen_adapt_init.h"
#include "cvTetGenAdapt.h"
//#include "cv_adapt_utils.h"
#include "cv_arg.h"

#include <stdio.h>
#include <string.h>
#include "cvRepository.h"
#include "cv_arg.h"
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
//
cvTetGenAdapt* CreateTetGenAdapt()
{
	return new cvTetGenAdapt();
}

// -----
// Adapt
// -----

int TetGenAdapt_AvailableCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

int TetGenAdapt_RegistrarsListCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

// ----------
// Tetgenmesh_Init
// ----------

int TetGenAdapt_Init( Tcl_Interp *interp )
{

  printf("  %-12s %s\n","","TetGen Adaption Enabled");
  
  // Associate the adapt registrar with the Tcl interpreter so it can be
  // retrieved by the DLLs.
  cvFactoryRegistrar* adaptObjectRegistrar = 
    (cvFactoryRegistrar *) Tcl_GetAssocData( interp, "AdaptObjectRegistrar", NULL);

  if (adaptObjectRegistrar != NULL) {
          // Register this particular factory method with the main app.
          adaptObjectRegistrar->SetFactoryMethodPtr( KERNEL_TETGEN, 
      (FactoryMethodPtr) &CreateTetGenAdapt );

    Tcl_CreateCommand( interp, "tetgenadapt_available", TetGenAdapt_AvailableCmd,
		       (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  }
  else {
    return TCL_ERROR;
  }

  Tcl_CreateCommand( interp, "tetgenadapt_registrars", TetGenAdapt_RegistrarsListCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  return TCL_OK;

}


int TetGenAdapt_AvailableCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  Tcl_SetResult( interp, "TetGen Mesh Adaption Available", TCL_VOLATILE );

  return TCL_OK;
}

int TetGenAdapt_RegistrarsListCmd( ClientData clientData, Tcl_Interp *interp,
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



