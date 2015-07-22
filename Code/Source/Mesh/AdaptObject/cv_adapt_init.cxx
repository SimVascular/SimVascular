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

// Adapt
// -----
int Adapt_RegistrarsListCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

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

