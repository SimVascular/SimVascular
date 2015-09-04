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

#include "cvAdaptObject.h"
//#include "cvTetGenAdapt.h"
#include "cvMeshObject.h"
#include "cv_misc_utils.h"

#include <string.h>
#include <assert.h>

#include "cv_globals.h"

KernelType cvAdaptObject::gCurrentKernel = KERNEL_TETGEN;
cvFactoryRegistrar cvAdaptObject::gRegistrar;

cvAdaptObject::cvAdaptObject( KernelType t)
  : cvRepositoryData( ADAPTOR_T )
{
  adapt_kernel_ = t;
}

cvAdaptObject::~cvAdaptObject()
{
  ;
}

// ----------------------------
// DefaultInstantiateAdaptObject
// ----------------------------

cvAdaptObject* cvAdaptObject::DefaultInstantiateAdaptObject( Tcl_Interp *interp,KernelType t )
{
  // Get the adapt object factory registrar associated with this Tcl interpreter.
  cvFactoryRegistrar* adaptObjectRegistrar;
  if (interp == NULL) {
    fprintf(stdout,"WARNING:  Null interpreter passed to AdaptObject.  Overriding with default.\n");
    fflush(stdout);
  }
  Tcl_Interp* myinterp = NULL;
  myinterp = gVtkTclInterp;
  assert(myinterp); 

  adaptObjectRegistrar = (cvFactoryRegistrar *) Tcl_GetAssocData( myinterp, "AdaptObjectRegistrar", NULL);

  cvAdaptObject* adaptor = NULL;
  if (t == KERNEL_TETGEN || 
      t == KERNEL_MESHSIM)
  {
    adaptor = (cvAdaptObject *) (adaptObjectRegistrar->UseFactoryMethod( t ));
    if (adaptor == NULL) {
		  fprintf( stdout, "Unable to create adaptor object for kernel (%i)\n",cvAdaptObject::gCurrentKernel);
    }

  } else {
    fprintf( stdout, "current kernel is not valid (%i)\n",t);
    Tcl_SetResult( interp, "current kernel is not valid", TCL_STATIC );
  }

  return adaptor;
}

