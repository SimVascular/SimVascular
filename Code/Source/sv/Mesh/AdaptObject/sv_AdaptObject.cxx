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

#include "sv_AdaptObject.h"
//#include "sv_TetGenAdapt.h"
#include "sv_MeshObject.h"
#include "sv_misc_utils.h"

#ifdef SV_USE_PYTHON
#include "Python.h"
#include "sv_adapt_init_py.h"
#endif

#include <string.h>
#include <assert.h>

#include "sv2_globals.h"

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
#ifdef SV_USE_TCL
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
#endif
// ----------------------------
// DefaultInstantiateAdaptObject for python
// ----------------------------
#ifdef SV_USE_PYTHON
cvAdaptObject* cvAdaptObject::DefaultInstantiateAdaptObject(KernelType t )
{
  // Get the adapt object factory registrar associated with the python interpreter
  
  PyObject* pyGlobal = PySys_GetObject("AdaptObjectRegistrar");
  pyAdaptObjectRegistrar* tmp = (pyAdaptObjectRegistrar *) pyGlobal;
  cvFactoryRegistrar* adaptObjectRegistrar =tmp->registrar;
  if (adaptObjectRegistrar==NULL)
  {
    fprintf(stdout,"Cannot get AdaptObjectRegistrar from pySys");
  }
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
  }

  return adaptor;
}
#endif
