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

// Adapted from vtk/tcl/tkAppInit.cxx.  The purpose of this function
// is to allow incorporation of the vtk system into larger Tcl
// applications.

#include <stdio.h>
#include "tk.h"
#include "vtkToolkits.h"
#include "cv_VTK_init.h"
#include "cvVTK.h"

#include "cv_globals.h"

// ----------
// myVtk_Init
// ----------

#ifndef VTK_BUILD_SHARED_LIBS
# include "vtktcl_static_prototypes.h"
#endif

int myVtk_Init( Tcl_Interp *interp )
{

#ifndef VTK_BUILD_SHARED_LIBS
# include "vtktcl_static_packages.h"
#endif

  // no longer can support gSimVascularBatchMode
  // since we are picking up the prototypes from auto
  // generated vtk files

  if (gSimVascularBatchMode) {
    fprintf(stderr,"real batch mode not supported anymore!\n");
  }

  // Print version info:
  vtkVersion *vobj = vtkVersion::New();
  if (gSimVascularBatchMode == 0) {
    printf("  %-12s %s\n", "vtk:", vobj->GetVTKVersion());
  } else {
    printf("  %-12s %s\n", "vtk:", vobj->GetVTKVersion());
  }
  vobj->Delete();

  return TCL_OK;
}
