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
#include "tcl.h"

#include "cv_get_tcl_interp_init.h"
#include "tcl.h"
#include "cv_globals.h"

int Getinterp_Init(Tcl_Interp *interp) {

  gVtkTclInterp = interp;

  const char name[] = "gTclInterp";
  char strptr[32];
  strptr[0]='\0';
  
  sprintf(strptr,"%p",interp);

  Tcl_SetVar(interp,name, strptr, TCL_GLOBAL_ONLY);

  /*
  const char* str;
  str = Tcl_GetVar(interp,name,TCL_GLOBAL_ONLY); 
  fprintf(stdout,"%s\n",str);
  void* ptr = NULL;
  sscanf(str,"%p",&ptr);
  fprintf(stdout,"\n%p\n",ptr);
  */
  
  return TCL_OK;

}
