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

#include "simvascular_options.h"

// needed by cmake build to not throw errors
// when compiling on Windows
#ifdef WIN32
#include "windows.h"
#endif

#include "sv_IOstream.h"
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>

#include "tcl.h"
#include "tk.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

#include "sv2_globals.h"

#ifdef WIN32
#include <windows.h>
#include <tchar.h>
#include "Shlwapi.h"
#include <Shlobj.h>

#define BUFSIZE 1024
#define BUF_SIZE 1024

#ifndef GetShortPathName
  #ifdef UNICODE
    #define GetShortPathName GetShortPathNameW
  #else
    #define GetShortPathName GetShortPathNameA
  #endif // !UNICODE
#endif

#ifdef SV_USE_WIN32_REGISTRY
#include "sv_use_win32_registry.h"
#endif

#endif

#include "svTcl_AppInit.h"
#include "SimVascular_Init.h"

// -----------
// Tcl_AppInit
// -----------

int Tcl_AppInit( Tcl_Interp *interp )
{

  // store interp into a global variable
  gVtkTclInterp = interp;

#ifdef WIN32
#ifdef SV_USE_WIN32_REGISTRY

  if(gSimVascularUseWin32Registry) {
  
  if (Tcl_AppInt_Win32ReadRegistryVar("HomeDir", "env(SV_HOME)", interp ) == TCL_ERROR) {
    exit(-1);
  }

  if (Tcl_AppInt_Win32ReadRegistryVar("TclLibDir", "tcl_library", interp ) == TCL_ERROR) {
    exit(-1);
  }

  if (Tcl_AppInt_Win32ReadRegistryVar("TkLibDir", "tk_library", interp ) == TCL_ERROR) {
    exit(-1);
  }

}
  
#endif
#endif

  if ( Tcl_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Tcl_Init\n" );
    return TCL_ERROR;
  }

  if (gSimVascularBatchMode == 0) {
    if ( Tk_Init(interp) == TCL_ERROR ) {
      fprintf( stderr, "error on Tk_Init\n" );
      return TCL_ERROR;
    }
  }

  if ( SimVascular_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on SimVascular_Init\n" );
    return TCL_ERROR;
  }

  return TCL_OK;

}
