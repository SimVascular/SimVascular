/* copyright (C) 2001-2004 Jean-Luc Fontaine (mailto:jfontain@free.fr)
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 */

/* when Tcl core was not compiled for multithreading, Tcl_GetCurrentThread() always returns 0, so */
/* use this macro instead to be able to detect whether request is coming from a different thread. */
#ifdef __WIN32__
    #include <Windows.h>
    #define CURRENTTHREAD ((Tcl_ThreadId)GetCurrentThreadId())
#else
    #include <pthread.h>
    #define CURRENTTHREAD ((Tcl_ThreadId)pthread_self())
#endif

#include "SimVascular.h"

void tclSendThread(Tcl_ThreadId, Tcl_Interp *, CONST char *);
/* public function for use in extensions to this extension: */
Tcl_Interp *tclInterpreter(CONST char *);

//int Tclpython_Init(Tcl_Interp *interpreter);
