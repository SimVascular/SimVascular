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

/* copyright (C) 2001-2004 Jean-Luc Fontaine (mailto:jfontain@free.fr) */
/* this library is free software: please read the README file enclosed in this package */

/* $Id: tclthread.c,v 1.2 2006/03/05 17:41:53 jfontain Exp $ */


#ifdef WIN32
  #define XSTRING(s) #s
#else
  #include <symcat.h>
#endif
#include <limits.h>
#include <string.h>
#include <tcl.h>
#include "tclpython.h"


TCL_DECLARE_MUTEX(threadMutex);

typedef struct ThreadEvent {                        /* copied from threadCmd.c in Tcl thread extension source code and simplified */
    Tcl_Event event;
    Tcl_Interp *interpreter;
    char *script;
} ThreadEvent;


static void ThreadErrorProc(Tcl_Interp *interpreter)
{
#ifdef WIN32
/* George Petasis, 21 Feb 2006:
 * Unfortunatelly, I cannot find a way to measure the LONG_MAX characters
 * with Visual C++ preprocessor. char buffer[strlen("")] does nto seem to work
 * with static functions under Visual C++ .NET.*/
    char buffer[15];
#else
    char buffer[strlen(XSTRING(LONG_MAX))];
#endif
    CONST char *errorInformation;
    Tcl_Channel errorChannel;

    errorInformation = Tcl_GetVar(interpreter, "errorInfo", TCL_GLOBAL_ONLY);
    if (errorInformation == 0) {
        errorInformation = "";
    }
    errorChannel = Tcl_GetStdChannel(TCL_STDERR);
    if (errorChannel == NULL) return;
    sprintf(buffer, "%ld", (long)CURRENTTHREAD);
    Tcl_WriteChars(errorChannel, "Error from thread ", -1);
    Tcl_WriteChars(errorChannel, buffer, -1);
    Tcl_WriteChars(errorChannel, "\n", 1);
    Tcl_WriteChars(errorChannel, errorInformation, -1);
    Tcl_WriteChars(errorChannel, "\n", 1);
}

static int ThreadEventProc(Tcl_Event *event, int mask)
{
    int code;
    ThreadEvent *data = (ThreadEvent *)event;                                                    /* event is really a ThreadEvent */

    Tcl_Preserve(data->interpreter);
    code = Tcl_EvalEx(data->interpreter, data->script, -1, TCL_EVAL_GLOBAL);
    Tcl_Free(data->script);
    if (code != TCL_OK) {
        ThreadErrorProc(data->interpreter);
    }
    Tcl_Release(data->interpreter);
    return 1;
}

void tclSendThread(Tcl_ThreadId thread, Tcl_Interp *interpreter, CONST char *script)
{
    ThreadEvent *event;
    Tcl_Channel errorChannel;
    Tcl_Obj *object;
    int boolean;

    object = Tcl_GetVar2Ex(interpreter, "::tcl_platform", "threaded", 0);
    if ((object == 0) || (Tcl_GetBooleanFromObj(interpreter, object, &boolean) != TCL_OK) || !boolean) {
        errorChannel = Tcl_GetStdChannel(TCL_STDERR);
        if (errorChannel == NULL) return;
        Tcl_WriteChars(
            errorChannel, "error: Python thread requested script evaluation on Tcl core not compiled for multithreading.\n", -1
        );
        return;
    }
    event = (ThreadEvent *)Tcl_Alloc(sizeof(ThreadEvent));
    event->event.proc = ThreadEventProc;
    event->interpreter = interpreter;
    event->script = strcpy(Tcl_Alloc(strlen(script) + 1), script);
    Tcl_MutexLock(&threadMutex);
    Tcl_ThreadQueueEvent(thread, (Tcl_Event *)event, TCL_QUEUE_TAIL);
    Tcl_ThreadAlert(thread);
    Tcl_MutexUnlock(&threadMutex);
}
