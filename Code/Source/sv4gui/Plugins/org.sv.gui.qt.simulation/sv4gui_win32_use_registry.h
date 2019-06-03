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

#ifndef _SV4GUI_USE_WIN32_REGISTRY_H
#define _SV4GUI_USE_WIN32_REGISTRY_H

#include "SimVascular.h"

#include "simvascular_options.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

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

#include <windows.h>
#include <stdio.h>
#include <tchar.h>

#define MAX_KEY_LENGTH 255
#define MAX_VALUE_NAME 16383

#ifdef WIN32
#ifdef SV_USE_WIN32_REGISTRY

void sv4guiQueryKey(HKEY hKey);
int sv4gui_parse_registry_for_svsolver(char* keytofind, char* rtnval);
int sv4gui_parse_registry_for_svsolver_internal(char* toplevel_key, char* keytofind, char* rtnval);

#endif
#endif

#endif // _SV4GUI_USE_WIN32_REGISTRY_H
