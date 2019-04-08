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

#ifndef SIMVASCULAR_H

#define SV_OK                 1
#define SV_ERROR              0


/* true / false don't seem to be defined on linux */
#ifndef WIN32
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif
#endif

#ifdef WINDOWS
  #ifndef SV_STATIC_LINK
    #define SV_DLL_EXPORT __declspec(dllexport)
    #define SV_DLL_IMPORT __declspec(dllimport)
  #else
    #define SV_DLL_EXPORT
    #define SV_DLL_IMPORT
  #endif
#else
  #define SV_DLL_EXPORT
  #define SV_DLL_IMPORT
#endif

#include "simvascular_version.h"
#include "simvascular_options.h"

#ifdef SVQTMAINWINDOW_EXPORT
  #undef SVQTMAINWINDOW_EXPORT
#endif
#ifdef SVQTMAINWINDOW_EXPORT_COMPILE
  #define SVQTMAINWINDOW_EXPORT SV_DLL_EXPORT
#else
  #define SVQTMAINWINDOW_EXPORT SV_DLL_IMPORT
#endif

#endif  /* SIMVASCULAR_H */

