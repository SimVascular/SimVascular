/* Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
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

#define CV_OK                 1
#define CV_ERROR              0

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

#ifdef SV_EXPORT_UTILS
  #undef SV_EXPORT_UTILS
#endif
#ifdef SV_EXPORT_UTILS_COMPILE
  #define SV_EXPORT_UTILS SV_DLL_EXPORT
#else
  #define SV_EXPORT_UTILS SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_GLOBALS
  #undef SV_EXPORT_GLOBALS
#endif
#ifdef SV_EXPORT_GLOBALS_COMPILE
  #define SV_EXPORT_GLOBALS SV_DLL_EXPORT
#else
  #define SV_EXPORT_GLOBALS SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_REPOSITORY
  #undef SV_EXPORT_REPOSITORY
#endif
#ifdef SV_EXPORT_REPOSITORY_COMPILE
  #define SV_EXPORT_REPOSITORY SV_DLL_EXPORT
#else
  #define SV_EXPORT_REPOSITORY SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_SYSGEOM
  #undef SV_EXPORT_SYSGEOM
#endif
#ifdef SV_EXPORT_SYSGEOM_COMPILE
  #define SV_EXPORT_SYSGEOM SV_DLL_EXPORT
#else
  #define SV_EXPORT_SYSGEOM SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_IMAGE
  #undef SV_EXPORT_IMAGE
#endif
#ifdef SV_EXPORT_IMAGE_COMPILE
  #define SV_EXPORT_IMAGE SV_DLL_EXPORT
#else
  #define SV_EXPORT_IMAGE SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_TCLPYTHON
  #undef SV_EXPORT_TCLPYTHON
#endif
#ifdef SV_EXPORT_TCLPYTHON_COMPILE
  #define SV_EXPORT_TCLPYTHON SV_DLL_EXPORT
#else
  #define SV_EXPORT_TCLPYTHON SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_POST
  #undef SV_EXPORT_POST
#endif
#ifdef SV_EXPORT_POST_COMPILE
  #define SV_EXPORT_POST SV_DLL_EXPORT
#else
  #define SV_EXPORT_POST SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_LSET
  #undef SV_EXPORT_LSET
#endif
#ifdef SV_EXPORT_LSET_COMPILE
  #define SV_EXPORT_LSET SV_DLL_EXPORT
#else
  #define SV_EXPORT_LSET SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_SOLID
  #undef SV_EXPORT_SOLID
#endif
#ifdef SV_EXPORT_SOLID_COMPILE
  #define SV_EXPORT_SOLID SV_DLL_EXPORT
#else
  #define SV_EXPORT_SOLID SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_DISCRETE
  #undef SV_EXPORT_DISCRETE
#endif
#ifdef SV_EXPORT_DISCRETE_COMPILE
  #define SV_EXPORT_DISCRETE SV_DLL_EXPORT
#else
  #define SV_EXPORT_DISCRETE SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_MESHSIM_SOLID
  #undef SV_EXPORT_MESHSIM_SOLID
#endif
#ifdef SV_EXPORT_MESHSIM_SOLID_COMPILE
  #define SV_EXPORT_MESHSIM_SOLID SV_DLL_EXPORT
#else
  #define SV_EXPORT_MESHSIM_SOLID SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_OPENCASCADE
  #undef SV_EXPORT_OPENCASCADE
#endif
#ifdef SV_EXPORT_OPENCASCADE_COMPILE
  #define SV_EXPORT_OPENCASCADE SV_DLL_EXPORT
#else
  #define SV_EXPORT_OPENCASCADE SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_POLYDATASOLID
  #undef SV_EXPORT_POLYDATASOLID
#endif
#ifdef SV_EXPORT_POLYDATASOLID_COMPILE
  #define SV_EXPORT_POLYDATASOLID SV_DLL_EXPORT
#else
  #define SV_EXPORT_POLYDATASOLID SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_ADAPTOR
  #undef SV_EXPORT_ADAPTOR
#endif
#ifdef SV_EXPORT_ADAPTOR_COMPILE
  #define SV_EXPORT_ADAPTOR SV_DLL_EXPORT
#else
  #define SV_EXPORT_ADAPTOR SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_MESH
  #undef SV_EXPORT_MESH
#endif
#ifdef SV_EXPORT_MESH_COMPILE
  #define SV_EXPORT_MESH SV_DLL_EXPORT
#else
  #define SV_EXPORT_MESH SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_MESHSIM_ADAPTOR
  #undef SV_EXPORT_MESHSIM_ADAPTOR
#endif
#ifdef SV_EXPORT_MESHSIM_ADAPTOR_COMPILE
  #define SV_EXPORT_MESHSIM_ADAPTOR SV_DLL_EXPORT
#else
  #define SV_EXPORT_MESHSIM_ADAPTOR SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_MESHSIM_MESH
  #undef SV_EXPORT_MESHSIM_MESH
#endif
#ifdef SV_EXPORT_MESHSIM_MESH_COMPILE
  #define SV_EXPORT_MESHSIM_MESH SV_DLL_EXPORT
#else
  #define SV_EXPORT_MESHSIM_MESH SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_MMG
  #undef SV_EXPORT_MMG
#endif
#ifdef SV_EXPORT_MMG_COMPILE
  #define SV_EXPORT_MMG SV_DLL_EXPORT
#else
  #define SV_EXPORT_MMG SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_TETGEN_ADAPTOR
  #undef SV_EXPORT_TETGEN_ADAPTOR
#endif
#ifdef SV_EXPORT_TETGEN_ADAPTOR_COMPILE
  #define SV_EXPORT_TETGEN_ADAPTOR SV_DLL_EXPORT
#else
  #define SV_EXPORT_TETGEN_ADAPTOR SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_TETGEN_MESH
  #undef SV_EXPORT_TETGEN_MESH
#endif
#ifdef SV_EXPORT_TETGEN_MESH_COMPILE
  #define SV_EXPORT_TETGEN_MESH SV_DLL_EXPORT
#else
  #define SV_EXPORT_TETGEN_MESH SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_SEGITK
  #undef SV_EXPORT_SEGITK
#endif
#ifdef SV_EXPORT_SEGITK_COMPILE
  #define SV_EXPORT_SEGITK SV_DLL_EXPORT
#else
  #define SV_EXPORT_SEGITK SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_SEGITKUTILS
  #undef SV_EXPORT_SEGITKUTILS
#endif
#ifdef SV_EXPORT_SEGITKUTILS_COMPILE
  #define SV_EXPORT_SEGITKUTILS SV_DLL_EXPORT
#else
  #define SV_EXPORT_SEGITKUTILS SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_PARASOLID
  #undef SV_EXPORT_PARASOLID
#endif
#ifdef SV_EXPORT_PARASOLID_COMPILE
  #define SV_EXPORT_PARASOLID SV_DLL_EXPORT
#else
  #define SV_EXPORT_PARASOLID SV_DLL_IMPORT
#endif

#ifdef SV_EXPORT_TCLPYTHON
  #undef SV_EXPORT_TCLPYTHON
#endif
#ifdef SV_EXPORT_TCLPYTHON_COMPILE
  #define SV_EXPORT_TCLPYTHON SV_DLL_EXPORT
#else
  #define SV_EXPORT_TCLPYTHON SV_DLL_IMPORT
#endif

#ifdef SVQTMAINWINDOW_EXPORT
  #undef SVQTMAINWINDOW_EXPORT
#endif
#ifdef SVQTMAINWINDOW_EXPORT_COMPILE
  #define SVQTMAINWINDOW_EXPORT SV_DLL_EXPORT
#else
  #define SVQTMAINWINDOW_EXPORT SV_DLL_IMPORT
#endif

#endif  /* SIMVASCULAR_H */

