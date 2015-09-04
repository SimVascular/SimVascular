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

#include "SimVascular.h" 

#include "cvIOstream.h"
#include <time.h>
#include <stdlib.h>

#ifdef __NON_STD_TCL_INSTALL
  #include "tcl.h"
  #include "tk.h"
#else
  #include <tcl.h>
  #include <tk.h>
#endif

#include "cv_repos_init.h"
#include "cv_LsetCore_init.h"
#include "cv_LsetV_init.h"
#include "cv_geom_init.h"
#include "cv_image_init.h"
#include "cv_math_init.h"
#include "cv_post_init.h"
#include "cv_get_tcl_interp_init.h"
#include "cv_polydatasolid_init.h"

#ifndef EXCLUDE_SOLID_MODEL
  #include "cv_solid_init.h"
#endif

#ifdef USE_MESHSIM
  #include "cv_mesh_init.h"
  #include "cv_meshsim_mesh_init.h"
#endif

#ifdef USE_MESHSIM_ADAPTOR
  #include "cv_adapt_init.h"
  #include "cv_meshsim_adapt_init.h"
#endif

#ifdef USE_TETGEN
  #include "cv_mesh_init.h"
  #include "cv_tetgen_mesh_init.h"
#endif

#ifdef USE_DISCRETE_MODEL
  #include "cv_discrete_init.h"
#endif

#ifdef USE_PARASOLID
  #include "cv_parasolid_solid_init.h"
#endif

#include "cv_VTK_init.h"

#ifdef USE_ITK
  #include "cv_ITKLset_init.h"
  #include "cv_ITKUtils_init.h"
#endif

#ifdef USE_TET_ADAPTOR
  #include "cv_adapt_init.h"
  #include "cv_tetgen_adapt_init.h"
#endif

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

#ifdef WINDOWS
extern "C" {__declspec(dllexport) int SimVascular_Init(Tcl_Interp *interp);}
#else
extern "C" {int SimVascular_Init( Tcl_Interp *interp );}
#endif

//#ifdef USE_VMTK
//extern "C" {int CV_DLL_EXPORT Vtkvmtkcommontcl_Init(Tcl_Interp *interp);}
//extern "C" {int CV_DLL_EXPORT Vtkvmtkcomputationalgeometrytcl_Init(Tcl_Interp *interp);}
//extern "C" {int CV_DLL_EXPORT Vtkvmtkdifferentialgeometrytcl_Init(Tcl_Interp *interp);}
//extern "C" {int CV_DLL_EXPORT Vtkvmtkiotcl_Init(Tcl_Interp *interp);}
//extern "C" {int CV_DLL_EXPORT Vtkvmtkmisctcl_Init(Tcl_Interp *interp);}
//extern "C" {int CV_DLL_EXPORT Vtkvmtksegmentationtcl_Init(Tcl_Interp *interp);}
//#endif



void SimVascularWelcome( Tcl_Interp *interp );

#include "cv_globals.h"

// ---------------
// SimVascularWelcome
// ---------------

void SimVascularWelcome( Tcl_Interp *interp )
{
  // Find the date of the executable we're running
  Tcl_Eval( interp, "puts \"\"");
  Tcl_Eval( interp, "set simvascular_home \"$env(SIMVASCULAR_HOME)\";"
                  "if { [file exists [file join $simvascular_home/Tcl/startup_configure.tcl]]} {"
                      "source [file join $simvascular_home/Tcl/startup_configure.tcl];"
                      "if { [file exists [file join $simvascular_home/release-date]] } {"
                          "set SIMVASCULAR_BUILD_STR \".$SIMVASCULAR_PATCH_VERSION\";"
                          "if {$SIMVASCULAR_VERSION !=\"simvascular\"} {"
                            "set SIMVASCULAR_BUILD_STR \".$SIMVASCULAR_PATCH_VERSION $SIMVASCULAR_VERSION\";"
                          "}"
                      "} else {"
                          "set SIMVASCULAR_BUILD_STR \" (dev build)\";"
                      "}"
                  "} else {"
                  "set SIMVASCULAR_FULL_VER_NO \"unknown version\";"
                  "set SIMVASCULAR_BUILD_STR \"unknown release\";"
                  "}");
  Tcl_Eval( interp, "puts [format \"  %-12s %s\" \"SimVascular:\" $SIMVASCULAR_FULL_VER_NO$SIMVASCULAR_BUILD_STR]" );
  Tcl_Eval( interp, "puts \"  Copyright (c) 2014-2015 The Regents of the University of California.\"" );
  Tcl_Eval( interp, "puts \"                         All Rights Reserved.\"");
  Tcl_Eval( interp, "puts \"\"");
  Tcl_Eval( interp, "flush" );


  Tcl_Eval( interp, "puts [format \"  %-12s %s\" \"Tcl:\" [info patchlevel]]" );

  return;
}

int SimVascular_Init( Tcl_Interp *interp )
{
  SimVascularWelcome(interp);

  if ( myVtk_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on myVtk_Init\n" );
    return TCL_ERROR;
  }

#ifdef SIMVASCULAR_STATIC_BUILD
  if ( Getinterp_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Getinterp_Init\n" );
    return TCL_ERROR;
  }

  if ( Repos_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Repos_Init\n" );
    return TCL_ERROR;
  }

  if ( Lsetcore_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on LsetCore_Init\n" );
    return TCL_ERROR;
  }

  if ( Lsetv_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on LsetV_Init\n" );
    return TCL_ERROR;
  }

  if ( Geom_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Geom_Init\n" );
    return TCL_ERROR;
  }

  if ( Image_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Image_Init\n" );
    return TCL_ERROR;
  }

  if ( Math_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Math_Init\n" );
    return TCL_ERROR;
  }

  if ( Gdscpost_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on gdscPost_Init\n" );
    return TCL_ERROR;
  }

#ifndef EXCLUDE_SOLID_MODEL
  if ( Solid_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Solid_Init\n" );
    return TCL_ERROR;
  }
#endif

  if ( PolyDataSolid_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on PolyDataSolid_Init\n" );
    return TCL_ERROR;
  }

#ifdef USE_MESHSIM
  if ( Gdscmesh_Init(interp) == TCL_ERROR ) {
      fprintf( stderr, "error on gdscMesh_Init\n" );
      return TCL_ERROR;
  }

#elif defined USE_TETGEN
  if ( Gdscmesh_Init(interp) == TCL_ERROR ) {
      fprintf( stderr, "error on gdscMesh_Init\n" );
      return TCL_ERROR;
  }
#endif

#ifdef USE_TETGEN
  if ( Tetgenmesh_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Tetgenmesh_Init\n" );
    return TCL_ERROR;
  }
#endif

#ifdef USE_MESHSIM_ADAPTOR
  if ( Adapt_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Adapt_Init\n" );
    return TCL_ERROR;
  }
#elif defined USE_TET_ADAPTOR
  if ( Adapt_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Adapt_Init\n" );
    return TCL_ERROR;
  }
#endif

#ifdef USE_TET_ADAPTOR
  if ( TetGenAdapt_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on TetGenAdapt_Init\n" );
    return TCL_ERROR;
  }
#endif

#ifdef USE_MESHSIM
#ifndef USE_MESHSIM_SHARED  
  if ( Meshsimmesh_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on gdscMesh_Init\n" );
    return TCL_ERROR;
  }
#endif
#endif

#ifdef USE_MESHSIM_ADAPTOR
  if ( MeshSimAdapt_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on MeshSimAdapt_Init\n" );
    return TCL_ERROR;
  }
#endif


#ifdef USE_DISCRETE_MODEL
#ifndef USE_DISCRETE_MODEL_SHARED
  if ( Meshsimdiscretesolid_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on gdscMesh_Init\n" );
    return TCL_ERROR;
  }
#endif
#endif

#ifdef USE_PARASOLID
#ifndef USE_PARASOLID_SHARED
  if ( Parasolidsolid_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on gdscMesh_Init\n" );
    return TCL_ERROR;
  }
#endif
#endif

//#ifdef USE_VMTK
//  if ( Vtkvmtkcommontcl_Init(interp) == TCL_ERROR ) {
//    fprintf( stderr, "error on Getinterp_Init\n" );
//    return TCL_ERROR;
//  }
//
//  if ( Vtkvmtkcomputationalgeometrytcl_Init(interp) == TCL_ERROR ) {
//    fprintf( stderr, "error on Repos_Init\n" );
//    return TCL_ERROR;
//  }
//
//  if ( Vtkvmtkdifferentialgeometrytcl_Init(interp) == TCL_ERROR ) {
//    fprintf( stderr, "error on LsetCore_Init\n" );
//    return TCL_ERROR;
//  }
//
//  if ( Vtkvmtkiotcl_Init(interp) == TCL_ERROR ) {
//    fprintf( stderr, "error on LsetV_Init\n" );
//    return TCL_ERROR;
//  }
//
//  if ( Vtkvmtkmisctcl_Init(interp) == TCL_ERROR ) {
//    fprintf( stderr, "error on Geom_Init\n" );
//    return TCL_ERROR;
//  }
//
//  if ( Vtkvmtksegmentationtcl_Init(interp) == TCL_ERROR ) {
//    fprintf( stderr, "error on Image_Init\n" );
//    return TCL_ERROR;
//  }
//#endif

#ifdef USE_ITK
  if ( itkls2d_Init(interp) == TCL_ERROR ) {
      fprintf( stderr, "error on itkls2d_Init\n" );
      return TCL_ERROR;
    }
  if ( itkls3d_Init(interp) == TCL_ERROR ) {
        fprintf( stderr, "error on itkls3d_Init\n" );
        return TCL_ERROR;
      }
  if ( itkutils_Init(interp) == TCL_ERROR ) {
        fprintf( stderr, "error on itkls2d_Init\n" );
        return TCL_ERROR;
      }
#endif

#endif



  Tcl_Eval( interp, "if {[file exists [file join $env(HOME) .simvascular_rc]]} {          "
                    "  set tcl_rcFileName [file join $env(HOME) .simvascular_rc]           "
                    "} else {                                                         "
                    "  set tcl_rcFileName [file join $env(SIMVASCULAR_HOME) simvascular.rc] "
                    "}                                                                ");
  Tcl_SetVar( interp, "tcl_prompt1", "puts -nonewline \"simvascular> \"",
	      TCL_GLOBAL_ONLY );

  Tcl_Eval( interp, "puts \"\"" );

  return TCL_OK;
}

