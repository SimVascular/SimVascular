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

#include "tcl.h"
#include "tk.h"

#include "vtkToolkits.h"
#include "cvVTK.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

#include "SimVascular_Init.h"

#include "vtkToolkits.h"

#include "cv_repos_init.h"
#include "cv_Lset_init.h"
#include "cv_geom_init.h"
#include "cv_image_init.h"
#include "cv_math_init.h"
#include "cv_post_init.h"
#include "cv_get_tcl_interp_init.h"
#include "cv_polydatasolid_init.h"

#include "cv_solid_init.h"

#ifdef SV_USE_MESHSIM
  #include "cv_mesh_init.h"
  #include "cv_meshsim_mesh_init.h"
#endif

#ifdef SV_USE_OpenCASCADE
  #include "cv_occt_init.h"
#endif

#ifdef SV_USE_MESHSIM_ADAPTOR
  #include "cv_adapt_init.h"
  #include "cv_meshsim_adapt_init.h"
#endif

#ifdef SV_USE_TETGEN
  #include "cv_mesh_init.h"
  #include "cv_tetgen_mesh_init.h"
#endif

#ifdef SV_USE_MMG
  #include "cv_mesh_init.h"
  #include "cv_mmg_mesh_init.h"
#endif

#ifdef SV_USE_MESHSIM_DISCRETE_MODEL
  #include "cv_discrete_init.h"
#endif

#ifdef SV_USE_PARASOLID
  #include "cv_parasolid_solid_init.h"
#endif

#ifdef SV_USE_ITK
  #include "cv_ITKLset_init.h"
#endif

#ifdef SV_USE_TETGEN_ADAPTOR
  #include "cv_adapt_init.h"
  #include "cv_tetgen_adapt_init.h"
#endif

#ifdef SV_USE_PYTHON
extern "C" int Tclpython_Init(Tcl_Interp *interp);
#endif

// ---------------
// SimVascularWelcome
// ---------------

void SimVascularWelcome( Tcl_Interp *interp )
{
  // Find the date of the executable we're running
  Tcl_Eval( interp, "puts \"\"");
  Tcl_Eval( interp, "set simvascular_home \"$env(SV_HOME)\";"
                  "if { [file exists [file join $simvascular_home/Tcl/startup_configure.tcl]]} {"
                      "source [file join $simvascular_home/Tcl/startup_configure.tcl];"
                      "if { [file exists [file join $simvascular_home/release-date]] } {"
                          "set SV_BUILD_STR \".$SV_PATCH_VERSION\";"
                          "if {$SV_VERSION !=\"simvascular\"} {"
                            "set SV_BUILD_STR \".$SV_PATCH_VERSION $SV_VERSION\";"
                          "}"
                      "} else {"
                          "set SV_BUILD_STR \" (dev build)\";"
                      "}"
                  "} else {"
                  "set SV_FULL_VER_NO \"unknown version\";"
                  "set SV_BUILD_STR \"unknown release\";"
                  "}");
  Tcl_Eval( interp, "puts [format \"  %-12s %s\" \"SimVascular:\" $SV_FULL_VER_NO$SV_BUILD_STR]" );
  Tcl_Eval( interp, "puts \"  Copyright (c) 2014-2015 The Regents of the University of California.\"" );
  Tcl_Eval( interp, "puts \"                         All Rights Reserved.\"");
  Tcl_Eval( interp, "puts \"\"");
  Tcl_Eval( interp, "flush" );


  Tcl_Eval( interp, "puts [format \"  %-12s %s\" \"Tcl:\" [info patchlevel]]" );

  return;
}

#ifndef SV_USE_VTK_SHARED
# include "vtktcl_static_prototypes.h"
#endif

int SimVascular_Init( Tcl_Interp *interp )
{
  SimVascularWelcome(interp);

  // include VTK

  #ifndef SV_USE_VTK_SHARED
  # include "vtktcl_static_packages.h"
  #endif

  // Print version info:
  vtkVersion *vobj = vtkVersion::New();
  printf("  %-12s %s\n", "Vtk:", vobj->GetVTKVersion());
  vobj->Delete();

#ifdef SV_STATIC_BUILD
  if ( Getinterp_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Getinterp_Init\n" );
    return TCL_ERROR;
  }

  if ( Repos_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Repos_Init\n" );
    return TCL_ERROR;
  }

  if ( Lset_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Lsetcore_Init\n" );
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

  if ( Post_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Post_Init\n" );
    return TCL_ERROR;
  }

  if ( Solid_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Solid_Init\n" );
    return TCL_ERROR;
  }

  if ( Polydatasolid_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on PolyDataSolid_Init\n" );
    return TCL_ERROR;
  }

#ifdef SV_USE_OpenCASCADE
#ifndef SV_USE_OpenCASCADE_SHARED
  if ( Occtsolid_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Opencascade_Init\n" );
    return TCL_ERROR;
  }
#endif
#endif

#ifdef SV_USE_MESHSIM
  if ( Mesh_Init(interp) == TCL_ERROR ) {
      fprintf( stderr, "error on Mesh_Init\n" );
      return TCL_ERROR;
  }

#elif defined SV_USE_TETGEN
  if ( Mesh_Init(interp) == TCL_ERROR ) {
      fprintf( stderr, "error on Mesh_Init\n" );
      return TCL_ERROR;
  }
#endif

#ifdef SV_USE_MMG
  if ( Mmgmesh_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Mmgmesh_Init\n" );
    return TCL_ERROR;
  }
#endif

#ifdef SV_USE_TETGEN
  if ( Tetgenmesh_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Tetgenmesh_Init\n" );
    return TCL_ERROR;
  }
#endif

#ifdef SV_USE_MESHSIM_ADAPTOR
  if ( Adapt_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Adapt_Init\n" );
    return TCL_ERROR;
  }
#elif defined SV_USE_TETGEN_ADAPTOR
  if ( Adapt_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Adapt_Init\n" );
    return TCL_ERROR;
  }
#endif

#ifdef SV_USE_TETGEN_ADAPTOR
  if ( Tetgenadapt_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on TetGenAdapt_Init\n" );
    return TCL_ERROR;
  }
#endif

#ifdef SV_USE_MESHSIM
#ifndef SV_USE_MESHSIM_SHARED
  if ( Meshsimmesh_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Mesh_Init\n" );
    return TCL_ERROR;
  }
#endif
#endif

#ifdef SV_USE_MESHSIM_ADAPTOR
#ifndef SV_USE_MESHSIM_SHARED
  if ( Meshsimadapt_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on MeshSimAdapt_Init\n" );
    return TCL_ERROR;
  }
#endif
#endif

#ifdef SV_USE_MESHSIM_DISCRETE_MODEL
#ifndef SV_USE_MESHSIM_DISCRETE_MODEL_SHARED
  if ( Meshsimdiscretesolid_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Mesh_Init\n" );
    return TCL_ERROR;
  }
#endif
#endif

#ifdef SV_USE_PARASOLID
#ifndef SV_USE_PARASOLID_SHARED
  if ( Parasolidsolid_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Parasolidsolid_Init\n" );
    return TCL_ERROR;
  }
#endif
#endif

#ifdef SV_USE_ITK
  if ( Itklset_Init(interp) == TCL_ERROR ) {
      fprintf( stderr, "error on itkls2d_Init\n" );
      return TCL_ERROR;
  }
#endif

#ifdef SV_USE_PYTHON
  if ( Tclpython_Init(interp) == TCL_ERROR ) {
      fprintf( stderr, "error on Tclpython_Init\n" );
      return TCL_ERROR;
  }
#endif

#endif

  Tcl_Eval( interp, "if {[file exists [file join $env(HOME) .simvascular_rc]]} {          "
                    "  set tcl_rcFileName [file join $env(HOME) .simvascular_rc]           "
                    "} else {                                                         "
                    "  set tcl_rcFileName [file join $env(SV_HOME) simvascular.rc] "
                    "}                                                                ");
  Tcl_SetVar( interp, "tcl_prompt1", "puts -nonewline \"simvascular> \"",
	      TCL_GLOBAL_ONLY );

  Tcl_Eval( interp, "puts \"\"" );

  return TCL_OK;
}

