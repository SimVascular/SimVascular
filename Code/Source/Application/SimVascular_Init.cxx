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

// needed by cmake build to not throw errors
// when compiling on Windows
#ifdef WIN32
#include "windows.h"
#endif

#include "sv_IOstream.h"

#include <time.h>
#include <stdlib.h>

#include "tcl.h"
#include "tk.h"

#include "vtkToolkits.h"
#include "sv_VTK.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

#include "SimVascular_Init.h"

#include "vtkToolkits.h"

#include "sv_repos_init.h"
#include "sv2_Lset_init.h"
#include "sv_geom_init.h"
#include "sv2_image_init.h"
#include "sv_math_init.h"
#include "sv2_post_init.h"
#include "sv_polydatasolid_init.h"

#include "sv_solid_init.h"

#ifdef SV_USE_VMTK
  #include "sv_vmtk_utils_init.h"
#endif

#ifdef SV_USE_OpenCASCADE
  #include "sv_occt_init.h"
#endif

#ifdef SV_USE_TETGEN
  #include "sv_mesh_init.h"
  #include "sv_tetgen_mesh_init.h"
#endif

#ifdef SV_USE_MMG
  #include "sv_mesh_init.h"
  #include "sv_mmg_mesh_init.h"
#endif

#ifdef SV_USE_ITK
  #include "sv3_ITKLset_init.h"
#endif

#ifdef SV_USE_TETGEN_ADAPTOR
  #include "sv_adapt_init.h"
  #include "sv_tetgen_adapt_init.h"
#endif

#ifdef SV_USE_PYTHON
//extern "C" int Tclpython_Init(Tcl_Interp *interp);
#endif

// ---------------
// SimVascularWelcome
// ---------------

void SimVascularWelcome( Tcl_Interp *interp )
{
  // Find the date of the executable we're running
  Tcl_Eval( interp, "puts \"\"");
  Tcl_Eval( interp, "set simvascular_home \"$env(SV_HOME)\"");
  Tcl_Eval( interp,
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
  Tcl_Eval( interp, "puts \"  Copyright (c) Stanford University, The Regents of the University of\"" );
  Tcl_Eval( interp, "puts \"                California, and others.  All Rights Reserved.\"" );
  Tcl_Eval( interp, "puts \"\"");
  Tcl_Eval( interp, "flush stdout" );

  Tcl_Eval( interp, "puts [format \"  %-12s %s\" \"Tcl:\" [info patchlevel]]" );

  Tcl_Eval( interp, "if {$argc == 0 && [info exists tk_version]} {"
	              "puts \"Use default gui script...\";"
	              "source $simvascular_home/Tcl/SimVascular_2.0/simvascular_startup.tcl;"
	            "} else {"
	            "}");
    
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

#ifdef SV_USE_VMTK
  if ( Vmtkutils_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Vmtkutils_Init\n" );
    return TCL_ERROR;
  }
#endif

#ifdef SV_USE_OpenCASCADE
//#ifndef SV_USE_OpenCASCADE_SHARED
  if ( Occtsolid_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Opencascade_Init\n" );
    return TCL_ERROR;
  }
//#endif
#endif

#ifdef SV_USE_TETGEN
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

#ifdef SV_USE_TETGEN_ADAPTOR
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

#ifdef SV_USE_ITK
  if ( Itklset_Init(interp) == TCL_ERROR ) {
      fprintf( stderr, "error on itkls2d_Init\n" );
      return TCL_ERROR;
  }
#endif

#ifdef SV_USE_PYTHON
//  if ( Tclpython_Init(interp) == TCL_ERROR ) {
//      fprintf( stderr, "error on Tclpython_Init\n" );
//      return TCL_ERROR;
//  }
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

