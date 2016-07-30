/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
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
 *
 *=========================================================================*/

/** @file cv_mmg_mesh_init.cxx
 *  @brief Ipmlements functions to register TetGenMeshObject as a mesh type
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#include "SimVascular.h"
#include "cv_misc_utils.h"
#include "cv_mmg_mesh_init.h"
#include "cv_arg.h"

#include <stdio.h>
#include <string.h>
#include "cvRepository.h"
#include "cvPolyData.h"
#include "cv_arg.h"
#include "cv_misc_utils.h"

#include "cv_mmg_mesh_utils.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "cv_globals.h"

// Prototypes:
// -----------
int MMG_RemeshCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] );

// ----------
// Mmgmesh_Init
// ----------

int Mmgmesh_Init( Tcl_Interp *interp )
{

  Tcl_CreateCommand( interp, "mmg_remesh", MMG_RemeshCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  return TCL_OK;
}

// MMG_RemeshCmd
// --------------

int MMG_RemeshCmd( ClientData clientData, Tcl_Interp *interp,
		    int argc, CONST84 char *argv[] )
{
  char *usage;
  char *srcName;
  char *dstName;
  double hmax = 0.1;
  double hmin = 0.1;
  double angle = 45.0;
  double hgrad = 1.1;
  double hausd = 0.01;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  int table_size = 7;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-hmin", DOUBLE_Type, &hmin, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-hmax", DOUBLE_Type, &hmax, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-angle", DOUBLE_Type, &angle, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-hgrad", DOUBLE_Type, &hgrad, NULL, SV_OPTIONAL, 0, { 0 } },
    { "-hausd", DOUBLE_Type, &hausd, NULL, SV_OPTIONAL, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, srcName, " not of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  vtkPolyData *surfacepd;
  surfacepd = ((cvPolyData*)src)->GetVtkPolyData();
  surfacepd->BuildLinks();
  int useSizingFunction = 0;
  int numAddedRefines = 0;
  vtkDoubleArray *meshSizingFunction = NULL;
  if ( MMGUtils_SurfaceRemeshing( surfacepd, hmin, hmax, hausd, angle, hgrad,
	useSizingFunction, meshSizingFunction, numAddedRefines) != CV_OK ) {
    Tcl_SetResult( interp, "remeshing error", TCL_STATIC );
    return TCL_ERROR;
  }

  dst = new cvPolyData(surfacepd);
  if ( dst == NULL ) {
    Tcl_AppendResult( interp, "error remeshing obj ", dstName,
		      " in repository", (char *)NULL );
    return TCL_ERROR;
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );

  return TCL_OK;
}

