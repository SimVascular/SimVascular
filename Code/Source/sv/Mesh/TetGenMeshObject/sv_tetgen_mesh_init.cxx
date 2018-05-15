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

/** @file sv_tetgen_mesh_init.cxx
 *  @brief Ipmlements functions to register TetGenMeshObject as a mesh type
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#include "SimVascular.h"
#include "sv_misc_utils.h"
#include "sv_tetgen_mesh_init.h"
#include "sv_TetGenMeshSystem.h"
#include "sv_tetgenmesh_utils.h"
#include "sv_arg.h"

#include <stdio.h>
#include <string.h>
#include "sv_Repository.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "sv2_globals.h"

// Prototypes:
// -----------

int TetGenMesh_AvailableCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

// ----------
// Tetgenmesh_Init
// ----------

int Tetgenmesh_Init( Tcl_Interp *interp )
{

#ifdef TETGEN151
  printf("  %-12s %s\n","TetGen:", "1.5.1");
#elif TETGEN150
  printf("  %-12s %s\n","TetGen:", "1.5.0");
#elif TETGEN143
  printf("  %-12s %s\n","TetGen:", "1.4.3");
#endif

  // Associate the mesh registrar with the Tcl interpreter so it can be
  // retrieved by the DLLs.

	MeshKernelRegistryMethodPtr pMeshKernelRegistryMethod =
    (MeshKernelRegistryMethodPtr) Tcl_GetAssocData( interp, "MeshSystemRegistrar", NULL);

  if (pMeshKernelRegistryMethod != NULL) {
    cvMeshSystem* tetGenSystem = new cvTetGenMeshSystem();
    if ((cvMeshSystem::RegisterKernel(cvMeshObject::KERNEL_TETGEN,tetGenSystem) == SV_OK)) {
      //printf("  TetGen module registered\n");
      Tcl_CreateCommand( interp, "tetgen_mesh_available", TetGenMesh_AvailableCmd,
		         (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
    }
  }
  else {
    return TCL_ERROR;
  }

  //Initialize Tetgenutils
  if (TGenUtils_Init() != SV_OK) {
    return TCL_ERROR;
  }

  return TCL_OK;
}


int TetGenMesh_AvailableCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  Tcl_SetResult( interp, "TetGen Mesh Module Available", TCL_VOLATILE );

  return TCL_OK;
}


