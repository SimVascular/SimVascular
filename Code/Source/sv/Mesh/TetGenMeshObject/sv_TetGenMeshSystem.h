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

/** @file sv_TetGenMeshSystem.h
 *  @brief Class derived from cvMeshSystem that is used to set up TetGen
 *  as a mesh system. Used to return a mesh system of the type tetgen.
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#ifndef __CVTETGENMESHSYSTEM_H
#define __CVTETGENMESHSYSTEM_H

#include "SimVascular.h"
#include "svTetGenMeshExports.h" // For exports
#include "sv_MeshSystem.h"
#include "sv_TetGenMeshObject.h"
#include "sv_FactoryRegistrar.h"

class SV_EXPORT_TETGEN_MESH cvTetGenMeshSystem : public cvMeshSystem {

public:
  cvTetGenMeshSystem();
  virtual ~cvTetGenMeshSystem();

  // Methods that concrete implementations must provide for meshing system abstraction.

  int LogOn( char *const filename );
  int LogOff();

protected:
  #ifdef SV_USE_TCL
  cvMeshObject* CreateMeshObject( Tcl_Interp *interp );
  #endif
  #ifdef SV_USE_PYTHON
  cvMeshObject* CreateMeshObject();
  #endif

private:

};


#endif // __CVTETGENMESHSYSTEM_H
