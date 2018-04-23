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

#include "sv4gui_RegisterMeshSimFunction.h"

#include <svMeshSimExports.h>

#include "sv4gui_MeshFactory.h"
#include "sv4gui_MeshSim.h"
#ifdef SV_USE_MESHSIM_ADAPTOR
  #include "sv4gui_MeshSimAdaptor.h"
#endif
#include "cv_meshsim_mesh_init.h"

sv4guiRegisterMeshSimFunction::sv4guiRegisterMeshSimFunction()
    {
        sv4guiMeshSim* tempmesh=new sv4guiMeshSim();
        std::string type=tempmesh->GetType();
        sv4guiMeshFactory::RegisterCreationFunction(type, &sv4guiMeshSim::CreateMesh);
        sv4guiMeshFactory::RegisterFileExtensions(type, tempmesh->GetFileExtensions());
#ifdef SV_USE_MESHSIM_ADAPTOR
        sv4guiMeshFactory::RegisterAdaptorFunction(type, &sv4guiMeshSimAdaptor::CreateAdaptor);
#endif
        delete tempmesh;

        Meshsimmesh_Init(NULL);
    }

sv4guiRegisterMeshSimFunction::~sv4guiRegisterMeshSimFunction(){}

static sv4guiRegisterMeshSimFunction registerMeshSimFunction = sv4guiRegisterMeshSimFunction();
