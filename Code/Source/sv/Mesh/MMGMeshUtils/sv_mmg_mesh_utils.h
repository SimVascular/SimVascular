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

#ifndef __CVMMG_MESH_UTILS_H
#define __CVMMG_MESH_UTILS_H

#include "SimVascular.h"
#include "svMMGExports.h" // For exports

#include "sv_PolyData.h"
#include "vtkPolyData.h"
#include "vtkUnstructuredGrid.h"

#include "mmg/mmgs/libmmgs.h"

SV_EXPORT_MMG int MMGUtils_ConvertToMMG(MMG5_pMesh mesh, MMG5_pSol sol, vtkPolyData *polydatasolid,
    double hmin, double hmax, double hausd, double angle, double hgrad,
    int useSizingFunction, vtkDoubleArray *meshSizingFunction, int numAddedRefines);

SV_EXPORT_MMG int MMGUtils_ConvertToVTK(MMG5_pMesh mesh, MMG5_pSol sol, vtkPolyData *polydatasolid);

SV_EXPORT_MMG int MMGUtils_SurfaceRemeshing(vtkPolyData *surface, double hmin, double hmax, double hausd, double angle, double hgrad, int useSizingFunction, vtkDoubleArray *meshSizingFunction, int numAddedRefines);

SV_EXPORT_MMG int MMGUtils_PassCellArray(vtkPolyData *newgeom,
    vtkPolyData *originalgeom,std::string newName,std::string originalName);

SV_EXPORT_MMG int MMGUtils_PassPointArray(vtkPolyData *newgeom,
    vtkPolyData *originalgeom,std::string newName,std::string originalName);

SV_EXPORT_MMG int MMGUtils_BuildRidgeTable(vtkPolyData *polydatasolid, vtkEdgeTable *ridges, std::string ridgePtArrayName);
#endif // __Mmgmesh_Init

