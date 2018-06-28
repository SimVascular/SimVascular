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

/** @file sv_tetgenmesh_utils.h
 *  @brief These functions are utilities that are called mostly by
 *  cvTetGenMeshObject
 *  @details These functions are called mostly by cvTetGenMeshObject and
 *  they provide a clean way for implementation of functions with that
 *  class. They provide the full code for conversion between tetgen
 *  and vtkPolyData
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#ifndef __CV_TETGENMESH_UTILS_H
#define __CV_TETGENMESH_UTILS_H

#include "SimVascular.h"
#include "svTetGenMeshExports.h" // For exports

#include "vtkPolyData.h"
#include "vtkUnstructuredGrid.h"

#include "simvascular_tetgen.h"

SV_EXPORT_TETGEN_MESH int TGenUtils_Init();
//int cvTetGenMeshObjectUtils_Logon(char *filename);
//int cvTetGenMeshObjectUtils_Logoff();
//
SV_EXPORT_TETGEN_MESH int TGenUtils_ConvertSurfaceToTetGen(tetgenio *inmesh,vtkPolyData *polydatasolid);

SV_EXPORT_TETGEN_MESH int TGenUtils_AddPointSizingFunction(tetgenio *inmesh,vtkPolyData *polydatasolid, std::string meshSizingFunctionName, double maxEdgeSize);

SV_EXPORT_TETGEN_MESH int TGenUtils_AddFacetMarkers(tetgenio *inmesh,vtkPolyData *polydatasolid, std::string markerListArrayName);

SV_EXPORT_TETGEN_MESH int TGenUtils_AddHoles(tetgenio *inmesh, vtkPoints *holeList);

SV_EXPORT_TETGEN_MESH int TGenUtils_AddRegions(tetgenio *inmesh, vtkPoints *regionList, vtkDoubleArray *regionSizeList);

SV_EXPORT_TETGEN_MESH int TGenUtils_ConvertVolumeToTetGen(vtkUnstructuredGrid *mesh,
    vtkPolyData *surfaceMesh,tetgenio *inmesh);

SV_EXPORT_TETGEN_MESH int TGenUtils_ConvertToVTK(tetgenio *outmesh,vtkUnstructuredGrid *volumemesh,
    vtkPolyData *surfacemesh,int *totRegions,int getBoundary);

SV_EXPORT_TETGEN_MESH int TGenUtils_GetFacePolyData(int id,vtkPolyData *mesh, vtkPolyData *face);

SV_EXPORT_TETGEN_MESH int TGenUtils_WriteVTU(char *filename,vtkUnstructuredGrid *UGrid);

SV_EXPORT_TETGEN_MESH int TGenUtils_WriteVTP(char *filename,vtkPolyData *PData);

SV_EXPORT_TETGEN_MESH int TGenUtils_writeDiffAdj(vtkUnstructuredGrid *volumemesh);

SV_EXPORT_TETGEN_MESH int TGenUtils_SetRefinementCylinder(vtkPolyData *polydatasolid,
    std::string sizingFunctionArrayName,double size,double radius,
    double* center,double length, double *normal, int secondarray,
    double maxedgesize, std::string refineIDArrayName,
    int refinecount);

SV_EXPORT_TETGEN_MESH int TGenUtils_SetRefinementSphere(vtkPolyData *polydatasolid,
    std::string sizingFunctionArrayName,double size,double radius,
    double* center,int secondarray, double maxedgesize, std::string refineIDArrayName,
    int refinecount);

SV_EXPORT_TETGEN_MESH int TGenUtils_SetSizeFunctionArray(vtkPolyData *polydatasolid,
    std::string sizingFunctionArrayName,double size,char *functionname, int secondarray);

SV_EXPORT_TETGEN_MESH int TGenUtils_LoadMesh(char *filename,vtkUnstructuredGrid *result);

SV_EXPORT_TETGEN_MESH int TGenUtils_ResetOriginalRegions(vtkPolyData *newgeom,
    vtkPolyData *originalgeom,
    std::string regionName);

SV_EXPORT_TETGEN_MESH int TGenUtils_ResetOriginalRegions(vtkPolyData *newgeom,
    vtkPolyData *originalgeom,
    std::string regionName,
    vtkIdList *excluedList);

SV_EXPORT_TETGEN_MESH int TGenUtils_ResetOriginalRegions(vtkPolyData *newgeom,
    vtkPolyData *originalgeom,
    std::string regionName,
    vtkIdList *onlyList,
    int dummy);

SV_EXPORT_TETGEN_MESH int TGenUtils_CheckSurfaceMesh(vtkPolyData *pd, int meshInfo[3]);

SV_EXPORT_TETGEN_MESH int TGenUtils_SetLocalMeshSize(vtkPolyData *pd,int regionId,double size);

#endif //__CV_TETGENMESH_UTILS_H
