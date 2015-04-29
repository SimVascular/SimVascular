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

/** @file cv_polydatasolid_utils.h
 *  @brief These functions are utilities that are called mostly by 
 *  cvPolyDataSolid
 *  @details These functions are called mostly by cvPolyDataSolid and 
 *  they provide a clean way for implementation of functions with that 
 *  class. They provide the full code for extracting boundaries, extracting
 *  faces, etc. 
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#ifndef __CV_POLYDATASOLID_UTILS_H
#define __CV_POLYDATASOLID_UTILS_H


#include "cvPolyData.h"
#include "cvLispList.h"
#include "cv_vtk_utils.h"

/* ------ */
/* Kernel */
/* ------ */

int PlyDtaUtils_Init();

/* -------- */
/* Get Info */
/* -------- */
int PlyDtaUtils_GetFaceIds( vtkPolyData *geom, int *v_num_faces, int **v_faces);

int PlyDtaUtils_GetBoundaryFaces( vtkPolyData *geom,double angle,int *numRegions);

int PlyDtaUtils_GetFacePolyData(vtkPolyData *geom, int *faceid, vtkPolyData *facepd);

/* -------- */
/* File I/O */
/* -------- */
int PlyDtaUtils_ReadNative( char *filename, vtkPolyData *result);

int PlyDtaUtils_WriteNative( vtkPolyData *geom, int file_version, char *filename );

/* -------- */
/* PolyData Change and Check Operations */
/* -------- */
int PlyDtaUtils_CombineFaces( vtkPolyData *geom, int *targetface, int *loseface);

int PlyDtaUtils_DeleteCells( vtkPolyData *geom, int *numcells, int *cells);

int PlyDtaUtils_DeleteRegion( vtkPolyData *geom, int *regionid);

int PlyDtaUtils_PDCheckArrayName( vtkPolyData *object, int datatype,std::string arrayname);
int PlyDtaUtils_UGCheckArrayName( vtkUnstructuredGrid *object, int datatype,std::string arrayname);
#endif // __PARASOLID_MODEL_H
