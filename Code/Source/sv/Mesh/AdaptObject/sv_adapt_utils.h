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

/** @file sv_adapt_utils.h
 *  @brief These functions are utilities that are called mostly by
 *  cvTetGenAdapt
 *  @details These functions are called mostly by cvTetGenAdapt and
 *  they provide a clean way for implementation of functions with that
 *  class. They provide the full code for conversion between tetgen
 *  and vtkPolyData
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#ifndef __CV_ADAPT_UTILS_H
#define __CV_ADAPT_UTILS_H

#include "SimVascular.h"
#include "svAdaptorExports.h" // For exports
#include "sv_misc_utils.h"

#include "vtkUnstructuredGrid.h"
#include "vtkPolyData.h"

#include "simvascular_tetgen.h"

// standard includes
#include <stdlib.h>
#include <stdio.h>
#include <algorithm>
#include <math.h>
#include <iostream>
#include <string>
#include <fstream>
#include <time.h>
#include <vector>
#include <map>

#ifndef WIN32
#include <strings.h>
#include <unistd.h>
#endif
#ifdef WIN32
  #include <direct.h>
  #define chdir _chdir
  #define M_PI 3.14159265353846f
  SV_EXPORT_ADAPTOR void  bzero(void* ptr, size_t sz);
#endif

#define ABS(x) ((x) < 0 ? -(x) : (x))
#define MAX(x,y) ((x)<(y) ? (y) : (x))

struct Hessian {
    double h[3];
    double dir[3][3];
  };
  typedef struct Hessian Hessian;

SV_EXPORT_ADAPTOR bool AdaptUtils_file_exists (const std::string& name);

// simple average over a patch surrounding the vertex
SV_EXPORT_ADAPTOR int AdaptUtils_SmoothHessians (vtkUnstructuredGrid *mesh);

// hessian returned : 6-component (symmetric)
// u_xx, u_xy, u_xz, u_yy, u_yz, u_zz
// called in setSizeFieldUsingHessians (sizefield.cc)
SV_EXPORT_ADAPTOR int AdaptUtils_getHessiansFromPhasta(double *hessiansFromPhasta,
    vtkUnstructuredGrid *mesh,int nvar, double *hessians);

SV_EXPORT_ADAPTOR int AdaptUtils_getHessian (vtkDoubleArray *Hessians,vtkIdType v, double T[3][3]);

SV_EXPORT_ADAPTOR int AdaptUtils_averageSolutionsOnMesh(vtkUnstructuredGrid *mesh, int begin,
    int end, int incr);

// attaches array to mesh entities
// `dataID' is the MeshDataId
// `nVar' is the no. of variables at each dof
// e.g., `nVar'=5 (for flow problems) or 27 (for hessians)
// `poly' is the polynomial order
// this routine attaches  "valueArray"
// the incoming "valueArray" which contains data
// for ALL vertices (general dofs) is split into
// local entity-level arrays to handle the memory
// during local mesh modifications
SV_EXPORT_ADAPTOR int AdaptUtils_attachArray ( double *valueArray, vtkUnstructuredGrid *mesh,
      	     std::string dataName,int nVar, int poly );

// get data (previously attached) from mesh
// `dataID' is the MeshDataId
// `nVar' is the no. of variables at each dof
// e.g., `nVar'=5 (for flow problems) or 27 (for hessians)
// `poly' is the polynomial order (ONLY order 1 is supported as of now)
// this routine gets attached data array from mesh
// in restart-writable format
// memory is allocated within the function
// user has to delete the memory
SV_EXPORT_ADAPTOR int AdaptUtils_getAttachedArray ( double *&valueArray, vtkUnstructuredGrid *mesh,
                       std::string dataName, int nVar, int poly, bool for_restart=false);

// just take the value from any adjacent vertex
SV_EXPORT_ADAPTOR int AdaptUtils_fix4SolutionTransfer (vtkUnstructuredGrid *inmesh,vtkUnstructuredGrid *outmesh,int outstep);

SV_EXPORT_ADAPTOR int AdaptUtils_modelFaceIDTransfer(vtkPolyData *inpd,vtkPolyData *outpd);

// temporary function to split speed from or avg_sols
SV_EXPORT_ADAPTOR int AdaptUtils_splitSpeedFromAvgSols( vtkUnstructuredGrid *mesh);

// recover gradients from a VTKFilter
SV_EXPORT_ADAPTOR int AdaptUtils_gradientsFromFilter (vtkUnstructuredGrid *mesh);

// recover hessians from a VTKFilter
SV_EXPORT_ADAPTOR int AdaptUtils_hessiansFromFilter (vtkUnstructuredGrid *mesh);
// hessian  returned : 6-component (symmetric)
// u_xx, u_xy, u_xz, u_yy, u_yz, u_zz
// the nodal data later can be retrieved via
// nodalHessianID
SV_EXPORT_ADAPTOR int AdaptUtils_hessiansFromSolution (vtkUnstructuredGrid *mesh);

// option is to decide how to compute the error value
// (i.e., use 3 EI for flow problem or use 1 EI for scalar problem)
SV_EXPORT_ADAPTOR double AdaptUtils_getErrorValue (double *nodalValues, int option);

SV_EXPORT_ADAPTOR int AdaptUtils_setSizeFieldUsingHessians ( vtkUnstructuredGrid *mesh,
      		           double factor, double hmax,
      		           double hmin, double sphere[5],int strategy);

// max relative interpolation error at a vertex
SV_EXPORT_ADAPTOR double AdaptUtils_maxLocalError (vtkUnstructuredGrid *mesh,vtkIdType vertex, double H[3][3]);

// relative interpolation error along an edge
SV_EXPORT_ADAPTOR double AdaptUtils_E_error (double xyz[2][3], double H[3][3]);

SV_EXPORT_ADAPTOR int AdaptUtils_convertToTetGen(vtkUnstructuredGrid *mesh,vtkPolyData *surfaceMesh,tetgenio *inmesh);

SV_EXPORT_ADAPTOR int AdaptUtils_getSurfaceBooleans(vtkUnstructuredGrid *mesh,bool *pointOnSurface);

SV_EXPORT_ADAPTOR int AdaptUtils_runAdaptor(tetgenio *inmesh,tetgenio *outmesh);

SV_EXPORT_ADAPTOR int AdaptUtils_convertToVTK(vtkUnstructuredGrid *mesh,vtkPolyData *surfaceMesh,tetgenio *outmesh);

// to read parameters from a phasta file (filename)
// parameters correspond to nshg & nvar, i.e., size of field-array
// these parameters are used as reference values
// (sometimes needed before reading the field-array)
SV_EXPORT_ADAPTOR int AdaptUtils_readParametersFromFile (char *filename, char *fieldName,
		       int &nshg, int &numVars);

// to read array from a phasta file (filename)
// memory is allocated HERE for 'valueArray'
// `fieldName' tells which block to read like solution, error etc.
SV_EXPORT_ADAPTOR int AdaptUtils_readArrayFromFile ( char *filename, char *fieldName,
		  double *&valueArray);

// to write array to a phasta file (filename)
// NOTE: array should be transposed!!!
// `fieldName' tells in which block to write like solution, error etc.
// `outputFormat' tells in which format to write, i.e., binary/ascii
// `mode' : "write", "appeand" etc.
SV_EXPORT_ADAPTOR void AdaptUtils_writeArrayToFile ( char *filename, char *fieldName,
		  char *outputFormat, char *mode,
		  int nshg, int numVars,
		  int stepNumber, double *valueArray);

SV_EXPORT_ADAPTOR int AdaptUtils_checkArrayExists(vtkUnstructuredGrid *object,int datatype,std::string arrayname);
#endif //__CV_ADAPT_UTILS_H
