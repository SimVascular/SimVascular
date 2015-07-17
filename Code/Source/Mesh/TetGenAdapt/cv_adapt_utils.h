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

/** @file cv_adapt_utils.h
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

//#include "cvSolverIO.h"

#include "SimVascular.h"
#include "cv_misc_utils.h"

#include "vtkUnstructuredGrid.h"
#include "vtkPolyData.h"

#include "AdaptHelpers.h"
#include "simvascular_tetgen.h"

struct Hessian {
    double h[3];
    double dir[3][3];
  };
  typedef struct Hessian Hessian;  

//extern "C" {
//
//  // for solving linear system (small)
//  // the last array is the right hand side
//  // it is being passed as a reference  and overridden
//  // to contain the linear system's solution !
//  #ifndef WIN32
//    void mytred_(int*,int*,double[3][3],double*,double*,double[3][3]);
//    #define mytred mytred_  
//    void tql2_(int*,int*,double*,double*,double[3][3],int*);
//    #define tql2 tql2_    
//  #else
//     void MYTRED(int*,int*,double[3][3],double*,double*,double[3][3]);
//     void TQL2(int*,int*,double*,double*,double[3][3],int*);
//    #define mytred MYTRED
//    #define tql2 TQL2
//  #endif
//};

// simple average over a patch surrounding the vertex    
int AdaptUtils_SmoothHessians (vtkUnstructuredGrid *mesh);

// hessian returned : 6-component (symmetric)
// u_xx, u_xy, u_xz, u_yy, u_yz, u_zz
// called in setSizeFieldUsingHessians (sizefield.cc)
int AdaptUtils_getHessiansFromPhasta(double *hessiansFromPhasta, 
    vtkUnstructuredGrid *mesh,int nvar, double *hessians);

int AdaptUtils_getHessian (vtkDoubleArray *Hessians,vtkIdType v, double T[3][3]);

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
int AdaptUtils_attachArray ( double *valueArray, vtkUnstructuredGrid *mesh, 
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
int AdaptUtils_getAttachedArray ( double *&valueArray, vtkUnstructuredGrid *mesh,
                       std::string dataName, int nVar, int poly);

// just take the value from any adjacent vertex
int AdaptUtils_fix4SolutionTransfer (vtkUnstructuredGrid *inmesh,vtkUnstructuredGrid *outmesh,int nVar);

int AdaptUtils_modelFaceIDTransfer(vtkPolyData *inpd,vtkPolyData *outpd);

// recover gradients from a VTKFilter
int AdaptUtils_gradientsFromFilter (vtkUnstructuredGrid *mesh);

// recover hessians from a VTKFilter
int AdaptUtils_hessiansFromFilter (vtkUnstructuredGrid *mesh);
// hessian  returned : 6-component (symmetric)
// u_xx, u_xy, u_xz, u_yy, u_yz, u_zz
// the nodal data later can be retrieved via
// nodalHessianID
int AdaptUtils_hessiansFromSolution (vtkUnstructuredGrid *mesh);

// option is to decide how to compute the error value
// (i.e., use 3 EI for flow problem or use 1 EI for scalar problem)
double AdaptUtils_getErrorValue (double *nodalValues, int option);


int AdaptUtils_setSizeFieldUsingHessians ( vtkUnstructuredGrid *mesh, tetgenio *inmesh,
      		           double factor, double hmax,
      		           double hmin);

// max relative interpolation error at a vertex
double AdaptUtils_maxLocalError (vtkUnstructuredGrid *mesh,vtkIdType vertex, double H[3][3]);

// relative interpolation error along an edge
double AdaptUtils_E_error (double xyz[2][3], double H[3][3]);

int AdaptUtils_convertToTetGen(vtkUnstructuredGrid *mesh,vtkPolyData *surfaceMesh,tetgenio *inmesh);

int AdaptUtils_getSurfaceBooleans(vtkUnstructuredGrid *mesh,bool *pointOnSurface,int *pointMapper);

int AdaptUtils_runAdaptor(tetgenio *inmesh,tetgenio *outmesh);

int AdaptUtils_convertToVTK(vtkUnstructuredGrid *mesh,vtkPolyData *surfaceMesh,tetgenio *outmesh);

#endif //__CV_ADAPT_UTILS_H
