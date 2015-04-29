/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical
 * Software Corporation, University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University, 
 * RPI, Charles Taylor, Ken Jansen, Nathan Wilson, Ken Wang.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:

 * Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer. 
 * Redistributions in binary form must reproduce the above copyright 
 * notice, this list of conditions and the following disclaimer in the 
 * documentation and/or other materials provided with the distribution. 
 * Neither the name of the Stanford University or Rensselaer Polytechnic
 * Institute nor the names of its contributors may be used to endorse or
 * promote products derived from this software without specific prior 
 * written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 *
 *=========================================================================*/

#ifndef _CVADAPTCORE_H
#define _CVADAPTCORE_H

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

using namespace std;

#ifndef WIN32
#include <unistd.h>
#endif
#ifdef WIN32
  #include <direct.h>
  #define chdir _chdir
#endif

#ifdef WIN32
void  bzero(void* ptr, size_t sz);
#define M_PI 3.14159265353846f
#endif

#ifndef WIN32
#include <strings.h>
#endif

#define ABS(x) ((x) < 0 ? -(x) : (x))
#define MAXN 32
#define R(i,j)  result[n*(i)+(j)]
#define ABS(x) ((x) < 0 ? -(x) : (x))
#define MAX(x,y) ((x)<(y) ? (y) : (x))

#include "MeshSim.h"
#include "MeshSimAdapt.h"
#include "SimMeshTools.h"
#include "SimParasolidKrnl.h"
#include "SimAdvMeshing.h"

#include "MeshSimInternal_phAdapt.h"
#include "cvAdaptHelp.h"
#include "cvSolverIO.h"

#include "SimVascular.h"
#include "cv_misc_utils.h"

struct simvascular_adaptor_greater_abs {
  bool operator () (const double &a, const double &b)
    {
      return fabs(a) > fabs(b);
    }
};


extern "C" {

  // for solving linear system (small)
  // the last array is the right hand side
  // it is being passed as a reference  and overridden
  // to contain the linear system's solution !
  #ifndef WIN32
    void ludcmp_(double*, int*, int*, int*,double*);
    #define ludcmp ludcmp_
    void lubksb_(double*, int*, int*, int*,double*);
    #define lubksb lubksb_
    void mytred_(int*,int*,double[3][3],double*,double*,double[3][3]);
    #define mytred mytred_  
    void tql2_(int*,int*,double*,double*,double[3][3],int*);
    #define tql2 tql2_    
  #else
     void LUDCMP(double*, int*, int*, int*,double*);
     void LUBKSB(double*, int*, int*, int*,double*);
     void MYTRED(int*,int*,double[3][3],double*,double*,double[3][3]);
     void TQL2(int*,int*,double*,double*,double[3][3],int*);
    #define ludcmp LUDCMP
    #define lubksb LUBKSB
    #define mytred MYTRED
    #define tql2 TQL2
  #endif
};


class cvAdaptCore {

  public:

  struct Hessian {
    double h[3];
    double dir[3][3];
  };
  typedef struct Hessian Hessian;  

  long eigen (double pos[3][3], double e[3][3], double v[3], 
              int checkOrthogonality);

  int checkUnitaryOthoganal (double e[3][3], int &factor);

  double trace (double pos[3][3]);
  double trace2 (double pos[3][3]);
  double det (double pos[3][3]);

  // solve x^2 + b x + c = 0
  // x[2] is always set to be zero
  long FindQuadraticRoots (const double b, const double c, double x[3]);

  // solve x^3 + a1 x^2 + a2 x + a3 = 0
  long FindCubicRoots (const double coeff[4], double x[3]);
  long NullSpace (const double *a, double *result, double eps, long n);

  double* InterpolateSolution (pRegion region, double xi[3], 
                               int ndof,pMeshDataId modes ); 

  void display_region ( pRegion region );
  int inverseMap ( pRegion region, double* qpt, double* pt );
  void ModifyHessiansAtBdry(pMesh mesh);

  // Modify mesh-metric to take special care
  // like in parallel plates (i.e, w=f(y)) 
  // if a user wants different hmax in x-direction
  // other examples can be situations where
  // user don't want to have one mesh edge 
  // connecting two geometric model edges (like no dofs)
  void ModifyMetric (pVertex vertex, double dir[3][3], double* h);
  void SmoothErrorIndicators (pMesh mesh, int option);

  // simple average over a patch surrounding the vertex    
  void  SmoothHessians (pMesh mesh);
  void V_AnalyticHessian (pVertex v, double H[3][3], int option);

  // hessian returned : 6-component (symmetric)
  // u_xx, u_xy, u_xz, u_yy, u_yz, u_zz
  // called in setSizeFieldUsingHessians (sizefield.cc)
  void V_Hessian (pVertex v, double T[3][3]);
  void V_getHessians (double *hessiansFromPhasta, pMesh mesh, 
		   int nvar, int option, double *hessians);

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
  void attachArray ( double *valueArray, pMesh mesh, 
		     pMeshDataId dataID,int nVar, int poly );

  // get data (previously attached) from mesh
  // `dataID' is the MeshDataId
  // `nVar' is the no. of variables at each dof
  // e.g., `nVar'=5 (for flow problems) or 27 (for hessians)
  // `poly' is the polynomial order (ONLY order 1 is supported as of now) 
  // this routine gets attached data array from mesh   
  // in restart-writable format 
  // memory is allocated within the function
  // user has to delete the memory
  void getAttachedArray ( double *&valueArray, pMesh mesh,
                         pMeshDataId dataID, int nVar, int poly);

  // cleans data attached to mesh entities
  // `dataID' is the MeshDataId
  // `en_type' is the entity type on which data is atached
  // e.g., 0 (for vertex), 3 (for regions), 4 (for all)
  // can use polynomial order to delete data on entities
  // i.e., for different modes, instead user should call routine
  // for different entities depending on poly. order/mode
  // with this attached data doesn't have to be solution
  void cleanAttachedData (pMesh mesh, pMeshDataId dataID,
		          int en_type, int array = 1);

  // build the linear system
  void buildSystem (pRegion region, double* eltMatrix);

  // arguments are :
  // mesh mod. type, look at file MeshSimAdapt.h
  // data containing mesh entities created, deleted or reshaped
  // userData used to set a pointer to be passed in callback function
  void phastaTransfer ( MeshModType mtype, pMeshChanges mco, void *userData);

  // reconstruct the element gradient
  void elementGradient (pRegion region, double* elementGradient);

  // reconstruct the element hessian : 6-component (symmetric)
  void elementHessian (pRegion region, double* elemHessian);

  // just take the value from any adjacent vertex
  void fix4SolutionTransfer (pMesh mesh);

  // recover gradient at a vertex using patch of elements around it
  void gradientsFromPatch (pMesh mesh);
      
  // recover hessian using a patch (from gradients)
  // hessian returned : 6-component (symmetric)
  // u_xx, u_xy, u_xz, u_yy, u_yz, u_zz
  void hessiansFromPatch (pMesh mesh);

  // hessian  returned : 6-component (symmetric)
  // u_xx, u_xy, u_xz, u_yy, u_yz, u_zz
  // the nodal data later can be retrieved via
  // nodalHessianID
  void hessiansFromSolution (pMesh mesh,int stepNumber);

  // this routine tags/marks the mesh entities for refinement (i.e., tag driven)
  // as of now only tags the edges (later, may introduce other choices)
  // tags entities for refinement which have error values greater than threshold
  // as of now do not use hmin and hmax
  // can introduce one more factor to compute threshold for coarsening
  int applyMarkingStrategy (pMesh mesh, pMSAdapt simAdapter,
		            double factor, double hmin, double hmax,
		            double &totalError, double &maxError, 
                            double &minError, double &threshold, int option); 

  double getErrorThreshold (pMesh mesh, double factor, double &totalError, 
		            double &maxError, double &minError, int option);

  // option is to decide how to compute the error value
  // (i.e., use 3 EI for flow problem or use 1 EI for scalar problem)
  double getErrorValue (double *nodalValues, int option);

  // to read parameters from a phasta file (filename)
  // parameters correspond to nshg & nvar, i.e., size of field-array
  // these parameters are used as reference values 
  // (sometimes needed before reading the field-array)
  void readParametersFromFile (char *filename, char *fieldName,
			       int &nshg, int &numVars);

  // to read array from a phasta file (filename)
  // memory is allocated HERE for 'valueArray'
  // `fieldName' tells which block to read like solution, error etc.
  void readArrayFromFile ( char *filename, char *fieldName,
			  double *&valueArray);

  // to write array to a phasta file (filename)
  // NOTE: array should be transposed!!!
  // `fieldName' tells in which block to write like solution, error etc.
  // `outputFormat' tells in which format to write, i.e., binary/ascii
  // `mode' : "write", "appeand" etc.
  void writeArrayToFile ( char *filename, char *fieldName,
		          char *outputFormat, char *mode,
		          int nshg, int numVars,
		          int stepNumber, double *valueArray); 

  void setSizeFieldUsingHessians ( pMesh mesh, pMSAdapt simAdapter,
			           double factor, double hmax,
			           double hmin, int option,
                                   double sphere[5]);

  // max relative interpolation error at a vertex
  double maxLocalError ( pVertex vertex, double H[3][3]);

  // relative interpolation error along an edge
  double E_error (pEdge edge, double H[3][3]);

  void setIsotropicSizeField (pMesh mesh, pMSAdapt simAdapter,
			      double factor,double hmax, 
			      double hmin, int option);

  void setManualSizeField (pMesh mesh, pMSAdapt simAdapter, 
			   int strategy);

  // tag the entities to be refinement (for isotropic refinement)
  // factor is used to evaluate the threshold for refinement
  // as of now do not use hmin and hmax 
  void tagEntitiesForRefinement(pMesh mesh, pMSAdapt simAdapter,
			        double factor, double hmax, 
			        double hmin, int option);


  void writeMEDITSolution (pMesh mesh);

  void writeMEDITSizeField (Hessian* hess, pMesh mesh);

  /////////////////////////////////
  // write out the restart files //
  // which contain the hessians  //
  /////////////////////////////////
  void writeRestartHessians (pMesh mesh );

  void writeSmoothEIs (pMesh mesh);

  void attachSolution (char* solfile, pMesh mesh, 
                       map<pEntity, double *>& data,
		       int ndof,  int P );

  void attachSoln ( char *solfile,  pMesh mesh, 
                   pMeshDataId phSol, int ndof, int P );

  int attachVPSoln ( char *solfile, pMesh mesh, 
                     pMeshDataId phSol, pMeshDataId modes, int ndof );

  void solution ( pEntity ent, pMeshDataId phSol, double** sol );

  void numberofmodes ( pEntity ent, pMeshDataId modes, int* sol );

  int restart ( char filename[], double* q, int nshg, int nvr );
 
  void check (pMesh mesh);

    pMeshDataId phasta_solution;
    pMeshDataId nodalgradientID;
    pMeshDataId nodalhessianID;
    pMeshDataId errorIndicatorID;

#ifdef DEBUG_MESHSIM_MSA_CALLBACK
    pMeshDataId pseudoNodeID_;
#endif

    std::ofstream adaptSimLog;

    // implementation of the callback fctn
    int CBcounter;
    int CBinvalidRegionCount;
    pMeshDataId modes;
    int numVars;
    char* iformat;

};

#endif


