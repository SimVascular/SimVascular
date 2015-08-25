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

/** @file cvTetGenAdapt.h
 *  @brief Class provides implementations of the TetGen adaption
 *
 *  This is derived from the SolidModel class and provides implementations
 *  of functions to be able to import a solid using vtkReaders, extract 
 *  the boundaries of the solid, and mesh the solid with TetGen.
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#ifndef __CVTETGENADAPT_H
#define __CVTETGENADAPT_H

#include "SimVascular.h"

#include "cvAdaptObject.h"
//#include "cvRepositoryData.h"

#ifdef USE_ZLIB
#include "simvascular_zlib.h"
#else
#include <stdlib.h>
#define gzopen fopen
#define gzprintf fprintf
#define gzFile FILE*
#define gzclose fclose
#endif


// Some elementary notes on abstract base classes (ABC's)
// ------------------------------------------------------
// ABC's provide a means for defining an *interface*.  Since (by
// definition) they contain pure  methods, objects of these
// classes can not be instantiated.  Clients of ABC's are interested
// in using the abstract interface, but can not work with the objects
// themselves.  Instead, clients instantiate concrete classes derived
// from the ABC.  And then, to use the abstraction, clients use
// *pointers* or *references* to the ABC.  See Meyers' Effective C++,
// Item 34.
//


class cvTetGenAdapt: public cvAdaptObject {

  typedef struct AdaptOptions {
    int poly_;
    int strategy_;
    int metric_option_;
    double ratio_;
    double hmax_;
    double hmin_;
    int instep_;
    int outstep_;
    int step_incr_;
    double sphere_[5];
  }  AdaptOptions;

public:
  cvTetGenAdapt();
  cvTetGenAdapt( const cvTetGenAdapt& Adapt);

  ~cvTetGenAdapt();

  cvAdaptObject *Copy() const;
  int Copy( const cvAdaptObject& src);
  int CreateInternalMeshObject(Tcl_Interp *interp,char *meshFileName,
		  char *solidFileName);

  //Setup Operations
  int LoadModel(char *fileName);
  int LoadMesh(char *fileName);
  int LoadSolutionFromFile(char *fileName);
  int LoadYbarFromFile(char *fileName);
  int LoadHessianFromFile(char *fileName);
  int ReadSolutionFromMesh();
  int ReadYbarFromMesh();

  //Setup Operations
  int SetAdaptOptions(char *flag,double value);
  int CheckOptions();
  int SetMetric(char *input,int option, int strategy);
  int SetupMesh();

  //Adapt Operations
  int RunAdaptor();
  int PrintStats();

  //Post Operations
  int GetAdaptedMesh();
  int TransferSolution();
  int TransferRegions();

  //Write Operations
  int WriteAdaptedModel(char *fileName);
  int WriteAdaptedModelFace(int faceid,char *fileName);
  int WriteAdaptedMesh(char *fileName);
  int WriteAdaptedSolution(char *fileName);

private:
  cvMeshObject *meshobject_;
  vtkUnstructuredGrid *inmesh_;
  vtkPolyData *insurface_mesh_;
  vtkUnstructuredGrid *outmesh_;
  vtkPolyData *outsurface_mesh_;

  double *sol_;
  double *ybar_;
  double *hessians_;
  double *errormetric_;

  AdaptOptions options;
};


#endif //__CVTETGEN_ADAPT_H


