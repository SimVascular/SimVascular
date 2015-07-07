/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
 *
 * Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 *=========================================================================*/

#ifndef __CVADAPTOBJECT_H
#define __CVADAPTOBJECT_H

#include "SimVascular.h"

//#include "sys/param.h"
#define MAXPATHLEN 1024

#include "cvRepositoryData.h"

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
// definition) they contain pure virtual methods, objects of these
// classes can not be instantiated.  Clients of ABC's are interested
// in using the abstract interface, but can not work with the objects
// themselves.  Instead, clients instantiate concrete classes derived
// from the ABC.  And then, to use the abstraction, clients use
// *pointers* or *references* to the ABC.  See Meyers' Effective C++,
// Item 34.

enum KernelType {
  KERNEL_INVALID,
  KERNEL_MESHSIM,
  KERNEL_TETGEN,
};

class cvMeshObject : public cvRepositoryData {

  static char* GetKernelName( KernelType kernel );
  static KernelType GetKernelType( const char* kernel_name );

  static cvAdaptObject* DefaultInstantiateAdaptobject( Tcl_Interp *interp=NULL);

  static KernelType gCurrentKernel;
  static cvFactoryRegistrar gRegistrar;

  //Copy Operation
  virtual void cvAdaptObject *Copy() const = 0;
  virtual int Copy( const cvAdaptObject& src) = 0;

  //Load Operations
  virtual int LoadModel(char *fileName)=0;
  virtual int LoadMesh(char *fileName)=0;
  int LoadFromRestart(char *fileName,char *name,double **array);
  virtual int SetModel(vtkPolyData *newModel)=0;
  virtual int SetMesh(vtkUnstructuredGrid *newMesh)=0;

  //Setup Operations
  void SetNumberOfVariables(int nvar) {nvar_ = nvar;}
  void SetStrategy(int strategy) {strategy_ = strategy;}
  void SetRatio(double ratio) {ratio_ = ratio;}
  void SetMaxRefinementSize(double hmax) {hmax = hmax_;}
  void SetMinRefinementSize(double hmin) {hmin = hmin_;}
  void SetTimeStep(int timestep) {timestep_ = timestep;}
  
  //Adapt Operations
  int SetHessians();
  int ConvertToTetGen(); //TODO
  int ConvertToVTK();   //TODO
  virtual int RunAdaptor();
  virtual int AdaptMesh();

  //Helper Functions
  int GetHessiansFromSolution();
  int GetHessiansFromRestart();
  int SetSizeFieldUsingHessians();
  virtual int GradientsFromPatch();
  virtual int HessiansFromPatch();
  virual int SmoothHessians();
  int GradientsFromFilter();
  int HessiansFromFilter();
  int AttachArray(double *valueArray, vtkUnstructuredGrid *mesh,std::string dataName, int nVar, int poly);

  //Write Operations
  virtual int WriteAdaptedModel(char *fileName)=0;
  virtual int WriteAdaptedMesh(char *fileName)=0;
  virtual int WriteAdaptedSolution(char *fileName)=0;

protected:
  
private:
  vtkUnstructuredGrid *inmesh_;
  vtkUnstructuredGrid *outmesh_;
  vtkPolyData *insurface_mesh;
  vtkPolyData *outsurface_mesh;

  KernelType adapt_kernel_;
  int poly_=1;
  int sn_=0;
  int strategy_=1;
  double ratio_=0.2;
  int nvar_=5;
  double hmax_=1;
  double hmin_=1;
  int timestep_=0;

  double *sol_;
  double *error_indicator_;
  double *hessians_;



};

#endif // _CVADAPTOBJECT_H
  

