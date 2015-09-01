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

#include "cvFactoryRegistrar.h"
#include "cvRepositoryData.h"
#include "cvPolyData.h"
#include "cvMeshSystem.h"
#include "cvMeshObject.h"

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


class cvAdaptObject : public cvRepositoryData {

public:

  cvAdaptObject( KernelType t );
  virtual ~cvAdaptObject();

  KernelType GetKernel() const {return adapt_kernel_;}
  //Instantiation function from SimVascular 
  static cvAdaptObject* DefaultInstantiateAdaptObject( Tcl_Interp *interp = NULL, KernelType t = KERNEL_TETGEN);
  //Called after Insantiation, create a cvMeshObject
  virtual int CreateInternalMeshObject(Tcl_Interp *interp,char *meshFileName,
		  char *solidFileName)=0;
  static KernelType gCurrentKernel;
  static cvFactoryRegistrar gRegistrar;

  //Copy Operation
  virtual cvAdaptObject *Copy() const = 0;
  virtual int Copy( const cvAdaptObject& src) = 0;

  //Load Operations
  virtual int LoadModel(char *fileName)=0;
  virtual int LoadMesh(char *fileName)=0;
  virtual int LoadSolutionFromFile(char *fileName)=0;
  virtual int LoadYbarFromFile(char *fileName)=0;
  virtual int LoadHessianFromFile(char *fileName)=0;
  virtual int ReadSolutionFromMesh()=0;
  virtual int ReadYbarFromMesh()=0;

  //Setup Operations
  virtual int SetAdaptOptions(char *flag,double value)=0;
  virtual int CheckOptions()=0;
  virtual int SetMetric(char *input,int option,
		  int strategy)=0;
  virtual int SetupMesh()=0;
  
  //Adapt Operations
  virtual int RunAdaptor()=0;
  virtual int PrintStats()=0;

  //Post Operations
  virtual int GetAdaptedMesh()=0;
  virtual int TransferSolution()=0;
  virtual int TransferRegions()=0;

  //Write Operations
  virtual int WriteAdaptedModel(char *fileName)=0;
  virtual int WriteAdaptedMesh(char *fileName)=0;
  virtual int WriteAdaptedSolution(char *fileName)=0;
  
private:
  KernelType adapt_kernel_;
};

#endif // _CVADAPTOBJECT_H
  

