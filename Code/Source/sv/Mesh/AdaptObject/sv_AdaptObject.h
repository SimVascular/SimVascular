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

#ifndef __CVADAPTOBJECT_H
#define __CVADAPTOBJECT_H

#include "SimVascular.h"
#include "svAdaptorExports.h" // For exports

//#include "sys/param.h"
#define MAXPATHLEN 1024

#include "sv_FactoryRegistrar.h"
#include "sv_RepositoryData.h"
#include "sv_PolyData.h"
#include "sv_MeshSystem.h"
#include "sv_MeshObject.h"

#ifdef SV_USE_ZLIB
  #ifdef SV_USE_SYSTEM_ZLIB
    #include <zlib.h>
  #else
    #include "simvascular_zlib.h"
  #endif
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


class SV_EXPORT_ADAPTOR cvAdaptObject : public cvRepositoryData {

public:

  cvAdaptObject( KernelType t );
  virtual ~cvAdaptObject();

  KernelType GetKernel() const {return adapt_kernel_;}
  //Instantiation function from SimVascular
  #ifdef SV_USE_TCL
  static cvAdaptObject* DefaultInstantiateAdaptObject( Tcl_Interp *interp = NULL, KernelType t = KERNEL_TETGEN);
  #endif
  #ifdef SV_USE_PYTHON
  static cvAdaptObject* DefaultInstantiateAdaptObject(KernelType t = KERNEL_TETGEN);
  #endif
  //Called after Insantiation, create a cvMeshObject
  #ifdef SV_USE_TCL
  virtual int CreateInternalMeshObject(Tcl_Interp *interp,char *meshFileName,
      char *solidFileName)=0;
  #endif
  #ifdef SV_USE_PYTHON
  virtual int CreateInternalMeshObject(char *meshFileName,
      char *solidFileName)=0;
  #endif
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
  virtual int LoadAvgSpeedFromFile(char *fileName)=0;
  virtual int LoadHessianFromFile(char *fileName)=0;
  virtual int ReadSolutionFromMesh()=0;
  virtual int ReadYbarFromMesh()=0;
  virtual int ReadAvgSpeedFromMesh()=0;

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


