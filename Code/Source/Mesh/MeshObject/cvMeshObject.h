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

#ifndef __CVMESHOBJECT_H
#define __CVMESHOBJECT_H

#include "SimVascular.h"

//#include "sys/param.h"
#define MAXPATHLEN 1024

#include "cvRepositoryData.h"
#include "cvUnstructuredGrid.h"
#include "cvSolidModel.h"

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

class cvMeshObject : public cvRepositoryData {

public:
  enum KernelType {
    KERNEL_INVALID,
    KERNEL_MESHSIM,
    KERNEL_TETGEN,
    KERNEL_GMSH,
    KERNEL_MAX_TYPES
  };

  static char* GetKernelName( KernelType kernel );
  static KernelType GetKernelType( const char* kernel_name );

  cvMeshObject();  // can never be called directly;
  virtual ~cvMeshObject();

  // Routines that are pulled out of old instantiation since they shouldn't be required.
  virtual int SetMeshFileName( const char* filename ) = 0;
  virtual int SetSolidFileName( const char* filename ) = 0;
  cvMeshObject::KernelType GetMeshKernel() const { return mesh_kernel_; }

  void SetSolidModelKernel(SolidModel_KernelT kernel) { solidmodeling_kernel_ = kernel; }
  int GetMeshLoaded() { return meshloaded_;}

  //Common functions for abstract base class
  virtual int Update() = 0;
  virtual int Print() = 0;
  virtual cvMeshObject *Copy() const = 0;

  // Routines promoted to abstract class from concrete implementation
  virtual int LoadModel(char *filename) = 0;
  virtual int GetBoundaryFaces(double angle) = 0;
  virtual int LoadMesh(char *filename,char *surfilename) = 0;
  virtual int NewMesh() = 0;

  //Set curve sizes and other mesh options
  virtual int SetMeshOptions(char *flags,int numValues,double *values) = 0;
 
  //Set boundary layer and/or specify wall faces
  virtual int SetBoundaryLayer(int type, int id, int side, int nL, double* H) = 0;
  virtual int SetWalls(int numWalls, int *walls) = 0;
  
  //Set refinement options
  virtual int SetCylinderRefinement(double size, double radius, double length,
                            double* center, double *normal) = 0;
  virtual int SetSphereRefinement(double size, double radius, double* center) = 0;
  virtual int SetSizeFunctionBasedMesh(double size, char *sizefunctionname) = 0;

  //Meshing operation and post-meshing cleanup/stats functions
  virtual int GenerateMesh() = 0;
  virtual int WriteMesh(char *filename, int smsver) = 0;
  virtual int WriteStats(char *filename) = 0;

  //Not necessary anymore, but leaving for now
  virtual int WriteMetisAdjacency (char *filename) = 0;

  // general queries
  virtual int GetNodeCoords(int node) = 0;
  virtual cvPolyData *GetPolyData() = 0;
  virtual cvPolyData *GetSolid() = 0;
  virtual cvUnstructuredGrid *GetUnstructuredGrid() = 0;
  virtual int GetModelFaceInfo(char rtnstr[99999]) = 0;

  // queries for bc's
  virtual cvPolyData* GetFacePolyData (int orgfaceid) = 0;

  //Set PolyData object after instantiation
  virtual int SetVtkPolyDataObject(vtkPolyData *newPolyData) = 0;
  virtual int SetInputUnstructuredGrid(vtkUnstructuredGrid *ug) = 0;

  //Adapt Functions
  virtual int Adapt() = 0;
  virtual int GetAdaptedMesh(vtkUnstructuredGrid *ug, vtkPolyData *pd) = 0;
  virtual int SetMetricOnMesh(double *error_indicator,int lstep,double factor, double hmax, double hmin,int strategy) = 0;

  int openOutputFile(char* filename);
  int closeOutputFile();

  // node info
  int nodeID_;
  double nodeX_;
  double nodeY_;
  double nodeZ_;
 
protected:
  cvMeshObject::KernelType mesh_kernel_;
  SolidModel_KernelT solidmodeling_kernel_;
  int meshloaded_;
  int quadElem_;

  int numLinearNodes_;
  int numNodes_;
  int numElements_;

  int numModelRegions_;
  int* regionID_;
  
  // element info
  int curElemID_;
  int connID_[30];
  int curElemNe_[4][2];
  
  // region info
  int curMdlRegID_;
  
  // output file
  gzFile fp_;
  
};


#endif // __CVMESHOBJECT_H
