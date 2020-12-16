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

#ifndef __CVMESHOBJECT_H
#define __CVMESHOBJECT_H

#include <map>

#include "SimVascular.h"
#include "svMeshObjectExports.h"

//#include "sys/param.h"
#define MAXPATHLEN 1024

#include "sv_RepositoryData.h"
#include "sv_UnstructuredGrid.h"
#include "sv_SolidModel.h"

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

class SV_EXPORT_MESH cvMeshObject : public cvRepositoryData {

public:
  enum KernelType {
    KERNEL_INVALID,
    KERNEL_MESHSIM,
    KERNEL_TETGEN,
    KERNEL_GMSH,
    KERNEL_MAX_TYPES
  };

  // Define the names used to access face information
  // in the map returned by the GetModelFaceInfo() method. 
  //
  class SV_EXPORT_MESH ModelFaceInfo {
      public:
        static const std::string ID;
        static const std::string NAME;
        static const std::string MODEL_ID;
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
#ifdef SV_USE_PYTHON
  virtual int pyPrint() = 0;
#endif
  virtual cvMeshObject *Copy() const = 0;

  // Routines promoted to abstract class from concrete implementation
  virtual int LoadModel(char *filename) = 0;
  virtual int LoadModel(vtkPolyData *pd) = 0;
  virtual int LoadModel(cvSolidModel *cvModel) = 0;
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
  virtual bool HasSolid() = 0;
  virtual cvUnstructuredGrid *GetUnstructuredGrid() = 0;
  virtual int GetModelFaceInfo(std::map<std::string,std::vector<std::string>>& faceInfo) = 0;
  virtual int GetModelFaceIDs(std::vector<int>& faceIDs) = 0;

  virtual bool HasVolumeMesh() = 0;
  virtual bool HasSurfaceMesh() = 0;

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
