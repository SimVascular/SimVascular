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
                   // calls cvRepositoryData   //constructor
  virtual ~cvMeshObject();

  // Routines that are pulled out of old instantiation since they shouldn't be required.
  virtual int SetMeshFileName( const char* filename ) = 0;
  virtual int SetSolidFileName( const char* filename ) = 0;

  // TODO -- Neither of the below are currently used?  solidmodeling_kernel_ is legacy.
  cvMeshObject::KernelType GetMeshKernel() const { return mesh_kernel_; }

  SolidModel_KernelT GetSolidModelKernel() const { return solidmodeling_kernel_; }
  void SetSolidModelKernel(SolidModel_KernelT kernel) { solidmodeling_kernel_ = kernel; }

  int GetMeshLoaded() { return meshloaded_;}
  int GetElementType() {return quadElem_;}

  virtual int Update() = 0;
  virtual int Print() = 0;
  virtual cvMeshObject *Copy() const = 0;

  // Routines promoted to abstract class from concrete implementation
  virtual int LoadModel(char *filename) = 0;
  virtual int GetBoundaryFaces(double angle) = 0;
  virtual int LoadMesh(char *filename,char *surfilename) = 0;
  virtual int NewMesh() = 0;
  
  virtual int SetSurfaceMeshFlag(int value) = 0;
  virtual int SetSurfaceOptimization(int value) = 0;
  virtual int SetSurfaceSmoothing(int value) = 0;

  virtual int SetVolumeMeshFlag(int value) = 0;   
  virtual int SetVolumeOptimization(int value) = 0;
  virtual int SetVolumeSmoothing(int value) = 0;
  
  virtual int SetGlobalSize(int type, double gsize) = 0;
  virtual int SetLocalSize(int type, int id, double size) = 0;

  virtual int SetGlobalCurv(int type, double size) = 0;
  virtual int SetLocalCurv(int type, int id, double size) = 0;
  virtual int SetGlobalMinCurv(int type, double size) = 0;
  virtual int SetLocalMinCurv(int type, int id, double size) = 0;
  virtual int SetMeshOptions(char *flags,double value) = 0;
 
  virtual int SetBoundaryLayer(int type, int id, int side, int nL, double* H) = 0;
  virtual int SetWalls(int numWalls, int *walls) = 0;
  
  virtual int SetCylinderRefinement(double size, double radius, double length,
                            double* center, double *normal) = 0;
  virtual int SetSphereRefinement(double size, double radius, double* center) = 0;
  virtual int SetSizeFunctionBasedMesh(double size, char *sizefunctionname) = 0;

  virtual int GenerateMesh() = 0;
  virtual int WriteMesh(char *filename, int smsver) = 0;
  virtual int WriteStats(char *filename) = 0;

  // output fe input decks
  int WriteAbaqusInputDeck (char *filename);
  int WriteProphlexInputDeck (char *filename);
  int WriteSpectrumSolverElements (char *filename);
  int WriteSpectrumSolverNodes (char *filename);

  // output visualization files
  int WriteSpectrumVisMesh (char *filename); 
  int WriteSpectrumVisData (char *filename);
  
  virtual int WriteDataExplorer (char *filename) = 0;

  int WriteMeshVrml (char *filename);
  int WriteSurfaceMeshVrml (char *filename);
  int WriteSurfaceMeshOogl (char *filename);

  int WriteHypermeshAscii (char *filename);

  virtual int WriteMetisAdjacency (char *filename) = 0;

  // general queries
  virtual int GetElementConnectivity(int element) = 0;
  virtual int GetNodeCoords(int node) = 0;  
  virtual cvPolyData *GetPolyData() = 0;
  virtual cvPolyData *GetSolid() = 0;
  virtual cvUnstructuredGrid *GetUnstructuredGrid() = 0;
  virtual int GetModelFaceInfo(char rtnstr[99999]) = 0;

  // queries for bc's
  virtual int GetElementNodesOnModelFace (int face, char* filename) = 0;
  virtual int GetElementFacesOnModelFace (int face, int explicitFaceOut, char* filename) = 0;
  virtual cvPolyData* GetFacePolyData (int orgfaceid) = 0;
  
  virtual int GetElementsInModelRegion (int region, char* filename) = 0;

  virtual int GetExteriorElementFacesOnRegion (int region, char* filename) = 0;

  // change elements
  virtual int GenerateQuadraticElements () = 0;

  // helper routines
  virtual void initNodeTraversal() = 0;
  virtual void initElementTraversal() = 0;
  virtual void initRegionTraversal() = 0;
  virtual int getNextNode() = 0;
  virtual int getNextElement() = 0;
  virtual int getNextRegion() = 0;
  virtual int getNeighborMdlRegIds () = 0;

  virtual int SetVtkPolyDataObject(vtkPolyData *newPolyData) = 0;

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
  
private:
  int outputOoglFaceNodes(int n1,int n2,int n3,int n4,double maxpoint);
};


#endif // __CVMESHOBJECT_H
