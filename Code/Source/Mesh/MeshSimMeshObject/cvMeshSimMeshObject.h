/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
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
 */

#ifndef __CVMESHSIMMESHOBJECT_H
#define __CVMESHSIMMESHOBJECT_H

#include "SimVascular.h"

#define MY_MESHSIM_VERTEX_ORDERING 1
#define MY_MESHSIM_EDGE_ORDERING 1
#define MY_MESHSIM_FACE_ORDERING 1

// why do I need to do this?
#ifdef T
#undef T
#endif

#include <math.h>

#include "cvMeshObject.h"
#include "MeshSim.h"
#include "SimError.h"
#include "SimErrorCodes.h"
#include "SimMeshingErrorCodes.h"
#include "SimAdvMeshing.h"

#ifdef USE_MESHSIM_ADAPTOR
#include "MeshSim.h"
#include "MeshSimAdapt.h"
#include "SimMeshTools.h"
#include "SimParasolidKrnl.h"
#include "SimAdvMeshing.h"

#include "MeshSimInternal_phAdapt.h"
#include "cvAdaptHelp.h"
#endif

#ifdef USE_PARASOLID
  #include "SimParasolidKrnl.h"
  #include "SimParasolidInt.h"
  #include "cv_parasolid_utils.h"
  #include "parasolid_kernel.h"
  #include "kernel_interface.h"
  #include "SimError.h"
  #include "SimErrorCodes.h"
//  #include "SimParasolidKrnl.h"
#endif

#ifdef USE_DISCRETE_MODEL
  #include "cvMeshSimDiscreteSolidModel.h"
  #include "SimError.h"
  #include "SimErrorCodes.h"
  #include "MeshSim.h"
  #include "SimModel.h"
  #include "SimDiscrete.h"
#endif

class cvMeshSimMeshObject : public cvMeshObject {
    
  typedef struct MSoptions {
      int surface;
      int volume;
      int surface_optimization;
      int surface_smoothing;
      int volume_optimization;
      int volume_smoothing;
      int gsize_type;
      double gsize;
      int gcurv_type;
      double gcurv;
      int gmincurv_type;
      double gmincurv;
  } MSoptions;

  public:

  cvMeshSimMeshObject( Tcl_Interp *interp ); // default constructor
  cvMeshSimMeshObject( const cvMeshSimMeshObject& sm );  // copy constructor
  ~cvMeshSimMeshObject();

  //Set mesh and model file locations
  int SetMeshFileName( const char* meshFileName );
  int SetSolidFileName( const char* solidFileName );

  // Meshing functions for licensing/startup/shutdown
  static int Logon( const char* filename );
  static int Logoff();

  int InitTraversal();
  int Update();
  int Print();
  // the copy command probably doesn't work right!
  cvMeshObject *Copy() const;

  // Routines promoted to abstract class from concrete implementation
  int LoadModel(char *filename);
  int GetBoundaryFaces(double angle) {return CV_ERROR;}
  int LoadMesh(char *filename,char *surfilename);
  int NewMesh();

  //Set curve sizes and other mesh options
  int SetMeshOptions(char *flags, int numValues,double *values);

  //Set boundary layer and/or specify wall faces
  int SetBoundaryLayer(int type, int id, int side, int nL, double* H);
  int SetWalls(int numWalls, int *walls) {return CV_ERROR;}
  
  //Set refinement options
  int SetCylinderRefinement(double size, double radius, double length,
                            double* center, double *normal);
  int SetSphereRefinement(double size, double radius, double* center);
  int SetSizeFunctionBasedMesh(double size, char *filename) 
    {return CV_ERROR;}

  //Meshing operation and post-meshing cleanup/stats functions
  int GenerateMesh();
  int WriteMesh(char *filename, int smsver);
  int WriteStats(char *filename);
      
  // output visualization files
  int WriteMetisAdjacency (char *filename);
  
  // general queries
  int GetNodeCoords(int node);  
  cvPolyData *GetPolyData();
  cvPolyData *GetSolid() {return CV_ERROR;}
  cvUnstructuredGrid *GetUnstructuredGrid();

  // queries for bc's
  cvPolyData* GetFacePolyData (int orgfaceid);
  int GetModelFaceInfo(char rtnstr[99999]);
  
  int SetVtkPolyDataObject(vtkPolyData *newPolyData) {return CV_ERROR;}
  int SetInputUnstructuredGrid(vtkUnstructuredGrid *ug) {return CV_ERROR;}

  //Adapt functions
  int Adapt();
  int GetAdaptedMesh(vtkUnstructuredGrid *ug, vtkPolyData *pd);
  int SetMetricOnMesh(double *error_indicator,int lstep,double factor, double hmax, double hmin,int strategy);

  // MESHSIMMESHOBJECT ONLY
  int FindFaceNumber (pRegion region, int pseudofaceID, int *facenum);
  int FindNodesOnElementFace (pFace face, int* nodes);
  void initNodeTraversal();
  void initElementTraversal();
  void initRegionTraversal();
  int getNextNode();
  int getNextElement();
  int getNextRegion();
  int getNeighborMdlRegIds();
  int OutputExteriorElementFaces(pRegion region, int pseudoFaceID, char *filename);
  int getIdentForFaceId(int orgfaceid, int *faceID);

  // output visualization files
  private:

  int MapIDtoPID(int id, pGEntity *pid);

  char meshFileName_[MAXPATHLEN];
  char solidFileName_[MAXPATHLEN];
  pMesh mesh;
  SGModel* model;

  pParasolidNativeModel paramodel_;

#ifdef USE_PARASOLID
  PK_PART_t part_;
#else
  int part_;
#endif
  Tcl_Interp* interp_;
  int loadedVolumeMesh_;
  pRegion pCurrentMeshRegion_;
  //int* nodemap_;
  double* pts_;
  
  VIter  ptrNodes_;
  int    iterNodes_;
  EIter  ptrEdges_;

  RIter  ptrElements_;
  int    iterElements_;

  GRIter ptrModelRegions_;
  int    iterModelRegions_;

  MSoptions meshoptions_;

  // hack
#ifdef USE_DISCRETE_MODEL
  cvMeshSimDiscreteSolidModel* discreteModel_;
#endif

  pAManager manager_;
  pACase case_;

#ifdef USE_MESHSIM_ADAPTOR
  pMeshDataId errorIndicatorID;
  pMeshDataId modes;
  pMeshDataId nodalhessianID;
  pMeshDataId nodalgradientID;
  pMeshDataId phasta_solution;
  pMSAdapt simAdapter;
#endif
};


#endif // __CVMESHSIMMESHOBJECT_H
