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

/** @file cvTetGenMeshObject.h
 *  @brief Class provides implementations of the TetGen Mesh type
 *
 *  This is derived from the MeshObject class and provides implementations
 *  of functions to be able to set up a mesh by converting to TetGen
 *  data structures, set the options for meshing, run the mesh, and convert
 *  the mesh back into vtkPolyData 
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  @note Most functions in class call functions in cv_polydatasolid_utils.
 */

#ifndef __CVTETGENMESHOBJECT_H
#define __CVTETGENMESHOBJECT_H

#include "SimVascular.h"

#include <math.h>

#include "cvMeshObject.h"

#include "simvascular_tetgen.h"

class cvTetGenMeshObject : public cvMeshObject {

  typedef struct TGoptions {
    int surfacemeshflag;
    int volumemeshflag;
    int nomerge;
    int quiet;
    int docheck;
    int verbose;
    int diagnose;
    int nobisect;
    int optlevel;
    double maxedgesize;
    double epsilon;
    double minratio;
    double coarsen_percent;
    int boundarylayermeshflag;
    int numsublayers;
    double blthicknessfactor;
    double sublayerratio;
    int sphererefinement;
    double refinedsize;
    double sphereradius;
    double spherecenter[3];
    int functionbasedmeshing;
    int secondarrayfunction;
    int meshwallfirst;
  } TGoptions;

  public:

  cvTetGenMeshObject( Tcl_Interp *interp ); // default constructor

  cvTetGenMeshObject( const cvTetGenMeshObject& sm); //copy constructor

  ~cvTetGenMeshObject();

  int SetMeshFileName( const char* meshFileName );
  int SetSolidFileName( const char* solidFileName );

  int Update();
  int Print();
  cvMeshObject *Copy() const;

  int LoadModel(char *filename);
  int GetBoundaryFaces(double angle);
  int LoadMesh(char *filename);
  int NewMesh();

  int SetSurfaceMeshFlag(int value);
  int SetSurfaceOptimization(int value) {return CV_ERROR;}
  int SetSurfaceSmoothing(int value) {return CV_ERROR;}

  int SetVolumeMeshFlag(int value);   
  int SetVolumeOptimization(int value) {return CV_ERROR;}
  int SetVolumeSmoothing(int value) {return CV_ERROR;}

  int SetGlobalSize(int type, double gsize) {return CV_ERROR;}
  int SetLocalSize(int type, int id, double size);

  int SetGlobalCurv(int type, double size) {return CV_ERROR;}
  int SetLocalCurv(int type, int id, double size) {return CV_ERROR;}
  int SetGlobalMinCurv(int type, double size) {return CV_ERROR;}
  int SetLocalMinCurv(int type, int id, double size) {return CV_ERROR;}
  int SetMeshOptions(char *flags,double value);
 
  int SetBoundaryLayer(int type, int id, int side, int nL, double* H); 
  int SetWalls(int numWalls,int *walls); 
  
  int SetCylinderRefinement(double size, double radius, double length,
                            double* center, double *normal) {return CV_ERROR;}
  int SetSphereRefinement(double size, double radius, double* center);
  int SetSizeFunctionBasedMesh(double size,char *sizefunctionname);

  int GenerateMesh();
  int WriteMesh(char *filename, int smsver);
  int WriteStats(char *filename);

  // output visualization files
  int WriteDataExplorer (char *filename) {return CV_ERROR;}
  int WriteMetisAdjacency (char *filename);

  // general queries
  int GetElementConnectivity(int element) {return CV_ERROR;}
  int GetNodeCoords(int node) {return CV_ERROR;}  
  cvPolyData *GetPolyData();
  cvPolyData *GetSolid();
  cvUnstructuredGrid *GetUnstructuredGrid();
  int GetModelFaceInfo(char rtnstr[99999]);

  // queries for bc's
  int GetElementNodesOnModelFace (int face, char* filename) {return CV_ERROR;}
  int GetElementFacesOnModelFace (int face, int explicitFaceOut, char* filename) {return CV_ERROR;}
  cvPolyData* GetFacePolyData (int orgfaceid);

  int GetElementsInModelRegion (int region, char* filename) {return CV_ERROR;}

  int GetExteriorElementFacesOnRegion (int region, char* filename) {return CV_ERROR;} 
  
  // change elements
  int GenerateQuadraticElements () {return CV_ERROR;}

  void initNodeTraversal() {return;} 
  void initElementTraversal() {return;} 
  void initRegionTraversal() {return;} 
  int getNextNode() {return CV_ERROR;}
  int getNextElement() {return CV_ERROR;}
  int getNextRegion() {return CV_ERROR;} 
  int getNeighborMdlRegIds() {return CV_ERROR;} 

  int SetVtkPolyDataObject(vtkPolyData *newPolyData);

  //These are helper functions for some of the more complicated mesh options
  int GenerateSurfaceRemesh();
  int GenerateBoundaryLayerMesh();
  int GenerateAndMeshCaps();
  int GenerateMeshSizingFunction();
  int AppendBoundaryLayerMesh();
  int ResetOriginalRegions(std::string originalName,std::string newName);

  private:
  char meshFileName_[MAXPATHLEN];
  char solidFileName_[MAXPATHLEN];

  Tcl_Interp* interp_;
  int loadedVolumeMesh_;
  int numModelRegions_ ;
  int numBoundaryRegions_;
  double* pts_;

  tetgenio *inmesh_;
  tetgenio *outmesh_;
  vtkPolyData *polydatasolid_;

  vtkPolyData *originalpolydata_;
  vtkPolyData *surfacemesh_;
  vtkUnstructuredGrid *volumemesh_;
  vtkUnstructuredGrid *boundarylayermesh_;
  vtkUnstructuredGrid *innerblmesh_;

  TGoptions meshoptions_;
  
};

#endif // _CVTETGENMESHOBJECT_H


