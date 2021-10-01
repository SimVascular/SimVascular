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

/** @file sv_TetGenMeshObject.h
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
#include "svTetGenMeshExports.h" // For exports

#include <math.h>
#include <set>

#include "sv_MeshObject.h"

#include "simvascular_tetgen.h"

class SV_EXPORT_TETGEN_MESH cvTetGenMeshObject : public cvMeshObject {

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
    double mindihedral;
    double coarsenpercent;
    int boundarylayermeshflag;
    int numsublayers;
    double blthicknessfactor;
    double sublayerratio;
    int useconstantblthickness;
    int newregionboundarylayer;
    int boundarylayerdirection;
    int refinement;
    double refinedsize;
    double sphereradius;
    double spherecenter[3];
    double cylinderradius;
    double cylindercenter[3];
    double cylinderlength;
    double cylindernormal[3];
    int numberofholes;
    int numberofregions;
    int functionbasedmeshing;
    int secondarrayfunction;
    int meshwallfirst;
    int startwithvolume;
    int refinecount;
    int usemmg;
    double hausd;
    bool allowMultipleRegions;
  } TGoptions;

  public:
  #ifdef SV_USE_TCL
  cvTetGenMeshObject( Tcl_Interp *interp ); // default constructor
  #endif
  #ifdef SV_USE_PYTHON
  cvTetGenMeshObject(); // default constructor for python
  #endif

  cvTetGenMeshObject( const cvTetGenMeshObject& sm); //copy constructor

  ~cvTetGenMeshObject();

  //Set mesh and model file locations
  int SetMeshFileName( const char* meshFileName );
  int SetSolidFileName( const char* solidFileName );

  int Update();
  #ifdef SV_USE_TCL
  int Print();
  #endif
  #ifdef SV_USE_PYTHON
  int pyPrint();
  #endif
  cvMeshObject *Copy() const;

  // Routines promoted to abstract class from concrete implementation
  int LoadModel(char *filename);
  int LoadModel(vtkPolyData *pd);
  int LoadModel(cvSolidModel *cvModel){return SV_ERROR;}
  int GetBoundaryFaces(double angle);
  int LoadMesh(char *filename,char *surfilename);
  int NewMesh();
  bool HasVolumeMesh();
  bool HasSurfaceMesh();

  //Set curve sizes and other mesh options
  int SetMeshOptions(char *flags,int numValues, double *values);

  //Set boundary layer and/or specify wall faces
  int SetBoundaryLayer(int type, int id, int side, int nL, double* H);
  int SetWalls(int numWalls,int *walls);

  //Set refinement options
  int SetCylinderRefinement(double size, double radius, double length,
                            double* center, double *normal);
  int SetSphereRefinement(double size, double radius, double* center);
  int SetSizeFunctionBasedMesh(double size,char *sizefunctionname);

  void SetAllowMultipleRegions(bool value);

  // Add these methods to generate mesh sizing arrays separate
  // from option processing.
  int GenerateLocalSizeSizingArray(int faceID, double edgeSize);
  void EnableSizeFunctionBasedMesh();
  void DisableSizeFunctionBasedMesh();
  bool SizeFunctionBasedMeshIsEnabled();

  //Meshing operation and post-meshing cleanup/stats functions
  int GenerateMesh();
  int WriteMesh(char *filename, int smsver);
  int WriteStats(char *filename);

  // output visualization files
  int WriteMetisAdjacency (char *filename);

  // general queries
  int GetNodeCoords(int node);
  cvPolyData *GetPolyData();
  cvPolyData *GetSolid();
  bool HasSolid();
  cvUnstructuredGrid *GetUnstructuredGrid();
  int GetModelFaceInfo(std::map<std::string,std::vector<std::string>>& faceInfo);
  int GetModelFaceIDs(std::vector<int>& faceIDs);

  // queries for bc's
  cvPolyData* GetFacePolyData (int orgfaceid);

  int SetVtkPolyDataObject(vtkPolyData *newPolyData);
  int SetInputUnstructuredGrid(vtkUnstructuredGrid *ug);

  //Adapt Function
  int Adapt();
  int GetAdaptedMesh(vtkUnstructuredGrid *ug, vtkPolyData *pd);
  int SetMetricOnMesh(double *error_indicator,int lstep,double factor, double hmax, double hmin,int strategy);

  //TETGENMESHOBJECT ONLY: These are helper functions for some of the more complicated mesh options
  int GenerateSurfaceRemesh();
  int GenerateBoundaryLayerMesh();
  int GenerateAndMeshCaps();
  int GenerateMeshSizingFunction();
  int AppendBoundaryLayerMesh();
  int ResetOriginalRegions(std::string regionName);

  private:
  char meshFileName_[MAXPATHLEN];
  char solidFileName_[MAXPATHLEN];

  #ifdef SV_USE_TCL
  Tcl_Interp* interp_;
  #endif
  int loadedVolumeMesh_;
  int numModelRegions_ ;
  int numBoundaryRegions_;
  double* pts_;

  tetgenio *inmesh_;
  tetgenio *outmesh_;
  vtkPolyData *polydatasolid_;
  vtkUnstructuredGrid *inputug_;

  vtkPolyData *originalpolydata_;
  vtkPolyData *surfacemesh_;
  vtkPoints   *holelist_;
  vtkPoints   *regionlist_;
  vtkDoubleArray *regionsizelist_;
  vtkUnstructuredGrid *volumemesh_;
  vtkUnstructuredGrid *boundarylayermesh_;
  vtkUnstructuredGrid *innerblmesh_;

  std::set<int> wallFaceIDs_;

  TGoptions meshoptions_;

  void SetCapBoundaryNormals(vtkPolyData* surface);
};

#endif // _CVTETGENMESHOBJECT_H


