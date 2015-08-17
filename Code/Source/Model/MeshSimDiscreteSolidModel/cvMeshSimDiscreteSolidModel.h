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

#ifndef __CVDISCRETE_MODEL_H
#define __CVDISCRETE_MODEL_H

#include "SimVascular.h"
#include "cvSolidModel.h"
#include "cv_misc_utils.h"

#include <stdlib.h>
#include "MeshSim.h"
#include "SimModel.h"
#include "SimDiscrete.h"

//#include "cvMeshSimMeshObject.h"
// this should come from meshsimmeshobject.h
#define MY_MESHSIM_VERTEX_ORDERING 1
#define MY_MESHSIM_EDGE_ORDERING 1
#define MY_MESHSIM_FACE_ORDERING 1

// meshsim internal function
extern "C" {
  void refDerefSurfMesh(pMesh mesh, double swapAngle, double splitAngle,
			double minAngle, double maxAngle, double clpsFcRot,
			double clpsEdRot, double clpsShrtThan, double shrtRatio,
			int gradeMesh);
}

class CV_DLL_EXPORT cvMeshSimDiscreteSolidModel : public cvSolidModel {

public:
  cvMeshSimDiscreteSolidModel();                        // default constructor
  cvMeshSimDiscreteSolidModel( const cvMeshSimDiscreteSolidModel& sm );  // copy constructor
  ~cvMeshSimDiscreteSolidModel();

  void Clear();
  void Print() const;
  void Check( int *nerr ) const;
  cvSolidModel *Copy() const {return (cvSolidModel*)NULL;}
  int Copy( const cvSolidModel& src ) {return CV_ERROR;}

  // 2D methods:
  int MakePoly2d( cvPolyData *pd ) {return CV_ERROR;}
  int MakePoly2dPts( cvPolyData *pd ) {return CV_ERROR;}
  int MakeCircle( double radius, double ctr[] ) {return CV_ERROR;}
  int MakeEllipse( double xr, double yr, double ctr[] ) {return CV_ERROR;}
  int MakeBox2d( double dims[], double ctr[] ) {return CV_ERROR;}

  // 3D methods:
  int MakeBox3d( double dims[], double ctr[] ) {return CV_ERROR;}
  int MakeSphere( double r, double ctr[] ) {return CV_ERROR;}
  int MakePoly3dSolid( cvPolyData *pd , double angle );
  int MakePoly3dSurface( cvPolyData *pd ) {return CV_ERROR;}
  int SetPoly3dFacetMethod( SolidModel_FacetT code ) {return CV_OK;}
  int MakeCylinder( double r, double length, double ctr[], double axis[] ) {return CV_ERROR;}
  int ExtrudeZ( cvSolidModel *in, double dist ) {return CV_ERROR;}
  int Extrude( cvSolidModel *in, double **dist ) {return CV_ERROR;}
  int MakeTruncatedCone( double pt[], double dir[], double r1, double r2) {return CV_ERROR;}
 
  int MakeInterpCurveLoop( cvPolyData *pd, int closed ) { return CV_ERROR; }
  int MakeApproxCurveLoop( cvPolyData *pd, double tol, int closed ) { return CV_ERROR; }
  int MakeLoftedSurf( cvSolidModel **curves, int numCurves, char *name ) { return CV_ERROR; }

  // Booleans:
  int Intersect( cvSolidModel *a, cvSolidModel *b, SolidModel_SimplifyT st ) {return CV_ERROR;}
  int Union( cvSolidModel *a, cvSolidModel *b, SolidModel_SimplifyT st ) {return CV_ERROR;}
  int Subtract( cvSolidModel *a, cvSolidModel *b, SolidModel_SimplifyT st ) {return CV_ERROR;}

  // Transformations:
  int Translate( double vec[], int ndim ) {return CV_ERROR;}
  int Rotate( double axis[], int ndim, double rad ) {return CV_ERROR;}
  int Scale( double factor ) {return CV_ERROR;}
  int Reflect( double pos[], double nrm[] ) { return CV_ERROR; }
  int Apply4x4( double mat[][4] ) { return CV_ERROR; }

  // Geometric computations:
  int FindExtent( double *extent ) {return  CV_ERROR;}
  int FindCentroid( double *centroid ) {return CV_ERROR;}
  int GetTopoDim( int *tdim ) const {return CV_ERROR;}
  int GetSpatialDim( int *sdim ) const {return CV_ERROR;}
  int ClassifyPt( double pt[], int v, int *ans ) const {return CV_ERROR;}
  int ClassifyPt( double x, double y, int v, int *ans ) const {return CV_ERROR;}
  int ClassifyPt( double x, double y, double z, int v, int *ans ) const {return CV_ERROR;}
  int DistanceAlongVec( double start[], double end[], int v,
			double *ans ) const {return CV_ERROR;}
  int Distance( double pos[], double upperLimit, double *dist ) {return CV_ERROR;};
  int GetFaceNormal (int faceid, double u, double v, double normal[]) {return CV_ERROR;}
  
  // Attribute related & required methods:
  int GetBoundaryFaces (double angle) {return CV_ERROR;}
  int GetFaceIds (int *numFaces, int **faceIds);
  cvPolyData *GetFacePolyData(int faceid, int useMaxDist, double max_dist) const;
  int GetFaceAttribute(char *attr,int faceid, char **value) {return CV_ERROR;}
  int SetFaceAttribute(char *attr,int faceid, char *value) {return CV_ERROR;}
  int GetRegionIds (int *numRegions, int **regionIds) {return CV_ERROR;}
  int GetRegionAttribute(char *attr,int regionid, char **value) {return CV_ERROR;}
  int SetRegionAttribute(char *attr,int regionid, char *value) {return CV_ERROR;}
  int DeleteRegion (int regionid) {return CV_ERROR;}
  
  // File I/O:
  int ReadNative( char *filename );
  int WriteNative( int file_version, char *filename ) const;
  int WriteVtkPolyData( char *filename ) {return CV_ERROR;}
  cvPolyData *GetPolyData(int useMaxDist, double max_dist) const;

  cvPolyData *GetDiscontinuities() const { return NULL; }
  cvSolidModel *GetAxialIsoparametricCurve( double p ) const { return NULL; }

  // geometric manipulation
  virtual int DeleteFaces (int numfaces, int *faces) {return CV_ERROR;}
  int CreateEdgeBlend(int faceA, int faceB, double radius) {return CV_ERROR;}
  int CombineFaces(int targetface, int loseface) {return CV_ERROR;}
  int RemeshFace (int numfaces,int *excludedFaces, double size) {return CV_ERROR;}

  int SetVtkPolyDataObject(vtkPolyData *newPolyData) {return CV_ERROR;}
 
  // hack for now to get access in the meshing layer
   pDiscreteModel geom_;

private:

   int FindNodesOnElementFace (pFace face, int* nodes, double *xyz) const;
   // quad flag is mostly meaningless in this class
   int quadElem_;

  // Private geom pointer:


};


#endif // __GDSCDISCRETE_MODEL_H
