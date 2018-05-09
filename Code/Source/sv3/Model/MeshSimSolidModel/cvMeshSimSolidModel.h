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

#ifndef __CVMESHSIMSOLID_MODEL_H
#define __CVMESHSIMSOLID_MODEL_H

#include "SimVascular.h"
#include "svMeshSimSolidExports.h"  // For exports
#include "cvSolidModel.h"
#include "cv_misc_utils.h"

#include <stdlib.h>
#include "MeshSim.h"
#include "SimModel.h"
#include "SimCreateModel.h"

//#include "cvMeshSimMeshObject.h"
// this should come from meshsimmeshobject.h
#define MY_MESHSIM_VERTEX_ORDERING 1
#define MY_MESHSIM_EDGE_ORDERING 1
#define MY_MESHSIM_FACE_ORDERING 1

class SV_EXPORT_MESHSIM_SOLID cvMeshSimSolidModel : public cvSolidModel {

public:
  cvMeshSimSolidModel();                        // default constructor
  cvMeshSimSolidModel( const cvMeshSimSolidModel& sm );  // copy constructor
  ~cvMeshSimSolidModel();

  void Clear();
  void Print() const;
  void Check( int *nerr ) const;
  cvSolidModel *Copy() const {return (cvSolidModel*)NULL;}
  int Copy( const cvSolidModel& src ) {return SV_ERROR;}

  // 2D methods:
  int MakePoly2d( cvPolyData *pd ) {return SV_ERROR;}
  int MakePoly2dPts( cvPolyData *pd ) {return SV_ERROR;}
  int MakeCircle( double radius, double ctr[] ) {return SV_ERROR;}
  int MakeEllipse( double xr, double yr, double ctr[] ) {return SV_ERROR;}
  int MakeBox2d( double dims[], double ctr[] ) {return SV_ERROR;}

  // 3D methods:
  int MakeBox3d( double dims[], double ctr[] ) {return SV_ERROR;}
  int MakeSphere( double r, double ctr[] ) {return SV_ERROR;}
  int MakePoly3dSolid( cvPolyData *pd , double angle ) {return SV_ERROR;}
  int MakePoly3dSurface( cvPolyData *pd ) {return SV_ERROR;}
  int SetPoly3dFacetMethod( SolidModel_FacetT code ) {return SV_OK;}
  int MakeCylinder( double r, double length, double ctr[], double axis[] ) {return SV_ERROR;}
  int ExtrudeZ( cvSolidModel *in, double dist ) {return SV_ERROR;}
  int Extrude( cvSolidModel *in, double **dist ) {return SV_ERROR;}
  int MakeTruncatedCone( double pt[], double dir[], double r1, double r2) {return SV_ERROR;}

  int MakeInterpCurveLoop( cvPolyData *pd, int closed ) { return SV_ERROR; }
  int MakeApproxCurveLoop( cvPolyData *pd, double tol, int closed ) { return SV_ERROR; }
  int MakeLoftedSurf( cvSolidModel **curves, int numCurves , char *name,
     int continuity,int partype,double w1,double w2,double w3 ,int smoothing) { return SV_ERROR; }

  // Booleans:
  int Intersect( cvSolidModel *a, cvSolidModel *b, SolidModel_SimplifyT st ) {return SV_ERROR;}
  int Union( cvSolidModel *a, cvSolidModel *b, SolidModel_SimplifyT st ) {return SV_ERROR;}
  int Subtract( cvSolidModel *a, cvSolidModel *b, SolidModel_SimplifyT st ) {return SV_ERROR;}

  // Transformations:
  int Translate( double vec[], int ndim ) {return SV_ERROR;}
  int Rotate( double axis[], int ndim, double rad ) {return SV_ERROR;}
  int Scale( double factor ) {return SV_ERROR;}
  int Reflect( double pos[], double nrm[] ) { return SV_ERROR; }
  int Apply4x4( double mat[][4] ) { return SV_ERROR; }

  // Geometric computations:
  int FindExtent( double *extent ) {return  SV_ERROR;}
  int FindCentroid( double *centroid ) {return SV_ERROR;}
  int GetTopoDim( int *tdim ) const {return SV_ERROR;}
  int GetSpatialDim( int *sdim ) const {return SV_ERROR;}
  int ClassifyPt( double pt[], int v, int *ans ) const {return SV_ERROR;}
  int ClassifyPt( double x, double y, int v, int *ans ) const {return SV_ERROR;}
  int ClassifyPt( double x, double y, double z, int v, int *ans ) const {return SV_ERROR;}
  int DistanceAlongVec( double start[], double end[], int v,
			double *ans ) const {return SV_ERROR;}
  int Distance( double pos[], double upperLimit, double *dist ) {return SV_ERROR;};
  int GetFaceNormal (int faceid, double u, double v, double normal[]) {return SV_ERROR;}

  // Attribute related & required methods:
  int GetBoundaryFaces (double angle) {return SV_ERROR;}
  int GetFaceIds (int *numFaces, int **faceIds);
  cvPolyData *GetFacePolyData(int faceid, int useMaxDist, double max_dist) const;
  int GetFaceAttribute(char *attr,int faceid, char **value) {return SV_ERROR;}
  int SetFaceAttribute(char *attr,int faceid, char *value) {return SV_ERROR;}
  int GetRegionIds (int *numRegions, int **regionIds) {return SV_ERROR;}
  int GetRegionAttribute(char *attr,int regionid, char **value) {return SV_ERROR;}
  int SetRegionAttribute(char *attr,int regionid, char *value) {return SV_ERROR;}
  int DeleteRegion (int regionid) {return SV_ERROR;}

  // File I/O:
  int ReadNative( char *filename );
  int WriteNative( int file_version, char *filename ) const;
  int WriteVtkPolyData( char *filename ) {return SV_ERROR;}
  int WriteGeomSim( char *filename ) {return SV_ERROR; }

  cvPolyData *GetPolyData(int useMaxDist, double max_dist) const;

  cvPolyData *GetDiscontinuities() const { return NULL; }
  cvSolidModel *GetAxialIsoparametricCurve( double p ) const { return NULL; }

  // geometric manipulation
  virtual int DeleteFaces (int numfaces, int *faces) {return SV_ERROR;}
  int CreateEdgeBlend(int faceA, int faceB, double radius,
      int filletshape) {return SV_ERROR;}
  int CombineFaces(int targetface, int loseface) {return SV_ERROR;}
  int RemeshFace (int numfaces,int *excludedFaces, double size) {return SV_ERROR;}

  int SetVtkPolyDataObject(vtkPolyData *newPolyData) {return SV_ERROR;}

  // hack for now to get access in the meshing layer
   pGModel geom_;

private:

   int FindNodesOnElementFace (pFace face, int* nodes, double *xyz) const;
   // quad flag is mostly meaningless in this class
   int quadElem_;

   pProgress progress_;

  // Private geom pointer:


};


#endif // __CVMESHSIMSOLID_MODEL_H
