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

/** @file cvPolyDataSolid.h
 *  @brief Class provides implementations of the PolyData solid type
 *
 *  This is derived from the SolidModel class and provides implementations
 *  of functions to be able to import a solid using vtkReaders, extract 
 *  the boundaries of the solid, and mesh the solid with TetGen.
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  @note Most functions in class call functions in cv_polydatasolid_utils.
 */

#ifndef __CVPOLYDATA_SOLID_H
#define __CVPOLYDATA_SOLID_H

#include "SimVascular.h"
#include "cvRepositoryData.h"
#include "cvSolidModel.h"
#include "cvPolyData.h"
#include "cvPolyDataSolid.h"
#include "cvFactoryRegistrar.h"
#include "cvVTK.h"
#include "cv_misc_utils.h"


// Some elementary notes on abstract base classes (ABC's)
// ------------------------------------------------------
// ABC's provide a means for defining an *interface*.  Since (by
// definition) they contain pure  methods, objects of these
// classes can not be instantiated.  Clients of ABC's are interested
// in using the abstract interface, but can not work with the objects
// themselves.  Instead, clients instantiate concrete classes derived
// from the ABC.  And then, to use the abstraction, clients use
// *pointers* or *references* to the ABC.  See Meyers' Effective C++,
// Item 34.
//

class cvPolyDataSolid : public cvSolidModel {

public:
  cvPolyDataSolid();  // can never be called directly;
                                       // calls cvRepositoryData constructor
  cvPolyDataSolid( const cvPolyDataSolid& sm);
  ~cvPolyDataSolid();

  // Modeler operations:
  void Clear() {return;}
  void ClearSurf() {return;}
  void Print() const {return;}
  void Check( int *nerr ) const {return;}
  cvSolidModel *Copy() const;

  // Constructive methods:
  int Copy( const cvSolidModel& src );
  int MakePoly2d( cvPolyData *pd ) {return CV_ERROR;}
  int MakePoly2dPts( cvPolyData *pd ) {return CV_ERROR;} 
  int MakeCircle( double radius, double ctr[] ) {return CV_ERROR;} 
  int MakeEllipse( double xr, double yr, double ctr[] ) {return CV_ERROR;} 
  int MakeBox2d( double dims[], double ctr[] ) {return CV_ERROR;} 

  int MakeBox3d( double dims[], double ctr[] );
  int MakeSphere( double r, double ctr[] ) {return CV_ERROR;} 
  int MakeEllipsoid( double r[], double ctr[] ) {return CV_ERROR;} 
  int MakeCylinder( double r, double length, double ctr[],
       		    double axis[] ) {return CV_ERROR;} 
  int MakeTorus( double rmaj, double rmin, double ctr[],
       		 double axis[] ) { return CV_ERROR; }
  int MakeTruncatedCone( double pt[], double dir[], double r1, double r2) {return CV_ERROR; };

  int SetPoly3dFacetMethod( SolidModel_FacetT code ) {return CV_ERROR; }
  int MakePoly3dSolid( cvPolyData *pd , double angle ) {return CV_ERROR; }
  int MakePoly3dSurface( cvPolyData *pd ) {return CV_ERROR; }
  int ExtrudeZ( cvSolidModel *in, double dist ) {return CV_ERROR; }
  int Extrude( cvSolidModel *in, double **dist ) {return CV_ERROR; }

  int MakeInterpCurveLoop( cvPolyData *pd, int closed ) {return CV_ERROR; }
  int MakeApproxCurveLoop( cvPolyData *pd, double tol, int closed ) {return CV_ERROR; }
  int MakeLoftedSurf( cvSolidModel **curves, int numCurves , char *name ) {return CV_ERROR; }
  int CapSurfToSolid( cvSolidModel *surf ) { return CV_ERROR; }

  // Booleans are compatible only between like-typed concrete objects:
  int Intersect( cvSolidModel *a, cvSolidModel *b,
       		 SolidModel_SimplifyT st = SM_Simplify_All );
  int Union( cvSolidModel *a, cvSolidModel *b,
       	     SolidModel_SimplifyT st = SM_Simplify_All );
  int Subtract( cvSolidModel *a, cvSolidModel *b,
       		SolidModel_SimplifyT st = SM_Simplify_All );

  // Transformations:
  int Translate( double vec[], int ndim ) { return CV_ERROR; } 
  int Rotate( double axis[], int ndim, double rad ) { return CV_ERROR; } 
  int Scale( double factor ) { return CV_ERROR; } 
  int Reflect( double pos[], double nrm[] ) { return CV_ERROR; } 
  int Apply4x4( double mat[][4] ) { return CV_ERROR; } 

  // Geometric query methods:
  cvPolyData *GetPolyData(int useMaxDist, double max_dist) const;
  cvPolyData *GetFacePolyData(int faceid, int useMaxDist, double max_dist) const;
  cvPolyData *GetDiscontinuities() const { return CV_ERROR; }
  cvPolyDataSolid *GetAxialIsoparametricCurve( double p ) const { return CV_ERROR; }
  int FindExtent( double *extent ) { return CV_ERROR; }
  int FindCentroid( double *centroid ) { return CV_ERROR; }
  int GetTopoDim( int *tdim ) const { return CV_ERROR; }
  int GetSpatialDim( int *sdim ) const { return CV_ERROR; }
  int ClassifyPt( double pt[], int v, int *ans ) const { return CV_ERROR; } 
  int ClassifyPt( double x, double y, int v, int *ans ) const { return CV_ERROR; } 
  int ClassifyPt( double x, double y, double z, int v,
       		  int *ans ) const { return CV_ERROR; } 
  int DistanceAlongVec( double start[], double end[], int v,
       			double *ans ) const { return CV_ERROR; }
  int Distance( double pos[], double upperLimit,
       		double *dist ) { return CV_ERROR; }
  int GetFaceNormal (int faceid, double u, double v, double normal[]) { return CV_ERROR; }
  
  // Attribute related & required methods:
  void MakeSurf() { return; }
  int GetBoundaryFaces(double angle);
  int GetFaceIds (int *numFaces, int **faceIds);
  int GetFaceAttribute(char *attr,int faceid, char **value);
  int SetFaceAttribute(char *attr,int faceid, char *value);
  int GetRegionIds (int *numRegions, int **RegionIds) {return CV_ERROR; }
  int GetRegionAttribute(char *attr,int regionid, char **value) {return CV_ERROR; }
  int SetRegionAttribute(char *attr,int regionid, char *value) {return CV_ERROR; }
  int DeleteRegion (int regionid);
  
  // I/O:
  int ReadNative( char *filename );                     
  int WriteNative( int file_version, char *filename ) const;
  int WriteVtkPolyData( char *filename ) {return CV_ERROR; }

  // geometric manipulation
  // this method is buggy and should be used with great care!
  int DeleteFaces (int numfaces, int *faces); 
  int CreateEdgeBlend(int faceA, int faceB, double radius) {return CV_ERROR; } 
  int CombineFaces (int targetface, int loseface);
  int RemeshFace (int numfaces,int *excludedFaces, double size);
  
  int SetVtkPolyDataObject(vtkPolyData *newPolyData);

protected:
  vtkPolyData *geom_;
  int numBoundaryRegions;

};

#endif 
