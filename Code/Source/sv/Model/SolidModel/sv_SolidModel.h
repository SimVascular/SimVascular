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

#ifndef __CVSOLID_MODEL_H
#define __CVSOLID_MODEL_H

#include "SimVascular.h"
#include "svSolidModelExports.h"
#include "sv_RepositoryData.h"
#include "sv_PolyData.h"
#include "sv_FactoryRegistrar.h"


enum SolidModel_KernelT {
  SM_KT_RESERVED,
  SM_KT_PARASOLID,
  SM_KT_DISCRETE,
  SM_KT_POLYDATA,
  SM_KT_OCCT,
  SM_KT_MESHSIMSOLID,
  SM_KT_INVALID
};

SV_EXPORT_SOLID SolidModel_KernelT SolidModel_KernelT_StrToEnum( char *name );
SV_EXPORT_SOLID char *SolidModel_KernelT_EnumToStr( SolidModel_KernelT val );

typedef enum {
  SM_Facet_Union,
  SM_Facet_Sew,
  SM_Facet_Web,
  SM_Facet_Invalid
} SolidModel_FacetT;

SV_EXPORT_SOLID SolidModel_FacetT SolidModel_FacetT_StrToEnum( char *name );
SV_EXPORT_SOLID char *SolidModel_FacetT_EnumToStr( SolidModel_FacetT val );


typedef enum {
  SM_Simplify_All,
  SM_Simplify_None,
  SM_Simplify_Invalid
} SolidModel_SimplifyT;

SV_EXPORT_SOLID SolidModel_SimplifyT SolidModel_SimplifyT_StrToEnum( char *name );
SV_EXPORT_SOLID char *SolidModel_SimplifyT_EnumToStr( SolidModel_SimplifyT val );


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


class SV_EXPORT_SOLID cvSolidModel : public cvRepositoryData {

public:
  cvSolidModel( SolidModel_KernelT t );  // can never be called directly;
                                       // calls cvRepositoryData constructor
  virtual ~cvSolidModel();

  SolidModel_KernelT GetKernelT() const { return kernel_; }

  // Solid Model factory method that delegates creation of models to the
  //  concrete implementations.
  #ifdef SV_USE_TCL
  static cvSolidModel* DefaultInstantiateSolidModel( Tcl_Interp *interp = NULL);
  #endif
  #ifdef SV_USE_PYTHON
  static cvSolidModel* pyDefaultInstantiateSolidModel();
  #endif

  // Global variables that handle management of solid model kernels.
  static SolidModel_KernelT gCurrentKernel;
  static cvFactoryRegistrar gRegistrar;

  // Modeler operations:
  virtual void Clear() = 0;
  virtual void Print() const = 0;
  virtual void Check( int *nerr ) const = 0;
  virtual cvSolidModel *Copy() const = 0;

  // Constructive methods:
  virtual int Copy( const cvSolidModel& src ) = 0;
  virtual int MakePoly2d( cvPolyData *pd ) = 0;
  virtual int MakePoly2dPts( cvPolyData *pd ) = 0;
  virtual int MakeCircle( double radius, double ctr[] ) = 0;
  virtual int MakeEllipse( double xr, double yr, double ctr[] ) = 0;
  virtual int MakeBox2d( double dims[], double ctr[] ) = 0;

  virtual int MakeBox3d( double dims[], double ctr[] ) = 0;
  virtual int MakeSphere( double r, double ctr[] ) = 0;
  virtual int MakeEllipsoid( double r[], double ctr[] )
    { return SV_ERROR; }
  virtual int MakeCylinder( double r, double length, double ctr[],
			    double axis[] ) = 0;
  virtual int MakeTorus( double rmaj, double rmin, double ctr[],
			 double axis[] ) { return SV_ERROR; }
  virtual int MakeTruncatedCone( double pt[], double dir[], double r1, double r2) = 0;

  virtual int SetPoly3dFacetMethod( SolidModel_FacetT code ) = 0;
  virtual int MakePoly3dSolid( cvPolyData *pd , double angle ) = 0;
  virtual int MakePoly3dSurface( cvPolyData *pd ) = 0;
  virtual int ExtrudeZ( cvSolidModel *in, double dist ) = 0;
  virtual int Extrude( cvSolidModel *in, double **dist ) = 0;

  virtual int MakeInterpCurveLoop( cvPolyData *pd, int closed ) = 0;
  virtual int MakeApproxCurveLoop( cvPolyData *pd, double tol, int closed ) = 0;
  virtual int MakeLoftedSurf( cvSolidModel **curves, int numCurves , char *name,
     int continuity,int partype,double w1,double w2,double w3 ,int smoothing,  bool capSurface=true) = 0;
  virtual int CapSurfToSolid( cvSolidModel *surf ) = 0;
  //virtual int CapSurfToSolid( cvSolidModel *surf ) { return SV_ERROR; }

  // Booleans are compatible only between like-typed concrete objects:
  virtual int Intersect( cvSolidModel *a, cvSolidModel *b,
			 SolidModel_SimplifyT st = SM_Simplify_All ) = 0;
  virtual int Union( cvSolidModel *a, cvSolidModel *b,
		     SolidModel_SimplifyT st = SM_Simplify_All ) = 0;
  virtual int Subtract( cvSolidModel *a, cvSolidModel *b,
			SolidModel_SimplifyT st = SM_Simplify_All ) = 0;

  // Transformations:
  virtual int Translate( double vec[], int ndim ) = 0;
  virtual int Rotate( double axis[], int ndim, double rad ) = 0;
  virtual int Scale( double factor ) = 0;
  virtual int Reflect( double pos[], double nrm[] ) = 0;
  virtual int Apply4x4( double mat[][4] ) = 0;

  // Geometric query methods:
  virtual cvPolyData *GetPolyData(int useMaxDist, double max_dist) const = 0;
  virtual cvPolyData *GetFacePolyData(int faceid, int useMaxDist, double max_dist) const = 0;
  virtual cvPolyData *GetDiscontinuities() const = 0;
  virtual cvSolidModel *GetAxialIsoparametricCurve( double p ) const = 0;
  virtual int FindExtent( double *extent ) = 0;
  virtual int FindCentroid( double *centroid ) = 0;
  virtual int GetTopoDim( int *tdim ) const = 0;
  virtual int GetSpatialDim( int *sdim ) const = 0;
  virtual int ClassifyPt( double pt[], int v, int *ans ) const = 0;
  virtual int ClassifyPt( double x, double y, int v, int *ans ) const = 0;
  virtual int ClassifyPt( double x, double y, double z, int v,
			  int *ans ) const = 0;
  virtual int DistanceAlongVec( double start[], double end[], int v,
				double *ans ) const = 0;
  virtual int Distance( double pos[], double upperLimit,
			double *dist ) = 0;
  virtual int GetFaceNormal (int faceid, double u, double v, double normal[]) = 0;

  // Attribute related & required methods:
  virtual int GetBoundaryFaces(double angle)=0;
  virtual int GetFaceIds (int *numFaces, int **faceIds) = 0;
  virtual int GetFaceAttribute(char *attr,int faceid, char **value) = 0;
  virtual int SetFaceAttribute(char *attr,int faceid, char *value) = 0;
  virtual int GetRegionIds (int *numRegions, int **RegionIds) = 0;
  virtual int GetRegionAttribute(char *attr,int regionid, char **value) = 0;
  virtual int SetRegionAttribute(char *attr,int regionid, char *value) = 0;
  virtual int DeleteRegion (int regionid) = 0;

  // I/O:
  virtual int ReadNative( char *filename ) = 0;
  virtual int WriteNative( int file_version, char *filename ) const = 0;
  virtual int WriteVtkPolyData( char *filename ) = 0;
  virtual int WriteGeomSim( char *filename ) = 0;

  // geometric manipulation
  // this method is buggy and should be used with great care!
  virtual int DeleteFaces (int numfaces, int *faces) = 0;
  virtual int CreateEdgeBlend(int faceA, int faceB, double radius,
      int filletshape) = 0;
  virtual int CombineFaces (int targetface, int loseface) = 0;
  virtual int RemeshFace (int numfaces,int *excludedFaces, double size) = 0;

  virtual int SetVtkPolyDataObject(vtkPolyData *newPolyData) = 0;

protected:
  double tol_;

private:
  SolidModel_KernelT kernel_;

};

#endif // __SOLID_MODEL_H
