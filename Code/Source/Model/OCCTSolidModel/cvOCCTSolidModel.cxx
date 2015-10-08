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

/** @file cvOCCTSolidModel.cxx
 *  @brief The implementations of functions in OCCTSolidModel
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  @note Most functions in class call functions in cv_polydatasolid_utils.
 */

#include "SimVascular.h" 

#include "cvOCCTSolidModel.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkMath.h"
#include "cv_get_tcl_interp_init.h"
#include "cv_polydatasolid_utils.h"
#include "cv_misc_utils.h"
#include "cv_sys_geom.h"
#include <string.h>
#include <assert.h>

#include "gp_Pnt.hxx"
#include "gp_Ax2.hxx"
#include "gp_Dir.hxx"
#include "gp_Vec.hxx"
#include "gp_Pln.hxx"
#include "gp_Circ.hxx"
#include "Geom_BezierCurve.hxx"
#include "TopoDS_Edge.hxx"
#include "TopoDS_Wire.hxx"
#include "TopoDS_Vertex.hxx"
#include "TopoDS_Face.hxx"

#include "BRepPrimAPI_MakeBox.hxx"
#include "BRepPrimAPI_MakeSphere.hxx"
#include "BRepPrimAPI_MakeCylinder.hxx"
#include "BRepBuilderAPI_MakeEdge.hxx"
#include "BRepBuilderAPI_MakeWire.hxx"
#include "BRepBuilderAPI_MakeFace.hxx"
#include "BRepBuilderAPI_MakeVertex.hxx"
#include "BRepBuilderAPI_Sewing.hxx"
#include "BRepOffsetAPI_MakePipe.hxx"
#include "BRepOffsetAPI_ThruSections.hxx"
#include "BRepLib_MakePolygon.hxx"
#include "BRepAlgoAPI_Fuse.hxx"
#include "BRepAlgoAPI_Common.hxx"
#include "BRepAlgoAPI_Cut.hxx"
#include "BRepTools_Quilt.hxx"

#include "IVtkOCC_Shape.hxx"
#include "IVtk_IShapeData.hxx"
#include "IVtk_IShapeMesher.hxx"
#include "IVtkVTK_ShapeData.hxx"
#include "IVtkOCC_ShapeMesher.hxx"

#include "TopExp_Explorer.hxx"
#include "Message_ProgressIndicator.hxx"

// ----------
// OCCTSolidModel
// ----------
/** 
 * @brief Constructor for OCCTSolidModel (Should never be called directly) 
 */

cvOCCTSolidModel::cvOCCTSolidModel()
  : cvSolidModel( SM_KT_OCCT)
{
/** 
 * @brief Data Member is a vtkPolyData. It is initiated as NULL. When a 
 * solid is loaded, a new PolyData is created
 */
  numBoundaryRegions = 0;
  geom_ = NULL;
  wire_ = NULL;
}

// -----------
// ~cvOCCTSolidModel
// -----------
/** 
 * @brief Destructor for cvOCCTSolidModel 
 */

cvOCCTSolidModel::~cvOCCTSolidModel() 
{
  if (geom_ != NULL)
    delete geom_;
  if (wire_ != NULL)
    delete wire_;
}

// -----------
// ~Copy( const cvOCCTSolidModel& sm)
// -----------
/** 
 * @brief Copy Constructor for cvOCCTSolidModel 
 */
cvOCCTSolidModel::cvOCCTSolidModel( const cvOCCTSolidModel& sm)
	: cvSolidModel( SM_KT_OCCT)
{
  geom_ = NULL;
  wire_ = NULL;
  Copy( sm );
}

// -----------
// ~Copy( const cvSolidModel& src )
// -----------
/** 
 * @brief Copy for cvSolidModel 
 */

int cvOCCTSolidModel::Copy(const cvSolidModel& src )
{
  cvOCCTSolidModel *solidPtr;

  if (geom_ != NULL) {
    return CV_ERROR;
  }
  if (wire_ != NULL) {
    return CV_ERROR;
  }
  if (src.GetKernelT() != SM_KT_OCCT) {
    return CV_ERROR;
  }

  solidPtr = (cvOCCTSolidModel *)( &src );
  if ( solidPtr->geom_ != NULL ) {
    geom_ = new TopoDS_Shape;
    *geom_ = *(solidPtr->geom_);
  }
  if ( solidPtr->wire_ != NULL ) {
    wire_ = new TopoDS_Wire;
    *wire_ = *(solidPtr->wire_);
  }

  return CV_OK;
}

// ----
// Copy
// ----

cvSolidModel *cvOCCTSolidModel::Copy() const
{
  cvOCCTSolidModel *result = new cvOCCTSolidModel(*this);
  return result;
}


// ----------
// MakeBox3d
// ----------

int cvOCCTSolidModel::MakeBox3d( double dims[], double ctr[])
{
  if(geom_ != NULL)
    delete geom_;

  double crn[3];
  crn[0] = ctr[0] - dims[0]/2.0;
  crn[1] = ctr[1] - dims[1]/2.0;
  crn[2] = ctr[2] - dims[2]/2.0;
  gp_Pnt corner(crn[0],crn[1],crn[2]);
  BRepPrimAPI_MakeBox boxmaker(corner,dims[0],dims[1],dims[2]);

  boxmaker.Build();
  geom_ = new TopoDS_Shape;
  *geom_ = boxmaker.Shape();

  return CV_OK;
}

// -----------
// MakeSphere
// -----------

int cvOCCTSolidModel::MakeSphere( double r, double ctr[])
{
  if (geom_ != NULL)
    delete geom_;

  gp_Pnt center(ctr[0],ctr[1],ctr[2]);
  BRepPrimAPI_MakeSphere spheremaker(center,r);

  spheremaker.Build();
  geom_ = new TopoDS_Shape;
  *geom_ = spheremaker.Shape();

  return CV_OK;
}

// -----------
// MakeCylinder
// -----------

int cvOCCTSolidModel::MakeCylinder( double r, double length, double ctr[],
    					double axis[])
{
  if (geom_ != NULL)
    delete geom_;

  double axiscopy[3]; axiscopy[0]=axis[0]; axiscopy[1]=axis[1]; axiscopy[2]=axis[2];
  vtkMath::Normalize(axiscopy);
  vtkMath::MultiplyScalar(axiscopy,length/2);
  gp_Pnt center(ctr[0]-axiscopy[0],ctr[1]-axiscopy[1],ctr[2]-axiscopy[2]);
  gp_Vec vector(axis[0],axis[1],axis[2]);
  gp_Dir direction(vector);
  gp_Ax2 cylaxis(center,direction);
  BRepPrimAPI_MakeCylinder cylindermaker(cylaxis,r,length);

  cylindermaker.Build();
  geom_ = new TopoDS_Shape;
  *geom_ = cylindermaker.Shape();

  return CV_OK;
}

// ------------
// MakeLoftedSurf
// ------------
int cvOCCTSolidModel::MakeLoftedSurf( cvSolidModel **curves, int numCurves, 
		char *name,int continuity,int partype,
		double w1,double w2,double w3)
{                              
  if (geom_ != NULL)
    delete geom_;                         

  if ( numCurves < 2 ) {
    return CV_ERROR;
  }

  cvOCCTSolidModel *wirePtr;
  BRepOffsetAPI_ThruSections lofter(Standard_False,Standard_False,1e-6);
  lofter.SetCriteriumWeight(w1,w2,w3);
  lofter.SetContinuity(continuity);
  lofter.SetParType(partype);
  lofter.SetSmoothing(Standard_True);

  fprintf(stderr,"Loft Continuity: %d\n",continuity);
  fprintf(stderr,"Loft Parameter: %d\n",partype);
  for ( int i = 0; i < numCurves; i++ ) {
    if ( curves[i]->GetKernelT() != SM_KT_OCCT ) {
      fprintf(stderr,"Solid kernel should be OCCT\n");
      return CV_ERROR;
    }
    wirePtr = (cvOCCTSolidModel *) curves[i];

    lofter.AddWire(*(wirePtr->wire_));
  }
  lofter.Build();
  TopExp_Explorer FaceExp;
  FaceExp.Init(lofter.Shape(),TopAbs_FACE);
  for (int i=0;FaceExp.More();FaceExp.Next(),i++)
  {
    TopoDS_Face tmpFace = TopoDS::Face(FaceExp.Current());
    //fprintf(stderr,"Face %d Orientation %d\n",i,tmpFace.Orientation());
  }

  geom_ = new TopoDS_Shape;
  *geom_ = lofter.Shape();

  //Standard_Real W1,W2,W3;
  //lofter.CriteriumWeight(W1,W2,W3);
  //fprintf(stderr,"Continuity Used: %d\n",lofter.Continuity());
  //fprintf(stderr,"Max Degree Used: %d\n",lofter.MaxDegree());
  //fprintf(stderr,"ParType Used: %d\n",lofter.ParType());
  //fprintf(stderr,"Weight Used: %.2f,%.2f,%.2f\n",W1,W2,W3);

  return CV_OK;
}

// ------------
// MakeInterpCurveLoop
// ------------
int cvOCCTSolidModel::MakeInterpCurveLoop( cvPolyData *pd, int closed)
{
  double *ord_pts;
  int num_pts;

  if ( wire_ != NULL ) {
    delete wire_;
  }

  // We're assuming / requiring that the given cvPolyData describes a
  // closed loop.  Get those points in their connected order.
  if ( sys_geom_GetOrderedPts( pd, &ord_pts, &num_pts ) != CV_OK ) {
    return CV_ERROR;
  }

  if ( num_pts < 3 ) {
    delete [] ord_pts;
    return CV_ERROR;
  }

  BRepBuilderAPI_MakeWire wiremaker;
  BRepLib_MakePolygon poly;
  int i=0;
  for (i=0;i<num_pts-1;i++)
  {
    TopoDS_Edge newedge = BRepBuilderAPI_MakeEdge(
        		gp_Pnt(ord_pts[3*i],ord_pts[3*i+1],ord_pts[3*i+2]),
        		gp_Pnt(ord_pts[3*i+3],ord_pts[3*i+4],ord_pts[3*i+5]));
    wiremaker.Add(newedge);
  }
  TopoDS_Edge lastedge = BRepBuilderAPI_MakeEdge(
      			gp_Pnt(ord_pts[3*i],ord_pts[3*i+1],ord_pts[3*i+2]),
        		gp_Pnt(ord_pts[0],ord_pts[1],ord_pts[2]));
  wiremaker.Add(lastedge);
  wiremaker.Build();

  wire_ = new TopoDS_Wire;
  *wire_ = wiremaker.Wire();
  geom_ = new TopoDS_Shape;
  *geom_ = wiremaker.Shape();

  return CV_OK;
}


// ------------
// Union
// ------------
int cvOCCTSolidModel::Union( cvSolidModel *a, cvSolidModel *b,
			      SolidModel_SimplifyT st)
{
  cvOCCTSolidModel *occtPtrA;
  cvOCCTSolidModel *occtPtrB;

  if (geom_ != NULL)
    return CV_ERROR;

  //Need both objects to create a union
  if (a == NULL)
    return CV_ERROR;
  if (a->GetKernelT() != SM_KT_OCCT ) {
    fprintf(stderr,"Model not of type OCCT\n");
    return CV_ERROR;
  }
  
  if (b == NULL)
    return CV_ERROR;
  if (b->GetKernelT() != SM_KT_OCCT ) {
    fprintf(stderr,"Model not of type OCCT\n");
    return CV_ERROR;
  }

  occtPtrA = (cvOCCTSolidModel *)( a );
  occtPtrB = (cvOCCTSolidModel *)( b );

  BRepAlgoAPI_Fuse unionOCCT(*(occtPtrA->geom_),*(occtPtrB->geom_));
  unionOCCT.Build();

  geom_ = new TopoDS_Shape;
  *geom_ = unionOCCT.Shape();

  return CV_OK;
}

// ------------
// Intersect
// ------------
int cvOCCTSolidModel::Intersect( cvSolidModel *a, cvSolidModel *b,
			      SolidModel_SimplifyT st)
{
  cvOCCTSolidModel *occtPtrA;
  cvOCCTSolidModel *occtPtrB;

  if (geom_ != NULL)
    return CV_ERROR;

  //Need both objects to create an intersection
  if (a == NULL)
    return CV_ERROR;
  if (a->GetKernelT() != SM_KT_OCCT ) {
    fprintf(stderr,"Model not of type OCCT\n");
    return CV_ERROR;
  }
  
  if (b == NULL)
    return CV_ERROR;
  if (b->GetKernelT() != SM_KT_OCCT ) {
    fprintf(stderr,"Model not of type OCCT\n");
    return CV_ERROR;
  }

  occtPtrA = (cvOCCTSolidModel *)( a );
  occtPtrB = (cvOCCTSolidModel *)( b );

  BRepAlgoAPI_Common intersectionOCCT(*(occtPtrA->geom_),*(occtPtrB->geom_));
  intersectionOCCT.Build();

  geom_ = new TopoDS_Shape;
  *geom_ = intersectionOCCT.Shape();

  return CV_OK;
}

// ------------
// Subtract
// ------------
int cvOCCTSolidModel::Subtract( cvSolidModel *a, cvSolidModel *b,
			      SolidModel_SimplifyT st)
{
  cvOCCTSolidModel *occtPtrA;
  cvOCCTSolidModel *occtPtrB;

  if (geom_ != NULL)
    return CV_ERROR;

  //Need both objects to create a subtraction
  if (a == NULL)
    return CV_ERROR;
  if (a->GetKernelT() != SM_KT_OCCT ) {
    fprintf(stderr,"Model not of type OCCT\n");
    return CV_ERROR;
  }
  
  if (b == NULL)
    return CV_ERROR;
  if (b->GetKernelT() != SM_KT_OCCT ) {
    fprintf(stderr,"Model not of type OCCT\n");
    return CV_ERROR;
  }

  occtPtrA = (cvOCCTSolidModel *)( a );
  occtPtrB = (cvOCCTSolidModel *)( b );

  BRepAlgoAPI_Cut subtractionOCCT(*(occtPtrA->geom_),*(occtPtrB->geom_));
  subtractionOCCT.Build();

  geom_ = new TopoDS_Shape;
  *geom_ = subtractionOCCT.Shape();

  return CV_OK;
}

// ------------
// GetPolyData
// ------------
cvPolyData *cvOCCTSolidModel::GetPolyData(int useMaxDist, double max_dist) const
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"Solid is null\n");
    return CV_ERROR;
  }
  cvPolyData *result;
  vtkPolyData *pd;

  IVtkOCC_Shape::Handle aShapeImpl = new IVtkOCC_Shape(*geom_);
  //IVtk_IShapeData::Handle aDataImpl = new IVtkVTK_ShapeData();
  IVtkVTK_ShapeData::Handle aDataImpl = new IVtkVTK_ShapeData();
  IVtk_IShapeMesher::Handle aMesher = new IVtkOCC_ShapeMesher();
  aMesher->Build(aShapeImpl,aDataImpl);

  pd = vtkPolyData::New();
  pd->DeepCopy(aDataImpl->getVtkPolyData());

  result = new cvPolyData(pd);
  pd->Delete();
  return result;
}
