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
#include "GeomPlate_CurveConstraint.hxx"
#include "Geom_Surface.hxx"
#include "TopoDS_Edge.hxx"
#include "TopoDS_Wire.hxx"
#include "TopoDS_Vertex.hxx"
#include "TopoDS_Face.hxx"
#include "TopoDS_Compound.hxx"

#include "GeomAPI_Interpolate.hxx"
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
#include "BRepAdaptor_Curve.hxx"
#include "BRepFill_Filling.hxx"
#include "BRepTools_Quilt.hxx"
#include "BRep_Tool.hxx"

#include "IVtkOCC_Shape.hxx"
#include "IVtk_IShapeData.hxx"
#include "IVtk_IShapeMesher.hxx"
#include "IVtkVTK_ShapeData.hxx"
#include "IVtkOCC_ShapeMesher.hxx"

#include "TopExp_Explorer.hxx"
#include "Message_ProgressIndicator.hxx"
#include "GCPnts_AbscissaPoint.hxx"
#include "Adaptor3d_Curve.hxx"
#include "Adaptor3d_CurveOnSurface.hxx"
#include "Adaptor3d_HCurveOnSurface.hxx"
#include "BRepGProp.hxx"
#include "GProp_GProps.hxx"
#include "TColgp_SequenceOfXY.hxx"
#include "TColgp_SequenceOfXYZ.hxx"
#include "TColgp_HArray1OfPnt.hxx"
#include "GeomPlate_PlateG0Criterion.hxx"
#include "GeomPlate_MakeApprox.hxx"
#include "ShapeFix_FreeBounds.hxx"

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
  if (src.GetKernelT() != SM_KT_OCCT) {
    return CV_ERROR;
  }

  solidPtr = (cvOCCTSolidModel *)( &src );
  if ( solidPtr->geom_ != NULL ) {
    geom_ = new TopoDS_Shape;
    *geom_ = *(solidPtr->geom_);
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

  cvOCCTSolidModel *shapePtr;
  BRepOffsetAPI_ThruSections lofter(Standard_False,Standard_False,1e-6);
  if (continuity == 0)
    lofter.SetContinuity(GeomAbs_C0);
  else if (continuity == 1)
    lofter.SetContinuity(GeomAbs_G1);
  else if (continuity == 2)
    lofter.SetContinuity(GeomAbs_C1);
  else if (continuity == 3)
    lofter.SetContinuity(GeomAbs_G2);
  else if (continuity == 4)
    lofter.SetContinuity(GeomAbs_C2);
  else if (continuity == 5)
    lofter.SetContinuity(GeomAbs_C3);
  else
    lofter.SetContinuity(GeomAbs_CN);

  if (partype == 0)
    lofter.SetParType(Approx_ChordLength);
  else if (partype == 1)
    lofter.SetParType(Approx_Centripetal);
  else
    lofter.SetParType(Approx_IsoParametric);

  lofter.CheckCompatibility(Standard_False);
  //lofter.SetCriteriumWeight(w1,w2,w3);
  //lofter.SetSmoothing(Standard_True);

  fprintf(stderr,"Loft Continuity: %d\n",continuity);
  fprintf(stderr,"Loft Parameter: %d\n",partype);
  for ( int i = 0; i < numCurves; i++ ) {
    if ( curves[i]->GetKernelT() != SM_KT_OCCT ) {
      fprintf(stderr,"Solid kernel should be OCCT\n");
      return CV_ERROR;
    }
    shapePtr = (cvOCCTSolidModel *) curves[i];

    TopoDS_Wire newwire = TopoDS::Wire(*(shapePtr->geom_));
    lofter.AddWire(newwire);
  }
  lofter.Build();

  const Standard_Integer aNbIter = 5; //number of algorithm iterations
  const Standard_Integer aNbPnts = 5; //sample points per each constraint
  const Standard_Integer aDeg = 3; //requested surface degree ?
  const Standard_Integer aMaxDeg = 6;
  const Standard_Integer aMaxSeg = 10000;
  const Standard_Real aTol3d = 1.e-04;
  const Standard_Real aTol2d = 1.e-05;
  const Standard_Real anAngTol = 1.e-02; //angular
  const Standard_Real aCurvTol = 1.e-01; //curvature

  BRepFill_Filling surfacemaker;
  //GeomPlate_BuildPlateSurface aPlateBuilder(aDeg,aNbPnts,
  //      	  aNbIter,aTol2d,aTol3d,anAngTol,aCurvTol);
  //surfacemaker.LoadInitSurface(lofter.Shape());
  TopExp_Explorer EdgeExp;
  EdgeExp.Init(lofter.Shape(),TopAbs_EDGE);
  for (int i=0;EdgeExp.More();EdgeExp.Next(),i++)
  {
    fprintf(stdout,"Edge #%d\n",i);
    TopoDS_Edge tmpEdge = TopoDS::Edge(EdgeExp.Current());
    GProp_GProps lineProps;
    BRepGProp::LinearProperties(tmpEdge,lineProps);
    fprintf(stdout,"Edge Length %.4f\n",lineProps.Mass());
    fprintf(stdout,"Closed? %d\n",tmpEdge.Closed());
    if (lineProps.Mass() > 1.0)
    {
      //BRepAdaptor_Curve repCurve(tmpEdge);
      //repCurve.Initialize(tmpEdge);
      //fprintf(stdout,"Is Curve %d\n",repCurve.IsCurveOnSurface());
      //fprintf(stdout,"Is 3d %d\n",repCurve.Is3DCurve());
      //Adaptor3d_CurveOnSurface curve3d = repCurve.CurveOnSurface();
      //Handle(Adaptor3d_HCurveOnSurface) hcurve3d = 
      //        new Adaptor3d_HCurveOnSurface(curve3d); 
      //Handle(GeomPlate_CurveConstraint) aConst; 
      //aConst = new GeomPlate_CurveConstraint(hcurve3d,GeomAbs_C0,
      //  			    aNbPnts,aTol3d,anAngTol,aCurvTol);
      //if (continuity == 0)
      //{
      //  surfacemaker.Add(tmpEdge,GeomAbs_C0);
      //}
      //else if (continuity == 1)
      //{
      //  surfacemaker.Add(tmpEdge,GeomAbs_G1);
      //}
      //else if (continuity == 2)
      //{
      //  surfacemaker.Add(tmpEdge,GeomAbs_C1);
      //}
      //else if (continuity == 3)
      //{
      //  surfacemaker.Add(tmpEdge,GeomAbs_G2);
      //}
      //else if (continuity == 4)
      //{
      //  surfacemaker.Add(tmpEdge,GeomAbs_C2);
      //}
      //else if (continuity == 5)
      //{
      //  surfacemaker.Add(tmpEdge,GeomAbs_C3);
      //}
      //else
      //{
      //  surfacemaker.Add(tmpEdge,GeomAbs_CN);
      //}
      //aPlateBuilder.Add(aConst);
    }
  }
//TopoDS_Edge newedge1 = BRepBuilderAPI_MakeEdge(
//		gp_Pnt(0.0,0.0,0.0),
//		gp_Pnt(0.0,1.0,0.0));
//  surfacemaker.Add(newedge1,GeomAbs_C0);
//TopoDS_Edge newedge2 = BRepBuilderAPI_MakeEdge(
//		gp_Pnt(1.0,0.0,0.0),
//		gp_Pnt(1.0,1.0,0.0));
//  surfacemaker.Add(newedge2,GeomAbs_C0);
//  surfacemaker.Build();
  //aPlateBuilder.Perform();
  //Handle(Geom_Surface) aRes;
  //const Handle(GeomPlate_Surface)& aPlate = aPlateBuilder.Surface();
  //Standard_Real aDMax = aPlateBuilder.G0Error();
  //TColgp_SequenceOfXY aS2d;
  //TColgp_SequenceOfXYZ aS3d;
  //aPlateBuilder.Disc2dContour (4, aS2d);
  //aPlateBuilder.Disc3dContour (4, 0, aS3d);
  //Standard_Real aMax = Max (aTol3d, 10. * aDMax);
  //GeomPlate_PlateG0Criterion aCriterion (aS2d, aS3d, aMax);
  ////data races in AdvApp2Var used by GeomApprox_Surface, use global mutex
  //GeomPlate_MakeApprox aMakeApprox (aPlate, aCriterion, aTol3d, aMaxSeg, aMaxDeg);
  //aRes = aMakeApprox.Surface();
  //return aRes;

  //BRepBuilderAPI_MakeFace facemaker(surfacemaker.Face());
  //facemaker.Build();
  
  BRepBuilderAPI_Sewing attacher;
  attacher.Add(lofter.Shape());
  Standard_Real sewtoler =  1.e-6;
  Standard_Real closetoler =  1.e-2;
  ShapeFix_FreeBounds findFree(lofter.Shape(),sewtoler,closetoler,
		  Standard_False,Standard_False);
  TopoDS_Compound freeWires = findFree.GetClosedWires();
  TopExp_Explorer NewEdgeExp;
  NewEdgeExp.Init(freeWires,TopAbs_EDGE);
  for (int i=0;NewEdgeExp.More();NewEdgeExp.Next(),i++)
  {
    fprintf(stderr,"New Wire #%d\n",i);
    TopoDS_Edge tmpEdge = TopoDS::Edge(NewEdgeExp.Current());
    GProp_GProps lineProps;
    BRepGProp::LinearProperties(tmpEdge,lineProps);
    fprintf(stdout,"Edge Length %.4f\n",lineProps.Mass());
    fprintf(stdout,"Closed? %d\n",tmpEdge.Closed());

    BRepBuilderAPI_MakeWire wiremaker(tmpEdge);
    wiremaker.Build();

    BRepFill_Filling filler(3,15,2,Standard_False,0.00001,0.0001,0.01,0.1,8,9);
    filler.Add(tmpEdge,GeomAbs_C0,Standard_True);
    filler.Build();
    
    attacher.Add(filler.Face());
  }
  attacher.Perform();

  geom_ = new TopoDS_Shape;
  *geom_ = attacher.SewedShape();
  //*geom_ = lofter.Shape();

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
  
  if (geom_ != NULL)
    delete geom_;

  // We're assuming / requiring that the given cvPolyData describes a
  // closed loop.  Get those points in their connected order.
  if ( sys_geom_GetOrderedPts( pd, &ord_pts, &num_pts ) != CV_OK ) {
    return CV_ERROR;
  }

  if ( num_pts < 3 ) {
    delete [] ord_pts;
    return CV_ERROR;
  }

  //BRepBuilderAPI_MakeWire wiremaker;
  //BRepLib_MakePolygon poly;
  //int i=0;
  //for (i=0;i<num_pts-1;i++)
  //{ 
  //  TopoDS_Edge newedge = BRepBuilderAPI_MakeEdge(
  //      		gp_Pnt(ord_pts[3*i],ord_pts[3*i+1],ord_pts[3*i+2]),
  //      		gp_Pnt(ord_pts[3*i+3],ord_pts[3*i+4],ord_pts[3*i+5]));
  //  wiremaker.Add(newedge);
  //}
  //TopoDS_Edge lastedge = BRepBuilderAPI_MakeEdge(
  //    			gp_Pnt(ord_pts[3*i],ord_pts[3*i+1],ord_pts[3*i+2]),
  //      		gp_Pnt(ord_pts[0],ord_pts[1],ord_pts[2]));
  //wiremaker.Add(lastedge);
  //wiremaker.Build();

  //geom_ = new TopoDS_Shape;
  //*geom_ = wiremaker.Shape();
  
  Handle(TColgp_HArray1OfPnt) hArray =
	  new TColgp_HArray1OfPnt(1,num_pts+1);
  int i=0;
  for (i=0;i<num_pts;i++)
  { 
    hArray->SetValue(i+1,gp_Pnt(ord_pts[3*i],ord_pts[3*i+1],ord_pts[3*i+2]));
  }
  hArray->SetValue(i+1,gp_Pnt(ord_pts[0],ord_pts[1],ord_pts[2]));
  GeomAPI_Interpolate pointinterp(hArray,Standard_False,1.0e-6);
  pointinterp.Perform();
  Handle(Geom_BSplineCurve) newCurve = pointinterp.Curve();

  BRepBuilderAPI_MakeEdge edgemaker(newCurve);
  edgemaker.Build();

  BRepBuilderAPI_MakeWire wiremaker(edgemaker.Edge());
  wiremaker.Build();

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

// ------------
// GetFaceIds
// ------------
int cvOCCTSolidModel::GetFaceIds (int *numFaces, int **faceIds) {

  
  if (geom_ == NULL)
  {
    fprintf(stderr,"Solid is null\n");
    return CV_ERROR;
  }

  int num = 0;

  const TopoDS_Shape& aShape = *geom_;
  
  TopExp_Explorer anExp (aShape, TopAbs_FACE);
  for (; anExp.More(); anExp.Next()) {
   const TopoDS_Face& aFace = TopoDS::Face (anExp.Current());
   num++;
  }

  *numFaces = num;
  
  if (num == 0) return CV_ERROR;

  (*faceIds) = new int [num];

  TopExp_Explorer anExp2 (aShape, TopAbs_FACE);

  int j = 0;
  
  for (; anExp2.More(); anExp2.Next()) {
   const TopoDS_Face& aFace = TopoDS::Face (anExp2.Current());
   (*faceIds)[j] = aFace.HashCode(9999999999);
   //(*faceIds)[j] = j;
   j++;
  }

  return CV_OK;

}

// ---------------
// GetFacePolyData
// ---------------
cvPolyData *cvOCCTSolidModel::GetFacePolyData(int faceid, int useMaxDist, double max_dist) const
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"Solid is null\n");
    return CV_ERROR;
  }
  cvPolyData *result;
  vtkPolyData *pd;

  int i = 0;
  const TopoDS_Shape& aShape = *geom_;
  TopExp_Explorer anExp (aShape, TopAbs_FACE);

  int foundFace = 0;


  
  for (; anExp.More(); anExp.Next()) {
   const TopoDS_Face& aFace = TopoDS::Face (anExp.Current());
   int hashcode = aFace.HashCode(9999999999);
   //do something with aFace
   if (faceid == hashcode) {
     foundFace = 1;
     break;
   }
   i++;
  }

  if (!foundFace) {
    fprintf(stderr,"ERROR: face not found!\n");
    return CV_ERROR;
  }
  
  //const TopoDS_Face& useFace = TopoDS::Face (anExp.Current());
  
  IVtkOCC_Shape::Handle aShapeImpl = new IVtkOCC_Shape(anExp.Current());
  IVtkVTK_ShapeData::Handle aDataImpl = new IVtkVTK_ShapeData();
  IVtk_IShapeMesher::Handle aMesher = new IVtkOCC_ShapeMesher();
  aMesher->Build(aShapeImpl,aDataImpl);

  pd = vtkPolyData::New();
  pd->DeepCopy(aDataImpl->getVtkPolyData());

  result = new cvPolyData(pd);
  pd->Delete();
  return result;
}

int cvOCCTSolidModel::MakeEllipsoid( double r[], double ctr[])
{
  if (geom_ != NULL)
    delete geom_;

  gp_Pnt pnt1(0.0,0.0,1.0);
  gp_Pnt pnt2(0.0,1.0,1.0);
  gp_Pnt pnt3(1.0,1.0,0.0);
  gp_Pnt pnt4(1.0,0.0,0.0);

  Handle(TColgp_HArray1OfPnt) hArray =
	  new TColgp_HArray1OfPnt(1,5);
  hArray->SetValue(1,pnt1);
  hArray->SetValue(2,pnt2);
  hArray->SetValue(3,pnt3);
  hArray->SetValue(4,pnt4);
  hArray->SetValue(5,pnt1);

  GeomAPI_Interpolate pointinterp(hArray,Standard_False,1.0e-6);
  pointinterp.Perform();
  Handle(Geom_BSplineCurve) newCurve = pointinterp.Curve();

  BRepBuilderAPI_MakeEdge edgemaker(newCurve);
  edgemaker.Build();

  BRepBuilderAPI_MakeWire wiremaker(edgemaker.Edge());
  wiremaker.Build();

  BRepFill_Filling filler(3,15,2,Standard_False,0.00001,0.0001,0.01,0.1,8,9);
  filler.Add(edgemaker.Edge(),GeomAbs_C0,Standard_True);
  filler.Build();

  BRepBuilderAPI_MakeFace facemaker(filler.Face()); 
  facemaker.Add(wiremaker.Wire());
  facemaker.Build();

  geom_ = new TopoDS_Shape;
  *geom_ = facemaker.Shape();

  return CV_OK;
}
