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
 *  @note Most functions in class call functions in cv_occtsolid_utils.
 */

#include "SimVascular.h"
#include "cv_globals.h"

#include "cvOCCTSolidModel.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkThreshold.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkQuadricDecimation.h"
#include "vtkMath.h"
#include "cv_get_tcl_interp_init.h"
#include "cv_polydatasolid_utils.h"
#include "cv_occtsolid_utils.h"
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
#include "TopoDS_Compound.hxx"

#include "GeomAPI_Interpolate.hxx"
#include "BRepPrimAPI_MakeBox.hxx"
#include "BRepPrimAPI_MakeSphere.hxx"
#include "BRepPrimAPI_MakeCylinder.hxx"
#include "BRepBuilderAPI_MakeEdge.hxx"
#include "BRepBuilderAPI_MakeWire.hxx"
#include "BRepBuilderAPI_MakeFace.hxx"
#include "BRepBuilderAPI_MakeVertex.hxx"
#include "BRepBuilderAPI_MakeSolid.hxx"
#include "BRepBuilderAPI_Sewing.hxx"
#include "BRep_Builder.hxx"
#include "BRepOffsetAPI_MakePipe.hxx"
#include "BRepOffsetAPI_ThruSections.hxx"
#include "BRepLib_MakePolygon.hxx"
#include "BRepAlgoAPI_Fuse.hxx"
#include "BRepAlgoAPI_Common.hxx"
#include "BRepAlgoAPI_Cut.hxx"
#include "BRepAdaptor_Curve.hxx"
#include "BRepAdaptor_HCurve.hxx"
#include "BRepFill_Filling.hxx"
#include "BRepFill_CurveConstraint.hxx"
#include "BRepFilletAPI_MakeFillet.hxx"
#include "BRepTools_Quilt.hxx"
#include "BRepTools.hxx"
#include "BRepTools_ReShape.hxx"
#include "BRep_Tool.hxx"

#include "IVtkOCC_Shape.hxx"
#include "IVtk_IShapeData.hxx"
#include "IVtk_IShapeMesher.hxx"
#include "IVtkVTK_ShapeData.hxx"
#include "IVtkOCC_ShapeMesher.hxx"

#include "TopExp.hxx"
#include "TopExp_Explorer.hxx"
#include "TopTools_DataMapOfIntegerShape.hxx"
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
#include "ShapeFix_Shape.hxx"

//Doc stuff
#include "TDF_Label.hxx"
#include "TDF_LabelSequence.hxx"
#include "TDocStd_Document.hxx"
#include "TNaming_Builder.hxx"
#include "TNaming_Tool.hxx"
#include "TNaming_NamedShape.hxx"
#include "TDataStd_Integer.hxx"
#include "TDataStd_Real.hxx"
#include "TDataStd_Name.hxx"
#include "XCAFDoc_ShapeTool.hxx"
#include "XCAFDoc_DocumentTool.hxx"
#include "TDF_ChildIterator.hxx"
#include "STEPCAFControl_Writer.hxx"
#include "STEPCAFControl_Reader.hxx"


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

  //Get the document created inside of the manager
  Handle(TDocStd_Document) doc;
  gOCCTManager->GetDocument(1,doc);

  shapetool_ = XCAFDoc_DocumentTool::ShapeTool(doc->Main());
  ////Get the main part of the document
  //TDF_Label root = doc->Main();

  ////Create label
  shapelabel_ = NULL;
  //*shapelabel_ = root.NewChild();
  numFaces_ = 0;
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
    this->RemoveShape();
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

  Handle(TDocStd_Document) doc;
  gOCCTManager->GetDocument(1,doc);

  shapetool_ = XCAFDoc_DocumentTool::ShapeTool(doc->Main());

  solidPtr = (cvOCCTSolidModel *)( &src );
  if ( solidPtr->geom_ != NULL ) {
    this->NewShape();
    *geom_ = *(solidPtr->geom_);
    //RegisterShapeFaces();
    this->AddShape();

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
    this->RemoveShape();

  double crn[3];
  crn[0] = ctr[0] - dims[0]/2.0;
  crn[1] = ctr[1] - dims[1]/2.0;
  crn[2] = ctr[2] - dims[2]/2.0;
  gp_Pnt corner(crn[0],crn[1],crn[2]);
  BRepPrimAPI_MakeBox boxmaker(corner,dims[0],dims[1],dims[2]);

  boxmaker.Build();
  this->NewShape();
  *geom_ = boxmaker.Shape();
  this->AddShape();

  return CV_OK;
}

// -----------
// MakeSphere
// -----------

int cvOCCTSolidModel::MakeSphere( double r, double ctr[])
{
  if (geom_ != NULL)
    this->RemoveShape();

  gp_Pnt center(ctr[0],ctr[1],ctr[2]);
  BRepPrimAPI_MakeSphere spheremaker(center,r);

  spheremaker.Build();
  this->NewShape();
  *geom_ = spheremaker.Shape();
  this->AddShape();

  return CV_OK;
}

// -----------
// MakeCylinder
// -----------

int cvOCCTSolidModel::MakeCylinder( double r, double length, double ctr[],
    					double axis[])
{
  if (geom_ != NULL)
    this->RemoveShape();

  double axiscopy[3]; axiscopy[0]=axis[0]; axiscopy[1]=axis[1]; axiscopy[2]=axis[2];
  vtkMath::Normalize(axiscopy);
  vtkMath::MultiplyScalar(axiscopy,length/2);
  gp_Pnt center(ctr[0]-axiscopy[0],ctr[1]-axiscopy[1],ctr[2]-axiscopy[2]);
  gp_Vec vector(axis[0],axis[1],axis[2]);
  gp_Dir direction(vector);
  gp_Ax2 cylaxis(center,direction);
  BRepPrimAPI_MakeCylinder cylindermaker(cylaxis,r,length);

  cylindermaker.Build();
  this->NewShape();
  *geom_ = cylindermaker.Shape();
  this->AddShape();

  return CV_OK;
}

// ------------
// MakeLoftedSurf
// ------------
int cvOCCTSolidModel::MakeLoftedSurf( cvSolidModel **curves, int numCurves,
		char *name,int continuity,int partype,
		double w1,double w2,double w3,int smoothing)
{
  if (geom_ != NULL)
    this->RemoveShape();

  if ( numCurves < 2 ) {
    return CV_ERROR;
  }

  cvOCCTSolidModel *shapePtr;
  BRepOffsetAPI_ThruSections lofter(Standard_True,Standard_False,1e-6);
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
  //else
  //  lofter.SetContinuity(GeomAbs_CN);

  if (partype == 0)
    lofter.SetParType(Approx_ChordLength);
  else if (partype == 1)
    lofter.SetParType(Approx_Centripetal);
  else
    lofter.SetParType(Approx_IsoParametric);

  lofter.CheckCompatibility(Standard_False);
  lofter.SetSmoothing(smoothing);
  lofter.SetCriteriumWeight(w1,w2,w3);

  fprintf(stdout,"Loft Continuity: %d\n",continuity);
  fprintf(stdout,"Loft Parameter: %d\n",partype);
  for ( int i = 0; i < numCurves; i++ ) {
    if ( curves[i]->GetKernelT() != SM_KT_OCCT ) {
      fprintf(stderr,"Solid kernel should be OCCT\n");
      return CV_ERROR;
    }
    shapePtr = (cvOCCTSolidModel *) curves[i];

    TopoDS_Wire newwire = TopoDS::Wire(*(shapePtr->geom_));
    lofter.AddWire(newwire);
  }
  try
  {
    lofter.Build();
  }
  catch (Standard_Failure)
  {
    fprintf(stderr,"Failure in lofting\n");
    return CV_ERROR;
  }

  this->NewShape();
  //*geom_ = attacher.SewedShape();
  try
  {
    *geom_ = lofter.Shape();
  }
  catch (StdFail_NotDone)
  {
    fprintf(stderr,"Difficulty in lofting, try changing parameters\n");
    return CV_ERROR;
  }
  this->AddShape();

  //Standard_Real W1,W2,W3;
  //lofter.CriteriumWeight(W1,W2,W3);
  //fprintf(stderr,"Continuity Used: %d\n",lofter.Continuity());
  //fprintf(stderr,"Max Degree Used: %d\n",lofter.MaxDegree());
  //fprintf(stderr,"ParType Used: %d\n",lofter.ParType());
  //fprintf(stderr,"Weight Used: %.2f,%.2f,%.2f\n",W1,W2,W3);


  fprintf(stdout,"Lofting Vessel Done\n");
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
    this->RemoveShape();

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

  //this->NewShape();
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

  this->NewShape();
  *geom_ = wiremaker.Shape();
  this->AddShape();

  return CV_OK;
}

// ------------
// CapSurfToSolid
// ------------
int cvOCCTSolidModel::CapSurfToSolid( cvSolidModel *surf)
{
  cvOCCTSolidModel *solidPtr;
  solidPtr = (cvOCCTSolidModel *) surf;
  TopoDS_Shape shape = *(solidPtr->geom_);

  //BRepBuilderAPI_Sewing attacher;
  //attacher.Add(shape);
  //Standard_Real sewtoler =  1.e-6;
  //Standard_Real closetoler =  1.e-2;
  //ShapeFix_FreeBounds findFree(shape,sewtoler,closetoler,
  //      	  Standard_False,Standard_False);
  //TopoDS_Compound freeWires = findFree.GetClosedWires();
  //TopExp_Explorer NewEdgeExp;
  //NewEdgeExp.Init(freeWires,TopAbs_EDGE);
  //for (int i=0;NewEdgeExp.More();NewEdgeExp.Next(),i++)
  //{
  //  const Standard_Integer aNbIter = 12; //number of algorithm iterations
  //  const Standard_Integer aNbPnts = 5; //sample points per each constraint
  //  const Standard_Integer aDeg = 3; //requested surface degree ?
  //  const Standard_Integer aMaxDeg = 8;
  //  const Standard_Integer aMaxSeg = 9;
  //  const Standard_Real aTol3d = 1.e-04;
  //  const Standard_Real aTol2d = 1.e-05;
  //  const Standard_Real anAngTol = 1.e-02; //angular
  //  const Standard_Real aCurvTol = 1.e-02; //curvature

  //  TopoDS_Edge tmpEdge = TopoDS::Edge(NewEdgeExp.Current());
  //  BRepAdaptor_Curve adC(tmpEdge);
  //  Handle(BRepAdaptor_HCurve) aHAD =
  //    new BRepAdaptor_HCurve(adC);
  //  Handle(GeomPlate_CurveConstraint) aConst =
  //    new GeomPlate_CurveConstraint(aHAD,(Standard_Integer) GeomAbs_C0,aNbPnts,aTol3d,
  //        0.01,0.1);

  //  GeomPlate_BuildPlateSurface aPlateBuilder(aDeg,aNbPnts,
  //      	    aNbIter,aTol2d,aTol3d,anAngTol,aCurvTol);
  //  aPlateBuilder.Add(aConst);
  //  aPlateBuilder.Perform();

  //  Handle(GeomPlate_Surface) aPlSurf = aPlateBuilder.Surface();
  //  Standard_Real aDist = aPlateBuilder.G0Error();

  //  TColgp_SequenceOfXY S2d;
  //  TColgp_SequenceOfXYZ S3d;
  //  S2d.Clear();
  //  S3d.Clear();
  //  aPlateBuilder.Disc2dContour(4,S2d);
  //  aPlateBuilder.Disc3dContour(4,0,S3d);
  //  Standard_Real amaxTol = Max( aTol3d, 10* aDist);
  //  GeomPlate_PlateG0Criterion Criterion( S2d, S3d, amaxTol );
  //  GeomPlate_MakeApprox Approx( aPlSurf, Criterion, aTol3d, aMaxSeg, aMaxDeg );
  //  //aSurf = Approx.Surface();
  //  attacher.Add(Approx.Surface());
  //}
  //attacher.Perform();

  //Attacher!
  int numFilled=0;
  BRepBuilderAPI_Sewing attacher;
  attacher.Add(shape);
  Standard_Real sewtoler =  1.e-6;
  Standard_Real closetoler =  1.e-2;
  ShapeFix_FreeBounds findFree(shape,sewtoler,closetoler,
        	  Standard_False,Standard_False);
  TopoDS_Compound freeWires = findFree.GetClosedWires();
  TopExp_Explorer NewEdgeExp;
  NewEdgeExp.Init(freeWires,TopAbs_EDGE);
  for (int i=0;NewEdgeExp.More();NewEdgeExp.Next(),i++)
  {
    numFilled += 1;
    fprintf(stderr,"Num filled INside %d\n",numFilled);
    fprintf(stderr,"I am in here\n");
    TopoDS_Edge tmpEdge = TopoDS::Edge(NewEdgeExp.Current());
    GProp_GProps lineProps;
    BRepGProp::LinearProperties(tmpEdge,lineProps);

    BRepBuilderAPI_MakeWire wiremaker(tmpEdge);
    wiremaker.Build();

    BRepFill_Filling filler(3,15,2,Standard_False,0.00001,0.0001,0.01,0.1,8,9);
    filler.Add(tmpEdge,GeomAbs_C0,Standard_True);
    filler.Build();

    attacher.Add(filler.Face());
  }
  fprintf(stderr,"Num filled %d\n",numFilled);
  attacher.Perform();

  if (geom_ != NULL)
    this->RemoveShape();

  TopoDS_Shell tmpShell = TopoDS::Shell(attacher.SewedShape());
  BRepBuilderAPI_MakeSolid solidmaker(tmpShell);
  solidmaker.Build();

  this->NewShape();
  *geom_ = solidmaker.Solid();
  this->AddShape();

  //Name face as cap for the new surfaces
  int numFaces = 0;
  OCCTUtils_GetNumberOfFaces(*geom_,numFaces);
  TopExp_Explorer anExp(*geom_,TopAbs_FACE);
  fprintf(stderr,"Num faces %d\n",numFaces);
  fprintf(stderr,"Num filled %d\n",numFilled);
  fprintf(stderr,"Num after %d\n",numFaces-numFilled);
  for (int i=0;anExp.More();anExp.Next(),i++)
  {
    TopoDS_Face tmpFace = TopoDS::Face(anExp.Current());
    if (i >= (numFaces-numFilled))
    {
      fprintf(stderr,"Setting face name!\n");
      OCCTUtils_SetFaceAttribute(
	  tmpFace,shapetool_,*shapelabel_,"name","cap");
    }
  }

  int issue=0;
  if (OCCTUtils_CheckIsSolid(*geom_,issue) != CV_OK)
  {
    fprintf(stderr,"Shape is not solid after cap\n");
    return CV_ERROR;
  }

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
  TopoDS_Shape shapeA = *(occtPtrA->geom_);
  TopoDS_Shape shapeB = *(occtPtrB->geom_);

  BRepAlgoAPI_Fuse unionOCCT(shapeA,shapeB);
  unionOCCT.Build();

  this->NewShape();
  *geom_ = unionOCCT.Shape();
  this->AddShape();


  TopExp_Explorer anExp(shapeA,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next(),i++)
  {
    const TopTools_ListOfShape &modListA =
      unionOCCT.Modified(anExp.Current());
    fprintf(stderr,"Modified Extent %d\n",modListA.Extent());
    if (modListA.Extent() != 0)
    {
      fprintf(stderr,"Changed!\n");
      //Transfer Data
    }
  }
  anExp.Init(shapeB,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next(),i++)
  {
    const TopTools_ListOfShape &modListB =
      unionOCCT.Modified(anExp.Current());
    if (modListB.Extent() != 0)
    {
      fprintf(stderr,"Changed!\n");
      //Transfer Data
    }
  }
  TopTools_ListOfShape modListB = unionOCCT.Modified(unionOCCT.Shape2());
  fprintf(stderr,"Modified Extent %d\n",modListB.Extent());
  //OCCTUtils_RenumberFaces(*geom_,shapetool_,*shapelabel_);

  fprintf(stderr,"HAS GENERATED? %d\n",unionOCCT.HasGenerated());
  fprintf(stderr,"HAS MODIFIED? %d\n",unionOCCT.HasModified());
  fprintf(stderr,"HAS DELETED? %d\n",unionOCCT.HasDeleted());

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

  this->NewShape();
  *geom_ = intersectionOCCT.Shape();
  this->AddShape();

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

  this->NewShape();
  *geom_ = subtractionOCCT.Shape();
  this->AddShape();

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
  //Deviation Coefficient is 0.0001,Deviation Angle = 5rad,default is 12rad,
  //Do not generate u isoline and do not generate v isoline
  double devcoeff = 0.0001;
  double angcoeff = 20.0 * M_PI/180.0;
  int uIsoLine= 0;
  int vIsoLine= 0;
  IVtk_IShapeMesher::Handle aMesher = new IVtkOCC_ShapeMesher(
      devcoeff,angcoeff,uIsoLine,vIsoLine);
  aMesher->Build(aShapeImpl,aDataImpl);

  pd = vtkPolyData::New();
  pd->DeepCopy(aDataImpl->getVtkPolyData());
  //this->GetOnlyPD(pd);

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
  if (OCCTUtils_GetFaceIds(*geom_,shapetool_,*shapelabel_, numFaces,faceIds) != CV_OK)
  {
    fprintf(stderr,"Error in retrieving Ids\n");
    return CV_ERROR;
  }


  return CV_OK;

}

// ------------
// GetFaceAttribute
// ------------
int cvOCCTSolidModel::GetFaceAttribute (char *attr,int faceid,char **value)
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"solid is null\n");
    return CV_ERROR;
  }

  int found =0;
  TopExp_Explorer anExp(*geom_,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next(),i++)
  {
    TopoDS_Face tmpFace = TopoDS::Face(anExp.Current());
    int id=-1;
    OCCTUtils_GetFaceLabel(tmpFace,shapetool_,*shapelabel_,id);
    if (id == faceid)
    {
      if (OCCTUtils_GetFaceAttribute(tmpFace,shapetool_,*shapelabel_,attr,value) != CV_OK)
      {
	fprintf(stderr,"Could not get face attribute\n");
	return CV_ERROR;
      }
      found=1;
    }
  }

  if (found == 0)
  {
    fprintf(stderr,"Face not found on shape, so attribute cannot be found\n");
    return CV_ERROR;
  }

  return CV_OK;
}

// ------------
// SetFaceAttribute
// ------------
int cvOCCTSolidModel::SetFaceAttribute (char *attr,int faceid,char *value)
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"solid is null\n");
    return CV_ERROR;
  }

  int found =0;
  TopExp_Explorer anExp(*geom_,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next(),i++)
  {
    TopoDS_Face tmpFace = TopoDS::Face(anExp.Current());
    int id=-1;
    OCCTUtils_GetFaceLabel(tmpFace,shapetool_,*shapelabel_,id);
    if (id == faceid)
    {
      if (OCCTUtils_SetFaceAttribute(tmpFace,shapetool_,*shapelabel_,attr,value) != CV_OK)
      {
	fprintf(stderr,"Could not set face attribute\n");
	return CV_ERROR;
      }
      found=1;
    }
  }

  if (found == 0)
  {
    fprintf(stderr,"Face not found on shape, so attribute cannot be set\n");
    return CV_ERROR;
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
   TopoDS_Face aFace = TopoDS::Face (anExp.Current());
   //int hashcode = aFace.HashCode(9999999999);
   int idonface;
   OCCTUtils_GetFaceLabel(aFace,shapetool_,*shapelabel_,idonface);
   //do something with aFace
   if (faceid == idonface) {
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
  //IVtk_IShapeMesher::Handle aMesher = new IVtkOCC_ShapeMesher();
  //Deviation Coefficient is 0.0001,Deviation Angle = 5rad,default is 12rad,
  //Do not generate u isoline and do not generate v isoline
  double devcoeff = 0.0001;
  double angcoeff = 20.0 * M_PI/180.0;
  int uIsoLine= 0;
  int vIsoLine= 0;
  IVtk_IShapeMesher::Handle aMesher = new IVtkOCC_ShapeMesher(
      devcoeff,angcoeff,uIsoLine,vIsoLine);
  aMesher->Build(aShapeImpl,aDataImpl);

  pd = vtkPolyData::New();
  pd->DeepCopy(aDataImpl->getVtkPolyData());
  //this->GetOnlyPD(pd);

  result = new cvPolyData(pd);
  pd->Delete();
  return result;
}

int cvOCCTSolidModel::MakeEllipsoid( double r[], double ctr[])
{
  if (geom_ != NULL)
    this->RemoveShape();

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

  BRepFill_Filling filler(3,5,12,Standard_False,0.00001,0.0001,0.01,0.1,8,9);
  filler.Add(edgemaker.Edge(),GeomAbs_C0,Standard_True);
  filler.Build();

  BRepBuilderAPI_MakeFace facemaker(filler.Face());
  facemaker.Add(wiremaker.Wire());
  facemaker.Build();

  this->NewShape();
  *geom_ = facemaker.Shape();
  this->AddShape();

  return CV_OK;
}

// ---------------
// DeleteFaces
// ---------------
int cvOCCTSolidModel::DeleteFaces(int numfaces, int *faces )
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"Solid is null\n");
    return CV_ERROR;
  }

  int *deleteFace = new int[numFaces_];
  for (int i=0; i < numFaces_; i++) {
    deleteFace[i] = 0;
  }
  for (int i=0; i < numfaces; i++) {
    deleteFace[faces[i]] = 1;
  }

  const TopoDS_Shape& aShape = *geom_;
  TopExp_Explorer anExp (aShape, TopAbs_FACE);

  //BRepBuilderAPI_Sewing attacher;
  for (int i=0; anExp.More(); anExp.Next(),i++) {
   TopoDS_Face aFace = TopoDS::Face (anExp.Current());
   int faceId = -1;
   OCCTUtils_GetFaceLabel(aFace,shapetool_,*shapelabel_,faceId);
   //int hashcode = aFace.HashCode(9999999999);
   if (faceId == -1)
   {
     fprintf(stderr,"Face not found\n");
     return CV_ERROR;
   }
   if (deleteFace[faceId] == 1) {
     BRepTools_ReShape remover;
     remover.Remove(aFace,Standard_True);
     *geom_ = remover.Apply(*geom_,TopAbs_FACE);
     numFaces_--;
     //TDF_Label tmpLabel;
     //shapetool_->FindSubShape(*shapelabel_,aFace,tmpLabel);
     //shapetool_->RemoveShape(tmpLabel,Standard_True);
   }
  }
  //attacher.Perform();

  //this->RemoveShape();
  //this->NewShape();
  //*geom_ = attacher.SewedShape();
  //this->AddShape();

  delete [] deleteFace;
  return CV_OK;
}

// ---------------
// CreateEdgeBlend
// ---------------
int cvOCCTSolidModel::CreateEdgeBlend(int faceA, int faceB, double radius)
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"Solid is Null\n");
    return CV_ERROR;
  }

  TopTools_IndexedDataMapOfShapeListOfShape anEFsMap;
  TopExp::MapShapesAndAncestors (*geom_, TopAbs_EDGE,
      TopAbs_FACE, anEFsMap);
  int num = anEFsMap.Extent();
  int found = 0;
  fprintf(stderr,"Extent! %d\n",num);
  TopoDS_Edge filletEdge;
  for (int i=1;i < num+1;i++)
  {
    TopTools_ListOfShape faces = anEFsMap.FindFromIndex(i);
    TopoDS_Shape face1 = faces.First();
    int faceId1,faceId2;
    OCCTUtils_GetFaceLabel(face1,shapetool_,*shapelabel_,faceId1);
    TopoDS_Shape face2 = faces.Last();
    OCCTUtils_GetFaceLabel(face2,shapetool_,*shapelabel_,faceId2);
    if (faceId1 == faceA && faceId2 == faceB)
    {
      found++;
      filletEdge = TopoDS::Edge(anEFsMap.FindKey(i));
    }
  }
  if (found != 1)
  {
    fprintf(stderr,"Single edge between faces found\n");
    return CV_ERROR;
  }

  BRepFilletAPI_MakeFillet filletmaker(*geom_);
  filletmaker.Add(radius,filletEdge);
  TopoDS_Shape tmpShape;
  try
  {
    filletmaker.Build();
  }
  catch (Standard_Failure)
  {
    fprintf(stderr,"Try different radius\n");
    return CV_ERROR;
  }
  try
  {
    tmpShape = filletmaker.Shape();
  }
  catch (StdFail_NotDone)
  {
    fprintf(stderr,"Try different radius\n");
    return CV_ERROR;
  }
  TopTools_ListOfShape modfaces = filletmaker.Modified(*geom_);
  fprintf(stderr,"Modified? %d\n",modfaces.Extent());
  TopTools_ListOfShape newfaces = filletmaker.Generated(*geom_);
  fprintf(stderr,"Generated? %d\n",newfaces.Extent());
  this->RemoveShape();
  this->NewShape();
  *geom_ = tmpShape;
  this->AddShape();
  TopExp_Explorer FaceExp;
  FaceExp.Init(*geom_,TopAbs_FACE);
  for (int i=0;FaceExp.More();FaceExp.Next(),i++)
  {
    fprintf(stderr,"New Face %d\n",i);
    TopoDS_Face tmpFace = TopoDS::Face(FaceExp.Current());
    int newid=-1;
    OCCTUtils_GetFaceLabel(tmpFace,shapetool_,*shapelabel_,newid);
    if (newid == -1)
    {
      AddFaceLabel(tmpFace,i);
      fprintf(stderr,"Giving label to face doh\n");
    }
  }
  if (OCCTUtils_RenumberFaces(*geom_,shapetool_,*shapelabel_) != CV_OK)
  {
    fprintf(stderr,"Renumbering did not work\n");
    return CV_ERROR;
  }
  FaceExp.Init(*geom_,TopAbs_FACE);
  for (int i=0;FaceExp.More();FaceExp.Next(),i++)
  {
    fprintf(stderr,"New Face %d\n",i);
    TopoDS_Face tmpFace = TopoDS::Face(FaceExp.Current());
    int newid=-1;
    OCCTUtils_GetFaceLabel(tmpFace,shapetool_,*shapelabel_,newid);
    fprintf(stderr,"ID %d\n",newid);
  }

  return CV_OK;
}


// ---------------
// ReadNative
// ---------------
int cvOCCTSolidModel::ReadNative( char *filename )
{
  if (geom_ != NULL)
    this->RemoveShape();

  const char *extension = strrchr(filename,'.');
  extension = extension+1;

  Handle(Message_ProgressIndicator) progress;
  BRep_Builder builder;
  this->NewShape();
  if (!strncmp(extension,"brep",4)) {
    fprintf(stdout,"Reading file %s\n",filename);
    Standard_Boolean worked =
      BRepTools::Read(*geom_,filename,builder,progress);
    if (worked = Standard_True)
      fprintf(stdout,"File read\n");
    else
    {
      fprintf(stderr,"File was not read\n");
      return CV_ERROR;
    }
    this->AddShape();
  }
  else if (!strncmp(extension,"step",4)) {
    Handle(TDocStd_Document) aDoc;
    gOCCTManager->GetDocument(1,aDoc);
    fprintf(stdout,"Reading file %s\n",filename);
    STEPCAFControl_Reader reader;
    reader.Perform(filename,aDoc);
    shapetool_ = XCAFDoc_DocumentTool::ShapeTool(aDoc->Main());
    TDF_LabelSequence allLabels;
    shapetool_->GetShapes(allLabels);
    *shapelabel_ = allLabels.First();
    *geom_ = shapetool_->GetShape(*shapelabel_);
    this->RegisterShapeFaces();
  }
  else {
    fprintf(stderr,"File can only be read with .brep extension\n");
    return CV_ERROR;
  }

  return CV_OK;
}

// ---------------
// WriteNative
// ---------------
int cvOCCTSolidModel::WriteNative(int file_version, char *filename ) const
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"Need geometry to write file\n");
    return CV_ERROR;
  }
  const char *extension = strrchr(filename,'.');
  extension = extension+1;

  Handle(Message_ProgressIndicator) progress;
  if (!strncmp(extension,"brep",4)) {
    fprintf(stdout,"Writing file %s\n",filename);
    Standard_Boolean worked =
      BRepTools::Write(*geom_,filename,progress);
    if (worked = Standard_True)
      fprintf(stdout,"File written\n");
    else
    {
      fprintf(stderr,"File was not written\n");
      return CV_ERROR;
    }
  }
  else if (!strncmp(extension,"step",4)) {
    Handle(TDocStd_Document) aDoc;
    gOCCTManager->GetDocument(1,aDoc);
    fprintf(stdout,"Writing file %s\n",filename);
    STEPCAFControl_Writer writer;
    writer.Perform(aDoc,filename);
  }
  else {
    fprintf(stderr,"File can only be written with .brep extension\n");
    return CV_ERROR;
  }

  return CV_OK;
}

// ---------------
// RegisterShapeFaces
// ---------------
int cvOCCTSolidModel::RegisterShapeFaces()
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"Geometry is NULL, cannot register faces\n");
    return CV_ERROR;
  }
  TopExp_Explorer FaceExp;
  FaceExp.Init(*geom_,TopAbs_FACE);
  for (int i=0;FaceExp.More();FaceExp.Next(),i++)
  {
    TopoDS_Face tmpFace = TopoDS::Face(FaceExp.Current());
    int faceid=i;
    AddFaceLabel(tmpFace,faceid);
  }

  return CV_OK;
}

// ---------------
// AddFaceLabel
// ---------------
//topotype face is 0
//topotype edge is 1
int cvOCCTSolidModel::AddFaceLabel(TopoDS_Shape &shape, int &id)
{
  if (shape.IsNull())
  {
    fprintf(stderr,"Face is NULL, cannot add\n");
    return CV_ERROR;
  }
  int checkid=-1;
  OCCTUtils_GetFaceLabel(shape,shapetool_,*shapelabel_,checkid);
  if (checkid != -1)
  {
    if (OCCTUtils_ReLabelFace(shape,shapetool_,*shapelabel_,id) != CV_OK)
    {
      fprintf(stderr,"Face has no label, which doesn't make sense\n");
      return CV_ERROR;
    }
    return CV_OK;
  }
  ////Create or find child label with a tag
  //TDF_Label topoLabels=shapelabel_->FindChild(topotype,Standard_True);

  ////Get new label for topo (face,edge)
  TDF_Label tmpLabel = shapetool_->AddSubShape(*shapelabel_,shape);
  numFaces_++;

  //Register the shape
  if (tmpLabel.IsNull())
  {
    fprintf(stderr,"Face is not part of shape\n");
    return CV_ERROR;
  }
  TNaming_Builder builder(tmpLabel);
  builder.Generated(shape);

  //Adding sub label for integer ids
  TDF_Label idLabel = tmpLabel.FindChild(0);
  TNaming_Builder idBuilder(idLabel);
  idBuilder.Generated(shape);
  Handle(TDataStd_Integer) INT = new TDataStd_Integer();
  idLabel.AddAttribute(INT);
  TDataStd_Name::Set(idLabel,"ID");
  INT->Set(id);

  //Adding sub label for face name
  TDF_Label nameLabel = tmpLabel.FindChild(1);
  TNaming_Builder nameBuilder(nameLabel);
  nameBuilder.Generated(shape);
  Handle(TDataStd_ExtStringArray) NSTRING = new TDataStd_ExtStringArray();
  nameLabel.AddAttribute(NSTRING);
  TDataStd_Name::Set(idLabel,"NAME");
  OCCTUtils_SetExtStringArrayFromChar(NSTRING,"noname");

  //Adding sub label for parent name
  TDF_Label parentLabel = tmpLabel.FindChild(2);
  TNaming_Builder parentBuilder(parentLabel);
  parentBuilder.Generated(shape);
  Handle(TDataStd_ExtStringArray) PSTRING = new TDataStd_ExtStringArray();
  parentLabel.AddAttribute(PSTRING);
  TDataStd_Name::Set(idLabel,"PARENT");
  OCCTUtils_SetExtStringArrayFromChar(PSTRING,"noparent");

  return CV_OK;
}

// ---------------
// NewShape
// ---------------
int cvOCCTSolidModel::NewShape()
{
  if (geom_ != NULL)
    this->RemoveShape();

  geom_ = new TopoDS_Shape;
  shapelabel_ = new TDF_Label;

  return CV_OK;
}

// ---------------
// AddShape
// ---------------
int cvOCCTSolidModel::AddShape()
{
  if (geom_ == NULL)
    return CV_ERROR;

  *shapelabel_ = shapetool_->NewShape();
  shapetool_->SetShape(*shapelabel_,*geom_);
  this->RegisterShapeFaces();

  return CV_OK;
}

// ---------------
// RemoveShape
// ---------------
int cvOCCTSolidModel::RemoveShape() const
{
  if (geom_ != NULL)
  {
    if (shapelabel_->IsNull())
    {
      fprintf(stderr,"Shape was not regsitered, cannot remove\n");
      return CV_ERROR;
    }
    //Want to remove shape, but shape isn't being copied over correctly
    //Full shape info for new shape cannot be retrieved if removed currently
    //Will try some other stuff
    //shapetool_->RemoveShape(*shapelabel_,Standard_False);
    delete shapelabel_;
    delete geom_;
  }
  else
  {
      fprintf(stderr,"Shape has already been deleted, cannot remove\n");
      return CV_ERROR;
  }

  return CV_OK;
}

// ---------------
// GetOnlyPD
// ---------------
int cvOCCTSolidModel::GetOnlyPD(vtkPolyData *pd) const
{
  if (pd == NULL)
    return CV_ERROR;

  if (PlyDtaUtils_PDCheckArrayName(pd,1,"MESH_TYPES"))
  {
    vtkSmartPointer<vtkThreshold> thresholder =
      vtkSmartPointer<vtkThreshold>::New();
    thresholder->SetInputData(pd);
    //Set Input Array to 0 port,0 connection,1 for Cell Data, and MESh_TYPES is the type name
    thresholder->SetInputArrayToProcess(0,0,0,1,"MESH_TYPES");
    //Source polydata is on MESH_TYPE 7
    thresholder->ThresholdBetween(7,7);
    thresholder->Update();
    //Extract surface
    vtkSmartPointer<vtkDataSetSurfaceFilter> surfacer =
      vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
    surfacer->SetInputData(thresholder->GetOutput());
    surfacer->Update();
    //For polydata of just edges, MESH_TYPE is not 7
    //Only want if not edges
    if (surfacer->GetOutput()->GetNumberOfPoints() != 0)
    {
      vtkSmartPointer<vtkQuadricDecimation> decimator =
	vtkSmartPointer<vtkQuadricDecimation>::New();
      decimator->SetInputData(surfacer->GetOutput());
      decimator->SetTargetReduction(0.8);
      decimator->GetOutput();
      pd->DeepCopy(decimator->GetOutput());
    }
  }

  return CV_OK;
}

