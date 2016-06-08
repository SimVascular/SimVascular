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
#include "Geom_BSplineSurface.hxx"
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
#include "BRepBuilderAPI_MakeShell.hxx"
#include "BRepBuilderAPI_MakeVertex.hxx"
#include "BRepBuilderAPI_MakeSolid.hxx"
#include "BRepBuilderAPI_Copy.hxx"
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
#include "ShapeFix_Shell.hxx"
#include "ShapeFix_FreeBounds.hxx"

#include "IVtkOCC_Shape.hxx"
#include "IVtk_IShapeData.hxx"
#include "IVtk_IShapeMesher.hxx"
#include "IVtkVTK_ShapeData.hxx"
#include "IVtkOCC_ShapeMesher.hxx"

#include "TopExp.hxx"
#include "TopExp_Explorer.hxx"
#include "TopTools_DataMapOfIntegerShape.hxx"
#include "TopTools_ListIteratorOfListOfShape.hxx"
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
#include "TColgp_Array2OfPnt.hxx"
#include "TColStd_Array1OfReal.hxx"
#include "TColStd_Array1OfInteger.hxx"
#include "GeomPlate_PlateG0Criterion.hxx"
#include "GeomPlate_MakeApprox.hxx"

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
#include "XCAFApp_Application.hxx"
#include "XCAFDoc_ShapeTool.hxx"
#include "XCAFDoc_DocumentTool.hxx"
#include "TDF_ChildIterator.hxx"
#include "STEPControl_StepModelType.hxx"
#include "STEPCAFControl_Writer.hxx"
#include "STEPCAFControl_Reader.hxx"
#include "IGESCAFControl_Writer.hxx"
#include "IGESCAFControl_Reader.hxx"
#include "StlAPI_Writer.hxx"


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
  Handle(XCAFApp_Application) OCCTManager =
    static_cast<XCAFApp_Application*>(gOCCTManager);
  OCCTManager->GetDocument(1,doc);

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
  Handle(XCAFApp_Application) OCCTManager =
    static_cast<XCAFApp_Application*>(gOCCTManager);
  OCCTManager->GetDocument(1,doc);

  shapetool_ = XCAFDoc_DocumentTool::ShapeTool(doc->Main());

  solidPtr = (cvOCCTSolidModel *)( &src );
  if ( solidPtr->geom_ != NULL ) {
    this->NewShape();
    BRepBuilderAPI_Copy copier(*(solidPtr->geom_));
    *geom_ = copier.Shape();
    this->AddShape();
    TopExp_Explorer oldExp(*(solidPtr->geom_),TopAbs_FACE);
    TopExp_Explorer newExp(*geom_,TopAbs_FACE);

    for (int i=0;oldExp.More();oldExp.Next(),newExp.Next(),i++)
    {
      TopoDS_Face oldFace = TopoDS::Face(oldExp.Current());
      TopoDS_Face newFace = TopoDS::Face(newExp.Current());

      if (OCCTUtils_PassFaceAttributes(oldFace,newFace,shapetool_,
	    *(solidPtr->shapelabel_),*shapelabel_) != CV_OK)
      {
	fprintf(stderr,"Could not pass face attributes to copy\n");
	return CV_ERROR;
      }
    }

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

// ----
// Print
// ----

void cvOCCTSolidModel::Print() const
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"No geometry in model\n");
  }
  fprintf(stdout,"---------------------------------------------------------\n");
  fprintf(stdout,"Model Info\n");
  fprintf(stdout,"Number of Faces: %s\n","TODO");
  fprintf(stdout,"Number of Edges: %s\n","TODO");
  fprintf(stdout,"TopoDS_Shape Info\n");
  fprintf(stdout,"Orientation: %d\n",geom_->Orientation());
  fprintf(stdout,"Shape Type:  %d\n",geom_->ShapeType());
  fprintf(stdout,"Free:        %d\n",geom_->Free());
  fprintf(stdout,"Locked:      %d\n",geom_->Locked());
  fprintf(stdout,"Modified:    %d\n",geom_->Modified());
  fprintf(stdout,"Orientable:  %d\n",geom_->Orientable());
  fprintf(stdout,"Closed:      %d\n",geom_->Closed());
  fprintf(stdout,"Infinite:    %d\n",geom_->Infinite());
  fprintf(stdout,"Convex:      %d\n",geom_->Convex());
  fprintf(stdout,"---------------------------------------------------------\n");
  return;
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

  //if (geom_ != NULL)
  //  this->RemoveShape();

  //if ( numCurves < 2 ) {
  //  return CV_ERROR;
  //}

  //cvOCCTSolidModel *shapePtr;
  //BRepOffsetAPI_ThruSections lofter(Standard_False,Standard_False,1e-6);
  //if (continuity == 0)
  //  lofter.SetContinuity(GeomAbs_C0);
  //else if (continuity == 1)
  //  lofter.SetContinuity(GeomAbs_G1);
  //else if (continuity == 2)
  //  lofter.SetContinuity(GeomAbs_C1);
  //else if (continuity == 3)
  //  lofter.SetContinuity(GeomAbs_G2);
  //else if (continuity == 4)
  //  lofter.SetContinuity(GeomAbs_C2);
  //else if (continuity == 5)
  //  lofter.SetContinuity(GeomAbs_C3);
  ////else
  ////  lofter.SetContinuity(GeomAbs_CN);

  //if (partype == 0)
  //  lofter.SetParType(Approx_ChordLength);
  //else if (partype == 1)
  //  lofter.SetParType(Approx_Centripetal);
  //else
  //  lofter.SetParType(Approx_IsoParametric);

  //lofter.SetMaxDegree(2);
  //lofter.CheckCompatibility(Standard_False);
  //lofter.SetSmoothing(smoothing);
  //lofter.SetCriteriumWeight(w1,w2,w3);

  //fprintf(stdout,"Loft Continuity: %d\n",continuity);
  //fprintf(stdout,"Loft Parameter: %d\n",partype);
  //for ( int i = 0; i < numCurves; i++ ) {
  //  if ( curves[i]->GetKernelT() != SM_KT_OCCT ) {
  //    fprintf(stderr,"Solid kernel should be OCCT\n");
  //    return CV_ERROR;
  //  }
  //  shapePtr = (cvOCCTSolidModel *) curves[i];

  //  TopoDS_Wire newwire = TopoDS::Wire(*(shapePtr->geom_));
  //  lofter.AddWire(newwire);
  //}
  //try
  //{
  //  lofter.Build();
  //}
  //catch (Standard_Failure)
  //{
  //  fprintf(stderr,"Failure in lofting\n");
  //  return CV_ERROR;
  //}

  //this->NewShape();
  ////*geom_ = attacher.SewedShape();
  //try
  //{
  //  *geom_ = lofter.Shape();
  //}
  //catch (StdFail_NotDone)
  //{
  //  fprintf(stderr,"Difficulty in lofting, try changing parameters\n");
  //  return CV_ERROR;
  //}
  //this->AddShape();

  //return CV_OK;

  if (geom_ != NULL)
    this->RemoveShape();

  if ( numCurves < 2 ) {
    return CV_ERROR;
  }

  cvOCCTSolidModel *shapePtr;
  TopoDS_Wire *bcurves;
  bcurves = new TopoDS_Wire[numCurves];
  for (int i =0; i<numCurves;i++)
  {
    if ( curves[i]->GetKernelT() != SM_KT_OCCT )
    {
      delete [] bcurves;
      fprintf(stderr,"Curves must be of type OCCT\n");
      return CV_ERROR;
    }
    shapePtr = (cvOCCTSolidModel *) curves[i];
    bcurves[i] = TopoDS::Wire(*(shapePtr->geom_));
  }
  this->NewShape();
  if (OCCTUtils_MakeLoftedSurf(bcurves,*geom_,numCurves,continuity,partype,
        		  w1,w2,w3,smoothing) != CV_OK)
  {
    delete [] bcurves;
    fprintf(stderr,"Error while lofting surface\n");
    return CV_ERROR;
  }

  this->AddShape();
  //Name faces
  TopExp_Explorer anExp(*geom_,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next(),i++)
  {
    TopoDS_Face tmpFace = TopoDS::Face(anExp.Current());
    if (i == 0)
      OCCTUtils_SetFaceAttribute(tmpFace,shapetool_,*shapelabel_,"gdscName","wall");
    else
      OCCTUtils_SetFaceAttribute(tmpFace,shapetool_,*shapelabel_,"gdscName","cap");
  }

  fprintf(stdout,"Lofting Vessel Done\n");
  delete [] bcurves;
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

  if (geom_ != NULL)
    this->RemoveShape();

  int numFilled=0;
  BRepBuilderAPI_Sewing attacher;
  this->NewShape();
  if (OCCTUtils_CapShapeToSolid(shape,*geom_,attacher,numFilled) != CV_OK)
  {
    fprintf(stderr,"Error capping shape\n");
    return CV_ERROR;
  }
  this->AddShape();


  //Name face as cap for the new surfaces
  int numFaces = 0;
  OCCTUtils_GetNumberOfFaces(*geom_,numFaces);
  TopExp_Explorer anExp(*geom_,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next(),i++)
  {
    TopoDS_Face tmpFace = TopoDS::Face(anExp.Current());
    if (i >= (numFaces-numFilled))
    {
      OCCTUtils_SetFaceAttribute(
	  tmpFace,shapetool_,*shapelabel_,"gdscName","cap");
    }
    else if (attacher.IsModified(shape))
    {
      GProp_GProps tmpFaceProps;
      BRepGProp::LinearProperties(tmpFace,tmpFaceProps);
      TopExp_Explorer faceExp(shape,TopAbs_FACE);
      for (int j=0;faceExp.More();faceExp.Next(),j++)
      {
	TopoDS_Face daFace = TopoDS::Face(faceExp.Current());
        GProp_GProps daFaceProps;
        BRepGProp::LinearProperties(tmpFace,daFaceProps);
	if (tmpFaceProps.Mass() == daFaceProps.Mass())
          OCCTUtils_PassFaceAttributes(daFace,tmpFace,shapetool_,
	      *shapelabel_,*shapelabel_);
      }
    }
  }

  int issue=0;
  if (OCCTUtils_CheckIsSolid(*geom_,issue) != CV_OK)
  {
    fprintf(stderr,"Shape is not solid after cap\n");
    return CV_ERROR;
  }
  if (issue != 0)
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

  //Transfer modified shape info from input A
  TopExp_Explorer anExp(shapeA,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next(),i++)
  {
    const TopTools_ListOfShape &modListA =
      unionOCCT.Modified(anExp.Current());
    TopoDS_Face oldFace = TopoDS::Face(anExp.Current());
    if (modListA.Extent() != 0)
    {
      TopTools_ListIteratorOfListOfShape modFaceIt(modListA);
      for (int j=0;modFaceIt.More();modFaceIt.Next(),j++)
      {
        TopoDS_Face newFace = TopoDS::Face(modFaceIt.Value());
	if (OCCTUtils_PassFaceAttributes(oldFace,newFace,
	      shapetool_,*shapelabel_,*shapelabel_) != CV_OK)
	{
	  fprintf(stderr,"Could not pass face info\n");
	  return CV_ERROR;
	}
      }
    }
  }

  //Transfer modified shape info from input B
  anExp.Init(shapeB,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next(),i++)
  {
    const TopTools_ListOfShape &modListB =
      unionOCCT.Modified(anExp.Current());
    TopoDS_Face oldFace = TopoDS::Face(anExp.Current());
    if (modListB.Extent() != 0)
    {
      TopTools_ListIteratorOfListOfShape modFaceIt(modListB);
      for (int j=0;modFaceIt.More();modFaceIt.Next(),j++)
      {
        TopoDS_Face newFace = TopoDS::Face(modFaceIt.Value());
	if (OCCTUtils_PassFaceAttributes(oldFace,newFace,
	      shapetool_,*shapelabel_,*shapelabel_) != CV_OK)
	{
	  fprintf(stderr,"Could not pass face info\n");
	  return CV_ERROR;
	}
      }
    }
  }

  //OCCTUtils_RenumberFaces(*geom_,shapetool_,*shapelabel_);

  //fprintf(stderr,"HAS GENERATED? %d\n",unionOCCT.HasGenerated());
  //fprintf(stderr,"HAS MODIFIED? %d\n",unionOCCT.HasModified());
  //fprintf(stderr,"HAS DELETED? %d\n",unionOCCT.HasDeleted());

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
  TopoDS_Shape shapeA = *(occtPtrA->geom_);
  TopoDS_Shape shapeB = *(occtPtrB->geom_);

  BRepAlgoAPI_Common intersectionOCCT(shapeA,shapeB);
  intersectionOCCT.Build();

  this->NewShape();
  *geom_ = intersectionOCCT.Shape();
  this->AddShape();

  //Transfer modified shape info from input A
  TopExp_Explorer anExp(shapeA,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next(),i++)
  {
    const TopTools_ListOfShape &modListA =
      intersectionOCCT.Modified(anExp.Current());
    TopoDS_Face oldFace = TopoDS::Face(anExp.Current());
    if (modListA.Extent() != 0)
    {
      TopTools_ListIteratorOfListOfShape modFaceIt(modListA);
      for (int j=0;modFaceIt.More();modFaceIt.Next(),j++)
      {
        TopoDS_Face newFace = TopoDS::Face(modFaceIt.Value());
	if (OCCTUtils_PassFaceAttributes(oldFace,newFace,
	      shapetool_,*shapelabel_,*shapelabel_) != CV_OK)
	{
	  fprintf(stderr,"Could not pass face info\n");
	  return CV_ERROR;
	}
      }
    }
  }

  //Transfer modified shape info from input B
  anExp.Init(shapeB,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next(),i++)
  {
    const TopTools_ListOfShape &modListB =
      intersectionOCCT.Modified(anExp.Current());
    TopoDS_Face oldFace = TopoDS::Face(anExp.Current());
    if (modListB.Extent() != 0)
    {
      TopTools_ListIteratorOfListOfShape modFaceIt(modListB);
      for (int j=0;modFaceIt.More();modFaceIt.Next(),j++)
      {
        TopoDS_Face newFace = TopoDS::Face(modFaceIt.Value());
	if (OCCTUtils_PassFaceAttributes(oldFace,newFace,
	      shapetool_,*shapelabel_,*shapelabel_) != CV_OK)
	{
	  fprintf(stderr,"Could not pass face info\n");
	  return CV_ERROR;
	}
      }
    }
  }

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
  TopoDS_Shape shapeA = *(occtPtrA->geom_);
  TopoDS_Shape shapeB = *(occtPtrB->geom_);

  BRepAlgoAPI_Cut subtractionOCCT(shapeA,shapeB);
  subtractionOCCT.Build();

  this->NewShape();
  *geom_ = subtractionOCCT.Shape();
  this->AddShape();

  //Transfer modified shape info from input A
  TopExp_Explorer anExp(shapeA,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next(),i++)
  {
    const TopTools_ListOfShape &modListA =
      subtractionOCCT.Modified(anExp.Current());
    TopoDS_Face oldFace = TopoDS::Face(anExp.Current());
    if (modListA.Extent() != 0)
    {
      TopTools_ListIteratorOfListOfShape modFaceIt(modListA);
      for (int j=0;modFaceIt.More();modFaceIt.Next(),j++)
      {
        TopoDS_Face newFace = TopoDS::Face(modFaceIt.Value());
	if (OCCTUtils_PassFaceAttributes(oldFace,newFace,
	      shapetool_,*shapelabel_,*shapelabel_) != CV_OK)
	{
	  fprintf(stderr,"Could not pass face info\n");
	  return CV_ERROR;
	}
      }
    }
  }

  //Transfer modified shape info from input B
  anExp.Init(shapeB,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next(),i++)
  {
    const TopTools_ListOfShape &modListB =
      subtractionOCCT.Modified(anExp.Current());
    TopoDS_Face oldFace = TopoDS::Face(anExp.Current());
    if (modListB.Extent() != 0)
    {
      TopTools_ListIteratorOfListOfShape modFaceIt(modListB);
      for (int j=0;modFaceIt.More();modFaceIt.Next(),j++)
      {
        TopoDS_Face newFace = TopoDS::Face(modFaceIt.Value());
	if (OCCTUtils_PassFaceAttributes(oldFace,newFace,
	      shapetool_,*shapelabel_,*shapelabel_) != CV_OK)
	{
	  fprintf(stderr,"Could not pass face info\n");
	  return CV_ERROR;
	}
      }
    }
  }

  return CV_OK;
}

// ------------
// GetPolyData
// ------------
cvPolyData *cvOCCTSolidModel::GetPolyData(int useMaxDist, double max_dist) const
{
  //In OpenCASCADE case, max_dist corresponds to angle in degrees for which
  //faceted normals are allowed
  if (geom_ == NULL)
  {
    fprintf(stderr,"Solid is null\n");
    return CV_ERROR;
  }
  cvPolyData *result;
  vtkPolyData *pd;

  if (useMaxDist == 0)
    max_dist = 20.0;

  IVtkOCC_Shape::Handle aShapeImpl = new IVtkOCC_Shape(*geom_);
  //IVtk_IShapeData::Handle aDataImpl = new IVtkVTK_ShapeData();
  IVtkVTK_ShapeData::Handle aDataImpl = new IVtkVTK_ShapeData();
  //Deviation Coefficient is 0.0001,Deviation Angle = 5rad,default is 12rad,
  //Do not generate u isoline and do not generate v isoline
  double devcoeff = 0.0001;
  double angcoeff = max_dist * M_PI/180.0;
  int uIsoLine= 0;
  int vIsoLine= 0;
  IVtk_IShapeMesher::Handle aMesher = new IVtkOCC_ShapeMesher(
      devcoeff,angcoeff,uIsoLine,vIsoLine);
  aMesher->Build(aShapeImpl,aDataImpl);

  pd = vtkPolyData::New();
  pd->DeepCopy(aDataImpl->getVtkPolyData());
  //if (max_dist != -1)
  //{
    this->GetOnlyPD(pd,max_dist);
  //}

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

  if (useMaxDist == 0)
    max_dist = 20.0;
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
  double angcoeff = max_dist * M_PI/180.0;
  int uIsoLine= 0;
  int vIsoLine= 0;
  IVtk_IShapeMesher::Handle aMesher = new IVtkOCC_ShapeMesher(
      devcoeff,angcoeff,uIsoLine,vIsoLine);
  aMesher->Build(aShapeImpl,aDataImpl);

  pd = vtkPolyData::New();
  pd->DeepCopy(aDataImpl->getVtkPolyData());
  //if (max_dist != -1)
    this->GetOnlyPD(pd,max_dist);

  result = new cvPolyData(pd);
  pd->Delete();
  return result;
}

// ---------------
// MakeEllipsoid
// ---------------
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
int cvOCCTSolidModel::CreateEdgeBlend(int faceA, int faceB, double radius, int filletshape)
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"Solid is Null\n");
    return CV_ERROR;
  }

  //Fillet shape can be three things
  //0 -> Rational
  //1 -> Quasi-Angular
  //2 -> Parametric
  if ((filletshape < 0) || (filletshape > 2))
  {
    fprintf(stderr,"Invalid shape type\n");
    return CV_ERROR;
  }

  TopoDS_Shape geomtmp = *geom_;
  TopoDS_Shape geompass = *geom_;
  char blendname[255];
  BRepFilletAPI_MakeFillet filletmaker(geompass,(ChFi3d_FilletShape) filletshape);
  if (OCCTUtils_CreateEdgeBlend(geompass,shapetool_,*shapelabel_,
	filletmaker,faceA,faceB,radius,blendname) != CV_OK)
  {
    fprintf(stderr,"Fillet creation didn't complete\n");
    return CV_ERROR;
  }
  this->RemoveShape();
  this->NewShape();
  *geom_ = geompass;
  this->AddShape();
  TopExp_Explorer anExp(geomtmp,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next(),i++)
  {
    const TopTools_ListOfShape &modfaces =
      filletmaker.Modified(anExp.Current());
    TopoDS_Face oldFace = TopoDS::Face(anExp.Current());
    if (modfaces.Extent() != 0)
    {
      fprintf(stderr,"Found modified face!\n");
      TopTools_ListIteratorOfListOfShape modFaceIt(modfaces);
      for (int j=0;modFaceIt.More();modFaceIt.Next(),j++)
      {
        TopoDS_Face newFace = TopoDS::Face(modFaceIt.Value());
	if (OCCTUtils_PassFaceAttributes(oldFace,newFace,
	      shapetool_,*shapelabel_,*shapelabel_) != CV_OK)
	{
	  fprintf(stderr,"Could not pass face info\n");
	  return CV_ERROR;
	}
	fprintf(stderr,"Also checking masses\n");
      	GProp_GProps oldFaceProps;
      	BRepGProp::LinearProperties(oldFace,oldFaceProps);
	fprintf(stderr,"Old Face Mass %.4f\n",oldFaceProps.Mass());
      	GProp_GProps newFaceProps;
      	BRepGProp::LinearProperties(newFace,newFaceProps);
	fprintf(stderr,"New Face Mass %.4f\n",newFaceProps.Mass());
      }
    }
    fprintf(stderr,"Checking if deleted %d\n",filletmaker.IsDeleted(anExp.Current()));
    const TopTools_ListOfShape &genfaces =
      filletmaker.Generated(anExp.Current());
    fprintf(stderr,"Checking generated %d\n",genfaces.Extent());
  }
  fprintf(stderr,"Number Faulty Surfaces: %d\n",filletmaker.NbFaultyContours());
  fprintf(stderr,"Has result?: %d\n",filletmaker.HasResult());
  fprintf(stderr,"Number of Contours: %d\n",filletmaker.NbContours());
  int numNew = 0;
  TopExp_Explorer FaceExp;
  FaceExp.Init(*geom_,TopAbs_FACE);
  for (int i=0;FaceExp.More();FaceExp.Next(),i++)
  {
    TopoDS_Face tmpFace = TopoDS::Face(FaceExp.Current());
    int newid=-1;
    OCCTUtils_GetFaceLabel(tmpFace,shapetool_,*shapelabel_,newid);
    char *name;
    OCCTUtils_GetFaceAttribute(tmpFace,shapetool_,*shapelabel_,"gdscName",&name);
    if (!strncmp(name,"noname",6))
    {
      char newbname[255];
      numNew++;
      sprintf(newbname,"%s_%d",blendname,numNew);
      OCCTUtils_SetFaceAttribute(tmpFace,shapetool_,*shapelabel_,"gdscName",newbname);
    }
    GProp_GProps faceProps;
    BRepGProp::LinearProperties(tmpFace,faceProps);
    TopoDS_Shape checkType = FaceExp.Current();
    fprintf(stderr,"Looking at masses of faces: %.4f\n",faceProps.Mass());
    fprintf(stderr,"Check Face Type!!!! %d\n",checkType.ShapeType());
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
    Handle(XCAFApp_Application) OCCTManager =
      static_cast<XCAFApp_Application*>(gOCCTManager);
    OCCTManager->GetDocument(1,aDoc);
    fprintf(stdout,"Reading file %s\n",filename);
    STEPCAFControl_Reader reader;
    reader.Perform(filename,aDoc);
    shapetool_ = XCAFDoc_DocumentTool::ShapeTool(aDoc->Main());
    TDF_LabelSequence allLabels;
    shapetool_->GetShapes(allLabels);
    *shapelabel_ = allLabels.Last();
    *geom_ = shapetool_->GetShape(*shapelabel_);
    this->RegisterShapeFaces();
  }
  else if (!strncmp(extension,"iges",4)) {
    Handle(TDocStd_Document) aDoc;
    Handle(XCAFApp_Application) OCCTManager =
      static_cast<XCAFApp_Application*>(gOCCTManager);
    OCCTManager->GetDocument(1,aDoc);
    fprintf(stdout,"Reading file %s\n",filename);
    IGESCAFControl_Reader reader;
    reader.Perform(filename,aDoc);
    shapetool_ = XCAFDoc_DocumentTool::ShapeTool(aDoc->Main());
    TDF_LabelSequence allLabels;
    shapetool_->GetShapes(allLabels);
    *shapelabel_ = allLabels.Last();
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
    fprintf(stdout,"Writing file %s\n",filename);
    STEPCAFControl_Writer writer;
    char *empty = NULL;
    writer.Transfer(*shapelabel_,STEPControl_AsIs);
    IFSelect_ReturnStatus ret = writer.Write(filename);
    if (ret == IFSelect_RetError || ret == IFSelect_RetFail || ret == IFSelect_RetStop)
    {
      fprintf(stderr,"File could not be opened\n");
      return CV_ERROR;
    }
  }
  else if (!strncmp(extension,"iges",4)) {
#ifdef __linux__
    fprintf(stdout,"Cannot write IGES file on linux %s\n",filename);
#else
    fprintf(stdout,"Writing file %s\n",filename);
    IGESCAFControl_Writer writer;
    char *empty = NULL;
    writer.Transfer(*shapelabel_);
    Standard_Boolean ret = writer.Write(filename);
#endif
  }
  else if (!strncmp(extension,"stl",3)) {
    fprintf(stdout,"Writing file %s\n",filename);
    StlAPI_Writer writer;
    writer.Write(*geom_,filename);
  }
  else {
    fprintf(stderr,"File type not excepted\n");
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
  for (int i=1;FaceExp.More();FaceExp.Next(),i++)
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
int cvOCCTSolidModel::RemoveShape()
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
    if (shapelabel_ != NULL)
    {
      delete shapelabel_;
      shapelabel_ = NULL;
    }
    if (geom_ != NULL)
    {
      delete geom_;
      geom_ = NULL;
    }
  }
  else
  {
      fprintf(stderr,"Shape has already been deleted, cannot remove\n");
      return CV_ERROR;
  }

  return CV_OK;
}

// -------------------
// CreateBSplineSurface
// -------------------
int cvOCCTSolidModel::CreateBSplineSurface(double **CX,double **CY,double **CZ,
    int &len1,int &len2,double *uKnots,int &uKlen,double *vKnots,int &vKlen,
    double *uMults,int &uMlen,double *vMults,int &vMlen,int &p,int &q)
{
  //Create a BSpline surface from the input control points,knots, and mults

  TColgp_Array2OfPnt cPoints(1,len2,1,len1);
  for (int i=0;i<len1;i++)
  {
    for (int j=0;j<len2;j++)
    {
      gp_Pnt newPnt(CX[i][j],CY[i][j],CZ[i][j]);
      cPoints.SetValue(j+1,i+1,newPnt);
    }
  }

  //Knot spans
  TColStd_Array1OfReal uKCol(1,uKlen);
  for (int i=0;i<uKlen;i++)
    uKCol.SetValue(i+1, uKnots[i]);
  TColStd_Array1OfReal vKCol(1,vKlen);
  for (int i=0;i<vKlen;i++)
    vKCol.SetValue(i+1, vKnots[i]);

  //Mult spans
  TColStd_Array1OfInteger uMCol(1,uMlen);
  for (int i=0;i<uMlen;i++)
    uMCol.SetValue(i+1,(int) uMults[i]);
  TColStd_Array1OfInteger vMCol(1,vMlen);
  for (int i=0;i<vMlen;i++)
    vMCol.SetValue(i+1,(int) vMults[i]);

  Standard_Real tol = 1.e-6;
  Handle(Geom_BSplineSurface) surface;
  Handle(Geom_Surface) aSurf;
  try {
    Standard_Boolean uPer=Standard_False,vPer=Standard_False;
    surface = new Geom_BSplineSurface(cPoints,uKCol,vKCol,uMCol,vMCol,p,q,uPer,vPer);
    //surface->SetUPeriodic();
    aSurf = surface;
    //fprintf(stdout,"-----------------BSPLINE PARAMETERS----------------------\n");
    //fprintf(stdout,"U Degree:             %d\n",surface->UDegree());
    //fprintf(stdout,"Is U Closed?:         %d\n",surface->IsUClosed());
    //fprintf(stdout,"Is U Periodic?:       %d\n",surface->IsUPeriodic());
    //fprintf(stdout,"Is U Rational?:       %d\n",surface->IsURational());
    //fprintf(stdout,"Nb U Poles:           %d\n",surface->NbUPoles());
    //fprintf(stdout,"Nb U Knots:           %d\n",surface->NbUKnots());
    //fprintf(stdout,"First U Knot Index:   %d\n",surface->FirstUKnotIndex());
    //fprintf(stdout,"Last U Knot Index:    %d\n",surface->LastUKnotIndex());
    //fprintf(stdout,"_________________________________________________________\n");
    //fprintf(stdout,"V Degree:             %d\n",surface->VDegree());
    //fprintf(stdout,"Is V Closed?:         %d\n",surface->IsVClosed());
    //fprintf(stdout,"Is V Periodic?:       %d\n",surface->IsVPeriodic());
    //fprintf(stdout,"Is U Rational?:       %d\n",surface->IsVRational());
    //fprintf(stdout,"Nb V Poles:           %d\n",surface->NbVPoles());
    //fprintf(stdout,"Nb V Knots:           %d\n",surface->NbVKnots());
    //fprintf(stdout,"First V Knot Index:   %d\n",surface->FirstVKnotIndex());
    //fprintf(stdout,"Last V Knot Index:    %d\n",surface->LastVKnotIndex());
    //fprintf(stdout,"_________________________________________________________\n");
  }
  catch (Standard_ConstructionError)
  {
    fprintf(stderr,"Construction Error\n");
    return CV_ERROR;
  }

  BRepBuilderAPI_MakeShell shellBuilder(aSurf);
  this->NewShape();
  *geom_ = shellBuilder.Shape();

  //Attacher!
  TopoDS_Wire wires[2];
  Standard_Real sewtoler =  1.e-6;
  Standard_Real closetoler =  1.e-2;
  ShapeFix_FreeBounds findFree(*geom_,sewtoler,closetoler,
        	  Standard_False,Standard_False);
  TopoDS_Compound freeWires = findFree.GetClosedWires();
  TopExp_Explorer NewEdgeExp;
  NewEdgeExp.Init(freeWires,TopAbs_EDGE);
  for (int i=0;NewEdgeExp.More();NewEdgeExp.Next(),i++)
  {
    TopoDS_Edge tmpEdge = TopoDS::Edge(NewEdgeExp.Current());

    BRepBuilderAPI_MakeWire wiremaker(tmpEdge);
    wiremaker.Build();

    wires[i] = wiremaker.Wire();
  }

  Standard_Real pres3d = 1.0e-6;
  if (OCCTUtils_ShapeFromBSplineSurface(surface,*geom_,wires[0],wires[1],pres3d) != CV_OK)
  {
    fprintf(stderr,"Error in conversion from bspline surface to shape\n");
    return CV_ERROR;
  }
  //TopoDS_Face firstFace,lastFace;
  //*geom_ = OCCTUtils_MakeSolid(shellBuilder.Shell(),wires[0],wires[1],firstFace,lastFace,pres3d);

  this->AddShape();

  //Name faces
  TopExp_Explorer anExp(*geom_,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next(),i++)
  {
    TopoDS_Face tmpFace = TopoDS::Face(anExp.Current());
    OCCTUtils_SetFaceAttribute(tmpFace,shapetool_,*shapelabel_,"gdscName","wall");
  }

  return CV_OK;
}

// ---------------
// GetOnlyPD
// ---------------
int cvOCCTSolidModel::GetOnlyPD(vtkPolyData *pd,double &max_dist) const
{
  if (pd == NULL)
    return CV_ERROR;

  if (VtkUtils_PDCheckArrayName(pd,1,"MESH_TYPES"))
  {
    vtkSmartPointer<vtkCleanPolyData> cleaner =
      vtkSmartPointer<vtkCleanPolyData>::New();
    cleaner->SetInputData(pd);
    cleaner->PointMergingOn();
    cleaner->Update();
    vtkSmartPointer<vtkThreshold> thresholder =
      vtkSmartPointer<vtkThreshold>::New();
    thresholder->SetInputData(cleaner->GetOutput());
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
    //fprintf(stderr,"Num Points bef! %d\n",surfacer->GetOutput()->GetNumberOfPoints());
    //  vtkSmartPointer<vtkQuadricDecimation> decimator =
    //    vtkSmartPointer<vtkQuadricDecimation>::New();
    //  decimator->SetInputData(surfacer->GetOutput());
    //  decimator->SetTargetReduction(0.2);
    //  decimator->GetOutput();
    //  pd->DeepCopy(decimator->GetOutput());
    if (surfacer->GetOutput()->GetNumberOfPoints() != 0)
    {
      pd->DeepCopy(surfacer->GetOutput());
    }
    //fprintf(stderr,"Num Points now! %d\n",pd->GetNumberOfPoints());
  }

  return CV_OK;
}

