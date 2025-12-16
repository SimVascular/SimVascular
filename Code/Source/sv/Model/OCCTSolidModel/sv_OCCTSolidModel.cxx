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

/** @file sv_OCCTSolidModel.cxx
 *  @brief The implementations of functions in OCCTSolidModel
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 *  @note Most functions in class call functions in cv_occtsolid_utils.
 */

#include "SimVascular.h"

// davep #include "sv2_globals.h"
#include "simvascular_options.h"

#include "sv_OCCTSolidModel.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkThreshold.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkQuadricDecimation.h"
#include "vtkMath.h"
#include "sv_polydatasolid_utils.h"
#include "sv_occtsolid_utils.h"
#include "sv_misc_utils.h"
#include "sv_sys_geom.h"
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
// davep #include "BRepAdaptor_HCurve.hxx"
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
// davep #include "Adaptor3d_HCurveOnSurface.hxx"
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

// Previously defined in sv2/Globals/sv2_globals.h.
void *cvOCCTSolidModel::gOCCTManager = nullptr;

//----------------
// OCCTSolidModel
//----------------
// This is called directly mainy times in sv4gui_ModelUtilsOCCT.cxx. 
//
/**
 * @brief Constructor for OCCTSolidModel (Should never be called directly)
 */

cvOCCTSolidModel::cvOCCTSolidModel():cvSolidModel( SM_KT_OCCT)
{
  #define n_debug_cvOCCTSolidModel
  #ifdef debug_cvOCCTSolidModel 
  std::string msg("[cvOCCTSolidModel] ");
  std::cout << msg << "----------" << std::endl;
  #endif

  numBoundaryRegions = 0;
  geom_ = NULL;

  // Get a single document created inside of the manager.
  //
  Handle(TDocStd_Document) doc;
  Handle(XCAFApp_Application) OCCTManager = XCAFApp_Application::GetApplication();
  Standard_Integer num_docs = 1;
  OCCTManager->GetDocument(num_docs, doc);

  // Get the Main Item of an XDE document, which records the root shape representation.
  //
  shapetool_ = XCAFDoc_DocumentTool::ShapeTool(doc->Main());

  // Test to see what aLabel is.
  //
  // aLabel: 0:1:1:1  
  //
  #ifdef debug_cvOCCTSolidModel 
  TDF_Label aLabel = shapetool_->NewShape();
  //std::cout << msg << "aLabel: " << aLabel << std::endl;
  #endif

  // Create label
  shapelabel_ = nullptr;
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
  #ifdef debug_Copy
  std::string msg("[cvOCCTSolidModel::Copy] ");
  std::cout << msg << std::endl;
  std::cout << msg << "========== Copy =========" << std::endl;
  #endif

  cvOCCTSolidModel *solidPtr;

  if (geom_ != NULL) {
    return SV_ERROR;
  }
  #ifdef debug_Copy
  std::cout << msg << "geom_: " << geom_ << std::endl;
  #endif

  if (src.GetKernelT() != SM_KT_OCCT) {
    return SV_ERROR;
  }

  // Get a single document created inside of the manager.
  //
  Handle(TDocStd_Document) doc;
  Handle(XCAFApp_Application) OCCTManager = XCAFApp_Application::GetApplication();
  Standard_Integer num_docs = 1;
  OCCTManager->GetDocument(num_docs, doc);

  // Get the Main Item of an XDE document, which records the root shape representation.
  //
  shapetool_ = XCAFDoc_DocumentTool::ShapeTool(doc->Main());

  solidPtr = (cvOCCTSolidModel *)( &src );
  #ifdef debug_Copy
  std::cout << msg << "solidPtr: " << solidPtr << std::endl;
  #endif

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
	    *(solidPtr->shapelabel_),*shapelabel_) != SV_OK)
      {
	fprintf(stderr,"Could not pass face attributes to copy\n");
	return SV_ERROR;
      }
    }

  }

  return SV_OK;
}

// ----
// Copy
// ----

cvSolidModel *cvOCCTSolidModel::Copy() const
{
  cvOCCTSolidModel *result = new cvOCCTSolidModel(*this);
  return result;
}

//-------
// Print
//-------
//
void cvOCCTSolidModel::Print() const
{
  if (geom_ == nullptr) {
    fprintf(stderr,"No geometry in model\n");
  }

  std::string msg("[cvOCCTSolidModel::Print] ");
  std::cout << msg << std::endl;
  std::cout << msg << "------------------- Print ------------------------------- " << std::endl;
  std::cout << msg << "Orientation: " << geom_->Orientation() << std::endl;
  std::cout << msg << "Shape Type:  " << geom_->ShapeType() << std::endl;
  std::cout << msg << "Free:        " << geom_->Free() << std::endl;
  std::cout << msg << "Locked:      " << geom_->Locked() << std::endl;
  std::cout << msg << "Modified:    " << geom_->Modified() << std::endl;
  std::cout << msg << "Orientable:  " << geom_->Orientable() << std::endl;
  std::cout << msg << "Closed:      " << geom_->Closed() << std::endl;

  int numFaces = 0;
  OCCTUtils_GetNumberOfFaces(*geom_, numFaces);
  TopExp_Explorer anExp(*geom_, TopAbs_FACE);
  
  std::cout << msg << "numFaces: " << numFaces << std::endl;
  std::cout << msg << "shapelabel_: " << *shapelabel_ << std::endl;
  
  for (int i = 0; anExp.More(); anExp.Next(), i++) {
    std::cout << msg << "----- face i " << i << " ----- " << std::endl;
    TopoDS_Face tmpFace = TopoDS::Face(anExp.Current());
    TDF_Label tmpLabel;
    shapetool_->FindSubShape(*shapelabel_, tmpFace, tmpLabel);

    if (tmpLabel.IsNull()) {
      std::cout << msg << "face is not labeled " << std::endl;
      continue;
    } else {
      std::cout << msg << "face is labeled " << std::endl;
      std::cout << msg << "label: " << tmpLabel << std::endl;
    }

    TDF_Label idLabel = tmpLabel.FindChild(0);

    if (idLabel.IsNull()) {
      std::cout << msg << "Face has no ID label. " << std::endl;
    } else {
      Handle(TDataStd_Integer) INT = new TDataStd_Integer();
      idLabel.FindAttribute(TDataStd_Integer::GetID(), INT);
      int id = INT->Get();
      std::cout << msg << "face id: " << id << std::endl;
    }

    TDF_Label nameLabel = tmpLabel.FindChild(1);

    if (nameLabel.IsNull()) {
      std::cout << msg << "Face has no name (1) label. " << std::endl;
    } else {
      char* value;
      Handle(TDataStd_ExtStringArray) NSTRING = new TDataStd_ExtStringArray();
      nameLabel.FindAttribute(TDataStd_ExtStringArray::GetID(), NSTRING);

      std::stringstream streamer;
      for (int i = NSTRING->Lower(); i < NSTRING->Upper();i++) {
        TCollection_AsciiString asciiString(NSTRING->Value(i),'?');
        streamer << asciiString.ToCString();
      }
      std::string name = streamer.str();
      std::cout << msg << "face name: " << name << std::endl;
    }

    TDF_Label parentLabel = tmpLabel.FindChild(2);

    if (parentLabel.IsNull()) {
      std::cout << msg << "Face has no parent (2) label. " << std::endl;
    } else {
      char* value;
      Handle(TDataStd_ExtStringArray) NSTRING = new TDataStd_ExtStringArray();
      parentLabel.FindAttribute(TDataStd_ExtStringArray::GetID(), NSTRING);
      std::stringstream streamer;
      for (int i = NSTRING->Lower(); i < NSTRING->Upper();i++) {
        TCollection_AsciiString asciiString(NSTRING->Value(i),'?');
        streamer << asciiString.ToCString();
      }
      std::string parent = streamer.str();
      std::cout << msg << "face parent: " << parent << std::endl;
    }


  }

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

  return SV_OK;
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

  return SV_OK;
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

  return SV_OK;
}

//----------------
// MakeLoftedSurf
//----------------
//
int cvOCCTSolidModel::MakeLoftedSurf( cvSolidModel **curves, int numCurves, char *name, 
    int continuity,int partype, double w1,double w2,double w3,int smoothing, bool capSurface)
{
  #define n_debug_MakeLoftedSurf
  #ifdef debug_MakeLoftedSurf 
  std::string msg("[sv4guiModelUtilsOCCT::MakeLoftedSurf] ");
  std::cout << msg << "========== MakeLoftedSurf =========" << std::endl;
  std::cout << msg << "name: " << name << std::endl;
  #endif

  if (geom_ != NULL)
    this->RemoveShape();

  if ( numCurves < 2 ) {
    return SV_ERROR;
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
      return SV_ERROR;
    }
    shapePtr = (cvOCCTSolidModel *) curves[i];
    bcurves[i] = TopoDS::Wire(*(shapePtr->geom_));
  }
  this->NewShape();

  #ifdef debug_MakeLoftedSurf 
  std::cout << msg << "OCCTUtils_MakeLoftedSurf ... " << std::endl;
  #endif

  if (OCCTUtils_MakeLoftedSurf(bcurves, *geom_, numCurves, continuity, partype, w1, w2, w3, smoothing) != SV_OK) {
    delete [] bcurves;
    fprintf(stderr,"Error while lofting surface\n");
    return SV_ERROR;
  }

  #ifdef debug_MakeLoftedSurf 
  std::cout << msg << "this->AddShape() ... " << std::endl;
  #endif
  this->AddShape();

  // Name faces.
  //
  #ifdef debug_MakeLoftedSurf 
  std::cout << msg << "Name faces ... " << std::endl;
  std::cout << msg << "OCCTUtils_SetFaceAttribute for gdscName " << std::endl;
  #endif
  TopExp_Explorer anExp(*geom_, TopAbs_FACE);

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
  return SV_OK;
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
  if ( sys_geom_GetOrderedPts( pd, &ord_pts, &num_pts ) != SV_OK ) {
    return SV_ERROR;
  }

  if ( num_pts < 3 ) {
    delete [] ord_pts;
    return SV_ERROR;
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

  return SV_OK;
}

//----------------
// CapSurfToSolid
//----------------
// Add faces (caps) to the ends of a lofted surface.
//
int cvOCCTSolidModel::CapSurfToSolid(cvSolidModel *surf)
{
  #define n_debug_CapSurfToSolid
  #ifdef debug_CapSurfToSolid
  std::string msg("[sv4guiModelUtilsOCCT::CapSurfToSolid] ");
  std::cout << msg << "========== CapSurfToSolid =========" << std::endl;
  #endif

  cvOCCTSolidModel *surfSolidPtr = (cvOCCTSolidModel*)surf;
  auto surf_geom = surfSolidPtr->geom_;
  //TopoDS_Shape surf_geom = *(surfSolidPtr->geom_);

  if (geom_ != NULL) {
    this->RemoveShape();
  }

  int numHoles = 0;
  BRepBuilderAPI_Sewing attacher;

  // Create new geom_ and shapelabel_ member data. 
  this->NewShape();

  #ifdef debug_CapSurfToSolid
  std::cout << msg << "OCCTUtils_CapShapeToSolid ... " << std::endl;
  #endif

  // Create a capped surface into geom_.
  //
  if (OCCTUtils_CapShapeToSolid(*surf_geom, *geom_, attacher, numHoles) != SV_OK) {
    fprintf(stderr,"Error capping shape\n");
    return SV_ERROR;
  }

  // Add *shapelabel_ for *geom_.
  //
  #ifdef debug_CapSurfToSolid
  std::cout << msg << "this->AddShape() ... " << std::endl;
  #endif
  this->AddShape();

  //std::cout << msg << "##### this #####" << std::endl;
  //this->Print();

  // Name faces (caps) that were added to the surface.
  //
  int numFaces = 0;
  OCCTUtils_GetNumberOfFaces(*geom_, numFaces);
  TopExp_Explorer anExp(*geom_, TopAbs_FACE);

  #ifdef debug_CapSurfToSolid
  std::cout << msg << "Name face as cap for the new surfaces ... " << std::endl;
  std::cout << msg << "numFaces: " << numFaces << std::endl;
  std::cout << msg << "numHoles: " << numHoles << std::endl;
  std::cout << msg << "numFaces - numHoles: " << numFaces - numHoles << std::endl;
  std::cout << msg << "shapelabel_: " << *shapelabel_ << std::endl;
  #endif

  for (int i = 0; anExp.More(); anExp.Next(), i++) {
    #ifdef debug_CapSurfToSolid
    std::cout << msg << "----- face i " << i << " ----- " << std::endl;
    #endif
    TopoDS_Face tmpFace = TopoDS::Face(anExp.Current());

    #ifdef debug_CapSurfToSolid
    TDF_Label tmpLabel;
    shapetool_->FindSubShape(*shapelabel_, tmpFace, tmpLabel);
    if (tmpLabel.IsNull()) {
      std::cout << msg << "face is not labeled " << std::endl;
    } else {
      std::cout << msg << "face is labeled " << std::endl;
    }
    int face_id;
    OCCTUtils_GetFaceLabel(tmpFace, shapetool_, *shapelabel_, face_id);
    std::cout << msg << "face_id: " << face_id << std::endl;
    #endif

    // Each hole is a cap so first process faces that are walls.
    //
    if (i >= (numFaces - numHoles)) {
      #ifdef debug_CapSurfToSolid
      std::cout << msg << "OCCTUtils_SetFaceAttribute gdscName cap " << std::endl;
      #endif
      OCCTUtils_SetFaceAttribute(tmpFace, shapetool_, *shapelabel_, "gdscName", "cap");

    // If the input shape has been modified.
    //
    } else if (attacher.IsModified(*surf_geom)) {
      #ifdef debug_CapSurfToSolid
      std::cout << msg << "attacher.IsModified(shape)" << std::endl;
      std::cout << msg << "Add wall face" << std::endl;
      #endif
      GProp_GProps tmpFaceProps;
      BRepGProp::LinearProperties(tmpFace, tmpFaceProps);
      TopExp_Explorer faceExp(*surf_geom, TopAbs_FACE);

      for (int j = 0; faceExp.More(); faceExp.Next(), j++) {
        TopoDS_Face daFace = TopoDS::Face(faceExp.Value());
        GProp_GProps daFaceProps;
        BRepGProp::LinearProperties(tmpFace, daFaceProps);

	if (tmpFaceProps.Mass() == daFaceProps.Mass()) {
          // It seems like tmpFace should be the first (source) shape 
          // but this seems to work.
          OCCTUtils_PassFaceAttributes(daFace, tmpFace, shapetool_, *surfSolidPtr->shapelabel_, *shapelabel_);
        }
      }
    }
  }

  int issue=0;
  if (OCCTUtils_CheckIsSolid(*geom_,issue) != SV_OK)
  {
    fprintf(stderr,"Shape is not solid after cap\n");
    return SV_ERROR;
  }
  if (issue != 0)
  {
    fprintf(stderr,"Shape is not solid after cap\n");
    return SV_ERROR;
  }

  return SV_OK;
}

//------
// Union
//------
// Union two solids 'A' and 'B'.
//
// The new solid is stored in geom_.
//
// After the union operation face attributes (e.g. "gdscName" ) are copied 
// from the faces of the two solids to the faces of the union result. 
//
int cvOCCTSolidModel::Union( cvSolidModel *A, cvSolidModel *B, SolidModel_SimplifyT st)
{
  #define n_debug_Union
  #ifdef debug_Union
  std::string msg("[cvOCCTSolidModel::Union] ");
  std::cout << msg << "========== Union ==========" << std::endl; 
  #endif

  cvOCCTSolidModel *occtPtrA;
  cvOCCTSolidModel *occtPtrB;

  if (geom_ != NULL) {
    return SV_ERROR;
  }

  if (A == NULL) {
    return SV_ERROR;
  }

  if (A->GetKernelT() != SM_KT_OCCT ) {
    fprintf(stderr,"Model not of type OCCT\n");
    return SV_ERROR;
  }

  if (B == NULL) {
    return SV_ERROR;
  }

  if (B->GetKernelT() != SM_KT_OCCT ) {
    fprintf(stderr,"Model not of type OCCT\n");
    return SV_ERROR;
  }

  occtPtrA = (cvOCCTSolidModel *)( A );
  occtPtrB = (cvOCCTSolidModel *)( B );
  TopoDS_Shape shapeA = *(occtPtrA->geom_);
  TopoDS_Shape shapeB = *(occtPtrB->geom_);

  BRepAlgoAPI_Fuse unionOCCT(shapeA,shapeB);
  unionOCCT.Build();
  auto union_shape = unionOCCT.Shape();

  this->NewShape();
  *geom_ = unionOCCT.Shape();
  this->AddShape();

  // Transfer face attributes from the input 'a' to 
  // the appropriate union faces.
  //
  // The cap faces that have not been removed from the union
  // solid need to be matched from the input 'A' solid.
  //
  TopExp_Explorer anExp(shapeA, TopAbs_FACE);
  const Handle(BRepTools_History)& history = unionOCCT.History();

  for (int i = 0; anExp.More(); anExp.Next(), i++) {
    #ifdef debug_Union
    std::cout << msg << "---------- shapeA face i " << i << " ----------" << std::endl;
    #endif
    const TopTools_ListOfShape &modListA = unionOCCT.Modified(anExp.Current());
    TopoDS_Face oldFace = TopoDS::Face(anExp.Current());
    bool oldFace_removed = history->IsRemoved(oldFace);

    if (modListA.Extent() != 0) {
      TopTools_ListIteratorOfListOfShape modFaceIt(modListA);
      for (int j = 0; modFaceIt.More(); modFaceIt.Next(), j++) {
        TopoDS_Face newFace = TopoDS::Face(modFaceIt.Value());
	if (OCCTUtils_PassFaceAttributes(oldFace, newFace, shapetool_, *occtPtrA->shapelabel_, *shapelabel_) != SV_OK) {
          #ifdef debug_Union
          std::cout << msg << "WARNING: Could not pass face info" << std::endl;
          #endif
	  fprintf(stderr,"Could not pass face info\n");
	  return SV_ERROR;
	}
      }

    } else if (!oldFace_removed) {
      TopExp_Explorer unionFaces(union_shape, TopAbs_FACE);
      for (int j = 0; unionFaces.More(); unionFaces.Next(), j++) {
        TopoDS_Face newFace = TopoDS::Face(unionFaces.Current());
        if (oldFace.IsSame(newFace)) {
          OCCTUtils_PassFaceAttributes(oldFace, newFace, shapetool_, *occtPtrA->shapelabel_, *shapelabel_);
        }
      }
    }
  }

  // Transfer modified shape info from input B
  //
  // The cap faces that have not been removed from the union
  // solid need to be matched from the input 'B' solid.
  //
  anExp.Init(shapeB, TopAbs_FACE);

  for (int i = 0; anExp.More(); anExp.Next(),i++) {
    const TopTools_ListOfShape &modListB = unionOCCT.Modified(anExp.Current());
    TopoDS_Face oldFace = TopoDS::Face(anExp.Current());
    bool oldFace_removed = history->IsRemoved(oldFace);

    if (modListB.Extent() != 0) {
      TopTools_ListIteratorOfListOfShape modFaceIt(modListB);
      for (int j = 0; modFaceIt.More(); modFaceIt.Next(), j++) {
        TopoDS_Face newFace = TopoDS::Face(modFaceIt.Value());
	if (OCCTUtils_PassFaceAttributes(oldFace, newFace, shapetool_, *occtPtrB->shapelabel_, *shapelabel_) != SV_OK) {
	  fprintf(stderr,"Could not pass face info\n");
	  return SV_ERROR;
	}
      }

    } else if (!oldFace_removed) {
      TopExp_Explorer unionFaces(union_shape, TopAbs_FACE);
      for (int j = 0; unionFaces.More(); unionFaces.Next(), j++) {
        TopoDS_Face newFace = TopoDS::Face(unionFaces.Current());
        if (oldFace.IsSame(newFace)) {
          OCCTUtils_PassFaceAttributes(oldFace, newFace, shapetool_, *occtPtrB->shapelabel_, *shapelabel_);
        }
      }
    }
  }

  return SV_OK;
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
    return SV_ERROR;

  //Need both objects to create an intersection
  if (a == NULL)
    return SV_ERROR;
  if (a->GetKernelT() != SM_KT_OCCT ) {
    fprintf(stderr,"Model not of type OCCT\n");
    return SV_ERROR;
  }

  if (b == NULL)
    return SV_ERROR;
  if (b->GetKernelT() != SM_KT_OCCT ) {
    fprintf(stderr,"Model not of type OCCT\n");
    return SV_ERROR;
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
	      shapetool_,*shapelabel_,*shapelabel_) != SV_OK)
	{
	  fprintf(stderr,"Could not pass face info\n");
	  return SV_ERROR;
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
	      shapetool_,*shapelabel_,*shapelabel_) != SV_OK)
	{
	  fprintf(stderr,"Could not pass face info\n");
	  return SV_ERROR;
	}
      }
    }
  }

  return SV_OK;
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
    return SV_ERROR;

  //Need both objects to create a subtraction
  if (a == NULL)
    return SV_ERROR;
  if (a->GetKernelT() != SM_KT_OCCT ) {
    fprintf(stderr,"Model not of type OCCT\n");
    return SV_ERROR;
  }

  if (b == NULL)
    return SV_ERROR;
  if (b->GetKernelT() != SM_KT_OCCT ) {
    fprintf(stderr,"Model not of type OCCT\n");
    return SV_ERROR;
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
	      shapetool_,*shapelabel_,*shapelabel_) != SV_OK)
	{
	  fprintf(stderr,"Could not pass face info\n");
	  return SV_ERROR;
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
	      shapetool_,*shapelabel_,*shapelabel_) != SV_OK)
	{
	  fprintf(stderr,"Could not pass face info\n");
	  return SV_ERROR;
	}
      }
    }
  }

  return SV_OK;
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
    return SV_ERROR;
  }
  cvPolyData *result;
  vtkPolyData *pd;

  if (useMaxDist == 0)
    max_dist = 20.0;

  // Deviation Coefficient is 0.0001,Deviation Angle = 5rad,default is 12rad,
  // Do not generate u isoline and do not generate v isoline
  double devcoeff = 0.0001;
  double angcoeff = max_dist * M_PI/180.0;
  int uIsoLine= 0;
  int vIsoLine= 0;

  IVtk_IShapeMesher::Handle aMesher = new IVtkOCC_ShapeMesher();
  IVtkOCC_Shape::Handle aShapeImpl = new IVtkOCC_Shape(*geom_);
  IVtkVTK_ShapeData::Handle aDataImpl = new IVtkVTK_ShapeData();
  aMesher->Build(aShapeImpl,aDataImpl);

  pd = vtkPolyData::New();
  pd->DeepCopy(aDataImpl->getVtkPolyData());

  pd = OrientSurfaceGemetry(pd);
  result = new cvPolyData(pd);
  pd->Delete();
  return result;
}

//----------------------
// OrientSurfaceGemetry
//----------------------
// Modify a surface so its polygons are oriented consistently.
//                                      
// The BREP triangulation does not create poloygons with consistent
// orientation so this is called when converting to vtkPolyData.
//  
vtkSmartPointer<vtkPolyData>
cvOCCTSolidModel::OrientSurfaceGemetry(vtkPolyData* geom) const
{ 
  auto surfaceNormals = vtkPolyDataNormals::New();
  surfaceNormals->SetInputData(geom);
  surfaceNormals->ComputeCellNormalsOn();
  surfaceNormals->ComputePointNormalsOn();
  surfaceNormals->ConsistencyOn();
  surfaceNormals->AutoOrientNormalsOn();
  surfaceNormals->SplittingOn();
  surfaceNormals->Update();
  
  vtkSmartPointer<vtkPolyData> oriented_geom = surfaceNormals->GetOutput();
  oriented_geom->BuildLinks();
  
  return oriented_geom;
} 


// ------------
// GetFaceIds
// ------------
int cvOCCTSolidModel::GetFaceIds (int *numFaces, int **faceIds) {


  if (geom_ == NULL)
  {
    fprintf(stderr,"Solid is null\n");
    return SV_ERROR;
  }
  if (OCCTUtils_GetFaceIds(*geom_,shapetool_,*shapelabel_, numFaces,faceIds) != SV_OK)
  {
    fprintf(stderr,"Error in retrieving Ids\n");
    return SV_ERROR;
  }


  return SV_OK;

}

// ------------
// GetFaceAttribute
// ------------
int cvOCCTSolidModel::GetFaceAttribute (char *attr,int faceid,char **value)
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"solid is null\n");
    return SV_ERROR;
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
      if (OCCTUtils_GetFaceAttribute(tmpFace,shapetool_,*shapelabel_,attr,value) != SV_OK)
      {
	fprintf(stderr,"Could not get face attribute\n");
	return SV_ERROR;
      }
      found=1;
    }
  }

  if (found == 0)
  {
    fprintf(stderr,"Face not found on shape, so attribute cannot be found\n");
    return SV_ERROR;
  }

  return SV_OK;
}

//------------------
// SetFaceAttribute
//------------------
// Set the 'attr' attribute for a face with face ID 'faceid' to
// the given 'value'.
//
// It may be that if this method returns an error it is
// more of a result than an actual compute error.
//
int cvOCCTSolidModel::SetFaceAttribute(char *attr, int faceid, char *value)
{
  #define n_debug_SetFaceAttribute
  #ifdef debug_SetFaceAttribute
  std::string msg("[cvOCCTSolidModel::SetFaceAttribute] ");
  std::cout << msg << "========== SetFaceAttribute ==========" << std::endl;
  std::cout << msg << "attr: " << attr << std::endl;
  std::cout << msg << "faceid: " << faceid << std::endl;
  std::cout << msg << "value: " << value << std::endl;
  //std::cout << msg << "shapelabel_: " << *shapelabel_ << std::endl;
  #endif                      

  if (geom_ == NULL) {
    fprintf(stderr,"solid is null\n");
    return SV_ERROR;
  }

  int found = 0;
  TopExp_Explorer anExp(*geom_, TopAbs_FACE);

  for (int i = 0; anExp.More(); anExp.Next(),i++) {
    TopoDS_Face tmpFace = TopoDS::Face(anExp.Current());
    int id = -1;
    // Look for tmpFace in shapetool_.
    OCCTUtils_GetFaceLabel(tmpFace, shapetool_, *shapelabel_, id);

    if (id == faceid) {
      if (OCCTUtils_SetFaceAttribute(tmpFace, shapetool_, *shapelabel_, attr, value) != SV_OK) {
        #ifdef debug_SetFaceAttribute
        std::cout << msg << "WARNING: Could not set face attribute " << std::endl;
        #endif
	fprintf(stderr,"Could not set face attribute\n");
	return SV_ERROR;
      }

      #ifdef debug_SetFaceAttribute
      std::cout << msg << "found id: " << id << std::endl;
      #endif
      found = 1;
      break;
    }
  }

  if (found == 0) {
    fprintf(stderr,"Face not found on shape, so attribute cannot be set\n");
    return SV_ERROR;
  }

  #ifdef debug_SetFaceAttribute
  std::cout << msg << "Done " << std::endl;
  #endif
  return SV_OK;
}

// ---------------
// GetFacePolyData
// ---------------
cvPolyData *cvOCCTSolidModel::GetFacePolyData(int faceid, int useMaxDist, double max_dist) const
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"Solid is null\n");
    return SV_ERROR;
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
    return SV_ERROR;
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

  IVtk_IShapeMesher::Handle aMesher = new IVtkOCC_ShapeMesher();
  aMesher->Build(aShapeImpl,aDataImpl);

  pd = vtkPolyData::New();
  pd->DeepCopy(aDataImpl->getVtkPolyData());
  this->GetOnlyPD(pd,max_dist);

  pd = OrientSurfaceGemetry(pd);
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

  return SV_OK;
}

// ---------------
// DeleteFaces
// ---------------
int cvOCCTSolidModel::DeleteFaces(int numfaces, int *faces )
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"Solid is null\n");
    return SV_ERROR;
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
     return SV_ERROR;
   }
   if (deleteFace[faceId] == 1) {
     BRepTools_ReShape remover;
#if OpenCASCADE_MAJOR_VERSION == 7 && OpenCASCADE_MINOR_VERSION == 0
     remover.Remove(aFace,Standard_True); // OpenCASCADE version 7.0.0
#else
     remover.Remove(aFace); // OpenCASCADE version 7.2.0
#endif
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
  return SV_OK;
}

// ---------------
// CreateEdgeBlend
// ---------------
int cvOCCTSolidModel::CreateEdgeBlend(int faceA, int faceB, double radius, int filletshape)
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"Solid is Null\n");
    return SV_ERROR;
  }

  //Fillet shape can be three things
  //0 -> Rational
  //1 -> Quasi-Angular
  //2 -> Parametric
  if ((filletshape < 0) || (filletshape > 2))
  {
    fprintf(stderr,"Invalid shape type\n");
    return SV_ERROR;
  }

  TopoDS_Shape geomtmp = *geom_;
  TopoDS_Shape geompass = *geom_;
  char blendname[255];
  BRepFilletAPI_MakeFillet filletmaker(geompass,(ChFi3d_FilletShape) filletshape);
  if (OCCTUtils_CreateEdgeBlend(geompass,shapetool_,*shapelabel_,
	filletmaker,faceA,faceB,radius,blendname) != SV_OK)
  {
    fprintf(stderr,"Fillet creation didn't complete\n");
    return SV_ERROR;
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
	      shapetool_,*shapelabel_,*shapelabel_) != SV_OK)
	{
	  fprintf(stderr,"Could not pass face info\n");
	  return SV_ERROR;
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


  return SV_OK;
}

//------------
// ReadNative
//------------
// Read in a brep, step or iges format file.
//
int cvOCCTSolidModel::ReadNative( char *filename )
{
  #define n_debug_ReadNative 
  #ifdef debug_ReadNative 
  std::string msg("[cvOCCTSolidModel::ReadNative] ");
  std::cout << msg << "========== ReadNative ==========" << std::endl;
  std::cout << msg << "filename: " << filename << std::endl;
  #endif

  if (geom_ != nullptr) {
    this->RemoveShape();
  }

  // Get the lowercase file extension.
  std::string strFileName(filename);
  auto strExtension = strFileName.substr(strFileName.find_last_of(".") + 1);
  transform(strExtension.begin(), strExtension.end(), strExtension.begin(), ::tolower);
  const char *extension = strExtension.c_str();
  #ifdef debug_ReadNative 
  std::cout << msg << "extension: " << extension << std::endl;
  #endif

  Message_ProgressRange progress;
  // davep Handle(Message_ProgressIndicator) progress;
  BRep_Builder builder;
  this->NewShape();

  if (!strncmp(extension,"brep",4)) {
    fprintf(stdout,"Reading file %s\n",filename);
    Standard_Boolean worked = BRepTools::Read(*geom_, filename, builder, progress);

    if (worked = Standard_True) {
      fprintf(stdout,"File read\n");
    } else {
      fprintf(stderr,"File was not read\n");
      return SV_ERROR;
    }

    this->AddShape();

  } else if (!strncmp(extension,"step",4)) {
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
    return SV_ERROR;
  }

  return SV_OK;
}

// ---------------
// WriteNative
// ---------------
int cvOCCTSolidModel::WriteNative(int file_version, char *filename ) const
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"Need geometry to write file\n");
    return SV_ERROR;
  }
  const char *extension = strrchr(filename,'.');
  extension = extension+1;

  Message_ProgressRange progress;
  // davep Handle(Message_ProgressIndicator) progress;

  if (!strncmp(extension,"brep",4)) {
    fprintf(stdout,"Writing file %s\n",filename);
    Standard_Boolean worked = BRepTools::Write(*geom_,filename,progress);

    if (worked = Standard_True)
      fprintf(stdout,"File written\n");
    else
    {
      fprintf(stderr,"File was not written\n");
      return SV_ERROR;
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
      return SV_ERROR;
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
    return SV_ERROR;
  }

  return SV_OK;
}

//--------------------
// RegisterShapeFaces
//--------------------
// Add a face ID (1 .. num faces) as a label for each
// face in geom_.
//
int cvOCCTSolidModel::RegisterShapeFaces()
{
  if (geom_ == NULL) {
    fprintf(stderr,"Geometry is NULL, cannot register faces\n");
    return SV_ERROR;
  }

  #define n_debug_RegisterShapeFaces
  #ifdef debug_RegisterShapeFaces
  std::string msg("[sv4guiModelUtilsOCCT::RegisterShapeFaces] ");
  std::cout << msg << "----------" << std::endl;
  //std::cout << msg << "geom_: " << geom_ << std::endl;
  #endif

  TopExp_Explorer FaceExp;
  FaceExp.Init(*geom_, TopAbs_FACE);
  int num_faces = 0;

  for (int i = 1; FaceExp.More(); FaceExp.Next(),i++) {
    TopoDS_Face tmpFace = TopoDS::Face(FaceExp.Current());
    int faceid = i;
    #ifdef debug_RegisterShapeFaces
    std::cout << msg << "Face: " << i << " AddFaceLabel for face ID: " << faceid << std::endl;
    #endif
    AddFaceLabel(tmpFace, faceid);
    num_faces += 1;
  }

  #ifdef debug_RegisterShapeFaces
  std::cout << msg << "num_faces: " << num_faces << std::endl;
  std::cout << msg << "Done\n" << std::endl;
  #endif

  return SV_OK;
}

//--------------
// AddFaceLabel
//--------------
// Add a face with ID 'id' to the shapetool_ assembly. 
//
// This adds a face as a sub-shape to 'shapetool_'
//
//   TDF_Label tmpLabel = shapetool_->AddSubShape(*shapelabel_, face);
//
// shape - a face entity
//
int cvOCCTSolidModel::AddFaceLabel(TopoDS_Shape &face, const int id)
{
  #define n_debug_AddFaceLabel
  #ifdef debug_AddFaceLabel 
  std::string msg("[sv4guiModelUtilsOCCT::AddFaceLabel] ");
  std::cout << msg << "----------" << std::endl;
  std::cout << msg << "id: " << id << std::endl;
  //std::cout << msg << "shapelabel_: " << *shapelabel_ << std::endl;
  #endif

  if (face.IsNull()) {
    fprintf(stderr,"Face is NULL, cannot add\n");
    return SV_ERROR;
  }

  const TopoDS_Face& faceTmp = TopoDS::Face(face);
  if (face.IsNull()) { 
    throw std::runtime_error("[AddFaceLabel] The shape is not a TopoDS_Face");
    return SV_ERROR;
  }

  // If a face already has an ID then return it in 'checkid'
  // and associate that with the ID for 'shape'.
  //
  int checkid = -1;
  OCCTUtils_GetFaceLabel(face, shapetool_, *shapelabel_, checkid);

  if (checkid != -1) {
    #ifdef debug_AddFaceLabel 
    std::cout << msg << "ReLabelFace with id: " << id << std::endl;
    #endif
    if (OCCTUtils_ReLabelFace(face, shapetool_, *shapelabel_, id) != SV_OK) {
      throw std::runtime_error("ERROR: Face has no label");
      //fprintf(stderr,"Face has no label, which doesn't make sense\n");
      //return SV_ERROR;
    }

    return SV_OK;
  }

  // Get new label for topo (face,edge)
  TDF_Label tmpLabel = shapetool_->AddSubShape(*shapelabel_, face);
  numFaces_ += 1;
  #ifdef debug_AddFaceLabel 
  std::cout << msg << "numFaces_: " << numFaces_ << std::endl;
  //std::cout << msg << "tmpLabel: " << tmpLabel << std::endl;
  #endif

  // Records the shape 'face' to maintain topological attributes.
  //
  if (tmpLabel.IsNull()) {
    fprintf(stderr,"Face is not part of shape\n");
    return SV_ERROR;
  }

  TNaming_Builder builder(tmpLabel);
  builder.Generated(face);

  // Add a sub-label for face ID and set it to 'id'.
  //
  TDF_Label idLabel = tmpLabel.FindChild(0);
  TNaming_Builder idBuilder(idLabel);
  idBuilder.Generated(face);
  Handle(TDataStd_Integer) INT = new TDataStd_Integer();
  idLabel.AddAttribute(INT);
  TDataStd_Name::Set(idLabel, "ID");
  INT->Set(id);
  #ifdef debug_AddFaceLabel 
  std::cout << msg << "FindChild(0) " << std::endl;
  std::cout << msg << "Add ID: " << id << std::endl;
  //std::cout << msg << "idLabel: " << idLabel << std::endl;
  #endif 

  // Add a sub-label for face name and set it to 'noname'.
  //
  TDF_Label nameLabel = tmpLabel.FindChild(1);
  TNaming_Builder nameBuilder(nameLabel);
  nameBuilder.Generated(face);
  Handle(TDataStd_ExtStringArray) NSTRING = new TDataStd_ExtStringArray();
  nameLabel.AddAttribute(NSTRING);
  TDataStd_Name::Set(idLabel, "NAME");
  OCCTUtils_SetExtStringArrayFromChar(NSTRING, "noname");
  #ifdef debug_AddFaceLabel 
  std::cout << msg << "FindChild(1) " << std::endl;
  std::cout << msg << "Set to noname " << std::endl;
  //std::cout << msg << "nameLabel: " << nameLabel << std::endl;
  #endif 

  // Add a sub-label for parent name and set to 'noparent'.
  //
  TDF_Label parentLabel = tmpLabel.FindChild(2);
  TNaming_Builder parentBuilder(parentLabel);
  parentBuilder.Generated(face);
  Handle(TDataStd_ExtStringArray) PSTRING = new TDataStd_ExtStringArray();
  parentLabel.AddAttribute(PSTRING);
  TDataStd_Name::Set(idLabel, "PARENT");
  OCCTUtils_SetExtStringArrayFromChar(PSTRING, "noparent");
  #ifdef debug_AddFaceLabel 
  std::cout << msg << "FindChild(2) " << std::endl;
  std::cout << msg << "Set to noparent" << std::endl;
  //std::cout << msg << "parentLabel: " << parentLabel << std::endl;
  #endif 

  #ifdef debug_AddFaceLabel 
  //std::cout << msg << "tmpLabel: " << tmpLabel << std::endl;
  //std::cout << msg << "shapelabel_: " << *shapelabel_ << std::endl;
  std::cout << msg << "Done " << std::endl;
  std::cout << msg << "----------" << std::endl;
  #endif 

  return SV_OK;
}

//----------
// NewShape
//----------
// Create a new shape.
//
int cvOCCTSolidModel::NewShape()
{
  if (geom_ != NULL) {
    this->RemoveShape();
  }

  geom_ = new TopoDS_Shape;

  shapelabel_ = new TDF_Label;

  return SV_OK;
}

//----------
// AddShape
//----------
// Sets the geom_ to the top-level shape.
//
int cvOCCTSolidModel::AddShape()
{
  if (geom_ == NULL) {
    return SV_ERROR;
  }

  // Creates new (empty) top-level shape  
  *shapelabel_ = shapetool_->NewShape();

  // Sets representation (TopoDS_Shape) for top-level shape.
  shapetool_->SetShape(*shapelabel_, *geom_);

  this->RegisterShapeFaces();

  return SV_OK;
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
      return SV_ERROR;
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
      return SV_ERROR;
  }

  return SV_OK;
}

//----------------------
// CreateBSplineSurface
//----------------------
//
int cvOCCTSolidModel::CreateBSplineSurface(double **CX,double **CY,double **CZ,
    int &len1,int &len2,double *uKnots,int &uKlen,double *vKnots,int &vKlen,
    double *uMults,int &uMlen,double *vMults,int &vMlen,int &p,int &q)
{
  #define n_debug_CreateBSplineSurface
  #ifdef debug_CreateBSplineSurface
  std::string msg("[sv4guiModelUtilsOCCT::CreateBSplineSurface] ");
  std::cout << msg << "========== CreateBSplineSurface =========" << std::endl;
  #endif

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
    return SV_ERROR;
  }

  BRepBuilderAPI_MakeShell shellBuilder(aSurf);
  this->NewShape();
  *geom_ = shellBuilder.Shape();

  //Attacher!
  TopoDS_Wire wires[2];
  Standard_Real sewtoler =  1.e-6;
  Standard_Real closetoler =  1.e-2;
  ShapeFix_FreeBounds findFree(*geom_,sewtoler,closetoler, Standard_False,Standard_False);
  TopoDS_Compound freeWires = findFree.GetClosedWires();
  TopExp_Explorer NewEdgeExp;
  NewEdgeExp.Init(freeWires,TopAbs_EDGE);

  for (int i=0;NewEdgeExp.More();NewEdgeExp.Next(),i++) {
    TopoDS_Edge tmpEdge = TopoDS::Edge(NewEdgeExp.Current());
    BRepBuilderAPI_MakeWire wiremaker(tmpEdge);
    wiremaker.Build();
    wires[i] = wiremaker.Wire();
  }

  Standard_Real pres3d = 1.0e-6;

  if (OCCTUtils_ShapeFromBSplineSurface(surface,*geom_,wires[0],wires[1],pres3d) != SV_OK)
  {
    fprintf(stderr,"Error in conversion from bspline surface to shape\n");
    return SV_ERROR;
  }
  //TopoDS_Face firstFace,lastFace;
  //*geom_ = OCCTUtils_MakeSolid(shellBuilder.Shell(),wires[0],wires[1],firstFace,lastFace,pres3d);

  #ifdef debug_CreateBSplineSurface
  std::cout << msg << "this->AddShape() ... " << std::endl;
  #endif
  this->AddShape();

  // Name faces
  //
  #ifdef debug_CreateBSplineSurface
  std::cout << msg << "Name faces  ... " << std::endl;
  std::cout << msg << "OCCTUtils_SetFaceAttribute gdscName wall " << std::endl;
  std::cout << msg << "shapelabel_: " << *shapelabel_ << std::endl;
  #endif

  TopExp_Explorer anExp(*geom_, TopAbs_FACE);
  int num_faces = 0;

  for (int i = 0; anExp.More(); anExp.Next(),i++) {
    TopoDS_Face tmpFace = TopoDS::Face(anExp.Current());
    OCCTUtils_SetFaceAttribute(tmpFace, shapetool_, *shapelabel_, "gdscName", "wall");
    num_faces += 1;
  }

  #ifdef debug_CreateBSplineSurface
  std::cout << msg << "num_faces: " << num_faces << std::endl;
  #endif

  return SV_OK;
}

// ---------------
// GetOnlyPD
// ---------------
int cvOCCTSolidModel::GetOnlyPD(vtkPolyData *pd,double &max_dist) const
{
  if (pd == NULL)
    return SV_ERROR;

  if (VtkUtils_PDCheckArrayName(pd,1,"MESH_TYPES"))
  {
    vtkSmartPointer<vtkCleanPolyData> cleaner = vtkSmartPointer<vtkCleanPolyData>::New();
    cleaner->SetInputData(pd);
    cleaner->PointMergingOn();
    cleaner->Update();

    auto threshold_surface = VtkUtils_ThresholdSurface(7.0, 7.0, "MESH_TYPES", cleaner->GetOutput());

    if (threshold_surface->GetNumberOfPoints() != 0)
    {
      pd->DeepCopy(threshold_surface);
    }

    // davep 
    #if 0 
    vtkSmartPointer<vtkThreshold> thresholder = vtkSmartPointer<vtkThreshold>::New();
    thresholder->SetInputData(cleaner->GetOutput());
    //Set Input Array to 0 port,0 connection,1 for Cell Data, and MESh_TYPES is the type name
    thresholder->SetInputArrayToProcess(0,0,0,1,"MESH_TYPES");
    //Source polydata is on MESH_TYPE 7
    thresholder->ThresholdBetween(7,7);
    thresholder->Update();
    //Extract surface
    vtkSmartPointer<vtkDataSetSurfaceFilter> surfacer = vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
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
   #endif

  }

  return SV_OK;
}

