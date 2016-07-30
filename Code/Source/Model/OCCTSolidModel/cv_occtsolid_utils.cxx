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

/** @file cv_occtsolid_utils.cxx
 *  @brief The implementations of functions in cv_polydatasolid_utils
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */
#include "SimVascular.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>
#include "cv_occtsolid_utils.h"

//OCCT Includes
#include "Precision.hxx"
#include "TopoDS.hxx"
#include "TopoDS_Face.hxx"
#include "TopoDS_Shell.hxx"
#include "TopExp.hxx"
#include "TopExp_Explorer.hxx"
#include "TopTools_Array1OfShape.hxx"
#include "TopTools_ListIteratorOfListOfShape.hxx"

#include "BSplCLib.hxx"
#include "BRepOffsetAPI_ThruSections.hxx"
#include "BRepClass3d_SolidClassifier.hxx"
#include "BRepTools_ReShape.hxx"
#include "BRepCheck_Solid.hxx"
#include "BRepCheck_ListOfStatus.hxx"
#include "BRepCheck_ListIteratorOfListOfStatus.hxx"
#include "BRep_Tool.hxx"
#include "BRep_Builder.hxx"
#include "BRepFill_Filling.hxx"
#include "BRepAdaptor_Curve.hxx"
#include "BRepBuilderAPI_FindPlane.hxx"
#include "BRepBuilderAPI_MakeFace.hxx"
#include "BRepBuilderAPI_MakeShell.hxx"
#include "BRepBuilderAPI_MakeSolid.hxx"
#include "BRepBuilderAPI_MakeWire.hxx"
#include "BRepBuilderAPI_MakeEdge.hxx"
#include "BRepBuilderAPI_Sewing.hxx"
#include "BRepFilletAPI_MakeFillet.hxx"

//#include "ChFi3D_FilletShape.hxx"

#include "TDataStd_Integer.hxx"
#include "TDataStd_Name.hxx"
#include "Standard_Real.hxx"
#include "Standard_NullObject.hxx"
#include "StdFail_NotDone.hxx"
#include "Standard_Integer.hxx"
#include "TDataStd_Integer.hxx"
#include "TNaming_Builder.hxx"

#include "ShapeAnalysis_Surface.hxx"
#include "ShapeFix_FreeBounds.hxx"
#include "ShapeFix_Shape.hxx"

#include "GeomAPI_Interpolate.hxx"
#include "Geom_Plane.hxx"
#include "GeomFill_Line.hxx"
#include "GeomFill_AppSurf.hxx"
#include "GeomFill_SectionGenerator.hxx"
#include "GeomConvert.hxx"
#include "GeomConvert_ApproxCurve.hxx"
#include "GeomConvert_CompCurveToBSplineCurve.hxx"
#include "Geom_BSplineSurface.hxx"
#include "Geom_BoundedCurve.hxx"
#include "Geom_TrimmedCurve.hxx"
#include "Geom2d_Line.hxx"
#include "Geom_Conic.hxx"
#include "GeomLProp_SLProps.hxx"
#include "GCPnts_UniformAbscissa.hxx"

#include <string>
#include <sstream>
#include <iostream>

//Function to turn an integer into a string
int charToInt(const char *in)
{
  int out = atoi(in);
  return out;
}

// ---------------------
// OCCTUtils_SetExtStringArray
// ---------------------
/**
 * @brief Helper function to create an occt string array from a character
 * array. Used to set labels on faces
 * @param &array the new string array in which to put the char array
 * @param &charstr the character to put in array
 * @return CV_OK if function completes properly
 */
int OCCTUtils_SetExtStringArrayFromChar(Handle(TDataStd_ExtStringArray) &array,
    char *charstr)
{
  int lower = 0;
  int upper = strlen(charstr);
  array->Init(lower,upper);
  for (int i=lower;i<upper;i++)
  {
    array->SetValue(i,charstr[i]);
  }

  return CV_OK;
}

// ---------------------
// OCCTUtils_GetExtStringArray
// ---------------------
/**
 * @brief Helper function get a character array from an occt string array.
 * @param &array the string array to be extracted
 * @param &charstr the char array to put in the information
 * @return CV_OK if function completes properly
 */
int OCCTUtils_GetExtStringArrayAsChar(Handle(TDataStd_ExtStringArray) &array,
    char *charstr)
{
    std::stringstream streamer;
    for (int i=array->Lower();i<array->Upper();i++)
    {
      TCollection_AsciiString asciiString(array->Value(i),'?');
      streamer << asciiString.ToCString();
    }
    std::string outstr = streamer.str();
    sprintf(charstr,"%s",outstr.c_str());

    return CV_OK;
}

// ---------------------
// Class LinearFunc
// ---------------------
/**
 * @brief Simple class to set up linear interpolation between points
 */
class LinearFunc
{
  public:
    LinearFunc(Standard_Real x1,Standard_Real y1,
	Standard_Real x2,Standard_Real y2);
    ~LinearFunc() {;}

    Standard_Real GetY(Standard_Real x);
  private:
    Standard_Real m_;
    Standard_Real b_;
};

//Constructor
LinearFunc::LinearFunc(Standard_Real x1,Standard_Real y1,
    		       Standard_Real x2,Standard_Real y2)
{
  m_ = (y2 - y1)/(x2 - x1);
  b_ = y2 - (m_*x2);
}

//Get output linear interpolated value
Standard_Real LinearFunc::GetY(Standard_Real x)
{
  Standard_Real y = m_*x + b_;
  return y;
}

// ---------------------
// OCCTUtils_CreateEdgeBlend
// ---------------------
/**
 * @brief Procedure to create edge blend between two faces faceA and faceB
 * @param &shape shape containing faces and resultant shape with blend
 * @param shapetool the XDEDoc manager that contains attribute info
 * @param shapelabel the label for the shape registered in XDEDoc
 * @param filletmake the occt API to make a fillet
 * @param faceA first integer face to blend
 * @param faceB second integer face to blend
 * @param radius Maximum radius to set anywhere on the fillet.
 * @param minRadius Minimum radius to set anywhere on the fillet. A linear
 * interpolation is created between the minimum and maximum radius specified
 * based on the angle created bewteen the two faces at a set number of points
 * around the fillet edge. The new fillet radius value will be somehwere
 * between the maximum and minimum radius values given.
 * @param blendname Name to be given the new face created for the shape
 * @return CV_OK if function completes properly
 */
int OCCTUtils_CreateEdgeBlend(TopoDS_Shape &shape,
		Handle(XCAFDoc_ShapeTool) &shapetool,TDF_Label &shapelabel,
		BRepFilletAPI_MakeFillet &filletmaker,
		int faceA,int faceB,double radius,
		char blendname[])
{
  //if (minRadius > radius)
  //  fprintf(stderr,"Minimum radius is larger than Maximum radius\n");

  char *name;
  char nameA[255];
  char nameB[255];
  TopTools_IndexedDataMapOfShapeListOfShape anEFsMap;
  TopExp::MapShapesAndAncestors (shape, TopAbs_EDGE,
      TopAbs_FACE, anEFsMap);
  int num = anEFsMap.Extent();
  int found = 0;

  TopoDS_Shape geomcopy = shape;
  TopoDS_Edge filletEdge;
  for (int i=1;i < num+1;i++)
  {
    TopTools_ListOfShape faces = anEFsMap.FindFromIndex(i);
    TopoDS_Shape face1 = faces.First();
    int faceId1,faceId2;
    OCCTUtils_GetFaceLabel(face1,shapetool,shapelabel,faceId1);
    TopoDS_Shape face2 = faces.Last();
    OCCTUtils_GetFaceLabel(face2,shapetool,shapelabel,faceId2);
    if ((faceId1 == faceA && faceId2 == faceB) ||
		    (faceId1 == faceB && faceId2 == faceA))
    {
      found++;
      char* checkid1;
      char* checkid2;
      int intcheck1,intcheck2;
      filletEdge = TopoDS::Edge(anEFsMap.FindKey(i));
      OCCTUtils_GetFaceAttribute(face1,shapetool,shapelabel,
	  "gdscName",&name);
      strncpy(nameA,name,sizeof(nameA));
      OCCTUtils_GetFaceAttribute(face2,shapetool,shapelabel,
	  "gdscName",&name);
      strncpy(nameB,name,sizeof(nameB));

      ////Trying to get a curvature measure
      //BRepAdaptor_Curve curveAdaptor;
      //curveAdaptor.Initialize(filletEdge);
      //Standard_Integer NbPts = 20;
      //GCPnts_UniformAbscissa uAbs;
      //uAbs.Initialize(curveAdaptor,NbPts);

      //if (!uAbs.IsDone())
      //{
      //  fprintf(stderr,"Could not create points on edge\n");
      //  return CV_ERROR;
      //}
      //TopoDS_Face face1ForCurve = TopoDS::Face(faces.First());
      //TopoDS_Face face2ForCurve = TopoDS::Face(faces.Last());
      //Handle(Geom_Surface) surfHand1 =
      //        BRep_Tool::Surface(face1ForCurve);
      //Handle(Geom_Surface) surfHand2 =
      //        BRep_Tool::Surface(face2ForCurve);
      //ShapeAnalysis_Surface analyzer1(surfHand1);
      //ShapeAnalysis_Surface analyzer2(surfHand2);
      //TColgp_Array1OfPnt2d radLawArray(1,uAbs.NbPoints());
      //Standard_Real minAng = M_PI + 1.0;
      //Standard_Real maxAng = -1.0;
      //for (int j=1;j<=uAbs.NbPoints();j++)
      //{
      //  gp_Pnt nextPnt = curveAdaptor.Value(uAbs.Parameter(j));
      //  gp_Pnt2d face1UV = analyzer1.ValueOfUV(nextPnt,1.0e-6);
      //  gp_Pnt2d face2UV = analyzer2.ValueOfUV(nextPnt,1.0e-6);
      //  GeomLProp_SLProps prop1(surfHand1,face1UV.X(),face1UV.Y(),1,1.0e-6);
      //  GeomLProp_SLProps prop2(surfHand2,face2UV.X(),face2UV.Y(),1,1.0e-6);
      //  fprintf(stderr,"Curvature 1 check %.4f\n",prop1.MeanCurvature());
      //  fprintf(stderr,"Curvature 2 check %.4f\n",prop2.MeanCurvature());
      //  gp_Vec f1tan1 = prop1.D1U();
      //  gp_Vec f1tan2 = prop1.D1V();
      //  gp_Vec f2tan1 = prop2.D1U();
      //  gp_Vec f2tan2 = prop2.D1V();
      //  gp_Vec norm1 = f1tan1.Crossed(f1tan2);
      //  gp_Vec norm2 = f2tan1.Crossed(f2tan2);
      //  Standard_Real ang = norm1.Angle(norm2);
      //  fprintf(stderr,"Angle between face normals at point is: %.4f\n",ang);
      //  gp_Pnt2d angSet(uAbs.Parameter(j),ang);
      //  radLawArray.SetValue(j,angSet);
      //  if (ang < minAng)
      //    minAng = ang;
      //  if (ang > maxAng)
      //    maxAng = ang;
      //}
      //fprintf(stderr,"Max angle: %.4f\n",maxAng);
      //fprintf(stderr,"Min angle: %.4f\n",minAng);
      //LinearFunc radLaw(minAng,minRadius,maxAng,radius);
      //fprintf(stderr,"Checking worked max radius: %.4f\n",radLaw.GetY(maxAng));
      //fprintf(stderr,"Checking worked min radius: %.4f\n",radLaw.GetY(minAng));
      //for (int j=1;j<=uAbs.NbPoints();j++)
      //{
      //  gp_Pnt2d currVal = radLawArray.Value(j);
      //  Standard_Real newRad = radLaw.GetY(currVal.Y());
      //  gp_Pnt2d newVal(currVal.X(),newRad);
      //  radLawArray.SetValue(j,newVal);
      //  fprintf(stderr,"%d point has new radius value of %.4f for parameter %.4f\n",j,newVal.Y(),newVal.X());
      //}
      //filletmaker.Add(radLawArray,filletEdge);
      filletmaker.Add(radius,filletEdge);
    }
  }

  fprintf(stderr,"Number of edges %d\n",found);
  if (found == 0)
  {
    fprintf(stderr,"No edges between faces\n");
    return CV_ERROR;
  }
  sprintf(blendname,"wall_blend_%s_%s",nameA,nameB);

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
  if (filletmaker.IsDone() != 1)
  {
    fprintf(stderr,"Not done\n");
    return CV_ERROR;
  }
  fprintf(stderr,"Number faulty contours: %d\n",filletmaker.NbFaultyContours());
  shape = tmpShape;

  return CV_OK;
}
// ---------------------
// OCCTUtils_CreateEdgeBlend
// ---------------------
/**
 * @brief Procedure to create edge blend between two faces faceA and faceB
 * @param &shape shape containing faces and resultant shape with blend
 * @param shapetool the XDEDoc manager that contains attribute info
 * @param shapelabel the label for the shape registered in XDEDoc
 * @param filletmake the occt API to make a fillet
 * @param faceA first integer face to blend
 * @param faceB second integer face to blend
 * @param radius Maximum radius to set anywhere on the fillet.
 * @param minRadius Minimum radius to set anywhere on the fillet. A linear
 * interpolation is created between the minimum and maximum radius specified
 * based on the angle created bewteen the two faces at a set number of points
 * around the fillet edge. The new fillet radius value will be somehwere
 * between the maximum and minimum radius values given.
 * @param blendname Name to be given the new face created for the shape
 * @return CV_OK if function completes properly
 */
int OCCTUtils_CapShapeToSolid(TopoDS_Shape &shape,TopoDS_Shape &geom,
    BRepBuilderAPI_Sewing &attacher,int &numFilled)
{
  if (shape.Closed())
  {
    geom = shape;
    fprintf(stdout,"Shape is closed, nothing to be done\n");
    return CV_OK;
  }

  //Attacher!
  attacher.Add(shape);
  Standard_Real sewtoler =  1.e-6;
  Standard_Real closetoler =  1.e-4;
  ShapeFix_FreeBounds findFree(shape,sewtoler,closetoler,
        	  Standard_False,Standard_False);
  TopoDS_Compound freeWires = findFree.GetClosedWires();
  TopExp_Explorer NewEdgeExp;
  NewEdgeExp.Init(freeWires,TopAbs_EDGE);
  for (int i=0;NewEdgeExp.More();NewEdgeExp.Next(),i++)
  {
    TopoDS_Edge tmpEdge = TopoDS::Edge(NewEdgeExp.Current());

    BRepBuilderAPI_MakeWire wiremaker(tmpEdge);
    wiremaker.Build();

    int degOfCap=2;
    int numPointsOnCurve = 20;
    BRepFill_Filling filler(degOfCap,numPointsOnCurve);
    filler.Add(tmpEdge,GeomAbs_C0,Standard_True);

    try {
      filler.Build();
    }
    catch (Standard_Failure)
    {
      fprintf(stderr,"Failure when filling holes\n");
      return CV_ERROR;
    }

    TopoDS_Face newFace = filler.Face();
    //if (!OCCTUtils_IsSameOrientedWEdge(newFace,shape,tmpEdge))
    //{
    //  fprintf(stderr,"Reversing\n");
    //  newFace.Reversed();
    //}
    attacher.Add(newFace);
    numFilled ++;
  }
  fprintf(stderr,"Number of holes found: %d\n",numFilled);
  if (numFilled == 0)
  {
    fprintf(stderr,"No holes found\n");
    return CV_OK;
  }
  attacher.Perform();

  TopoDS_Shell tmpShell;
  try {
    tmpShell = TopoDS::Shell(attacher.SewedShape());
  }
  catch (Standard_TypeMismatch) {
    fprintf(stderr,"No open boundaries found\n");
    return CV_ERROR;
  }

  BRepBuilderAPI_MakeSolid solidmaker(tmpShell);
  geom = solidmaker.Solid();
  geom.Closed(Standard_True);

  //BRep_Builder BB;
  //TopoDS_Solid solid;
  //BB.MakeSolid(solid);
  //BB.Add(solid,tmpShell);

  ////Set Orientation
  //BB.MakeSolid(solid);
  //BRepClass3d_SolidClassifier clas3d(solid);
  //clas3d.PerformInfinitePoint(Precision::Confusion());
  //fprintf(stderr,"Print state: %d\n",clas3d.State());
  //if (clas3d.State() == TopAbs_IN) {
  //  BB.MakeSolid(solid);
  //  TopoDS_Shape aLocalShape = tmpShell.Reversed();
  //  BB.Add(solid, TopoDS::Shell(aLocalShape));
  //}

  //solid.Closed(Standard_True);
  //geom = solid;
  return CV_OK;
}

// ---------------------
// OCCTUtils_MakeLoftedSurf
// ---------------------
/**
 * @brief Procedure to create a lofted surface from a set of wires
 * @param curves list of wires to be lofted. Wires should be created by
 * uniformly spaced points circumferentially and should be aligned with
 * first points matching
 * @param shape place to store the output lofted surface
 * @param numCurves number of wires contained in curves
 * @param continuity desired continuity of the ouptut surface. Typically, 2
 * works okay. Trouble with higher or lower
 * @param partype The parametrization method. 0 is chord, 1 is centripetal,
 * 2 is isoparametric. Depends on wires which works best.
 * @param w1, first weighting to be used only if smoothing
 * @param w2, second weighting to be used only if smoothing
 * @param w3, third weighting to be used only if smoothing
 * @param smoothing indicates whether smoothing should be used
 * @return CV_OK if function completes properly
 */
int OCCTUtils_MakeLoftedSurf(TopoDS_Wire *curves, TopoDS_Shape &shape,
		int numCurves,int continuity,
		int partype, double w1, double w2, double w3, int smoothing)
{
  //Methods using GeomFill_SectionGenerator
  GeomFill_SectionGenerator sectioner;
  Handle(Geom_BSplineSurface) surface;
  Handle(Geom_BSplineSurface) tmpSurface;
  Handle(Geom_BSplineCurve) BS, BS1;
  Handle(Geom_TrimmedCurve) curvTrim;

  Standard_Boolean checkDegenerate = Standard_False;
  for (int i = 0; i< numCurves;i++)
  {
    TopExp_Explorer getEdge(curves[i],TopAbs_EDGE);
    TopoDS_Edge tmpEdge = TopoDS::Edge(getEdge.Current());

    checkDegenerate = BRep_Tool::Degenerated(tmpEdge);
    if (checkDegenerate == Standard_True)
    {
      fprintf(stderr,"Degenerate wire detected\n");
      return CV_ERROR;
    }
    Handle(Geom_BSplineCurve) curvBS = OCCTUtils_EdgeToBSpline(tmpEdge);

    Standard_Real aTolV = Precision::Confusion();
    aTolV = 1.e-3;
    GeomConvert_CompCurveToBSplineCurve compBS(curvBS);
    compBS.Add(curvBS,aTolV,Standard_True,Standard_False,1);
    BS = compBS.BSplineCurve();
    sectioner.AddCurve(BS);
  }

  sectioner.Perform(Precision::PConfusion());
  Handle(GeomFill_Line) line = new GeomFill_Line(numCurves);

  Standard_Real pres3d = 1.e-6;
  Standard_Integer nbIt = 3;
  if(pres3d <= 1.e-3) nbIt = 0;

  Standard_Integer degmin = 2, degmax = 2;//Max(myDegMax, degmin);
  Standard_Boolean SpApprox = Standard_True;

  GeomFill_AppSurf anApprox(degmin, degmax, pres3d, pres3d, nbIt);
  anApprox.SetContinuity((GeomAbs_Shape) continuity);

  //anApprox.SetCriteriumWeight(w1, w2, w3);
  if(smoothing) {
    anApprox.SetCriteriumWeight(w1, w2, w3);
    try
    {
      anApprox.PerformSmoothing(line, sectioner);
    }
    catch (std::bad_alloc)
    {
      fprintf(stderr,"Not enough memory for this smoothing\n");
      return CV_ERROR;
    }
  }
  else
  {
    anApprox.SetParType((Approx_ParametrizationType) partype);
    anApprox.Perform(line, sectioner, SpApprox);
  }

  if(anApprox.IsDone()) {
    //fprintf(stderr,"UDegree %d\n",anApprox.UDegree());
    //fprintf(stderr,"VDegree %d\n",anApprox.VDegree());
    //TColStd_Array2OfReal surfweights = anApprox.SurfWeights();
    //fprintf(stderr,"RowLength %d\n",surfweights.RowLength());
    //fprintf(stderr,"ColLength %d\n",surfweights.ColLength());
    //fprintf(stderr,"SurfWeights\n");
    //for (int i=1;i<surfweights.ColLength();i++)
    //{
    //  for (int j=1;j<surfweights.RowLength();j++)
    //  {
    //    fprintf(stderr,"%.2f ",surfweights.Value(i,j));
    //  }
    //  fprintf(stderr,"\n");
    //}
    //TColStd_Array1OfReal uknots = anApprox.SurfUKnots();
    //TColStd_Array1OfReal vknots = anApprox.SurfVKnots();
    //fprintf(stderr,"Uknot length %d\n",uknots.Length());
    //for (int i=1;i<=uknots.Length();i++)
    //{
    //  fprintf(stderr,"%.8f ",uknots.Value(i));
    //}
    //fprintf(stderr,"\n");
    //fprintf(stderr,"Vknot length %d\n",vknots.Length());
    //for (int i=1;i<=vknots.Length();i++)
    //{
    //  fprintf(stderr,"%.2f ",vknots.Value(i));
    //}
    //fprintf(stderr,"\n");
    //TColStd_Array1OfInteger umults = anApprox.SurfUMults();
    //TColStd_Array1OfInteger vmults = anApprox.SurfVMults();
    //fprintf(stderr,"Umult length %d\n",umults.Length());
    //for (int i=1;i<=umults.Length();i++)
    //{
    //  fprintf(stderr,"%d ",umults.Value(i));
    //}
    //fprintf(stderr,"\n");
    //fprintf(stderr,"Vmult length %d\n",vmults.Length());
    //for (int i=1;i<=vmults.Length();i++)
    //{
    //  fprintf(stderr,"%d ",vmults.Value(i));
    //}
    //fprintf(stderr,"\n");
    surface =
      new Geom_BSplineSurface(anApprox.SurfPoles(), anApprox.SurfWeights(),
      anApprox.SurfUKnots(), anApprox.SurfVKnots(),
      anApprox.SurfUMults(), anApprox.SurfVMults(),
      anApprox.UDegree(), anApprox.VDegree());
    //surface->SetUPeriodic();
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

  if (OCCTUtils_ShapeFromBSplineSurface(surface,shape,curves[0],curves[numCurves-1],pres3d) != CV_OK)
  {
    fprintf(stderr,"Error in conversion from bspline surface to shape\n");
    return CV_ERROR;
  }

  return CV_OK;
}

// ---------------------
// OCCTUtils_ShapeFromBSplineSurface
// ---------------------
int OCCTUtils_ShapeFromBSplineSurface(const Handle(Geom_BSplineSurface) surface,
    		TopoDS_Shape &shape,const TopoDS_Wire &first_wire,
		const TopoDS_Wire &last_wire,
		const int pres3d)
{
  gp_Pnt tmpPoint = surface->Pole(1,1);
  tmpPoint.SetX(tmpPoint.X()+10.0);
  tmpPoint.SetY(tmpPoint.Y()+10.0);
  tmpPoint.SetZ(tmpPoint.Z()+10.0);
  //surface->SetPole(1,1,tmpPoint);

  if(surface.IsNull()) {
    fprintf(stderr,"Lofting did not complete\n");
    return CV_ERROR;
  }

  // create the new surface
  TopoDS_Shell shell;
  TopoDS_Face face;
  TopoDS_Wire W;
  TopoDS_Edge edge, edge1, edge2, edge3, edge4, couture;
  TopTools_Array1OfShape vcouture(1, 1);

  BRep_Builder B;
  B.MakeShell(shell);

  TopoDS_Wire newW1, newW2;
  BRep_Builder BW1, BW2;
  BW1.MakeWire(newW1);
  BW2.MakeWire(newW2);

  TopLoc_Location loc;
  TopoDS_Vertex v1f,v1l,v2f,v2l;

  Standard_Integer nbPnts = 21;

  TopoDS_Shape firstEdge;

  // segmentation of TS
  Standard_Real Ui1,Ui2,V0,V1;
  Ui1 = 0;
  Ui2 = 1;
  Ui1 = OCCTUtils_PreciseUpar(Ui1, surface);
  Ui2 = OCCTUtils_PreciseUpar(Ui2, surface);
  V0  = surface->VKnot(surface->FirstVKnotIndex());
  V1  = surface->VKnot(surface->LastVKnotIndex());
  surface->Segment(Ui1,Ui2,V0,V1);

  // return vertices
  TopExp_Explorer edge_one(first_wire,TopAbs_EDGE);
  edge =  TopoDS::Edge(edge_one.Current());
  TopExp::Vertices(edge,v1f,v1l);
  if (edge.Orientation() == TopAbs_REVERSED)
    TopExp::Vertices(edge,v1l,v1f);
  firstEdge = edge;

  TopExp_Explorer edge_last(last_wire,TopAbs_EDGE);
  edge =  TopoDS::Edge(edge_last.Current());
  TopExp::Vertices(edge,v2f,v2l);
  if (edge.Orientation() == TopAbs_REVERSED)
    TopExp::Vertices(edge,v2l,v2f);

  // make the face
  B.MakeFace(face, surface, Precision::Confusion());

  // make the wire
  B.MakeWire(W);

  // make the missing edges
  Standard_Real f1, f2, l1, l2;
  surface->Bounds(f1,l1,f2,l2);

  // --- edge 1
  B.MakeEdge(edge1, surface->VIso(f2), Precision::Confusion());
  v1f.Orientation(TopAbs_FORWARD);
  B.Add(edge1, v1f);
  v1l.Orientation(TopAbs_REVERSED);
  B.Add(edge1, v1l);
  B.Range(edge1, f1, l1);
  // processing of looping sections
  // store edges of the 1st section

  // --- edge 2
  B.MakeEdge(edge2, surface->VIso(l2), Precision::Confusion());
  v2f.Orientation(TopAbs_FORWARD);
  B.Add(edge2, v2f);
  v2l.Orientation(TopAbs_REVERSED);
  B.Add(edge2, v2l);
  B.Range(edge2, f1, l1);
  edge2.Reverse();


  // --- edge 3
  B.MakeEdge(edge3, surface->UIso(f1), Precision::Confusion());
  v1f.Orientation(TopAbs_FORWARD);
  B.Add(edge3, v1f);
  v2f.Orientation(TopAbs_REVERSED);
  B.Add(edge3, v2f);
  B.Range(edge3, f2, l2);
  couture = edge3;
  edge3.Reverse();

  // --- edge 4
  edge4 = couture;

  B.Add(W,edge1);
  B.Add(W,edge4);
  B.Add(W,edge2);
  B.Add(W,edge3);

  // set PCurve
  B.UpdateEdge(edge1,new Geom2d_Line(gp_Pnt2d(0,f2),gp_Dir2d(1,0)),face,
    Precision::Confusion());
  B.Range(edge1,face,f1,l1);
  B.UpdateEdge(edge2,new Geom2d_Line(gp_Pnt2d(0,l2),gp_Dir2d(1,0)),face,
    Precision::Confusion());
  B.Range(edge2,face,f1,l1);

  B.UpdateEdge(edge3,
    new Geom2d_Line(gp_Pnt2d(l1,0),gp_Dir2d(0,1)),
    new Geom2d_Line(gp_Pnt2d(f1,0),gp_Dir2d(0,1)),face,
    Precision::Confusion());
  B.Range(edge3,face,f2,l2);

  B.Add(face,W);
  B.Add(shell, face);

  // complete newW1 newW2
  TopoDS_Edge edge12 = edge1;
  TopoDS_Edge edge22 = edge2;
  edge12.Reverse();
  edge22.Reverse();
  BW1.Add(newW1, edge12);
  BW2.Add(newW2, edge22);

  // history
  TopTools_DataMapOfShapeShape generated;
  generated.Bind(firstEdge, face);

  shape = shell;
  TopoDS_Face first,last;
  shape = OCCTUtils_MakeShell(shell, newW1, newW2, pres3d, first, last);

  return CV_OK;
}

// ---------------------
// OCCTUtils_IsSameOriented
// ---------------------
/**
 * @brief Taken from BRepOffsetAPI_ThruSections
 */
Standard_Boolean OCCTUtils_IsSameOriented(const TopoDS_Shape& aFace,
  const TopoDS_Shape& aShell)
{
  TopExp_Explorer Explo(aFace, TopAbs_EDGE);
  TopoDS_Shape anEdge = Explo.Current();
  TopAbs_Orientation Or1 = anEdge.Orientation();

  TopTools_IndexedDataMapOfShapeListOfShape EFmap;
  TopExp::MapShapesAndAncestors( aShell, TopAbs_EDGE, TopAbs_FACE, EFmap );

  const TopoDS_Shape& AdjacentFace = EFmap.FindFromKey(anEdge).First();
  TopoDS_Shape theEdge;
  for (Explo.Init(AdjacentFace, TopAbs_EDGE); Explo.More(); Explo.Next())
  {
    theEdge = Explo.Current();
    if (theEdge.IsSame(anEdge))
      break;
  }

  TopAbs_Orientation Or2 = theEdge.Orientation();
  if (Or1 == Or2)
    return Standard_False;
  return Standard_True;
}

// ---------------------
// OCCTUtils_IsSameOrientedWEdge
// ---------------------
/**
 * @brief Taken from BRepOffsetAPI_ThruSections
 */
Standard_Boolean OCCTUtils_IsSameOrientedWEdge(const TopoDS_Shape& aFace,
  const TopoDS_Shape& aShell,const TopoDS_Shape &anEdge)
{
  TopAbs_Orientation Or1 = anEdge.Orientation();

  TopTools_IndexedDataMapOfShapeListOfShape EFmap;
  TopExp::MapShapesAndAncestors( aShell, TopAbs_EDGE, TopAbs_FACE, EFmap );

  const TopoDS_Shape& AdjacentFace = EFmap.FindFromKey(anEdge).First();
  TopoDS_Shape theEdge;
  TopExp_Explorer Explo(AdjacentFace, TopAbs_EDGE);
  for (Explo.Init(AdjacentFace, TopAbs_EDGE); Explo.More(); Explo.Next())
  {
    theEdge = Explo.Current();
    if (theEdge.IsSame(anEdge))
      break;
  }

  TopAbs_Orientation Or2 = theEdge.Orientation();
  if (Or1 == Or2)
    return Standard_False;
  return Standard_True;
}

// ---------------------
// OCCTUtils_MakeSolid
// ---------------------
/**
 * @brief Taken from BRepOffsetAPI_ThruSections
 */
TopoDS_Solid OCCTUtils_MakeSolid(TopoDS_Shell& shell, const TopoDS_Wire& wire1,
  const TopoDS_Wire& wire2, const Standard_Real presPln,
  TopoDS_Face& face1, TopoDS_Face& face2)
{
  if (shell.IsNull())
    StdFail_NotDone::Raise("Thrusections is not build");
  Standard_Boolean B = shell.Closed();
  BRep_Builder BB;

  if (!B)
  {
    // It is necessary to close the extremities
    B =  OCCTUtils_PerformPlan(wire1, presPln, face1);
    if (B) {
      B =  OCCTUtils_PerformPlan(wire2, presPln, face2);
      if (B) {
        if (!face1.IsNull() && !OCCTUtils_IsSameOriented( face1, shell ))
          face1.Reverse();
        if (!face2.IsNull() && !OCCTUtils_IsSameOriented( face2, shell ))
          face2.Reverse();

        if (!face1.IsNull())
          BB.Add(shell, face1);
        if (!face2.IsNull())
          BB.Add(shell, face2);

        shell.Closed(Standard_True);
      }
    }
  }

  TopoDS_Solid solid;
  BB.MakeSolid(solid);
  BB.Add(solid, shell);

  // verify the orientation the solid
  BRepClass3d_SolidClassifier clas3d(solid);
  clas3d.PerformInfinitePoint(Precision::Confusion());
  if (clas3d.State() == TopAbs_IN) {
    BB.MakeSolid(solid);
    TopoDS_Shape aLocalShape = shell.Reversed();
    BB.Add(solid, TopoDS::Shell(aLocalShape));
    //    B.Add(solid, TopoDS::Shell(newShell.Reversed()));
  }

  solid.Closed(Standard_True);
  return solid;
}

// ---------------------
// OCCTUtils_PerformPlan
// ---------------------
/**
 * @brief Taken from BRepOffsetAPI_ThruSections
 */
Standard_Boolean OCCTUtils_PerformPlan(const TopoDS_Wire& W,
  const Standard_Real presPln,
  TopoDS_Face& theFace)
{
  Standard_Boolean isDegen = Standard_True;
  TopoDS_Iterator iter(W);
  for (; iter.More(); iter.Next())
  {
    const TopoDS_Edge& anEdge = TopoDS::Edge(iter.Value());
    if (!BRep_Tool::Degenerated(anEdge))
      isDegen = Standard_False;
  }
  if (isDegen)
    return Standard_True;

  Standard_Boolean Ok = Standard_False;
  if (!W.IsNull()) {
    BRepBuilderAPI_FindPlane Searcher( W, presPln );
    if (Searcher.Found())
    {
      theFace = BRepBuilderAPI_MakeFace(Searcher.Plane(), W);
      Ok = Standard_True;
    }
    else // try to find another surface
    {
      BRepBuilderAPI_MakeFace MF( W );
      if (MF.IsDone())
      {
        theFace = MF.Face();
        Ok = Standard_True;
      }
    }
  }

  return Ok;
}

// ---------------------
// OCCTUtils_MakeShell
// ---------------------
/**
 * @brief Taken from BRepOffsetAPI_ThruSections
 */
TopoDS_Solid OCCTUtils_MakeShell(TopoDS_Shell& shell, const TopoDS_Wire& wire1,
  const TopoDS_Wire& wire2, const Standard_Real presPln,
  TopoDS_Face& face1, TopoDS_Face& face2)
{
  if (shell.IsNull())
    StdFail_NotDone::Raise("Thrusections is not build");
  Standard_Boolean B = shell.Closed();
  BRep_Builder BB;

  //if (!B)
  //{
  //  // It is necessary to close the extremities
  //  B =  OCCTUtils_PerformPlan(wire1, presPln, face1);
  //  if (B) {
  //    B =  OCCTUtils_PerformPlan(wire2, presPln, face2);
  //    if (B) {
  //      if (!face1.IsNull() && !OCCTUtils_IsSameOriented( face1, shell ))
  //        face1.Reverse();
  //      if (!face2.IsNull() && !OCCTUtils_IsSameOriented( face2, shell ))
  //        face2.Reverse();

  //      if (!face1.IsNull())
  //        BB.Add(shell, face1);
  //      if (!face2.IsNull())
  //        BB.Add(shell, face2);

  //      shell.Closed(Standard_True);
  //    }
  //  }
  //}

  TopoDS_Solid orientedshell;
  BB.MakeSolid(orientedshell);
  BB.Add(orientedshell, shell);

  // verify the orientation the solid
  BRepClass3d_SolidClassifier clas3d(orientedshell);
  clas3d.PerformInfinitePoint(Precision::Confusion());
  if (clas3d.State() == TopAbs_IN) {
    BB.MakeSolid(orientedshell);
    TopoDS_Shape aLocalShape = shell.Reversed();
    BB.Add(orientedshell, TopoDS::Shell(aLocalShape));
    //    B.Add(orientedshell, TopoDS::Shell(newShell.Reversed()));
  }

  //orientedshell.Closed(Standard_True);
  return orientedshell;
}

// ---------------------
// OCCTUtils_PreciseUpar
// ---------------------
/**
 * @brief Taken from BRepOffsetAPI_ThruSections
 */
Standard_Real OCCTUtils_PreciseUpar(const Standard_Real anUpar,
  const Handle(Geom_BSplineSurface)& aSurface)
{
  Standard_Real Tol = Precision::PConfusion();
  Standard_Integer i1, i2;

  aSurface->LocateU(anUpar, Tol, i1, i2);
  Standard_Real U1 = aSurface->UKnot(i1);
  Standard_Real U2 = aSurface->UKnot(i2);

  Standard_Real NewU = anUpar;

  NewU = (anUpar - U1 < U2 - anUpar)? U1 : U2;
  return NewU;
}

// ---------------------
// OCCTUtils_EdgeToBSpline
// ---------------------
/**
 * @brief Taken from BRepOffsetAPI_ThruSections
 */
Handle(Geom_BSplineCurve) OCCTUtils_EdgeToBSpline(const TopoDS_Edge& theEdge)
{
  Handle(Geom_BSplineCurve) aBSCurve;
  if (BRep_Tool::Degenerated(theEdge)) {
    // degenerated edge : construction of a point curve
    TColStd_Array1OfReal aKnots (1,2);
    aKnots(1) = 0.;
    aKnots(2) = 1.;

    TColStd_Array1OfInteger aMults (1,2);
    aMults(1) = 2;
    aMults(2) = 2;

    TColgp_Array1OfPnt aPoles(1,2);
    TopoDS_Vertex vf, vl;
    TopExp::Vertices(theEdge,vl,vf);
    aPoles(1) = BRep_Tool::Pnt(vf);
    aPoles(2) = BRep_Tool::Pnt(vl);

    aBSCurve = new Geom_BSplineCurve (aPoles, aKnots, aMults, 1);
  }
  else
  {
    // get the curve of the edge
    TopLoc_Location aLoc;
    Standard_Real aFirst, aLast;
    Handle(Geom_Curve) aCurve = BRep_Tool::Curve (theEdge, aLoc, aFirst, aLast);
    if (aCurve.IsNull())
      Standard_NullObject::Raise("Null 3D curve in edge");

    // convert its part used by edge to bspline; note that if edge curve is bspline,
    // conversion made via trimmed curve is still needed -- it will copy it, segment
    // as appropriate, and remove periodicity if it is periodic (deadly for approximator)
    Handle(Geom_TrimmedCurve) aTrimCurve = new Geom_TrimmedCurve (aCurve, aFirst, aLast);

    // special treatment of conic curve
    if (aTrimCurve->BasisCurve()->IsKind(STANDARD_TYPE(Geom_Conic)))
    {
      const Handle(Geom_Curve)& aCurveTrimmed = aTrimCurve; // to avoid ambiguity
      GeomConvert_ApproxCurve anAppr (aCurveTrimmed, Precision::Confusion(), GeomAbs_C1, 16, 14);
      if (anAppr.HasResult())
        aBSCurve = anAppr.Curve();
    }

    // general case
    if (aBSCurve.IsNull())
      aBSCurve = GeomConvert::CurveToBSplineCurve (aTrimCurve);

    // apply transformation if needed
    if (! aLoc.IsIdentity())
      aBSCurve->Transform (aLoc.Transformation());

    // reparameterize to [0,1]
    TColStd_Array1OfReal aKnots (1, aBSCurve->NbKnots());
    aBSCurve->Knots (aKnots);
    BSplCLib::Reparametrize (0., 1., aKnots);
    aBSCurve->SetKnots (aKnots);
  }

  // reverse curve if edge is reversed
  if (theEdge.Orientation() == TopAbs_REVERSED)
    aBSCurve->Reverse();

  return aBSCurve;
}

// ---------------------
// OCCTUtils_GetFaceIds
// ---------------------
/**
 * @brief Procedure to get face numbers that correspond to the scalars
 * assigned to the geometry
 * @param *geom input TopoDS_Shape on which to get the face ids
 * @param *v_num_faces int that contains the number of total face regions
 * @param **v_faces vector containing the array of numerical values
 * corresponding to each face region
 * @return CV_OK if function completes properly
 */
int OCCTUtils_GetFaceIds( const TopoDS_Shape &geom,
		Handle(XCAFDoc_ShapeTool) &shapetool,TDF_Label &shapelabel,
	       	int *v_num_faces, int **v_faces)
{
  int num = 0;

  const TopoDS_Shape& aShape = geom;

  TopExp_Explorer anExp (aShape, TopAbs_FACE);
  for (; anExp.More(); anExp.Next()) {
   const TopoDS_Face& aFace = TopoDS::Face (anExp.Current());
   num++;
  }

  *v_num_faces = num;

  if (num == 0) return CV_ERROR;

  (*v_faces) = new int [num];

  TopExp_Explorer anExp2 (aShape, TopAbs_FACE);

  int j = 0;

  for (; anExp2.More(); anExp2.Next()) {
    TopoDS_Face aFace = TopoDS::Face (anExp2.Current());
    int faceId= -1;
    OCCTUtils_GetFaceLabel(aFace,shapetool,shapelabel,faceId);
    //(*v_faces)[j] = aFace.HashCode(9999999999);
    (*v_faces)[j] = faceId;
    j++;
  }

  return CV_OK;
}

// -----------------
// OCCTUtils_GetFaceLabel
// -----------------
/**
 * @brief Procedure to get a singular face number
 * @param *geom input TopoDS_Shape on which to get the face ids
 * @param id is the scalar of the face region
 * @param returns input value and an error if the face does not have id
 * @return CV_OK if function completes properly
 */
int OCCTUtils_GetFaceLabel(const TopoDS_Shape &geom,
		const Handle(XCAFDoc_ShapeTool) &shapetool,TDF_Label &shapelabel,
	       	int &id)
{
  TDF_Label tmpLabel;
  shapetool->FindSubShape(shapelabel,geom,tmpLabel);
  if (tmpLabel.IsNull())
  {
    //fprintf(stderr,"Face does not have label\n");
    return CV_ERROR;
  }

  TDF_Label idLabel = tmpLabel.FindChild(0);
  if (idLabel.IsNull())
  {
    fprintf(stderr,"Face does not have id\n");
    return CV_ERROR;
  }
  //Retrive attribute
  Handle(TDataStd_Integer) INT = new TDataStd_Integer();
  idLabel.FindAttribute(TDataStd_Integer::GetID(),INT);
  id = INT->Get();

  return CV_OK;
}

// -------------------
// OCCTUtils_RenumberFaces
// -------------------
/**
 * @brief Procedure to renumber faces (not used)
 * @return CV_OK if function completes properly
 */
int OCCTUtils_RenumberFaces(TopoDS_Shape &shape,
		Handle(XCAFDoc_ShapeTool) &shapetool,TDF_Label &shapelabel)
{
  int *faces;
  int numFaces;
  int facerange;
  OCCTUtils_GetFaceIds(shape,shapetool,shapelabel,&numFaces,&faces);
  OCCTUtils_GetFaceRange(shape,shapetool,shapelabel,facerange);

  //Initialize to zero
  int *newmap = new int[numFaces];

  //Increase map one at spot for each face id
  for (int i=0;i<numFaces;i++)
    newmap[i] = -2;

  int checkid=-1;
  int currentid=1;
  TopExp_Explorer anExp(shape,TopAbs_FACE);
  while (checkid != facerange)
  {
    anExp.Init(shape,TopAbs_FACE);
    int found=0;
    for (int j=0;anExp.More();anExp.Next(),j++)
    {
      TopoDS_Face tmpFace = TopoDS::Face(anExp.Current());
      int faceid=-1;
      OCCTUtils_GetFaceLabel(tmpFace,shapetool,shapelabel,faceid);
      if (faceid == checkid)
      {
	if (newmap[j] == -2)
	{
	  newmap[j] = currentid++;
	  found=1;
	  break;
	}
      }
    }
    if (!found)
      checkid++;
  }

  anExp.Init(shape,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next(),i++)
  {
    TopoDS_Face tmpFace = TopoDS::Face(anExp.Current());
    if (OCCTUtils_ReLabelFace(tmpFace,shapetool,shapelabel,newmap[i]) != CV_OK)
    {
      fprintf(stderr,"Could not label face\n");
      return CV_ERROR;
    }
  }

  delete [] newmap;
  delete [] faces;
  return CV_OK;
}

// ----------------
// GetFaceRange
// ----------------
/**
 * @brief Procedure to get the range of id values on a shape
 * @param face_range the returned range of faces
 * @return CV_OK if function completes properly
 */
int OCCTUtils_GetFaceRange(const TopoDS_Shape &shape,
		Handle(XCAFDoc_ShapeTool) &shapetool,TDF_Label &shapelabel,
    		int &face_range)
{
  face_range = 0;
  TopExp_Explorer anExp(shape,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next())
  {
    const TopoDS_Face &tmpFace = TopoDS::Face(anExp.Current());
    int faceid =-1;
    OCCTUtils_GetFaceLabel(tmpFace,shapetool,shapelabel,faceid);
    if (faceid > face_range)
      face_range = faceid;
  }

  return CV_OK;
}


// -------------------
// OCCTUtils_GetOrientation
// -------------------
/**
 * @brief Procedure to get a shape orientation
 * @param *geom input TopoDS_Shape on which to get orientation
 * @return CV_OK if function completes properly
 */
int OCCTUtils_GetOrientation(const TopoDS_Shape &shape,int &orientation)
{
  orientation = (int) shape.Orientation();
  return CV_OK;
}

// -------------------
// OCCTUtils_SetOrientation
// -------------------
/**
 * @brief Procedure to set a face orientation
 * @param shape input TopoDS_Shape on which to set a faces orientation
 * @param face the face on which to set the orientation
 * @return CV_OK if function completes properly
 */
int OCCTUtils_SetOrientation(TopoDS_Shape &shape,TopoDS_Shape &face,int &orientation)
{
  Handle(BRepTools_ReShape) reshaper =  new BRepTools_ReShape();
  reshaper->ModeConsiderOrientation() = Standard_True;

  TopoDS_Shape compFace = face.Complemented();
  reshaper->Replace(face,compFace,Standard_True);
  TopoDS_Shape tmpShape = reshaper->Apply(shape,TopAbs_FACE);
  shape = tmpShape;

  return CV_OK;
}

// -------------------
// OCCTUtils_ReLabelFace
// -------------------
/**
 * @brief Procedure to relabel the face of a shape
 * @param shape input TopoDS_Shape face that needs to be relabled
 * @param id desired id for face
 * @return CV_OK if function completes properly
 */
int OCCTUtils_ReLabelFace( TopoDS_Shape &shape,
		Handle(XCAFDoc_ShapeTool) &shapetool,TDF_Label &shapelabel,
		int &id)
{
  if (shape.IsNull())
  {
    fprintf(stderr,"Face is NULL, cannot add\n");
    return CV_ERROR;
  }

  TDF_Label tmpLabel;
  shapetool->FindSubShape(shapelabel,shape,tmpLabel);
  if (tmpLabel.IsNull())
  {
    fprintf(stderr,"Face has not been given a label\n");
    return CV_ERROR;
  }
  TDF_Label idLabel = tmpLabel.FindChild(0);
  if (idLabel.IsNull())
  {
    fprintf(stderr,"Face has not been given an id\n");
    return CV_ERROR;
  }
  Handle(TDataStd_Integer) INT = new TDataStd_Integer();
  idLabel.FindAttribute(TDataStd_Integer::GetID(),INT);
  INT->Set(id);

  return CV_OK;
}

// -------------------
// OCCTUtils_GetNumberFaces
// -------------------
/**
 * @brief Procedure to return the number of faces in shape
 * @param shape input TopoDS_Shape to find number of faces
 * @param num_faces returns the number of faces found
 * @return CV_OK if function completes properly
 */
int OCCTUtils_GetNumberOfFaces(const TopoDS_Shape &shape,int &num_faces)
{
  num_faces = 0;
  TopExp_Explorer anExp(shape,TopAbs_FACE);
  for (int i=0;anExp.More();anExp.Next())
    num_faces++;

  return CV_OK;
}

// -------------------
// OCCTUtils_GetFaceAttribute
// -------------------
/**
 * @brief Procedure to get an attribute of the shape
 * @param shape input TopoDS_Shape to get attribute
 * @param shapetool the XDEDoc manager that contains attribute info
 * @param shapelabel the label for the shape registered in XDEDoc
 * @note attributes includ id, gdscName, and parent
 * @return CV_OK if function completes properly
 */
int OCCTUtils_GetFaceAttribute(const TopoDS_Shape &face,
		Handle(XCAFDoc_ShapeTool) &shapetool,TDF_Label &shapelabel,
    				char *attr, char **value)
{
  static char returnString[255];
  TDF_Label tmpLabel;
  shapetool->FindSubShape(shapelabel,face,tmpLabel);
  if (tmpLabel.IsNull())
  {
    fprintf(stderr,"Face is not labelled and thus has no attribute\n");
    return CV_ERROR;
  }
  if (!strncmp(attr,"gdscName",4))
  {
    TDF_Label nameLabel = tmpLabel.FindChild(1,Standard_False);
    if (nameLabel.IsNull())
    {
      fprintf(stderr,"gdscName label doesn't exist, cannot retrive name\n");
      return CV_ERROR;
    }
    Handle(TDataStd_ExtStringArray) NSTRING = new
      TDataStd_ExtStringArray();
    int isLabel = nameLabel.FindAttribute(TDataStd_ExtStringArray::GetID(),NSTRING);
    if (isLabel == 0)
    {
      fprintf(stderr,"gdscName attribute does not exist on face\n");
      return CV_ERROR;
    }
    returnString[0]='\0';
    OCCTUtils_GetExtStringArrayAsChar(NSTRING,returnString);
    *value = returnString;
  }
  else if (!strncmp(attr,"parent",6))
  {
    TDF_Label parentLabel = tmpLabel.FindChild(2,Standard_False);
    if (parentLabel.IsNull())
    {
      fprintf(stderr,"gdscName label doesn't exist, cannot retrive name\n");
      return CV_ERROR;
    }
    Handle(TDataStd_ExtStringArray) PSTRING = new
      TDataStd_ExtStringArray();
    int isLabel = parentLabel.FindAttribute(TDataStd_ExtStringArray::GetID(),PSTRING);
    if (isLabel == 0)
    {
      fprintf(stderr,"parent attribute does not exist on face\n");
      return CV_ERROR;
    }
    returnString[0]='\0';
    OCCTUtils_GetExtStringArrayAsChar(PSTRING,returnString);
    *value = returnString;
  }
  else if (!strncmp(attr,"id",2))
  {
    TDF_Label idLabel = tmpLabel.FindChild(0);
    Handle(TDataStd_Integer) INT = new TDataStd_Integer();
    int isLabel = idLabel.FindAttribute(TDataStd_Integer::GetID(),INT);
    if (isLabel == 0)
    {
      fprintf(stderr,"id attribute does not exist on face\n");
      return CV_ERROR;
    }
    fprintf(stderr,"Inside id and want to check the actual id!! %d\n",INT->Get());
    returnString[0]='\0';
    sprintf(returnString,"%d",INT->Get());
    *value = returnString;
  }
  else
  {
    fprintf(stderr,"Attribute %s is not attribute of shape. Options are gdscName, parent, id\n",attr);
    return CV_ERROR;
  }

  return CV_OK;
}

// -------------------
// OCCTUtils_SetFaceAttribute
// -------------------
/**
 * @brief Procedure to set an attribute of the shape
 * @param shape input TopoDS_Shape to set attribute
 * @param shapetool the XDEDoc manager that contains attribute info
 * @param shapelabel the label for the shape registered in XDEDoc
 * @note attributes include id, name, and parent
 * @return CV_OK if function completes properly
 */
int OCCTUtils_SetFaceAttribute(const TopoDS_Shape &face,
		Handle(XCAFDoc_ShapeTool) &shapetool,TDF_Label &shapelabel,
    				char *attr, char *value)
{
  TDF_Label tmpLabel;
  shapetool->FindSubShape(shapelabel,face,tmpLabel);
  if (tmpLabel.IsNull())
  {
    fprintf(stderr,"Face is not labelled and thus has no attribute\n");
    return CV_ERROR;
  }
  if (!strncmp(attr,"gdscName",4))
  {
    TDF_Label nameLabel = tmpLabel.FindChild(1,Standard_False);
    Handle(TDataStd_ExtStringArray) NSTRING = new
      TDataStd_ExtStringArray();
    int isLabel = nameLabel.FindAttribute(TDataStd_ExtStringArray::GetID(),NSTRING);

    OCCTUtils_SetExtStringArrayFromChar(NSTRING,value);
  }
  else if (!strncmp(attr,"parent",6))
  {
    TDF_Label parentLabel = tmpLabel.FindChild(2,Standard_False);
    Handle(TDataStd_ExtStringArray) PSTRING = new
      TDataStd_ExtStringArray();
    int isLabel = parentLabel.FindAttribute(TDataStd_ExtStringArray::GetID(),PSTRING);

    OCCTUtils_SetExtStringArrayFromChar(PSTRING,value);
  }
  else if (!strncmp(attr,"id",2))
  {
    TDF_Label idLabel = tmpLabel.FindChild(0,Standard_False);
    Handle(TDataStd_Integer) INT = new TDataStd_Integer();
    int isLabel = idLabel.FindAttribute(TDataStd_Integer::GetID(),INT);

    INT->Set(charToInt(value));
  }
  else
  {
    fprintf(stderr,"Attribute %s is not attribute of shape. Options are gdscName, parent, id\n",attr);
    return CV_ERROR;
  }

  return CV_OK;
}

// -------------------
// OCCTUtils_PassFaceAttributes
// -------------------
/**
 * @brief Procedure to get an attribute of the shape
 * @param shape input TopoDS_Shape to get attribute
 * @param shapetool the XDEDoc manager that contains attribute info
 * @param shapelabel the label for the shape registered in XDEDoc
 * @note attributes includ id, name, and parent
 * @return CV_OK if function completes properly
 */
int OCCTUtils_PassFaceAttributes(TopoDS_Shape &faceSrc,TopoDS_Shape &faceDst,
		Handle(XCAFDoc_ShapeTool) &shapetool,TDF_Label &labelSrc,
		TDF_Label &labelDst)
{
  //Pass the face names first
  char *name;
  if (OCCTUtils_GetFaceAttribute(
	faceSrc,shapetool,labelSrc,"gdscName",&name) != CV_OK)
  {
    fprintf(stderr,"Failure in getting gdscName for shape\n");
    return CV_ERROR;
  }
  if (OCCTUtils_SetFaceAttribute(
	faceDst,shapetool,labelDst,"gdscName",name) != CV_OK)
  {
    fprintf(stderr,"Failure in setting gdscName for shape\n");
    return CV_ERROR;
  }

  //Now parent name
  char *parent;
  if (OCCTUtils_GetFaceAttribute(
	faceSrc,shapetool,labelSrc,"parent",&parent) != CV_OK)
  {
    fprintf(stderr,"Failure in getting parent for shape\n");
    return CV_ERROR;
  }
  if (OCCTUtils_SetFaceAttribute(
	faceDst,shapetool,labelDst,"parent",parent) != CV_OK)
  {
    fprintf(stderr,"Failure in setting parent for shape\n");
    return CV_ERROR;
  }
  return CV_OK;
}

// -------------------
// OCCTUtils_CheckIsSolid
// -------------------
/**
 * @brief Procedure to check and see if it is solid
 * @param shape input TopoDS_Shape to check
 * @param issue contains integer for issue. BRepCheck_Status doc of occt
 * @return CV_OK if solid, CV_ERROR if it is not solid
 */
int OCCTUtils_CheckIsSolid(const TopoDS_Shape &shape,int &issue)
{
  try {
    BRepCheck_Solid solidchecker(TopoDS::Solid(shape));
    BRepCheck_ListOfStatus status = solidchecker.Status();
    BRepCheck_ListIteratorOfListOfStatus statit;
    statit.Initialize(status);
    for (int i=0;statit.More();statit.Next())
    {
      BRepCheck_Status checker = statit.Value();
      if (checker != 0)
      {
	issue = checker;
	fprintf(stderr,"Shape is not solid!\n");
	return CV_ERROR;
      }
    }
    issue = 0;
  }
  catch (Standard_TypeMismatch) {
    fprintf(stderr,"Shape caused error in solid check\n");
    return CV_ERROR;
  }

  return CV_OK;
}
