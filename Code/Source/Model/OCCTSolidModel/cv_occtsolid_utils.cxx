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
#include "TopoDS.hxx"
#include "TopoDS_Face.hxx"
#include "TopExp_Explorer.hxx"

#include "BRepOffsetAPI_ThruSections.hxx"
#include "BRepTools_Reshape.hxx"
#include "BRepCheck_Solid.hxx"

#include "TDataStd_Integer.hxx"
#include "TDataStd_Name.hxx"
#include "Standard_Real.hxx"
#include "StdFail_NotDone.hxx"
#include "Standard_Integer.hxx"
#include "TDataStd_Integer.hxx"
#include "TNaming_Builder.hxx"

#include <string>
#include <sstream>
#include <iostream>

//Function to turn an integer into a string
char *intToChar(int in)
{
  char *out;
  sprintf(out,"%d",in);
  return out;
}

int charToInt(const char *in)
{
  int out = atoi(in);
  return out;
}

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
// OCCTUtils_MakeLoftedSurf
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
int OCCTUtils_MakeLoftedSurf(TopoDS_Wire *curves, TopoDS_Shape &shape,
		int numCurves,int continuity,
		int partype, double w1, double w2, double w3, int smoothing)
{

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
    TopoDS_Wire newwire = curves[i];
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

  //shape = attacher.SewedShape();
  try
  {
    shape = lofter.Shape();
  }
  catch (StdFail_NotDone)
  {
    fprintf(stderr,"Difficulty in lofting, try changing parameters\n");
    return CV_ERROR;
  }

  return CV_OK;
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
 * @brief Procedure to get a shape orientation
 * @param *geom input TopoDS_Shape on which to get orientation
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
  static char returnString[256];
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
      fprintf(stderr,"Name label doesn't exist, cannot retrive name\n");
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
      fprintf(stderr,"Name label doesn't exist, cannot retrive name\n");
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
		Handle(XCAFDoc_ShapeTool) &shapetool,TDF_Label &shapelabel)
{
  //Pass the face names first
  char *name;
  if (OCCTUtils_GetFaceAttribute(
	faceSrc,shapetool,shapelabel,"gdscName",&name) != CV_OK)
  {
    fprintf(stderr,"Failure in getting gdscName for shape\n");
    return CV_ERROR;
  }
  if (OCCTUtils_SetFaceAttribute(
	faceDst,shapetool,shapelabel,"gdscName",name) != CV_OK)
  {
    fprintf(stderr,"Failure in setting gdscName for shape\n");
    return CV_ERROR;
  }

  //Now parent name
  char *parent;
  if (OCCTUtils_GetFaceAttribute(
	faceSrc,shapetool,shapelabel,"parent",&parent) != CV_OK)
  {
    fprintf(stderr,"Failure in getting parent for shape\n");
    return CV_ERROR;
  }
  if (OCCTUtils_SetFaceAttribute(
	faceDst,shapetool,shapelabel,"parent",parent) != CV_OK)
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

  return CV_OK;
}
