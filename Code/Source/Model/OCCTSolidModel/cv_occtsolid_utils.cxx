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

#include "Standard_Real.hxx"
#include "Standard_Integer.hxx"
#include "TDataStd_Integer.hxx"

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
		const Handle(XCAFDoc_ShapeTool) &shapetool,TDF_Label &shapelabel,
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
    fprintf(stderr,"Face has not been given a label\n");
    return CV_ERROR;
  }

  //Retrive attribute
  Handle(TDataStd_Integer) INT = new TDataStd_Integer();
  tmpLabel.FindAttribute(TDataStd_Integer::GetID(),INT);
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
		const Handle(XCAFDoc_ShapeTool) &shapetool,TDF_Label &shapelabel)
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
    newmap[i] = 0;

  fprintf(stderr,"Face range %d\n",facerange);
  int checkid=-1;
  int currentid=0;
  TopExp_Explorer anExp(shape,TopAbs_FACE);
  while (checkid != facerange)
  {
    anExp.Init(shape,TopAbs_FACE);
    int found=0;
    for (int j=0;anExp.More();anExp.Next(),j++)
    {
      TopoDS_Face tmpFace = TopoDS::Face(anExp.Current());
      int faceid=-1;
      OCCTUtils_GetFaceLabel(shape,shapetool,shapelabel,faceid);
      fprintf(stderr,"Face id %d\n",faceid);
      fprintf(stderr,"CheckId %d\n",checkid);
      if (faceid == checkid)
      {
	newmap[j] = currentid++;
	found=1;
	break;
      }
    }
    if (!found)
      checkid++;
  }
  for (int i=0;i<numFaces;i++)
  {
    fprintf(stderr,"OldFace %d\n",faces[i]);
    fprintf(stderr,"CheckMapping %d\n",newmap[i]);

  }

  delete [] newmap;
  delete [] faces;
  return CV_OK;
}

// ----------------
// GetFaceRange
// ----------------
int OCCTUtils_GetFaceRange(const TopoDS_Shape &shape,
		const Handle(XCAFDoc_ShapeTool) &shapetool,TDF_Label &shapelabel,
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
  face_range++;

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

