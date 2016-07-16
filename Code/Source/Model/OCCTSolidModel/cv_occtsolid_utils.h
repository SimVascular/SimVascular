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

/** @file cv_occtsolid_utils.h
 *  @brief These functions are utilities that are called mostly by
 *  cvOCCTSolidModel
 *  @details These functions are called mostly by cvPolyDataSolid and
 *  they provide a clean way for implementation of functions with that
 *  class. They provide the full code for extracting boundaries, extracting
 *  faces, etc.
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#ifndef __CV_OCCTSOLID_UTILS_H
#define __CV_OCCTSOLID_UTILS_H

#include "cvSolidModel.h"
#include "cvOCCTSolidModel.h"

#include "BRepFilletAPI_MakeFillet.hxx"
#include "BRepBuilderAPI_Sewing.hxx"
#include "Standard_Real.hxx"
#include "TopoDS_Shape.hxx"
#include "TopoDS_Face.hxx"
#include "TopoDS_Edge.hxx"
#include "TopoDS_Wire.hxx"
#include "TopoDS_Shell.hxx"
#include "TopoDS_Solid.hxx"
#include "TDF_Label.hxx"
#include "XCAFDoc_ShapeTool.hxx"
#include "XCAFDoc_ShapeTool.hxx"
#include "TDataStd_ExtStringArray.hxx"
#include "Geom_BSplineCurve.hxx"
#include "Geom_BSplineSurface.hxx"

/* -------- */
/* Get Info */
/* -------- */
SV_EXPORT_OPENCASCADE int OCCTUtils_GetFaceIds( const TopoDS_Shape &geom,
		Handle(XCAFDoc_ShapeTool) &shapetool,TDF_Label &shapelabel,
	       	int *v_num_faces, int **v_faces);

SV_EXPORT_OPENCASCADE int OCCTUtils_GetFaceLabel( const TopoDS_Shape &geom,
	       const Handle(XCAFDoc_ShapeTool) &shapetool, TDF_Label &shapelabel,
	       int &id);

SV_EXPORT_OPENCASCADE int OCCTUtils_GetFaceRange( const TopoDS_Shape &geom,
	       Handle(XCAFDoc_ShapeTool) &shapetool, TDF_Label &shapelabel,
	       int &face_range);

SV_EXPORT_OPENCASCADE int OCCTUtils_GetNumberOfFaces( const TopoDS_Shape &geom,int &num_faces);

SV_EXPORT_OPENCASCADE int OCCTUtils_ReLabelFace( TopoDS_Shape &geom,
	       Handle(XCAFDoc_ShapeTool) &shapetool, TDF_Label &shapelabel,
	       int &id);

SV_EXPORT_OPENCASCADE int OCCTUtils_GetFaceAttribute(const TopoDS_Shape &geom,
	       Handle(XCAFDoc_ShapeTool) &shapetool, TDF_Label &shapelabel,
	       char *attr,char **value);

SV_EXPORT_OPENCASCADE int OCCTUtils_GetExtStringArrayAsChar(Handle(TDataStd_ExtStringArray) &array,
    					char *charstr);

/* -------- */
/* Ops */
/* -------- */
SV_EXPORT_OPENCASCADE int OCCTUtils_MakeLoftedSurf(TopoDS_Wire *curves,TopoDS_Shape &shape,int numCurves,int continuity,
		int partype, double w1, double w2, double w3, int smoothing);

SV_EXPORT_OPENCASCADE int OCCTUtils_CreateEdgeBlend(TopoDS_Shape &shape,
		Handle(XCAFDoc_ShapeTool) &shapetool,TDF_Label &shapelabel,
		BRepFilletAPI_MakeFillet &filletmaker,
    		int faceA, int faceB, double radius,
		char blendname[]);

SV_EXPORT_OPENCASCADE int OCCTUtils_ShapeFromBSplineSurface(const Handle(Geom_BSplineSurface) surface,
    		TopoDS_Shape &shape,
		const TopoDS_Wire &first_wire, const TopoDS_Wire &last_wire,
		const int pres3d);

SV_EXPORT_OPENCASCADE int OCCTUtils_CapShapeToSolid(TopoDS_Shape &shape,TopoDS_Shape &geom,
    		BRepBuilderAPI_Sewing &attacher,int &numFilled);

/* -------- */
/* Helpers for loft */
/* -------- */

SV_EXPORT_OPENCASCADE TopoDS_Solid OCCTUtils_MakeSolid(TopoDS_Shell& shell, const TopoDS_Wire& wire1,
  const TopoDS_Wire& wire2, const Standard_Real presPln,
  TopoDS_Face& face1, TopoDS_Face& face2);

SV_EXPORT_OPENCASCADE Standard_Boolean OCCTUtils_PerformPlan(const TopoDS_Wire& W,
		const Standard_Real presPln,
		TopoDS_Face& theFace);

SV_EXPORT_OPENCASCADE Standard_Boolean OCCTUtils_IsSameOriented(const TopoDS_Shape& aFace,
  const TopoDS_Shape& aShell);

SV_EXPORT_OPENCASCADE Standard_Boolean OCCTUtils_IsSameOrientedWEdge(const TopoDS_Shape& aFace,
  const TopoDS_Shape& aShell,const TopoDS_Shape &anEdge);

SV_EXPORT_OPENCASCADE TopoDS_Solid OCCTUtils_MakeShell(TopoDS_Shell& shell, const TopoDS_Wire& wire1,
		const TopoDS_Wire& wire2, const Standard_Real presPln,
		TopoDS_Face& face1, TopoDS_Face& face);

SV_EXPORT_OPENCASCADE Standard_Real OCCTUtils_PreciseUpar(const Standard_Real anUpar,
		const Handle(Geom_BSplineSurface)& aSurface);

SV_EXPORT_OPENCASCADE Handle(Geom_BSplineCurve) OCCTUtils_EdgeToBSpline(const TopoDS_Edge& theEdge);


/* -------- */
/* Set */
/* -------- */

SV_EXPORT_OPENCASCADE int OCCTUtils_RenumberFaces(TopoDS_Shape &shape,
		Handle(XCAFDoc_ShapeTool) &shapetool,TDF_Label &shapelabel);

SV_EXPORT_OPENCASCADE int OCCTUtils_SetFaceAttribute(const TopoDS_Shape &geom,
	       Handle(XCAFDoc_ShapeTool) &shapetool, TDF_Label &shapelabel,
	       char *attr,char *value);

SV_EXPORT_OPENCASCADE int OCCTUtils_PassFaceAttributes(TopoDS_Shape &faceSrc,TopoDS_Shape &faceDst,
	       Handle(XCAFDoc_ShapeTool) &shapetool, TDF_Label &labelSrc,
	       TDF_Label &labelDst);

SV_EXPORT_OPENCASCADE int OCCTUtils_SetExtStringArrayFromChar(Handle(TDataStd_ExtStringArray) &array,
    					char *charstr);
/* -------- */
/* Check */
/* -------- */
SV_EXPORT_OPENCASCADE int OCCTUtils_CheckIsSolid(const TopoDS_Shape &geom,int &issue);

/* -------- */
/* Orientation  */
/* -------- */
SV_EXPORT_OPENCASCADE int OCCTUtils_GetOrientation(const TopoDS_Shape &geom,int &orientation);

SV_EXPORT_OPENCASCADE int OCCTUtils_SetOrientation(TopoDS_Shape &geom, TopoDS_Shape &face,
		int &orientation);
#endif // __OCCTSOLID_UTILS_H
