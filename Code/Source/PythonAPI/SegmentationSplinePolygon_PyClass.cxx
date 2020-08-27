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

// The functions defined here implement the SV Python API spline polygon segmentation class.
//
// The class name is 'SplinePolygon'.
//
// In SV the polygon segmentation uses a list of control points with implicit meaning
//
//    control_points[0] = segmentation center
//    control_points[1] = controls scaling factor or in the case of a circle, its radius
//    control_points[2] = 1st control point on the polygon boundary
//    control_points[3] = 2nd control point on the polygon boundary
//
// To hide these details the API uses control points to only define the polygon boundary.
// The SV control points are constructed using the boundary control points and other data.

#include "SimVascular.h"
#include "sv_misc_utils.h"
#include "sv3_Contour.h"
#include "Segmentation_PyModule.h"
#include "sv3_SplinePolygonContour.h"
#include "sv_arg.h"

#include <stdio.h>
#include <string.h>
#include "sv_Repository.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"

#include "Python.h"
#include "PyUtils.h"
#include "sv2_globals.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

//-----------------------------
// PySplinePolygonSegmentation
//-----------------------------
// Define the SplinePolygon class (type).
//
typedef struct {
  PySegmentation super;
  // These must go after any data accessed by Python.
  std::array<double,3> center;
  std::vector<std::array<double,3>> controlPoints;
  std::array<double,3> normal;
  double planeDistTol;
} PySplinePolygonSegmentation;

//////////////////////////////////////////////////////
//        U t i l i t y     F u n c t i o n s       //
//////////////////////////////////////////////////////

//-------------------------------------
// PySplinePolygonCopySegmentationData
//-------------------------------------
//
void PySplinePolygonCopySegmentationData(sv4guiContour* sv4Contour, PyObject* contourObj)
{
  auto contour = ((PySegmentation*)contourObj)->contour;
  PySegmentationCopySv4ContourData(sv4Contour, contour);

  // Set PySplinePolygonSegmentation data.
  //
  auto polygonContour = (PySplinePolygonSegmentation*)contourObj;
  polygonContour->center = contour->GetCenterPoint();

  double normal[3];
  contour->GetPlaneGeometry()->GetNormal(normal);
  polygonContour->normal = {normal[0], normal[1], normal[2]};

  // Copy control points omittinng the first two which
  // are the polygon center and scaling factor.
  //
  auto controlPoints = contour->GetControlPoints();
  for (int i = 2; i < controlPoints.size(); i++) {
      polygonContour->controlPoints.push_back(controlPoints[i]);
  }
}

//-----------------------------
// PySplinePolygonGenerateData
//-----------------------------
// Generate the PySPlinePolygonSegmentation and SV Contour data from
// a list of control points.
//
void PySplinePolygonGenerateData(PyUtilApiFunction& api, PySplinePolygonSegmentation* polygonSeg, const std::vector<std::array<double,3>>& points)
{
  auto contour = polygonSeg->super.contour;

  // Create SV control points, adding two points for center and scale.
  //
  std::vector<std::array<double,3>> controlPoints;
  controlPoints.push_back(std::array<double,3>{0.0,0.0,0.0});
  controlPoints.push_back(std::array<double,3>{0.0,0.0,0.0});
  for (auto pt : points) {
      controlPoints.push_back(pt);
  }

  // Set object data.
  polygonSeg->controlPoints = points;
  polygonSeg->center = PyUtilComputePointsCenter(points);
  polygonSeg->normal = PyUtilComputeNormalFromlPoints(points);

  // Define plane.
  auto plane = vtkSmartPointer<vtkPlane>::New();
  plane->SetOrigin(polygonSeg->center.data());
  plane->SetNormal(polygonSeg->normal.data());
  contour->SetPlaneGeometry(plane);

  // Need to set control points after defining the plane.
  contour->SetControlPoints(controlPoints);
}

//////////////////////////////////////////////////////
//          C l a s s    M e t h o d s              //
//////////////////////////////////////////////////////

//--------------------------------------
// SplinePolygonSegmentation_get_center
//--------------------------------------
//
PyDoc_STRVAR(SplinePolygonSegmentation_get_center_doc,
  "get_center()  \n\
   \n\
   Get the center of the polygon segmentation. \n\
   \n\
   Returns (list([float,float,float]): The polygon segmentation center. \n\
");

static PyObject*
SplinePolygonSegmentation_get_center(PySplinePolygonSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  return Py_BuildValue("[d, d, d]", self->center[0], self->center[1], self->center[2]);
}

//----------------------------------------------
// SplinePolygonSegmentation_get_control_points
//----------------------------------------------
//
PyDoc_STRVAR(SplinePolygonSegmentation_get_control_points_doc,
  "get_control_points()  \n\
   \n\
   Get the control points for a polygon segmentation. \n\
   \n\
   Returns  (list(list([float,float,float]))): The list of control points. \n\
");

static PyObject*
SplinePolygonSegmentation_get_control_points(PySplinePolygonSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  /*
  auto contour = self->super.contour;
  auto control_points = contour->GetControlPoints();
  for (auto pt : control_points) {
      std::cout << "[SplinePolygonSegmentation_get_control_points] pt: " << pt[0] << " " << pt[1] << " " << pt[2] << std::endl;
  }
  */
  auto control_points = self->controlPoints;
  return PyUtilPointVectorDataToPyList(control_points);
}

//--------------------------------------
// SplinePolygonSegmentation_get_normal
//--------------------------------------
//
PyDoc_STRVAR(SplinePolygonSegmentation_get_normal_doc,
  "get_normal()  \n\
   \n\
   Get the normal of the plane the spline polygon segmentation lies in. \n\
   \n\
   Returns (list([float,float,float])): The spline polygon segmentation normal. \n\
");

static PyObject*
SplinePolygonSegmentation_get_normal(PySplinePolygonSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  return Py_BuildValue("[d, d, d]", self->normal[0], self->normal[1], self->normal[2]);
}

//--------------------------------------------------
// SplinePolygonSegmentation_get_subdivision_params
//--------------------------------------------------
//
PyDoc_STRVAR(SplinePolygonSegmentation_get_subdivision_params_doc,
  "get_subdivision_params()  \n\
   \n\
   Get the subdivision parameters used to control the interpolating spline. \n\
   \n\
   Returns (str, float, int): The subdivision type, spacing and number. \n\
");

static PyObject*
SplinePolygonSegmentation_get_subdivision_params(PySplinePolygonSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto contour = self->super.contour;

  auto subdivType = contour->GetSubdivisionType();
  std::string strType;
  for (auto const& entry : subdivTypeNameEnumMap) {
      if (subdivType == entry.second) {
          strType = entry.first;
          break;
      }
  }

  auto spacing = contour->GetSubdivisionSpacing();
  int number = contour->GetSubdivisionNumber();

  return Py_BuildValue("s,d,i", strType.c_str(), spacing, number);
}

//------------------------------------------------
// SplinePolygonSegmentation_get_subdivision_type
//------------------------------------------------
//
PyDoc_STRVAR(SplinePolygonSegmentation_get_subdivision_type_doc,
  "get_subdivision_type()  \n\
   \n\
   Get the subdivision type of the interpolating spline. \n\
   \n\
   Return type (str): The subdivision type. \n\
   \n\
");

static PyObject*
SplinePolygonSegmentation_get_subdivision_type(PySplinePolygonSegmentation* self, PyObject*)
{
  auto contour = self->super.contour;

  // Get the subdivision type string.
  auto subdivType = contour->GetSubdivisionType();
  std::string strType;
  for (auto const& entry : subdivTypeNameEnumMap) {
      if (subdivType == entry.second) {
          strType = entry.first;
          break;
      }
  }

  return Py_BuildValue("s", strType.c_str());
}

//----------------------------------------------
// SplinePolygonSegmentation_set_control_points
//----------------------------------------------
//
PyDoc_STRVAR(SplinePolygonSegmentation_set_control_points_doc,
  "set_control_points(control_points)  \n\
   \n\
   Set the control points for a polygon segmentation. \n\
   \n\
   Args: \n\
     points (list(list([float,float,float]))): The list of control points. \n\
   \n\
");

static PyObject*
SplinePolygonSegmentation_set_control_points(PySplinePolygonSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"control_points", NULL};
  PyObject* pointsArg= nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &pointsArg)) {
      return nullptr;
  }

  std::string msg;
  std::vector<std::array<double,3>> points;
  if (!PyUtilGetPointVectorData(pointsArg, points, msg)) {
      api.error("The 'points' argument " + msg);
      return nullptr;
  }

  // Generate the PySplinePolygonSegmentation and SV Contour data from the list of control points.
  PySplinePolygonGenerateData(api, self, points);

  // Check control points distance to plane.
  auto maxDist = PyUtilComputeDistPointsToPlane(self->center, self->normal, self->controlPoints);
  if (maxDist > self->planeDistTol) {
      api.error("The 'points' data do not lie in a plane within " + std::to_string(self->planeDistTol) + ".");
      return nullptr;
  }

  Py_RETURN_NONE;
}

//--------------------------------------------------
// SplinePolygonSegmentation_set_subdivision_params
//--------------------------------------------------
//
PyDoc_STRVAR(SplinePolygonSegmentation_set_subdivision_params_doc,
  "set_subdivision_params(type=None, spacing=None, number=None)  \n\
   \n\
   Set the subdivision parameters used to control the interpolating spline. \n\
   \n\
   Args: \n\
     type (Optional[str): The subdivision type. Valid types: CONSTANT_SPACING,\n\
        CONSTANT_SUBDIVISION_NUMBER, or CONSTANT_TOTAL_NUMBER.             \n\
     spacing (Optional[float]): The subdivision spacing. \n\
     number (Optional[int]): The number of subdivisions. \n\
   \n\
");

static PyObject*
SplinePolygonSegmentation_set_subdivision_params(PySplinePolygonSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("|sO!O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"type", "spacing", "number", NULL};
  char* typeName = nullptr;
  PyObject* spacingArg = nullptr;
  PyObject* numberArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &typeName, &PyFloat_Type, &spacingArg, &PyInt_Type, &numberArg)) {
      return nullptr;
  }

  auto contour = self->super.contour;

  // Set type.
  //
  if (typeName != nullptr) {
      sv3::Contour::SubdivisionType subdivType;

      try {
          subdivType = subdivTypeNameEnumMap.at(std::string(typeName));
      } catch (const std::out_of_range& except) {
          auto msg = "Unknown subdivision type '" + std::string(typeName) + "'." +
              " Valid names are: " + subdivTypeValidNames + ".";
          api.error(msg);
          return nullptr;
      }

      // This regenerates the contour.
      contour->SetSubdivisionType(subdivType);
  }

  // Set spacing.
  //
  if (spacingArg != nullptr) {
      auto spacing = PyFloat_AsDouble(spacingArg);
      // This regenerates the contour.
      contour->SetSubdivisionSpacing(spacing);
  }

  // Set number.
  //
  if (numberArg != nullptr) {
      auto number = PyInt_AsLong(numberArg);
      // This regenerates the contour.
      contour->SetSubdivisionNumber(number);
  }

  Py_RETURN_NONE;
}

//------------------------------------------------
// SplinePolygonSegmentation_set_subdivision_type
//------------------------------------------------
//
PyDoc_STRVAR(SplinePolygonSegmentation_set_subdivision_type_doc,
  "set_subdivision_type(type)  \n\
   \n\
   Set the subdivision type of the interpolating spline.                   \n\
   \n\
   Args: \n\
     type (str): The subdivision type. Valid types: CONSTANT_SPACING,       \n\
        CONSTANT_SUBDIVISION_NUMBER, or CONSTANT_TOTAL_NUMBER. \n\
   \n\
");

static PyObject*
SplinePolygonSegmentation_set_subdivision_type(PySplinePolygonSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  static char *keywords[] = {"type", NULL};
  char* typeName = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &typeName)) {
      return nullptr;
  }

  sv3::Contour::SubdivisionType subdivType;

  try {
      subdivType = subdivTypeNameEnumMap.at(std::string(typeName));
  } catch (const std::out_of_range& except) {
      auto msg = "Unknown subdivision type '" + std::string(typeName) + "'." +
          " Valid names are: " + subdivTypeValidNames + ".";
      api.error(msg);
      return nullptr;
  }

  auto contour = self->super.contour;

  // This regenerates the contour.
  contour->SetSubdivisionType(subdivType);

  Py_RETURN_NONE;
}

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* SEGMENTATION_SPLINE_POLYGON_CLASS = "SplinePolygon";
static char* SEGMENTATION_SPLINE_POLYGON_MODULE_CLASS = "segmentation.SplinePolygon";

//--------------------------------------------
// PySplineSplinePolygonSegmentationClass_doc
//--------------------------------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(PySplinePolygonSegmentationClass_doc,
   "SplinePolygon(control_points)  \n\
   \n\
   The SplinePolygon class provides an interface for creating a spline     \n\
   curve interpolating a set of control points.                            \n\
   \n\
   Control points can be defined using any segmentation type.              \n\
   \n\
   Args: \n\
     control_points (list(list([float,float,float]))): The list of 3D      \n\
        control points.                                                    \n\
   \n\
");

//------------------------------------------
// PySplineSplinePolygonSegmentationMethods
//------------------------------------------
// Define the module methods.
//
PyMethodDef PySplinePolygonSegmentationMethods[] = {

 {"get_center", (PyCFunction)SplinePolygonSegmentation_get_center, METH_VARARGS, SplinePolygonSegmentation_get_center_doc },

 {"get_control_points", (PyCFunction)SplinePolygonSegmentation_get_control_points, METH_VARARGS, SplinePolygonSegmentation_get_control_points_doc },

 {"get_normal", (PyCFunction)SplinePolygonSegmentation_get_normal, METH_VARARGS, SplinePolygonSegmentation_get_normal_doc },

 {"get_subdivision_params", (PyCFunction)SplinePolygonSegmentation_get_subdivision_params, METH_NOARGS, SplinePolygonSegmentation_get_subdivision_params_doc},

 {"get_subdivision_type", (PyCFunction)SplinePolygonSegmentation_get_subdivision_type, METH_NOARGS, SplinePolygonSegmentation_get_subdivision_type_doc},

 {"set_control_points", (PyCFunction)SplinePolygonSegmentation_set_control_points, METH_VARARGS|METH_KEYWORDS, SplinePolygonSegmentation_set_control_points_doc },

 {"set_subdivision_type", (PyCFunction)SplinePolygonSegmentation_set_subdivision_type, METH_VARARGS|METH_KEYWORDS, SplinePolygonSegmentation_set_subdivision_type_doc},

 {"set_subdivision_params", (PyCFunction)SplinePolygonSegmentation_set_subdivision_params, METH_VARARGS|METH_KEYWORDS, SplinePolygonSegmentation_set_subdivision_params_doc},

  {NULL, NULL}
};

//---------------------------------
// PySplinePolygonSegmentationInit
//---------------------------------
// This is the __init__() method for the SplinePolygonSegmentation class.
//
// This function is used to initialize an object after it is created.
//
static int
PySplinePolygonSegmentationInit(PySplinePolygonSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("|O!", PyRunTimeErr, "SplinePolygonSegmentation");
  static char *keywords[] = {"control_points", NULL};
  PyObject* pointsArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &pointsArg)) {
      return -1;
  }

  self->super.contour = new sv3::ContourSplinePolygon();
  self->super.CopySv4ContourData = PySplinePolygonCopySegmentationData;
  self->planeDistTol = 1e-6;

  if ((args == nullptr) && (kwargs == nullptr)) {
      return 0;
  }

  auto polygonContour = dynamic_cast<sv3::ContourSplinePolygon*>(self->super.contour);
  polygonContour->SetSubdivisionType(sv3::Contour::CONSTANT_SPACING);

  // Extract control point data.
  //
  if (pointsArg != nullptr) {
      std::string msg;
      std::vector<std::array<double,3>> points;
      if (!PyUtilGetPointVectorData(pointsArg, points, msg)) {
          api.error("The 'points' argument " + msg);
          return -1;
      }
      // Generate the PyPolygonSegmentation and SV Contour data from the list of control points.
      PySplinePolygonGenerateData(api, self, points);

      // Check control points distance to plane.
      auto maxDist = PyUtilComputeDistPointsToPlane(self->center, self->normal, self->controlPoints);
      if (maxDist > self->planeDistTol) {
          api.error("The 'points' data do not lie in a plane within " + std::to_string(self->planeDistTol) + ".");
          return -1;
      }
    }

  return 0;
}

//--------------------------------
// PySplinePolygonSegmentationNew
//--------------------------------
//
static PyObject *
PySplinePolygonSegmentationNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PySplinePolygonSegmentationNew] PySplinePolygonSegmentationNew " << std::endl;
  auto self = (PySplinePolygonSegmentation*)type->tp_alloc(type, 0);
  if (self != NULL) {
      //self->super.id = 2;
  }
  return (PyObject *) self;
}

//------------------------------------
// PySplinePolygonSegmentationDealloc
//------------------------------------
//
static void
PySplinePolygonSegmentationDealloc(PySplinePolygonSegmentation* self)
{
  //std::cout << "[PySplinePolygonSegmentationDealloc] Free PySplinePolygonSegmentation" << std::endl;
  delete self->super.contour;
  Py_TYPE(self)->tp_free(self);
}

//---------------------------------
// PySplinePolygonSegmentationType
//---------------------------------
// Define the Python type object that stores Segmentation data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static PyTypeObject PySplinePolygonSegmentationType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and
  // the name of the type within the module.
  SEGMENTATION_SPLINE_POLYGON_MODULE_CLASS,
  sizeof(PySplinePolygonSegmentation)
};

//----------------------------------------
// SetSplinePolygonSegmentationTypeFields
//----------------------------------------
// Set the Python type object fields that stores Contour data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetSplinePolygonSegmentationTypeFields(PyTypeObject& contourType)
 {
  // Doc string for this type.
  contourType.tp_doc = PySplinePolygonSegmentationClass_doc;

  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  contourType.tp_new = PySplinePolygonSegmentationNew;
  //.tp_new = PyType_GenericNew,

  contourType.tp_base = &PySegmentationType;

  contourType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  contourType.tp_init = (initproc)PySplinePolygonSegmentationInit;
  contourType.tp_dealloc = (destructor)PySplinePolygonSegmentationDealloc;
  contourType.tp_methods = PySplinePolygonSegmentationMethods;
};
