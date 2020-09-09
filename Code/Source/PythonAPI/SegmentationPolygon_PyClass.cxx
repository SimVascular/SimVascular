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

// This is the implementation of the SV Python API polygon segmentation class.
//
// The class name is 'segmentation.Polygon'.
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
//
#include "SimVascular.h"
#include "sv_misc_utils.h"
#include "sv3_Contour.h"
#include "Segmentation_PyModule.h"
#include "sv3_PolygonContour.h"
#include "sv_arg.h"

#include <stdio.h>
#include <string.h>
#include "sv_Repository.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"

#include "Python.h"
#include "sv2_globals.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

//-----------------------
// PyPolygonSegmentation
//-----------------------
// Define the Polygon class.
//
// center: The center of the polygon.
// controlPoints: Control points on the polygon bounday.
//
typedef struct {
  PySegmentation super;
  // These must go after any data accessed by Python.
  std::array<double,3> center;
  std::vector<std::array<double,3>> controlPoints;
  std::array<double,3> normal;
  double planeDistTol;
} PyPolygonSegmentation;

//////////////////////////////////////////////////////
//        U t i l i t y     F u n c t i o n s       //
//////////////////////////////////////////////////////

//-------------------------------
// PyPolygonCopySegmentationData
//-------------------------------
// Copy segmentation data from an sv4guiContour object into
// a Contour object.
//
// After copying the PyPolygonSegmentation data that the API
// user will access is set.
//
void PyPolygonCopySegmentationData(sv4guiContour* sv4Contour, PyObject* contourObj)
{
  auto contour = ((PySegmentation*)contourObj)->contour;
  PySegmentationCopySv4ContourData(sv4Contour, contour);

  // Set PyPolygonSegmentation data.
  //
  auto polygonContour = (PyPolygonSegmentation*)contourObj;
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

//-----------------------
// PyPolygonGenerateData
//-----------------------
// Generate the PyPolygonSegmentation and SV Contour data from
// a list of control points.
//
void PyPolygonGenerateData(PyUtilApiFunction& api, PyPolygonSegmentation* polygonSeg, const std::vector<std::array<double,3>>& points)
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
//
// Python API functions.

//--------------------------------
// PolygonSegmentation_get_center
//--------------------------------
//
PyDoc_STRVAR(PolygonSegmentation_get_center_doc,
  "get_center()  \n\
   \n\
   Get the center of the polygon segmentation. \n\
   \n\
   \n\
   Returns (list([float,float,float]): The polygon segmentation center. \n\
");

static PyObject*
PolygonSegmentation_get_center(PyPolygonSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  return Py_BuildValue("[d, d, d]", self->center[0], self->center[1], self->center[2]);
}

//----------------------------------------
// PolygonSegmentation_get_control_points
//----------------------------------------
//
PyDoc_STRVAR(PolygonSegmentation_get_control_points_doc,
  "get_control_points()  \n\
   \n\
   Get the control points for a polygon segmentation. \n\
   \n\
   Returns  (list(list([float,float,float])): The list of control points. \n\
");

static PyObject*
PolygonSegmentation_get_control_points(PyPolygonSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  /*
  auto contour = self->super.contour;
  auto control_points = contour->GetControlPoints();
  for (auto pt : control_points) {
      std::cout << "[PolygonSegmentation_get_control_points] pt: " << pt[0] << " " << pt[1] << " " << pt[2] << std::endl;
  }
  */
  auto control_points = self->controlPoints;
  return PyUtilPointVectorDataToPyList(control_points);
}

//----------------------------------------
// PolygonSegmentation_set_control_points
//----------------------------------------
//
PyDoc_STRVAR(PolygonSegmentation_set_control_points_doc,
  "set_control_points(control_points)  \n\
   \n\
   Set the control points for a polygon segmentation. \n\
   \n\
   Args: \n\
     points (list(list([float,float,float])): The list of control points. \n\
   \n\
");

static PyObject*
PolygonSegmentation_set_control_points(PyPolygonSegmentation* self, PyObject* args, PyObject *kwargs)
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

  // Generate the PyPolygonSegmentation and SV Contour data from the list of control points.
  PyPolygonGenerateData(api, self, points);

  // Check control points distance to plane.
  auto maxDist = PyUtilComputeDistPointsToPlane(self->center, self->normal, self->controlPoints);
  if (maxDist > self->planeDistTol) {
      api.error("The 'points' data do not lie in a plane within " + std::to_string(self->planeDistTol) + ".");
      return nullptr;
  }

  Py_RETURN_NONE;
}

//--------------------------------
// PolygonSegmentation_get_normal
//--------------------------------
//
PyDoc_STRVAR(PolygonSegmentation_get_normal_doc,
  "get_normal()  \n\
   \n\
   Get the normal of the polygon segmentation. \n\
   \n\
   Returns (list([float,float,float]): The polygon segmentation normal. \n\
");

static PyObject*
PolygonSegmentation_get_normal(PyPolygonSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  return Py_BuildValue("[d, d, d]", self->normal[0], self->normal[1], self->normal[2]);
}

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* SEGMENTATION_POLYGON_CLASS = "Polygon";
static char* SEGMENTATION_POLYGON_MODULE_CLASS = "segmentation.Polygon";

//--------------------------------
// PyPolygonSegmentationClass_doc
//--------------------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(PyPolygonSegmentationClass_doc,
   "Polygon(control_points)  \n\
   \n\
   The Polygon class provides an interface for creating a polygon          \n\
   segmentation. A polygon segmentation is defined by a list of control    \n\
   points defining its boundary.                                           \n\
   \n\
   Args: \n\
     control_points (list(list([float,float,float]))): The list of control \n\
        points.                                                            \n\
   \n\
");

PyMethodDef PyPolygonSegmentationMethods[] = {

 {"get_center", (PyCFunction)PolygonSegmentation_get_center, METH_VARARGS, PolygonSegmentation_get_center_doc },

 {"get_control_points", (PyCFunction)PolygonSegmentation_get_control_points, METH_VARARGS, PolygonSegmentation_get_control_points_doc },

 {"get_normal", (PyCFunction)PolygonSegmentation_get_normal, METH_VARARGS, PolygonSegmentation_get_normal_doc },

 {"set_control_points", (PyCFunction)PolygonSegmentation_set_control_points, METH_VARARGS|METH_KEYWORDS, PolygonSegmentation_set_control_points_doc },

  {NULL, NULL}
};

//---------------------------
// PyPolygonSegmentationInit
//---------------------------
// This is the __init__() method for the Segmentation class.
//
// This function is used to initialize an object after it is created.
//
// A Polygon segmentation can be created directly using argument data or
// from reading contour group in which case no arguements are required.
//
static int
PyPolygonSegmentationInit(PyPolygonSegmentation* self, PyObject* args, PyObject *kwargs)
{
  //std::cout << "[PyPolygonSegmentationInit] ========== New Polygon Segmentation object ==========  " << std::endl;
  auto api = PyUtilApiFunction("|O!", PyRunTimeErr, "PolygonSegmentation");
  static char *keywords[] = {"control_points", NULL};
  PyObject* pointsArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &pointsArg)) {
      return -1;
  }

  // Create the ContourPolygon object.
  //
  self->super.contour = new sv3::ContourPolygon();
  self->super.CopySv4ContourData = PyPolygonCopySegmentationData;
  self->planeDistTol = 1e-6;

  if ((args == nullptr) && (kwargs == nullptr)) {
      return 0;
  }

  auto polygonContour = dynamic_cast<sv3::ContourPolygon*>(self->super.contour);

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
      PyPolygonGenerateData(api, self, points);

      // Check control points distance to plane.
      auto maxDist = PyUtilComputeDistPointsToPlane(self->center, self->normal, self->controlPoints);
      if (maxDist > self->planeDistTol) {
          api.error("The 'points' data do not lie in a plane within " + std::to_string(self->planeDistTol) + ".");
          return -1;
      }
    }

  return 0;
}

//--------------------------
// PyPolygonSegmentationNew
//--------------------------
//
static PyObject *
PyPolygonSegmentationNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PyPolygonSegmentationNew] PyPolygonSegmentationNew " << std::endl;
  auto self = (PyPolygonSegmentation*)type->tp_alloc(type, 0);
  if (self != NULL) {
      //self->super.id = 2;
  }
  return (PyObject *) self;
}

//------------------------------
// PyPolygonSegmentationDealloc
//------------------------------
//
static void
PyPolygonSegmentationDealloc(PyPolygonSegmentation* self)
{
  //std::cout << "[PyPolygonSegmentationDealloc] Free PyPolygonSegmentation" << std::endl;
  delete self->super.contour;
  Py_TYPE(self)->tp_free(self);
}

//---------------------------
// PyPolygonSegmentationType
//---------------------------
// Define the Python type object that stores Segmentation data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static PyTypeObject PyPolygonSegmentationType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and
  // the name of the type within the module.
  SEGMENTATION_POLYGON_MODULE_CLASS,
  sizeof(PyPolygonSegmentation)
};

//----------------------------------
// SetPolygonSegmentationTypeFields
//----------------------------------
// Set the Python type object fields that stores Segmentation  data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetPolygonSegmentationTypeFields(PyTypeObject& contourType)
 {
  // Doc string for this type.
  contourType.tp_doc = PyPolygonSegmentationClass_doc;

  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  contourType.tp_new = PyPolygonSegmentationNew;
  //.tp_new = PyType_GenericNew,

  contourType.tp_base = &PySegmentationType;
  contourType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  contourType.tp_init = (initproc)PyPolygonSegmentationInit;
  contourType.tp_dealloc = (destructor)PyPolygonSegmentationDealloc;
  contourType.tp_methods = PyPolygonSegmentationMethods;
};

