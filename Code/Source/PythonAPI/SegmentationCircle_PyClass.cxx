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

// The functions defined here implement the SV Python API circle segmentation class.
//
// The class name is 'segmentation.Circle'.
//

//----------------------
// PyCircleSegmentation
//----------------------
// Define the Circle class.
//
typedef struct {
  PySegmentation super;
  double radius;
} PyCircleSegmentation;

//////////////////////////////////////////////////////
//        U t i l i t y     F u n c t i o n s       //
//////////////////////////////////////////////////////

//------------------------------
// PyCircleCopySegmentationData
//------------------------------
//
void PyCircleCopySegmentationData(sv4guiContour* sv4Contour, PyObject* contourObj)
{
  auto contour = ((PySegmentation*)contourObj)->contour;
  PySegmentationCopySv4ContourData(sv4Contour, contour);
}

//////////////////////////////////////////////////////
//          C l a s s    M e t h o d s              //
//////////////////////////////////////////////////////
//
// Python API functions.

//-------------------------------
// CircleSegmentation_get_center
//-------------------------------
// There is also a 'get_center' method in the Segmentation class.
// This is a geometric center based on contour points so it may
// not be the exact center for a circle.
//
PyDoc_STRVAR(CircleSegmentation_get_center_doc,
  "get_center() \n\
  \n\
  Get the circle segmentation center. \n\
  \n\
  Returns (list([float,float,float])): The circle center. \n\
");

static PyObject*
CircleSegmentation_get_center(PyCircleSegmentation* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  //std::cout << "[CircleSegmentation_get_center] ========= CircleSegmentation_get_center ==========" << std::endl;
  auto circleContour = dynamic_cast<sv3::circleContour*>(self->super.contour);
  //std::cout << "[CircleSegmentation_get_center] circleContour: " << circleContour << std::endl;

  if (circleContour == nullptr) {
      api.error("**** Internal error: circleContour is null.");
      return nullptr;
  }

  auto center = circleContour->GetControlPoint(0);
  return Py_BuildValue("[d, d, d]", center[0], center[1], center[2]);
}

//-------------------------------
// CircleSegmentation_get_normal
//-------------------------------
//
PyDoc_STRVAR(CircleSegmentation_get_normal_doc,
  "get_normal() \n\
   \n\
   Get the circle segmentation normal. \n\
   \n\
   Returns (list([float,float,float]): The circle normal. \n\
   \n\
");

static PyObject*
CircleSegmentation_get_normal(PyCircleSegmentation* self, PyObject* args)
{
  auto circleContour = dynamic_cast<sv3::circleContour*>(self->super.contour);
  auto plane = circleContour->GetPlaneGeometry();
  auto normal = plane->GetNormal();

  return Py_BuildValue("[d, d, d]", normal[0], normal[1], normal[2]);
}

//-------------------------------
// CircleSegmentation_get_radius
//-------------------------------
//
PyDoc_STRVAR(CircleSegmentation_get_radius_doc,
  "get_radius(r) \n\
   \n\
   Get the radius for a circle segmentation. \n\
   \n\
   Returns (float): The radius of the circle. \n\
");

static PyObject*
CircleSegmentation_get_radius(PyCircleSegmentation* self, PyObject* args)
{
  auto contour = dynamic_cast<sv3::circleContour*>(self->super.contour);
  auto radius = contour->GetRadius();
  return Py_BuildValue("d", radius);
}

//-------------------------------
// CircleSegmentation_set_center
//-------------------------------
//
PyDoc_STRVAR(CircleSegmentation_set_center_doc,
  "set_center(center) \n\
   \n\
   Set the circle segmentation center. \n\
   \n\
   Args: \n\
     center (list([float,float,float]): The circle center. \n\
   \n\
");

static PyObject*
CircleSegmentation_set_center(PyCircleSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"center", NULL};
  PyObject* centerArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &centerArg)) {
      return nullptr;
  }

  std::string emsg;
  std::array<double,3> center;
  if (!PyUtilGetPointData(centerArg, emsg, center.data())) {
      api.error("The 'center' argument " + emsg);
      return nullptr;
  }

  auto circleContour = dynamic_cast<sv3::circleContour*>(self->super.contour);

  // Set the circle center (control point 0);
  int index = 0;
  circleContour->SetControlPoint(index, center);

  Py_RETURN_NONE;
}

//------------------------------
// CircleSegmentation_set_frame
//------------------------------
//
PyDoc_STRVAR(CircleSegmentation_set_frame_doc,
  "set_frame(frame) \n\
   \n\
   Set the circle segmentation coordinate frame using a PathFrame object. \n\
   \n\
   Args: \n\
     frame (PathFrame): The PathFrame object defing the circle's center and coordinate frame. \n\
   \n\
");

static PyObject*
CircleSegmentation_set_frame(PyCircleSegmentation* self, PyObject* args, PyObject *kwargs)
{
  //std::cout << "[CircleSegmentation_set_frame] ========== CircleSegmentation_set_frame ========== " << std::endl;
  auto api = PyUtilApiFunction("O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"frame", NULL};
  PyObject* frameArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyPathFrameType, &frameArg)) {
      return nullptr;
  }

  // Extract data from the input arguments.
  //
  PyObject* centerArg = nullptr;
  PyObject* normalArg = nullptr;
  sv3::PathElement::PathPoint pathPoint;
  std::array<double,3> normal;
  std::array<double,3> center;

  if (!PyUtilGetFrameData(api, centerArg, center, normalArg, normal, frameArg, pathPoint)) {
      return nullptr;
  }

  auto circleContour = dynamic_cast<sv3::circleContour*>(self->super.contour);

  // Set the circle path point.
  //
  circleContour->SetPathPoint(pathPoint);

  // Set the circle center (control point 0);
  int index = 0;
  circleContour->SetControlPoint(index, center);

  Py_RETURN_NONE;
}

//-------------------------------
// CircleSegmentation_set_normal
//-------------------------------
//
PyDoc_STRVAR(CircleSegmentation_set_normal_doc,
  "set_normal(normal)  \n\
   \n\
   Set the circle segmentation normal. \n\
   \n\
   Args: \n\
     normal (list([float,float,float]): The circle normal. \n\
   \n\
");

static PyObject*
CircleSegmentation_set_normal(PyCircleSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"normal", NULL};
  PyObject* normalArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &normalArg)) {
      return nullptr;
  }

  std::string emsg;
  std::array<double,3> normal;
  if (!PyUtilGetPointData(normalArg, emsg, normal.data())) {
      api.error("The 'normal' argument " + emsg);
      return nullptr;
  }

  auto circleContour = dynamic_cast<sv3::circleContour*>(self->super.contour);

  // Set the circle normal using a plane.
  auto center = circleContour->GetControlPoint(0);
  auto plane = vtkSmartPointer<vtkPlane>::New();
  plane->SetOrigin(center.data());
  plane->SetNormal(normal.data());
  circleContour->SetPlaneGeometry(plane);

  // Generate new curve points.
  circleContour->ControlPointsChanged();

  Py_RETURN_NONE;
}

//-------------------------------
// CircleSegmentation_set_radius
//-------------------------------
//
PyDoc_STRVAR(CircleSegmentation_set_radius_doc,
  "set_radius(radius) \n\
   \n\
   Set the radius for a circle segmentation. \n\
   \n\
   Args: \n\
     radius (float): The radius of the circle. \n\
");

static PyObject*
CircleSegmentation_set_radius(PyCircleSegmentation* self, PyObject* args)
{
  auto api = PyUtilApiFunction("d", PyRunTimeErr, __func__);
  double radius = 0.0;

  if (!PyArg_ParseTuple(args, api.format, &radius)) {
      return nullptr;
  }

  if (radius <= 0.0) {
      api.error("The 'radius' argument must be > 0.");
      return nullptr;
  }

  auto contour = dynamic_cast<sv3::circleContour*>(self->super.contour);
  contour->SetRadius(radius);

  Py_RETURN_NONE;
}

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* SEGMENTATION_CIRCLE_CLASS = "Circle";
static char* SEGMENTATION_CIRCLE_MODULE_CLASS = "segmentation.Circle";

//-------------------------------
// PyCircleSegmentationClass_doc
//-------------------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(PyCircleSegmentationClass_doc,
   "Circle(radius, center=None, normal=None, frame=None)  \n\
   \n\
   The Circle class provides an interface for creating a circle segmentation. \n\
   A circle segmentation is defined by a radius, a 3D point defining its      \n\
   center and a normal. The normal defines its orientation (i.e. the plane the\n\
   circle lies in).                                                           \n\
   \n\
   A Circle object is created using a radius, center and normal or a PathFrame\n\
   object.                                                                    \n\
   \n\
   A PathFrame object contains a path's interpolating spline (curve points)   \n\
   position, tangent, and normal data at a given location. The position is    \n\
   used for the circle center, the tangent for its normal.\n\
   \n\
   Args: \n\
     radius (float): The circle radius. \n\
     center (list([float,float,float]): The circle center. \n\
     normal(list([float,float,float]): The circle normal direction. \n\
     frame (Optional[PathFrame]): A PathFrame object defing the circle's   \n\
        center and coordinate frame. \n\
   \n\
");

//-----------------------------
// PyCircleSegmentationMethods
//-----------------------------
//
static PyMethodDef PyCircleSegmentationMethods[] = {

  { "get_center", (PyCFunction)CircleSegmentation_get_center, METH_NOARGS, CircleSegmentation_get_center_doc},
  { "get_normal", (PyCFunction)CircleSegmentation_get_normal, METH_NOARGS, CircleSegmentation_get_normal_doc},
  { "get_radius", (PyCFunction)CircleSegmentation_get_radius, METH_NOARGS, CircleSegmentation_get_radius_doc},

  { "set_center", (PyCFunction)CircleSegmentation_set_center, METH_VARARGS|METH_KEYWORDS, CircleSegmentation_set_center_doc},
  { "set_frame", (PyCFunction)CircleSegmentation_set_frame, METH_VARARGS|METH_KEYWORDS, CircleSegmentation_set_frame_doc},
  { "set_normal", (PyCFunction)CircleSegmentation_set_normal, METH_VARARGS|METH_KEYWORDS, CircleSegmentation_set_normal_doc},
  { "set_radius", (PyCFunction)CircleSegmentation_set_radius, METH_VARARGS, CircleSegmentation_set_radius_doc},

  {NULL, NULL}
};

//--------------------------
// PyCircleSegmentationInit
//--------------------------
// This is the __init__() method for the CircleSegmentation class.
//
// This function is used to initialize an object after it is created.
//
// When create a Circle directly a 'radius', 'center' and 'normal or 'frame'
// arguments are required. A Circle can also be created from reading contour
// group in which case no arguements are required.
//
static int
PyCircleSegmentationInit(PyCircleSegmentation* self, PyObject* args, PyObject *kwargs)
{
  //std::cout << "[PyCircleSegmentationInit] ========== Init Circle Segmentation object ========== " << std::endl;
  auto api = PyUtilApiFunction("|O!O!O!O!", PyRunTimeErr, "CircleSegmentation");
  static char *keywords[] = {"radius", "center", "normal", "frame", NULL};
  PyObject* radiusArg = nullptr;
  PyObject* centerArg = nullptr;
  PyObject* normalArg = nullptr;
  PyObject* frameArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyFloat_Type, &radiusArg,
        &PyList_Type, &centerArg, &PyList_Type, &normalArg, &PyPathFrameType, &frameArg)) {
      return -1;
  }

  double radius = 0.0;
  std::array<double,3> normal;
  std::array<double,3> center;
  sv3::PathElement::PathPoint pathPoint;

  // If keyword args have been given.
  //
  if (kwargs != nullptr) {
      if (radiusArg == nullptr) {
          api.error("A 'radius' argument must be given.");
          return -1;
      }
      // Get the radius argument value.
      radius = PyFloat_AsDouble(radiusArg);
      if (radius <= 0.0) {
          api.error("The 'radius' argument must be > 0.");
          return -1;
      }

      // Extract data from the input arguments.
      //
      if (!PyUtilGetFrameData(api, centerArg, center, normalArg, normal, frameArg, pathPoint)) {
          return -1;
      }
  }

  // Create the circle contour.
  self->super.contour = new sv3::circleContour();
  self->super.CopySv4ContourData = PyCircleCopySegmentationData;
  auto circleContour = dynamic_cast<sv3::circleContour*>(self->super.contour);

  // Set circle data if it has been given.
  //
  if (kwargs != nullptr) {

      // Set the circle path point if it is given, else set its plane geometry.
      //
      if (frameArg != nullptr) {
          circleContour->SetPathPoint(pathPoint);
      } else {
          auto plane = vtkSmartPointer<vtkPlane>::New();
          plane->SetOrigin(center.data());
          plane->SetNormal(normal.data());
          circleContour->SetPlaneGeometry(plane);
      }

      // Set the circle point and radius.
      //
      // The circle center is set to the projection of the 'point'
      // onto the given plane or frame.
      //
      circleContour->SetControlPointByRadius(radius, center.data());
  }

  return 0;
}

//-------------------------
// PyCircleSegmentationNew
//-------------------------
//
static PyObject *
PyCircleSegmentationNew(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
  //std::cout << "[PyCircleSegmentationNew] New CircleSegmentation " << std::endl;
  auto self = (PyCircleSegmentation*)type->tp_alloc(type, 0);
  if (self == NULL) {
      std::cout << "[PyCircleSegmentationNew] ERROR: alloc failed." << std::endl;
      return nullptr;
  }
  return (PyObject*)self;
}

//-----------------------------
// PyCircleSegmentationDealloc
//-----------------------------
//
static void
PyCircleSegmentationDealloc(PyCircleSegmentation* self)
{
  //std::cout << "[PyCircleSegmentationDealloc] **** Free PyCircleSegmentation ****" << std::endl;
  delete self->super.contour;
  //auto circleContour = dynamic_cast<sv3::circleContour*>(self->super.contour);
  //delete circleContour;
  Py_TYPE(self)->tp_free(self);
}

//-------------------------------------
// Define the PyCircleSegmentationType
//-------------------------------------
// Define the Python type object that stores Segmentation data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static PyTypeObject PyCircleSegmentationType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and
  // the name of the type within the module.
  SEGMENTATION_CIRCLE_MODULE_CLASS,
  sizeof(PyCircleSegmentation)
};

//---------------------------------
// SetCircleSegmentationTypeFields
//---------------------------------
// Set the Python type object fields that stores Segmentation data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetCircleSegmentationTypeFields(PyTypeObject& segType)
 {
  // Doc string for this type.
  segType.tp_doc = PyCircleSegmentationClass_doc;

  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  segType.tp_new = PyCircleSegmentationNew;
  //.tp_new = PyType_GenericNew,

  segType.tp_base = &PySegmentationType;

  segType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  segType.tp_init = (initproc)PyCircleSegmentationInit;
  segType.tp_dealloc = (destructor)PyCircleSegmentationDealloc;
  segType.tp_methods = PyCircleSegmentationMethods;
};


