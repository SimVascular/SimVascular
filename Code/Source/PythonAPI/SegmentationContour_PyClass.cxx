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

// This is the implementation for the SV Python API contour segmentation class.
//
// The class name is 'segmentation.Contour'.
//
// The Contour class only stores contour points data obtained from a image
// segmentation computation, like levelset or thresholding.

//-----------------------
// PyContourSegmentation
//-----------------------
// Define the Contour class.
//
// center: The center of the contour points.
//
typedef struct {
  PySegmentation super;
  // These must go after any data accessed by Python.
  std::array<double,3> center;
  std::array<double,3> normal;
  double planeDistTol;
} PyContourSegmentation;

//////////////////////////////////////////////////////
//        U t i l i t y     F u n c t i o n s       //
//////////////////////////////////////////////////////

//-------------------------------
// PyContourCopySegmentationData
//-------------------------------
//
void PyContourCopySegmentationData(sv4guiContour* sv4Contour, PyObject* contourObj)
{
  auto svContour = ((PySegmentation*)contourObj)->contour;
  PySegmentationCopySv4ContourData(sv4Contour, svContour);

  // Set PyContourSegmentation data.
  //
  auto contour = (PyContourSegmentation*)contourObj;
  contour->center = svContour->GetCenterPoint();

  double normal[3];
  svContour->GetPlaneGeometry()->GetNormal(normal);
  contour->normal = {normal[0], normal[1], normal[2]};
}

//-----------------------
// PyContourGenerateData
//-----------------------
// Generate the PyContournSegmentation and SV Contour data
// from a list of contour points.
//
void PyContourGenerateData(PyUtilApiFunction& api, PyContourSegmentation* contSeg, const std::vector<std::array<double,3>>& points)
{
  auto contour = contSeg->super.contour;

  // Create two SV control points for center and scale.
  //
  std::vector<std::array<double,3>> controlPoints;
  controlPoints.push_back(std::array<double,3>{0.0,0.0,0.0});
  controlPoints.push_back(std::array<double,3>{0.0,0.0,0.0});
  for (auto pt : points) {
      controlPoints.push_back(pt);
  }

  // Set object data.
  contSeg->center = PyUtilComputePointsCenter(points);
  contSeg->normal = PyUtilComputeNormalFromlPoints(points);

  // Define plane.
  auto plane = vtkSmartPointer<vtkPlane>::New();
  plane->SetOrigin(contSeg->center.data());
  plane->SetNormal(contSeg->normal.data());
  contour->SetPlaneGeometry(plane);

  // Need to set control points after defining the plane.
  contour->SetControlPoints(controlPoints);
  contour->SetContourPoints(points);
}

//////////////////////////////////////////////////////
//          C l a s s    M e t h o d s              //
//////////////////////////////////////////////////////
//
// Python API functions.

//--------------------------------
// ContourSegmentation_get_center
//--------------------------------
//
PyDoc_STRVAR(ContourSegmentation_get_center_doc,
  "get_center()  \n\
   \n\
   Get the center of the polygon segmentation. \n\
   \n\
   \n\
   Returns (list([float,float,float]): The polygon segmentation center. \n\
");

static PyObject*
ContourSegmentation_get_center(PyContourSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  return Py_BuildValue("[d, d, d]", self->center[0], self->center[1], self->center[2]);
}

//--------------------------------
// ContourSegmentation_get_normal
//--------------------------------
//
PyDoc_STRVAR(ContourSegmentation_get_normal_doc,
  "get_normal()  \n\
   \n\
   Get the normal of the polygon segmentation. \n\
   \n\
   \n\
   Returns (list([float,float,float]): The polygon segmentation normal. \n\
");

static PyObject*
ContourSegmentation_get_normal(PyContourSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  return Py_BuildValue("[d, d, d]", self->normal[0], self->normal[1], self->normal[2]);
}

//----------------------------------------
// ContourSegmentation_set_contour_points
//----------------------------------------
//
PyDoc_STRVAR(ContourSegmentation_set_contour_points_doc,
  "set_contour_points(contour_points)  \n\
   \n\
   Set the contour points for a contour segmentation. \n\
   \n\
   Args: \n\
     points (list(list([float,float,float])): The list of contour points. \n\
   \n\
");

static PyObject*
ContourSegmentation_set_contour_points(PyContourSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"contour_points", NULL};
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

  // Generate the PyContourSegmentation and SV Contour data from the list of contour points.
  PyContourGenerateData(api, self, points);

  // Check contour points distance to plane.
  auto maxDist = PyUtilComputeDistPointsToPlane(self->center, self->normal, points);
  if (maxDist > self->planeDistTol) {
      api.error("The 'points' data do not lie in a plane within " + std::to_string(self->planeDistTol) +
            ". Maximum distance computed for the points: " + std::to_string(maxDist));
      return nullptr;
  }

  Py_RETURN_NONE;
}

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* SEGMENTATION_CONTOUR_CLASS = "Contour";
static char* SEGMENTATION_CONTOUR_MODULE_CLASS = "segmentation.Contour";

//--------------------------------
// PyContourSegmentationClass_doc
//--------------------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(PyContourSegmentationClass_doc,
   "Contour(contour_points)  \n\
   \n\
   The Contour class provides an interface for creating a contour          \n\
   segmentation. A contur segmentation is defined by a set of 3D points.   \n\
   \n\
   Args: \n\
     contour_points (list([float,float,float])): The list of 3D points     \n\
        defining the contour.                                              \n\
   \n\
");

//------------------------------
// PyContourSegmentationMethods
//------------------------------
//
static PyMethodDef PyContourSegmentationMethods[] = {
  {"get_center", (PyCFunction)ContourSegmentation_get_center, METH_VARARGS, ContourSegmentation_get_center_doc },
  {"get_normal", (PyCFunction)ContourSegmentation_get_normal, METH_VARARGS, ContourSegmentation_get_normal_doc },
  {"set_contour_points", (PyCFunction)ContourSegmentation_set_contour_points, METH_VARARGS|METH_KEYWORDS, ContourSegmentation_set_contour_points_doc },
  {NULL, NULL}
};

//---------------------------
// PyContourSegmentationInit
//---------------------------
// This is the __init__() method for the ContourSegmentation class.
//
// This function is used to initialize an object after it is created.
//
// A 'radius' and 'normal or 'frame' arguments are required.
//
static int
PyContourSegmentationInit(PyContourSegmentation* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("|O!", PyRunTimeErr, "ContourSegmentation");
  static char *keywords[] = {"contour_points", NULL};
  PyObject* pointsArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &pointsArg)) {
      return -1;
  }

  // Create the SV Contour object.
  //
  self->super.contour = new sv3::Contour();
  self->super.CopySv4ContourData = PyContourCopySegmentationData;
  self->planeDistTol = 1e-6;

  if ((args == nullptr) && (kwargs == nullptr)) {
      return 0;
  }

  auto polygonContour = dynamic_cast<sv3::ContourPolygon*>(self->super.contour);

  // Extract contour point data.
  //
  if (pointsArg != nullptr) {
      std::string msg;
      std::vector<std::array<double,3>> points;
      if (!PyUtilGetPointVectorData(pointsArg, points, msg)) {
          api.error("The 'points' argument " + msg);
          return -1;
      }

      // Generate the PyContourSegmentation and SV Contour data from the list of control points.
      PyContourGenerateData(api, self, points);
      #ifdef debug_PyContourSegmentationInit
      std::cout << "[PyContourSegmentationInit] Number of contour points: " << points.size() << std::endl;
      std::cout << "[PyContourSegmentationInit] Contour normal: " << self->normal[0] << " " << self->normal[1] << " " << self->normal[2] << std::endl;
      std::cout << "[PyContourSegmentationInit] Contour center: " << self->center[0] << " " << self->center[1] << " " << self->center[2] << std::endl;
      #endif

      // Check control points distance to plane.
      auto maxDist = PyUtilComputeDistPointsToPlane(self->center, self->normal, points);
      if (maxDist > self->planeDistTol) {
          api.error("The 'points' data do not lie in a plane within " + std::to_string(self->planeDistTol) +
                ". Maximum distance computed for the points: " + std::to_string(maxDist));
          return -1;
      }
    }

  return 0;
}

//--------------------------
// PyContourSegmentationNew
//--------------------------
//
static PyObject *
PyContourSegmentationNew(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
  //std::cout << "[PyContourSegmentationNew] New ContourSegmentation " << std::endl;
  auto self = (PyContourSegmentation*)type->tp_alloc(type, 0);
  if (self == NULL) {
      std::cout << "[PyContourSegmentationNew] ERROR: alloc failed." << std::endl;
      return nullptr;
  }
  return (PyObject*)self;
}

//------------------------------
// PyContourSegmentationDealloc
//------------------------------
//
static void
PyContourSegmentationDealloc(PyContourSegmentation* self)
{
  //std::cout << "[PyContourSegmentationDealloc] **** Free PyContourSegmentation ****" << std::endl;
  delete self->super.contour;
  Py_TYPE(self)->tp_free(self);
}

//--------------------------------------
// Define the PyContourSegmentationType
//--------------------------------------
// Define the Python type object that stores Segmentation data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static PyTypeObject PyContourSegmentationType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and
  // the name of the type within the module.
  SEGMENTATION_CONTOUR_MODULE_CLASS,
  sizeof(PyContourSegmentation)
};

//----------------------------------
// SetContourSegmentationTypeFields
//----------------------------------
// Set the Python type object fields that stores Segmentation data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetContourSegmentationTypeFields(PyTypeObject& segType)
 {
  // Doc string for this type.
  segType.tp_doc = PyContourSegmentationClass_doc;

  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  segType.tp_new = PyContourSegmentationNew;
  //.tp_new = PyType_GenericNew,

  segType.tp_base = &PySegmentationType;

  segType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  segType.tp_init = (initproc)PyContourSegmentationInit;
  segType.tp_dealloc = (destructor)PyContourSegmentationDealloc;
  segType.tp_methods = PyContourSegmentationMethods;
};


