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

// The functions defined here implement the SV Python API 'Segmentation' class.
//
// The 'Segmentation' class is used to store segmentation data. It is base class
// for the segmentation cirlce, ellipse, level set, polygon and threshold types.
//
using sv3::Contour;
using sv3::PathElement;

//----------------
// ContourCtorMap
//----------------
// Define an object factory for creating objects for Contour derived classes.
//
using ContourCtorMapType = std::map<cKernelType, std::function<Contour*()>>;
ContourCtorMapType ContourCtorMap = {
    {cKernelType::cKERNEL_CIRCLE, []() -> Contour* { return new sv3::circleContour(); } },
    {cKernelType::cKERNEL_ELLIPSE, []() -> Contour* { return new sv3::circleContour(); } },
    {cKernelType::cKERNEL_LEVELSET, []() -> Contour* { return new sv3::levelSetContour(); } },
    {cKernelType::cKERNEL_POLYGON, []() -> Contour* { return new sv3::ContourPolygon(); } },
    {cKernelType::cKERNEL_SPLINEPOLYGON, []() -> Contour* { return new sv3::ContourSplinePolygon(); } },
    {cKernelType::cKERNEL_THRESHOLD, []() -> Contour* { return new sv3::thresholdContour(); } },
};

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

//--------------------------
// CreateSegmentationObject
//--------------------------
// Create an SV Segmentation derived object.
//
// If 'contourType' is not valid then create a Contour object.
// Contour objects can be used with diffetent methods.
//
static Contour *
CreateSegmentationObject(cKernelType contourType, PathElement::PathPoint pathPoint)
{
  Contour* contour = nullptr;

  try {
      contour = ContourCtorMap[contourType]();
  } catch (const std::bad_function_call& except) {
      contour = new sv3::Contour();
      std::cout << "[CreateSegmentationObject] ERROR: Unknown type: " << contourType << std::endl;
  }

  contour->SetPathPoint(pathPoint);
  return contour;
}

//////////////////////////////////////////////////////
//          C l a s s   M e t h o d s               //
//////////////////////////////////////////////////////
//
// Python 'Contour' class methods.

//-------------------------
// Segmentation_get_center
//-------------------------
//
PyDoc_STRVAR(Segmentation_get_center_doc,
  "get_center()  \n\
   \n\
   Get the center of the segmentation. \n\
   \n\
   Returns list([x,y,z]): The center of the segmentation. \n\
");

static PyObject *
Segmentation_get_center(PySegmentation* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto contour = self->contour;
  auto center = contour->GetCenterPoint();
  return Py_BuildValue("[d,d,d]", center[0], center[1], center[2]);
}

//-------------------------
// Segmentation_get_points
//-------------------------
//
PyDoc_STRVAR(Segmentation_get_points_doc,
  "get_points()  \n\
   \n\
   Get the segmentation contour points. \n\
   \n\
   Returns list([x,y,z]): The list of contour points. \n\
");

static PyObject *
Segmentation_get_points(PySegmentation* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto contour = self->contour;
  auto contour_points = contour->GetContourPoints();
  auto pointList = PyList_New(contour_points.size());
  int n = 0;

  for (auto const& point : contour_points) {
      auto pointValues = PyList_New(3);
      for (int i = 0; i < 3; i++) {
          auto val = PyFloat_FromDouble((double)point[i]);
          PyList_SetItem(pointValues, i, val);
      }
      PyList_SetItem(pointList, n, pointValues);
      n += 1;
  }

  return Py_BuildValue("N", pointList);
}

//---------------------
// Segmentation_get_id
//---------------------
//
PyDoc_STRVAR(Segmentation_get_id_doc,
  "get_id()  \n\
   \n\
   Get the contour ID. \n\
   \n\
   Returns int: The contour ID. \n\
");

static PyObject *
Segmentation_get_id(PySegmentation* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto contour = self->contour;
  auto id = contour->GetContourID();
  return Py_BuildValue("i", id);
}

//-----------------------------
// Segmentation_get_path_point
//-----------------------------
//
PyDoc_STRVAR(Segmentation_get_path_point_doc,
  "get_path_point()  \n\
   \n\
   Get the contour path point. \n\
   \n\
   Returns dict(pos:[x,y,z], tangent:[x,y,z], rotation:[x,y,z]): The contour path point. \n\
");

static PyObject *
Segmentation_get_path_point(PySegmentation* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto contour = self->contour;
  auto pathPoint = contour->GetPathPoint();
  return Py_BuildValue("{s:[d,d,d], s:[d,d,d], s:[d,d,d]}",
    "pos", pathPoint.pos[0], pathPoint.pos[1], pathPoint.pos[1],
    "tangent", pathPoint.tangent[0], pathPoint.tangent[1], pathPoint.tangent[1],
    "rotation", pathPoint.rotation[0], pathPoint.rotation[1], pathPoint.rotation[1] );
}

//-----------------------
// Segmentation_get_type
//-----------------------
//
PyDoc_STRVAR(Segmentation_get_type_doc,
  "get_type()  \n\
   \n\
   Get the contour type. \n\
   \n\
   Returns (str): contour type. \n\
");

static PyObject *
Segmentation_get_type(PySegmentation* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto contour = self->contour;
  auto contourType = contour->GetType();
  return Py_BuildValue("s", contourType.c_str());
}

//---------------------------
// Segmentation_get_polydata
//---------------------------
//
PyDoc_STRVAR(Segmentation_get_polydata_doc,
  "get_polydata()  \n\
   \n\
   Get the vtkPolyData object representing contour points. \n\
   \n\
   Returns (vtkPolyData): The vtkPolyData object. \n\
");

static PyObject *
Segmentation_get_polydata(PySegmentation* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto contour = self->contour;

  // Don't include the center and scaling control points.
  bool includingAllLines = false;

  // Get the vtkPolyData object.
  vtkSmartPointer<vtkPolyData> polydata = contour->CreateVtkPolyDataFromContour(includingAllLines);
  return vtkPythonUtil::GetObjectFromPointer(polydata);
}

//=======================================================================================================
//                                   O L D   M E T H O D S
//=======================================================================================================

//------------------------
// Segmentation_set_image
//------------------------
//
PyDoc_STRVAR(Segmentation_set_image_doc,
  "Segmentation_set_image(image)  \n\
   \n\
   Set the image data for a contour. \n\
   \n\
   Args: \n\
     image (vtkImageData): A VTK image object.  \n\
");

static PyObject*
Segmentation_set_image(PySegmentation* self, PyObject* args)
{
    PyObject *vtkName;
    auto api = PyUtilApiFunction("O", PyRunTimeErr, __func__);

    if (!PyArg_ParseTuple(args, api.format, &vtkName)) {
        return api.argsError();
    }

    // Check the Contour object has data.
    Contour* contour = self->contour;
    if (contour == nullptr) {
        api.error("The Contour object does not have geometry.");
        return nullptr;
    }

    // Look up the named vtk object:
    auto vtkObj = (vtkImageData *)vtkPythonUtil::GetPointerFromObject(vtkName, "vtkImageData");
    if (vtkObj == nullptr) {
        api.error("The vtkImageData object does not exist.");
        return nullptr;
    }

    // [TODO:DaveP] What does this do?
    vtkImageData* slice = sv3::SegmentationUtils::GetSlicevtkImage(contour->GetPathPoint(),vtkObj, 5.0);
    contour->SetVtkImageSlice(slice);

    Py_INCREF(contour);
    self->contour=contour;
    Py_DECREF(contour);
    return Py_None;
}

//---------------------------------
// Segmentation_set_control_points
//---------------------------------
//
// Use try-catch block for error handling.
//
// [TODO:DaveP] I'm not sure we need this function, think it should be
// defined for each Contour object type.
//
PyDoc_STRVAR(Segmentation_set_control_points_doc,
  "Contour.set_control_points(control_points)  \n\
   \n\
   Set the control points for a contour. \n\
   \n\
   Args: \n\
     control_points (list[]): The list of control points to set for the contour. The number of control points needed depends on the Contour kernel set for this object.\n\
");

static PyObject*
Segmentation_set_control_points(PySegmentation* self, PyObject* args)
{
    PyObject *controlPoints = nullptr;
    auto api = PyUtilApiFunction("O", PyRunTimeErr, __func__);

    if (!PyArg_ParseTuple(args, api.format, &controlPoints)) {
        return api.argsError();
    }

    try {

    // Check control points data.
    //
    if (!PyList_Check(controlPoints)) {
        throw std::runtime_error("Control points argument is not a Python list.");
    }

    int numPts = PyList_Size(controlPoints);
    for (int i = 0; i < numPts; i++) {
        PyObject* pt = PyList_GetItem(controlPoints,i);
        if ((PyList_Size(pt) != 3) || !PyList_Check(pt)) {
            throw std::runtime_error("Control points argument data at " + std::to_string(i) +
              " in the list is not a 3D point (three float values).");
        }
        for (int j = 0; j < 3; j++) {
            if (!PyFloat_Check(PyList_GetItem(pt,j))) {
                throw std::runtime_error("Control points argument data at " + std::to_string(i) +
                  " in the list is not a 3D point (three float values).");
            }
        }
    }

    // Check that the number of control is consistant
    // with the kernel type.
    //
    // [TODO:DaveP] The kernel should be set in the object.
    //
    if ((Contour::gCurrentKernel == cKERNEL_CIRCLE) && (numPts != 2)) {
        throw std::runtime_error("Circle contour requires two points: a center and a point on its boundary.");

    } else if ((Contour::gCurrentKernel == cKERNEL_ELLIPSE) && (numPts != 3)) {
        throw std::runtime_error("Ellipse contour requires three points: a center and two points on its boundary.");

    } else if ((Contour::gCurrentKernel == cKERNEL_POLYGON) && (numPts < 3)) {
        throw std::runtime_error("Polygon contour requires at least three points");
    }

    Contour* contour = self->contour;
    if (contour == NULL ) {
        throw std::runtime_error("Geometry has not been created for the contour.");
    }

    // Copy control points to contour object.
    std::vector<std::array<double,3> > pts(numPts);
    for (int i = 0; i < numPts; i++) {
        PyObject* tmpList = PyList_GetItem(controlPoints,i);
        for (int j = 0; j < 3; j++) {
            pts[i][j] = PyFloat_AsDouble(PyList_GetItem(tmpList,j));
        }
    }
    contour->SetControlPoints(pts);
    return Py_None;

   } catch (std::exception &e) {
       api.error(e.what());
       return nullptr;
  }

}

//-------------------------------------------
// Segmentation_set_control_points_by_radius
//-------------------------------------------
//
// [TODO:DaveP] I think this should be removed, have it only defined
// for the CircelContour type.
//
PyDoc_STRVAR(Segmentation_set_control_points_by_radius_doc,
  "Contour.set_control_points_by_radius(control_points)  \n\
   \n\
   Set the control points for a Circle Contour with a center point and radius. \n\
   \n\
   Args: \n\
     center ([x,y,z]): The list of three floats defining the center of the Circle Contour.   \n\
     radius (float)): The radius of the Circle Contour.   \n\
");

static PyObject*
Segmentation_set_control_points_by_radius(PySegmentation* self, PyObject* args)
{
    auto api = PyUtilApiFunction("Od", PyRunTimeErr, __func__);

    if (Contour::gCurrentKernel != cKERNEL_CIRCLE) {
        api.error("Contour kernel is not set to 'Circle'");
        return nullptr;
    }

    PyObject *center;
    double radius = 0.0;
    if (!PyArg_ParseTuple(args, api.format, &center, &radius )) {
        return api.argsError();
    }

    double ctr[3];
    if (PyList_Size(center) != 3) {
        api.error("Center argument is not a 3D point (three float values).");
        return nullptr;
    }

    for (int i = 0; i < PyList_Size(center); i++) {
        if (!PyFloat_Check(PyList_GetItem(center,i))) {
            api.error("Center argument is not a 3D point (three float values).");
            return nullptr;
        }
        ctr[i] = PyFloat_AsDouble(PyList_GetItem(center,i));
    }

    auto contour = self->contour;
    if (contour == NULL) {
        api.error("No geometry has been created for the contour.");
        return nullptr;
    }

    if (radius <= 0.0) {
        api.error("Radius argument must be > 0.0.");
        return nullptr;
    }

    contour->SetControlPointByRadius(radius,ctr);
    return Py_None;
}


//-----------------------
// Segmentation_get_area
//-----------------------
//
PyDoc_STRVAR(Segmentation_get_area_doc,
  "Contour.area()  \n\
   \n\
   Get the area of the contour. \n\
   \n\
   Returns: Area (float) of the contour. \n\
");

static PyObject*
Segmentation_get_area(PySegmentation* self, PyObject* args)
{
    auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
    auto contour = self->contour;
    if (contour == NULL) {
        api.error("No geometry has been created for the contour.");
        return nullptr;

    }
    double area = contour->GetArea();
    return Py_BuildValue("d",area);
}

//----------------------------
// Segmentation_get_perimeter
//----------------------------
//
PyDoc_STRVAR(Segmentation_get_perimeter_doc,
  "Contour.perimeter()  \n\
   \n\
   Get the length of the contour perimeter. \n\
   \n\
   Returns: Length (float) of the contour perimeter. \n\
");

PyObject* Segmentation_get_perimeter(PySegmentation* self, PyObject* args)
{
    auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
    auto contour = self->contour;
    if (contour == NULL) {
        api.error("No geometry has been created for the Contour.");
        return nullptr;
    }
    double perimeter = contour->GetPerimeter();
    return Py_BuildValue("d",perimeter);
}


//----------------------------------
// Segmentation_set_threshold_value
//----------------------------------
//
PyDoc_STRVAR(Segmentation_set_threshold_value_doc,
  "Contour.set_threshold_value()  \n\
   \n\
   Set the threshold value for a Threshold Contour. \n\
   \n\
   Args: \n\
     threshold (float): Threshold value. \n\
");

static PyObject *
Segmentation_set_threshold_value(PySegmentation* self, PyObject* args)
{
    double threshold = 0.0;
    auto api = PyUtilApiFunction("d", PyRunTimeErr, __func__);

    if (!PyArg_ParseTuple(args, api.format, &threshold)) {
        return api.argsError();
    }

    if (Contour::gCurrentKernel != cKERNEL_THRESHOLD) {
        api.error("Contour kernel is not set to 'Threshold'");
        return nullptr;
    }

    Contour* contour = self->contour;
    if (contour == NULL) {
        api.error("No geometry has been created for the contour.");
        return nullptr;
    }

    contour->SetThresholdValue(threshold);
    return Py_None;
}

//------------------------------------
// Segmentation_create_smooth_contour
//------------------------------------
//
PyDoc_STRVAR(Segmentation_create_smooth_contour_doc,
  "Contour.create_smooth_contour()  \n\
   \n\
   Create a smoothed contour. \n\
   \n\
   Args:                                    \n\
     num_modes (int): Number of Fourier modes.\n\
     name (str): Name of the new smoothed contour. \n\
");

static PySegmentation*
Segmentation_create_smooth_contour(PySegmentation* self, PyObject* args)
{
    auto api = PyUtilApiFunction("is", PyRunTimeErr, __func__);
    int fourierNumber = 0;
    char* contourName;

    if (!PyArg_ParseTuple(args, api.format, &fourierNumber, &contourName)) {
        api.argsError();
        return nullptr;
    }

    Contour* contour = self->contour;
    if (contour == NULL) {
        api.error("No geometry has been created for the Contour.");
        return nullptr;
    }

    auto newContour = CreateSegmentationObject(Contour::gCurrentKernel, contour->GetPathPoint());
    //Contour *newContour = sv3::Contour::DefaultInstantiateContourObject(Contour::gCurrentKernel, contour->GetPathPoint());
    newContour= contour->CreateSmoothedContour(fourierNumber);

    if ( !( gRepository->Register(contourName, newContour))) {
        delete newContour;
        api.error("Could not add the new contour into the repository.");
        return nullptr;
    }

    Py_INCREF(newContour);
    PySegmentation* pyNewCt;
    pyNewCt = PyCreateSegmentationType();
    pyNewCt->contour = newContour;
    Py_DECREF(newContour);
    return pyNewCt;
}

//---------------------
// Segmentation_create
//---------------------
//
static PyObject *
Segmentation_create(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char* kernelName = nullptr;

  if (!PyArg_ParseTuple(args, api.format, &kernelName)) {
      return api.argsError();
  }

  cKernelType contourType;

  try {
      contourType = kernelNameEnumMap.at(std::string(kernelName));
  } catch (const std::out_of_range& except) {
      auto msg = "Unknown kernel name '" + std::string(kernelName) + "'." +
          " Valid names are: " + kernelValidNames + ".";
      api.error(msg);
      return nullptr;
  }

  //std::cout << "[Segmentation_create] Kernel name: " << kernelName << std::endl;
  auto cont = PyCreateSegmentation(contourType);
  Py_INCREF(cont);
  return cont;
}

////////////////////////////////////////////////////////
//           C l a s s   D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* SEGMENTATION_CLASS = "Segmentation";

// Dotted name that includes both the module name and
// the name of the type within the module.
static char* SEGMENTATION_MODULE_CLASS = "segmentation.Segmentation";

PyDoc_STRVAR(SegmentationClass_doc, "segmentation class functions.");

//-----------------------
// PySegmentationMethods
//-----------------------
// Define the methods for the Python 'Segmentation' class.
//
static PyMethodDef PySegmentationMethods[] = {

  {"get_center", (PyCFunction)Segmentation_get_center, METH_NOARGS, Segmentation_get_center_doc },

  {"get_points", (PyCFunction)Segmentation_get_points, METH_NOARGS, Segmentation_get_points_doc},

  {"get_id", (PyCFunction)Segmentation_get_id, METH_NOARGS, Segmentation_get_id_doc},

  {"get_path_point", (PyCFunction)Segmentation_get_path_point, METH_NOARGS, Segmentation_get_path_point_doc},

  {"get_polydata", (PyCFunction)Segmentation_get_polydata, METH_NOARGS, Segmentation_get_polydata_doc},

  {"get_type", (PyCFunction)Segmentation_get_type, METH_NOARGS, Segmentation_get_type_doc},


  // ======================= old methods ================================================ //
  /*

  { "area", (PyCFunction)Segmentation_get_area, METH_NOARGS, Segmentation_get_area_doc },

  {"create_smooth_contour", (PyCFunction)Segmentation_create_smooth_contour, METH_VARARGS, Segmentation_create_smooth_contour_doc },

  {"get_polydata", (PyCFunction)Segmentation_get_polydata, METH_VARARGS, Segmentation_get_polydata_doc },

  {"perimeter", (PyCFunction)Segmentation_get_perimeter, METH_NOARGS, Segmentation_get_perimeter_doc },

  {"set_control_points", (PyCFunction)Segmentation_set_control_points, METH_VARARGS, Segmentation_set_control_points_doc },

  {"set_control_points_by_radius", (PyCFunction)Segmentation_set_control_points_by_radius, METH_VARARGS, Segmentation_set_control_points_by_radius_doc },

  {"set_image", (PyCFunction)Segmentation_set_image, METH_VARARGS, Segmentation_set_image_doc },

  {"set_threshold_value", (PyCFunction)Segmentation_set_threshold_value, METH_VARARGS, Segmentation_set_threshold_value_doc },
  */

  {NULL,NULL}
};

//--------------------
// PySegmentationType
//--------------------
// Define the Python type object for the Python 'Segmentation' class.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static PyTypeObject PySegmentationType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  SEGMENTATION_CLASS,
  sizeof(PySegmentation)
};

//--------------------
// PySegmentationInit
//--------------------
// This function is used to initialize an object after it is created.
//
// This implements the Python __init__ method for the Segmentation class.
// It is called after calling the __new__ method.
//
static int
PySegmentationInit(PySegmentation* self, PyObject* args, PyObject *kwds)
{
  static int numObjs = 1;
  //std::cout << "[PySegmentationInit] New Segmentation object: " << numObjs << std::endl;
  char* kernelName = nullptr;
  if (!PyArg_ParseTuple(args, "|s", &kernelName)) {
      return -1;
  }

  self->contour = new Contour();
  self->id = numObjs;
  numObjs += 1;
  return 0;
}

//--------------------
// PySegmentationtNew
//--------------------
// Create a new instance of a PySegmentation object.
//
// This implements the Python __new__ method. It is called before the
// __init__ method.
//
static PyObject *
PySegmentationNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PySegmentationNew] PySegmentationNew " << std::endl;
  auto self = (PySegmentation*)type->tp_alloc(type, 0);
  if (self != NULL) {
      //self->id = 1;
  }

  return (PyObject *) self;
}

//-----------------------
// PySegmentationDealloc
//-----------------------
//
static void
PySegmentationDealloc(PySegmentation* self)
{
  //std::cout << "[PySegmentationDealloc] " << std::endl;
  //std::cout << "[PySegmentationDealloc] *********  F r e e   P y S e g m e n t a t i o n  *********  " << self->id << std::endl;
  //std::cout << "[PySegmentationDealloc] " << std::endl;
  delete self->contour;
  Py_TYPE(self)->tp_free(self);
}

//---------------------------
// SetSegmentationTypeFields
//---------------------------
// Set the Python type object fields that stores Segmentation data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetSegmentationTypeFields(PyTypeObject& contourType)
{
  // Doc string for this type.
  contourType.tp_doc = "Segmentation objects";
  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  contourType.tp_new = PySegmentationNew;
  //contourType.tp_new = PyType_GenericNew,
  contourType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  contourType.tp_init = (initproc)PySegmentationInit;
  contourType.tp_dealloc = (destructor)PySegmentationDealloc;
  contourType.tp_methods = PySegmentationMethods;
};

//--------------------------
// PyCreateSegmentationType
//--------------------------
// Create a Python PySegmentation object.
//
static PySegmentation *
PyCreateSegmentationType()
{
  return PyObject_New(PySegmentation, &PySegmentationType);
}

