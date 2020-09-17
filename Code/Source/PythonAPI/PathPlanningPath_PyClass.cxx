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

// The functions defined here implement the SV Python API 'pathplanning' module 'Path' class.
//
//     path = pathplanning.Path()
//
// The Python Path class is implemented using the PyPath struct defined in PathPlanning_PyModule.h.
//
//   PyPath:
//      Attributes: None
//      Data members:
//         sv3::PathElement* path;

// Declare SV object that stores path data.
using sv3::PathElement;

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

//----------------
// GetPathElement
//----------------
// Get the path PathElement object.
//
static PathElement*
GetPathElement(PyUtilApiFunction& api, PyPath* self)
{
  auto path = self->path;
  if (path == NULL) {
      api.error("The path element data has not be created.");
      return nullptr;
  }
  return path;
}

//////////////////////////////////////////////////////
//          C l a s s   M e t h o d s               //
//////////////////////////////////////////////////////
//
// Python 'Path' class methods.

//------------------------
// Path_add_control_point
//------------------------
//
PyDoc_STRVAR(Path_add_control_point_doc,
  "add_control_point(point, index=None) \n\
   \n\
   Add a control point to a path. \n\
   \n\
   The point is inserted into the path's list of control points at the location to the control point it is closest to.  \n\
   If the 'location' argument is given then the point is added at that location in the list. \n\
   \n\
   Args: \n\
     point (list[float,float,float]): The control point (x,y,z) coordinates. \n\
     index (Optional[int]): The index into the current list of control points to add the control point. 0 <= index <= number of path control points - 1\n\
");

static PyObject *
Path_add_control_point(PyPath* self, PyObject* args)
{
  auto api = PyUtilApiFunction("O!|i", PyRunTimeErr, __func__);
  PyObject* pointArg;
  int index = -2;

  if (!PyArg_ParseTuple(args, api.format, &PyList_Type, &pointArg, &index)) {
      return api.argsError();
  }

  // Check control point data.
  //
  std::string emsg;
  if (!PyUtilCheckPointData(pointArg, emsg)) {
      api.error("Control point argument " + emsg);
      return nullptr;
  }

  auto path = GetPathElement(api, self);
  if (path == NULL) {
      return nullptr;
  }

  std::array<double,3> point;
  point[0] = PyFloat_AsDouble(PyList_GetItem(pointArg,0));
  point[1] = PyFloat_AsDouble(PyList_GetItem(pointArg,1));
  point[2] = PyFloat_AsDouble(PyList_GetItem(pointArg,2));

  // Check if the control point is already
  // defined for the path.
  //
  // [TODO:DaveP] Get rid of this '-2'? What about '-1'?
  //
  if (path->SearchControlPoint(point,0) !=- 2) {
      auto msg = "The control point (" + std::to_string(point[0]) + ", " + std::to_string(point[1]) + ", " +
        std::to_string(point[2]) + ") has already been defined for the path.";
      api.error(msg);
      return nullptr;
  }

  // Set the path control point by index or by point.
  //
  int numPts = path->GetControlPoints().size();
  if (index != -2) {
      if ((index >= numPts) || (index < 0)) {
          auto msg = "The 'index' argument " + std::to_string(index) + " must be >= 0 and < the number of control points -1 (" +
                std::to_string(numPts-1 ) + ").";
          api.error(msg);
          return nullptr;
      }
  } else {
      index = path->GetInsertintIndexByDistance(point);
  }

  // Insert the control point.
  //
  // The path curve points are generated each time a new control point is added.
  //
  path->InsertControlPoint(index, point);
  return SV_PYTHON_OK;
}

//-------------------------
// Path_get_control_points
//-------------------------
//
PyDoc_STRVAR(Path_get_control_points_doc,
  "get_control_points() \n\
   \n\
   Get the path's control points. \n\
   \n\
   Returns (list(list[float,float,float])): The list of the path's control points. \n\
");

static PyObject *
Path_get_control_points(PyPath* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto path = GetPathElement(api, self);
  if (path == NULL) {
    return nullptr;
  }

  int num = path->GetControlPointNumber();
  if (num == 0) {
    api.error("The path does not have control points defined for it.");
    return nullptr;
  }

  // [TODO:DaveP] Do we need to Py_INCREF() here?
  //
  PyObject* output = PyList_New(num);
  for (int i = 0; i < num; i++) {
      PyObject* coordList = PyList_New(3);
      std::array<double,3> pos = path->GetControlPoint(i);
      for (int j=0; j<3; j++) {
          PyList_SetItem(coordList, j, PyFloat_FromDouble(pos[j]));
      }
      PyList_SetItem(output, i, coordList);
    }

  if (PyErr_Occurred() != NULL) {
      api.error("Error generating path control points output.");
      return nullptr;
  }

  return output;
}

//----------------------
// Path_get_curve_frame
//----------------------
//
PyDoc_STRVAR(Path_get_curve_frame_doc,
  "get_curve_frame(index) \n\
   \n\
   Get the coordinate frame at the path's interpolating curve at a given location. \n\
   \n\
   Args: \n\
     index (int): The index into the path's curve frames. 0 <= index <= number of path curve frames - 1. \n\
   \n\
   Returns (list([float,float,float])): The path's curve normal at the given location. \n\
");

static PyObject *
Path_get_curve_frame(PyPath* self, PyObject* args)
{
  auto api = PyUtilApiFunction("i", PyRunTimeErr, __func__);
  int indexArg;

  if (!PyArg_ParseTuple(args, api.format, &indexArg)) {
      return api.argsError();
  }

  auto path = GetPathElement(api, self);
  if (path == NULL) {
    return nullptr;
  }

  int num = path->GetPathPointNumber();
  if (num == 0) {
    api.error("The path does not have points created for it.");
    return nullptr;
  }

  if ((indexArg < 0) || (indexArg >= num)) {
    api.error("The 'index' argument must be between 0 and " + std::to_string(num-1) + ".");
    return nullptr;
  }

  auto pathPoint = path->GetPathPoint(indexArg);
  auto pathFrameObj = CreatePyPathFrame(pathPoint);
  return pathFrameObj;
  //return Py_BuildValue("[d, d, d]", pathPoint.rotation[0], pathPoint.rotation[1], pathPoint.rotation[2]);
}

//-----------------------
// Path_get_curve_normal
//-----------------------
//
PyDoc_STRVAR(Path_get_curve_normal_doc,
  "get_curve_normal(index) \n\
   \n\
   Get the normal to the path's interpolating curve at a given location. \n\
   \n\
   Args: \n\
     index (int): The index into the path's curve normals. 0 <= index <= number of path curve points - 1. \n\
   \n\
   Returns (list([float,float,float])): The path's curve normal at the given location. \n\
");

static PyObject *
Path_get_curve_normal(PyPath* self, PyObject* args)
{
  auto api = PyUtilApiFunction("i", PyRunTimeErr, __func__);
  int indexArg;

  if (!PyArg_ParseTuple(args, api.format, &indexArg)) {
      return api.argsError();
  }

  auto path = GetPathElement(api, self);
  if (path == NULL) {
    return nullptr;
  }

  int num = path->GetPathPointNumber();
  if (num == 0) {
    api.error("The path does not have points created for it.");
    return nullptr;
  }

  if ((indexArg < 0) || (indexArg >= num)) {
    api.error("The 'index' argument must be between 0 and " + std::to_string(num-1) + ".");
    return nullptr;
  }

  auto pathPoint = path->GetPathPoint(indexArg);
  return Py_BuildValue("[d, d, d]", pathPoint.rotation[0], pathPoint.rotation[1], pathPoint.rotation[2]);
}

//----------------------
// Path_get_curve_point
//----------------------
//
PyDoc_STRVAR(Path_get_curve_point_doc,
  "get_curve_point(index) \n\
   \n\
   Get the point on the path's interpolating curve at a given location. \n\
   \n\
   Args: \n\
     index (int): The index into the path's curve points. 0 <= index <= number of path curve points - 1. \n\
   \n\
   Returns (list([float,float,float])): The path's curve point at the given location. \n\
");

static PyObject *
Path_get_curve_point(PyPath* self, PyObject* args)
{
  auto api = PyUtilApiFunction("i", PyRunTimeErr, __func__);
  int index;

  if (!PyArg_ParseTuple(args, api.format, &index)) {
      return api.argsError();
  }

  auto path = GetPathElement(api, self);
  if (path == NULL) {
    return nullptr;
  }

  int num = path->GetPathPointNumber();
  if (num == 0) {
    api.error("The path does not have curve points created for it.");
    return nullptr;
  }

 if ((index >= num) || (index < 0)) {
      auto msg = "The 'index' argument " + std::to_string(index) + " must be >= 0 and < the number of curve points (" + std::to_string(num) + ").";
      api.error(msg);
      return nullptr;
  }

  std::array<double,3> pos = path->GetPathPosPoint(index);
  return Py_BuildValue("[d, d, d]", pos[0], pos[1], pos[2]);
}

//-----------------------
// Path_get_curve_points
//-----------------------
//
PyDoc_STRVAR(Path_get_curve_points_doc,
  "get_curve_points() \n\
   \n\
   Get the points along the path's interpolating curve. \n\
   \n\
   Returns (list(list[float,float,float])): The list of the path's interpolating curve points. \n\
");

static PyObject *
Path_get_curve_points(PyPath* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto path = GetPathElement(api, self);
  if (path == NULL) {
    return nullptr;
  }

  int num = path->GetPathPointNumber();
  if (num == 0) {
    api.error("The path does not have points created for it.");
    return nullptr;
  }

  PyObject* output = PyList_New(num);

  for (int i = 0; i < num; i++) {
    PyObject* tmpList = PyList_New(3);
    std::array<double,3> pos = path->GetPathPosPoint(i);
    for (int j=0; j<3; j++)
        PyList_SetItem(tmpList,j,PyFloat_FromDouble(pos[j]));
        PyList_SetItem(output,i,tmpList);
    }

  if(PyErr_Occurred()!=NULL) {
    api.error("error generating pathpospt output");
    return nullptr;
  }

  return output;
}

//-------------------------
// Path_get_curve_polydata
//-------------------------
//
PyDoc_STRVAR(Path_get_curve_polydata_doc,
  "get_curve_polydata() \n\
   \n\
   Get the vtkPolyData object representing the path's interpolating curve. \n\
   \n\
   Returns (vtkPolyData): The vtkPolyData object representing the path's interpolating curve.  \n\
");

static PyObject*
Path_get_curve_polydata(PyPath* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  char* dstName = NULL;

  auto path = GetPathElement(api, self);
  if (path == NULL) {
      return nullptr;
  }

  // Get the VTK polydata.
  vtkSmartPointer<vtkPolyData> polydata = path->CreateVtkPolyDataFromPath(true);
  if (polydata == NULL) {
      api.error("Could not get polydata for the path's interpolating curve..");
      return nullptr;
  }

  return vtkPythonUtil::GetObjectFromPointer(polydata);
}

//------------------------
// Path_get_curve_tangent
//------------------------
//
PyDoc_STRVAR(Path_get_curve_tangent_doc,
  "get_curve_tangent(index) \n\
   \n\
   Get the tangent to the path's interpolating curve at a given location. \n\
   \n\
   Args: \n\
     index (int): The index into the path's curve tangents. 0 <= index <= number of path curve tangents - 1. \n\
   \n\
   Returns (list([float,float,float])): The path's curve tangent at the given location. \n\
");

static PyObject *
Path_get_curve_tangent(PyPath* self, PyObject* args)
{
  auto api = PyUtilApiFunction("i", PyRunTimeErr, __func__);
  int indexArg;

  if (!PyArg_ParseTuple(args, api.format, &indexArg)) {
      return api.argsError();
  }

  auto path = GetPathElement(api, self);
  if (path == NULL) {
    return nullptr;
  }

  int num = path->GetPathPointNumber();
  if (num == 0) {
    api.error("The path does not have points created for it.");
    return nullptr;
  }

  if ((indexArg < 0) || (indexArg >= num)) {
    api.error("The path 'index' argument must be between 0 and " + std::to_string(num-1) + ".");
    return nullptr;
  }

  auto pathPoint = path->GetPathPoint(indexArg);
  return Py_BuildValue("[d, d, d]", pathPoint.tangent[0], pathPoint.tangent[1], pathPoint.tangent[2]);
}

//---------------------------
// Path_get_num_curve_points
//---------------------------
//
PyDoc_STRVAR(Path_get_num_curve_points_doc,
  "get_num_curve_points() \n\
   \n\
   Get the number of points along the path's interpolating curve. \n\
   \n\
   Returns (int): The number of points along the path's interpolating curve. \n\
");

static PyObject *
Path_get_num_curve_points(PyPath* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto path = GetPathElement(api, self);
  if (path == NULL) {
    return nullptr;
  }

  int num = path->GetPathPointNumber();
  return Py_BuildValue("i", num);
}

//---------------------------
// Path_get_num_subdivisions
//---------------------------
//
PyDoc_STRVAR(Path_get_num_subdivisions_doc,
  "get_num_subdivisions() \n\
   \n\
   Get the path group's calculation number. \n\
   \n\
   Returns (int): The path group's calculation number. \n\
");

static PyObject *
Path_get_num_subdivisions(PyPath* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto path = GetPathElement(api, self);
  if (path == NULL) {
    return nullptr;
  }

  int number = path->GetCalculationNumber();
  return Py_BuildValue("i", number);
}

//----------------------------
//Path_get_subdivision_method
//----------------------------
//
PyDoc_STRVAR(Path_get_subdivision_method_doc,
  "get_subdivision_method() \n\
   \n\
   Get the path's subdivision method. \n\
   \n\
   Returns (str): The path's subdivsion method. \n\
");

static PyObject *
Path_get_subdivision_method(PyPath* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto path = GetPathElement(api, self);
  if (path == NULL) {
      return nullptr;
  }

  // Get the method name.
  PathElement::CalculationMethod method = path->GetMethod();
  std::string methodName;
  for (auto const& element : subdivMethodNameTypeMap) {
      if (method == element.second) {
          methodName = element.first;
          break;
      }
  }

  if (methodName == "") {
      api.error("No subdivision method is set.");
      return nullptr;
  }

  return Py_BuildValue("s", methodName.c_str());
}

//------------------------------
// Path_get_subdivision_spacing
//------------------------------
//
PyDoc_STRVAR(Path_get_subdivision_spacing_doc,
  "get_subdivision_spacing() \n\
   \n\
   Get the path's subdivsion spacing value. \n\
   \n\
   Returns (float): The path's subdivsion spacling value. \n\
");

static PyObject *
Path_get_subdivision_spacing(PyPath* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto path = GetPathElement(api, self);
  if (path == NULL) {
      return nullptr;
  }

  double spacing = path->GetSpacing();
  return Py_BuildValue("d", spacing);
}

//---------------------------
// Path_remove_control_point
//---------------------------
//
PyDoc_STRVAR(Path_remove_control_point_doc,
  "remove_control_point(index) \n\
   \n\
   Remove a control point from a path. \n\
   \n\
   Args: \n\
     index (int): The index into the path's list of control points of the control point to remove. 0 <= index <= number of control points - 1. \n\
");

static PyObject *
Path_remove_control_point(PyPath* self, PyObject* args)
{
  auto api = PyUtilApiFunction("i", PyRunTimeErr, __func__);
  int index;

  if (!PyArg_ParseTuple(args, api.format, &index)) {
      return PyUtilResetException(PyRunTimeErr);
  }

  auto path = GetPathElement(api, self);
  if (path == NULL) {
      return nullptr;
  }

  int numPts = path->GetControlPoints().size();
  if (numPts == 0) {
    api.error("The path does not have control points defined for it.");
    return nullptr;
  }

  if ((index >= numPts) || (index < 0)) {
      auto msg = "The 'index' argument " + std::to_string(index) + " must be >= 0 and < the number of control points (" +
            std::to_string(numPts-1) + ").";
      api.error(msg);
      return nullptr;
  }
  path->RemoveControlPoint(index);

  return SV_PYTHON_OK;
}

//----------------------------
// Path_replace_control_point
//----------------------------
//
PyDoc_STRVAR(Path_replace_control_point_doc,
  "replace_control_point(index, point) \n\
   \n\
   Replace a control point in a path. \n\
   \n\
   Args: \n\
     index (int): The index into the path's list of control points of the control point to remove. 0 <= index <= number of control points - 1. \n\
     point (list[float,float,float]): A list of three floats represent the coordinates of a 3D point. \n\
");

static PyObject *
Path_replace_control_point(PyPath* self, PyObject* args)
{
  auto api = PyUtilApiFunction("iO!", PyRunTimeErr, __func__);
  PyObject* pointArg;
  int index;

  if (!PyArg_ParseTuple(args, api.format, &index, &PyList_Type, &pointArg)) {
      return api.argsError();
  }

  auto path = GetPathElement(api, self);
  if (path == NULL) {
      return nullptr;
  }

  int numPts = path->GetControlPoints().size();
  if (numPts == 0) {
    api.error("The path does not have control points defined for it.");
    return nullptr;
  }

  std::string msg;
  if (!PyUtilCheckPointData(pointArg, msg)) {
      api.error("The 'point' argument " + msg);
      return nullptr;
  }

  if ((index >= numPts) || (index < 0)) {
      auto msg = "The 'index' argument " + std::to_string(index) + " must be >= 0 and < the number of control points (" +
            std::to_string(numPts-1) + ").";
      api.error(msg);
      return nullptr;
  }

  std::array<double,3> point;
  point[0] = PyFloat_AsDouble(PyList_GetItem(pointArg,0));
  point[1] = PyFloat_AsDouble(PyList_GetItem(pointArg,1));
  point[2] = PyFloat_AsDouble(PyList_GetItem(pointArg,2));
  path->SetControlPoint(index, point);

  return SV_PYTHON_OK;
}

//-------------------------
// Path_set_control_points
//-------------------------
//
PyDoc_STRVAR(Path_set_control_points_doc,
  "set_control_points(points) \n\
   \n\
   Set the path's control points. \n\
   \n\
   Args: \n\
     points (list(list[float,float,float])): The list of control points. \n\
");

static PyObject *
Path_set_control_points(PyPath* self, PyObject* args)
{
  auto api = PyUtilApiFunction("O!", PyRunTimeErr, __func__);
  PyObject* pointsArg;

  if (!PyArg_ParseTuple(args, api.format, &PyList_Type, &pointsArg)) {
      return api.argsError();
  }
  auto path = GetPathElement(api, self);
  if (path == NULL) {
      return nullptr;
  }

  std::vector<std::array<double,3> > points;
  int numPts = PyList_Size(pointsArg);
  for (int i = 0; i < numPts; i++) {
      PyObject* ptObj = PyList_GetItem(pointsArg,i);
      std::string msg;
      //double point[3];
      std::array<double,3> point;
      if (!PyUtilGetPointData(ptObj, msg, point.data())) {
          api.error("The 'points' argument at index " + std::to_string(i) + " " + msg);
          return nullptr;
      }
      points.push_back(point);
  }

  bool update = false;
  path->SetControlPoints(points, update);

  // Generate curve points.
  path->ControlPointsChanged();

  return Py_None;
}

//---------------------------
// Path_set_num_subdivisions
//---------------------------
//
PyDoc_STRVAR(Path_set_num_subdivisions_doc,
  "set_num_subdivisions(number) \n\
   \n\
   Set the number of subdivisions used for the TOTAL and SUBDIVISION subdivision methods. \n\
   \n\
   Args: \n\
     number (int): The number of subdivision. \n\
");

static PyObject *
Path_set_num_subdivisions(PyPath* self, PyObject* args)
{
  auto api = PyUtilApiFunction("i", PyRunTimeErr, __func__);
  int number;

  if (!PyArg_ParseTuple(args, api.format, &number)) {
     return api.argsError();
  }

  auto path = GetPathElement(api, self);
  if (path == NULL) {
      return nullptr;
  }

  if (number < 1) {
      api.error("The 'number' argument must be > 0.");
      return nullptr;
  }

  path->SetCalculationNumber(number);
  return Py_None;
}

//-----------------------------
// Path_set_subdivision_method
//-----------------------------
//
PyDoc_STRVAR(Path_set_subdivision_method_doc,
  "set_subdivision_method(method, num_subdiv=None, num_total=None, spacing=None) \n\
   \n\
   Set the subdivision method and paramters used to determine the number of path curve points created between two adjacent control points. \n\
   \n\
   The optional parameters are only used with the appropriate method being set. The path's curve points are updated using the given \n\
   method and paramater value. \n\
   \n\
   Args: \n\
     method (str): The subdivision method name. Valid names are: SPACING, SUBDIVISION or TOTAL \n\
     num_div (Optional[int]): The number of subdivisions used with the SUBDIVISION method. \n\
     num_total (Optional[int]): The number of total subdivisions used with the TOTAL method. \n\
     spacing (Optional[float]): The spacing by the SPACING method. \n\
");

static PyObject *
Path_set_subdivision_method(PyPath* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("s|O!O!O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"method", "num_div", "num_total", "spacing", NULL};
  char* methodName;
  PyObject* numDivObj = nullptr;
  PyObject* numTotalObj = nullptr;
  PyObject* spacingObj = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &methodName, &PyInt_Type, &numDivObj, &PyInt_Type, &numTotalObj,
        &PyFloat_Type, &spacingObj)) {
      return api.argsError();
  }

  auto path = GetPathElement(api, self);
  if (path == NULL) {
      return nullptr;
  }

  // Set the subdision method.
  //
  PathElement::CalculationMethod method;

  try {
      method = subdivMethodNameTypeMap.at(std::string(methodName));
  } catch (const std::out_of_range& except) {
      auto msg = "Unknown method name '" + std::string(methodName) + "'." + " Valid names are: " + subdivMethodValidNames + ".";
      api.error(msg);
      return nullptr;
  }

  path->SetMethod(method);

  // Set the parameters for the SPACING method.
  //
  if (method == sv3::PathElement::CONSTANT_SPACING) {
      if (spacingObj == nullptr) {
          api.error("The SPACING method requires a 'spacing' value.");
          return nullptr;
      }

      double spacing = PyFloat_AsDouble(spacingObj);
      if (PyErr_Occurred()) {
          return nullptr;
      }

      if (spacing <= 0.0) {
          api.error("The 'spacing' argument must be >= 0.0.");
          return nullptr;
      }

      path->SetCalculationNumber(0);
      path->SetSpacing(spacing);

  // Set the parameters for the SUBDIVISION method.
  //
  } else if (method == sv3::PathElement::CONSTANT_SUBDIVISION_NUMBER) {
      if (numDivObj == nullptr) {
          api.error("The SUBDIVISION method requires a 'num_div' value.");
          return nullptr;
      }

      int numDiv = PyInt_AsLong(numDivObj);
      if (PyErr_Occurred()) {
          return nullptr;
      }

      if (numDiv < 2) {
          api.error("The 'num_div' argument must be > 1.");
          return nullptr;
      }

      path->SetCalculationNumber(numDiv);
      path->SetSpacing(0.0);

  // Set the parameters for the TOTAL method.
  //
  } else if (method == sv3::PathElement::CONSTANT_TOTAL_NUMBER) {
      if (numTotalObj == nullptr) {
          api.error("The TOTAL method requires a 'num_total' value.");
          return nullptr;
      }

      int numTotal = PyInt_AsLong(numTotalObj);
      if (PyErr_Occurred()) {
          return nullptr;
      }

      if (numTotal < 2) {
          api.error("The 'num_total' argument must be > 1.");
          return nullptr;
      }

      path->SetCalculationNumber(numTotal);
      path->SetSpacing(0.0);

  // This is an internal error, all methods should be supported.
  //
  } else {
      auto msg = "INTERNAL ERROR: Unsupported method name '" + std::string(methodName) + "'." + " Valid names are: " + subdivMethodValidNames + ".";
      api.error(msg);
      return nullptr;
  }

  // Generate path curve points.
  path->ControlPointsChanged();

  return Py_None;
}

//-------------
// Path_smooth
//-------------
//
// [TODO:DaveP] This does not constrain the path end points so
// what use is it really?
//
PyDoc_STRVAR(Path_smooth_doc,
  "smooth(sample_rate, num_modes, smooth_control_pts=False) \n\
   \n\
   Smooth a path's control points or curve points as a source. \n\
   \n\
   Args: \n\
     sample_rate (int): The rate used to sample the source points for smoothing. \n\
        A sample rate of 2 uses every other point for smoothing. \n\
     num_modes (int): The number of Fourier modes used to reconstuct the source points. \n\
        The lower the number of modes the more the source points are smoothed. \n\
     control_point_based (Optional[bool]): If True then smooth control points; otherwise smooth curve points. \n\
   \n\
   Returns (sv.path.Path object): A new smoothed path. \n\
");

static PyObject *
Path_smooth(PyPath* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("ii|O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"sample_rate", "num_modes", "smooth_control_pts", NULL};
  int sampleRate, numModes;
  PyObject *smoothControlPtsArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &sampleRate, &numModes, &PyBool_Type, &smoothControlPtsArg)) {
      return api.argsError();
  }

  auto path = GetPathElement(api, self);
  if (path == NULL) {
      return nullptr;
  }

  int numPts = path->GetControlPoints().size();
  if (numPts == 0) {
    api.error("The path does not have control points defined for it.");
    return nullptr;
  }

  // Set controlPointsBased parameter.
  bool controlPointsBased = false;
  if ((smoothControlPtsArg != nullptr) && PyObject_IsTrue(smoothControlPtsArg)) {
      controlPointsBased = true;
  }

  if (sampleRate < 1) {
      api.error("The 'sample_rate' argument must be > 0.");
      return nullptr;
  }

  if (numModes < 1) {
      api.error("The 'num_modes' argument must be > 0.");
      return nullptr;
  }

  auto newPath = path->CreateSmoothedPathElement(sampleRate, numModes, controlPointsBased);

  if (newPath == nullptr) {
      api.error("Unable to smooth the path.");
      return nullptr;
  }

  return CreatePyPath(newPath);
}

////////////////////////////////////////////////////////
//           C l a s s   D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* PATH_CLASS = "Path";
// Dotted name that includes both the module name and
// the name of the type within the module.
static char* PATHPLANNING_MODULE_CLASS = "pathplanning.Path";

//---------------
// PathClass_doc
//---------------
// Define the Path class documentation.
//
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(PathClass_doc,
   "The Path class provides methods for querying, creating, and modifying SV  \n\
    path planning objects.                                                    \n\
   \n\
");

//---------------
// PyPathMethods
//---------------
// Path class methods.
//
static PyMethodDef PyPathMethods[] = {

  {"add_control_point", (PyCFunction)Path_add_control_point, METH_VARARGS, Path_add_control_point_doc },
  {"get_control_points", (PyCFunction)Path_get_control_points, METH_NOARGS, Path_get_control_points_doc },

  {"get_curve_frame", (PyCFunction)Path_get_curve_frame, METH_VARARGS, Path_get_curve_frame_doc },
  {"get_curve_normal", (PyCFunction)Path_get_curve_normal, METH_VARARGS, Path_get_curve_normal_doc },
  {"get_curve_point", (PyCFunction)Path_get_curve_point, METH_VARARGS, Path_get_curve_point_doc },
  {"get_curve_points", (PyCFunction)Path_get_curve_points, METH_NOARGS, Path_get_curve_points_doc },
  {"get_curve_polydata", (PyCFunction)Path_get_curve_polydata, METH_VARARGS, Path_get_curve_polydata_doc},
  {"get_curve_tangent", (PyCFunction)Path_get_curve_tangent, METH_VARARGS, Path_get_curve_tangent_doc },

  {"get_num_curve_points", (PyCFunction)Path_get_num_curve_points, METH_NOARGS, Path_get_num_curve_points_doc },
  {"get_num_subdivisions", (PyCFunction)Path_get_num_subdivisions, METH_NOARGS, Path_get_num_subdivisions_doc},

  {"get_subdivision_spacing", (PyCFunction)Path_get_subdivision_spacing, METH_NOARGS, Path_get_subdivision_spacing_doc},

  {"get_subdivision_method", (PyCFunction)Path_get_subdivision_method, METH_NOARGS, Path_get_subdivision_method_doc},

  {"remove_control_point", (PyCFunction)Path_remove_control_point, METH_VARARGS, Path_remove_control_point_doc },

  {"replace_control_point", (PyCFunction)Path_replace_control_point, METH_VARARGS, Path_replace_control_point_doc },

  {"set_control_points", (PyCFunction)Path_set_control_points, METH_VARARGS, Path_set_control_points_doc },
  {"set_subdivision_method", (PyCFunction)Path_set_subdivision_method, METH_VARARGS|METH_KEYWORDS, Path_set_subdivision_method_doc},

  {"smooth", (PyCFunction)Path_smooth, METH_VARARGS|METH_KEYWORDS, Path_smooth_doc },

  {NULL,NULL}
};

//------------
// PyPathType
//------------
// Define the Python type object that stores Path data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
PyTypeObject PyPathType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and
  // the name of the type within the module.
  PATHPLANNING_MODULE_CLASS,
  sizeof(PyPath)
};

//------------
// PyPathInit
//------------
// This is the __init__() method for the Path class.
//
// This function is used to initialize an object after it is created.
//
static int
PyPathInit(PyPath* self, PyObject* args, PyObject *kwds)
{
  static int numObjs = 1;
  //std::cout << "[PyPathInit] New Path object: " << numObjs << std::endl;
  self->path = new PathElement();
  self->id = numObjs;
  numObjs += 1;
  return 0;
}

//-----------
// PyPathNew
//-----------
// Object creation function, equivalent to the Python __new__() method.
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PyPathNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PyPathNew] PyPathNew " << std::endl;
  auto self = (PyPath*)type->tp_alloc(type, 0);
  if (self != NULL) {
      self->id = 1;
  }

  return (PyObject *) self;
}

//---------------
// PyPathDealloc
//---------------
//
static void
PyPathDealloc(PyPath* self)
{
  //std::cout << "[PyPathDealloc] Free PyPath" << std::endl;
  delete self->path;
  Py_TYPE(self)->tp_free(self);
}

//-------------------
// SetPathTypeFields
//-------------------
// Set the Python type object fields that stores Path data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetPyPathTypeFields(PyTypeObject& pathType)
{
  // Doc string for this type.
  pathType.tp_doc = PathClass_doc;
  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  pathType.tp_new = PyPathNew;
  pathType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  pathType.tp_init = (initproc)PyPathInit;
  pathType.tp_dealloc = (destructor)PyPathDealloc;
  pathType.tp_methods = PyPathMethods;
}

//--------------
// CreatePyPath
//--------------
// Create a PyPath object.
//
// If the path argument is not null then use it
// for the PyPath object.
//
PyObject *
CreatePyPath(PathElement* path)
{
  //std::cout << "[CreatePyPath] Create Path object ... " << std::endl;
  auto pathObj = PyObject_CallObject((PyObject*)&PyPathType, NULL);
  auto pyPath = (PyPath*)pathObj;

  if (path != nullptr) {
      delete pyPath->path;
      pyPath->path = path;
  }
  //std::cout << "[CreatePyPath] pyPath id: " << pyPath->id << std::endl;
  //std::cout << "[CreatePyPath] pathObj ref count: " << Py_REFCNT(pathObj) << std::endl;
  return pathObj;
}

