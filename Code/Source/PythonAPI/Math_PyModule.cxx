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

// The functions defined here implement the SV Python API math module.
//
// The module name is 'math'.
//
// A Python exception sv.math.MathError is defined for this module.
// The exception can be used in a Python 'try' statement with an 'except' clause
// like this
//
//    except sv.math.MathError:
//
// ******************** NOTE ************************
// The sv.math module is not currently exposed.
//
#include "Python.h"
#include "SimVascular.h"

#include <stdio.h>
#include <iostream>

#include "sv_arg.h"
#include "sv_misc_utils.h"
#include "sv_Math.h"
#include "PyUtils.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif


// Exception type used by PyErr_SetString() to set the for the error indicator.
static PyObject *PyRunTimeErr;

//////////////////////////////////////////////////////
//        U t i l i t y     F u n c t i o n s       //
//////////////////////////////////////////////////////

//-------------------
// GetPointsFromList
//-------------------
// Create an array of points from a Python list.
//
// [TODO:DaveP] should really use exceptions here.
//
static double **
GetPointsFromList(PyUtilApiFunction& api, PyObject* listArg, int dim, const std::string& argName)
{
  // First check that the listArg is a list.
  if (!PyList_Check(listArg)){
      api.error("The " + argName + " argument is not a list.");
      return nullptr;
  }

  int listSize = PyList_Size(listArg);
  if (listSize == 0) {
      api.error("The " + argName + " argument is empty.");
      return nullptr;
  }

  // Convert the Python list to a double array.
  //
  double **points = cvMath().createArray(listSize, dim);
  for (int i = 0; i < listSize; i++) {
      PyObject *temp = PyList_GetItem(listArg,i);
      if (temp == nullptr) {
          api.error("The " + std::to_string(i) + "th element of the " + argName + "argument is not defined.");
          return nullptr;
      }
      if (PyList_Size(temp) != dim) {
          api.error("The " + std::to_string(i) + "th element of the " + argName + " argument list != "+std::to_string(dim)+".");
          return nullptr;
      }
      for (int j = 0; j < dim; j++) {
          points[i][j] = PyFloat_AsDouble(PyList_GetItem(temp,j));
      }
  }

  return points;
}

//////////////////////////////////////////////////////
//          M o d u l e  F u n c t i o n s          //
//////////////////////////////////////////////////////
//
// Python API functions.

//------------
// Math_fft
//------------
//
PyDoc_STRVAR(Math_fft_doc,
  "fft(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Math_fft(PyObject *self, PyObject *args)
{
  auto api = PyUtilApiFunction("Oii", PyRunTimeErr, __func__);
  PyObject *pointsArg;
  int nterms = 0;
  int numInterpPoints = 0;

  if (!PyArg_ParseTuple(args,api.format, &pointsArg, &nterms, &numInterpPoints)) {
      return api.argsError();
  }

  // [TODO:DaveP] check that other arguments are valid.

  // Get an array of points from the pointsArgs list.
  int dim = 2;
  auto pts = GetPointsFromList(api, pointsArg, dim, "points");
  if (pts == nullptr) {
      return nullptr;
  }
  int nlistpts = PyList_Size(pointsArg);

  // Perform the fft operation.
  auto mathObj = cvMath();
  double **terms = NULL;
  if (mathObj.FFT(pts, nlistpts, numInterpPoints, nterms, &terms) == SV_ERROR) {
       mathObj.deleteArray(pts,nlistpts,dim);
       api.error("Error calculating the fft.");
       return nullptr;
  }

  // Create result list.
  PyObject *pylist = PyList_New(nterms);
  for (int i = 0; i < nterms; i++) {
      PyObject* rr = PyList_New(dim);
      PyList_SetItem(rr,0,PyFloat_FromDouble(terms[i][0]));
      PyList_SetItem(rr,1,PyFloat_FromDouble(terms[i][1]));
      PyList_SET_ITEM(pylist, i, rr);
  }

  mathObj.deleteArray(pts,nlistpts,dim);
  mathObj.deleteArray(terms,nterms,dim);
  return pylist;
}

//--------------------
// Math_inverse_fft
//--------------------
//
PyDoc_STRVAR(Math_inverse_fft_doc,
  "inverse_fft(kernel)  \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Math_inverse_fft(PyObject *self, PyObject *args)
{
  auto api = PyUtilApiFunction("Odddi", PyRunTimeErr, __func__);
  PyObject *termsArg;
  double t0 = 0;
  double dt = 0;
  double omega = 0;
  int numPts = 0;

  if (!PyArg_ParseTuple(args, api.format, &termsArg, &t0, &dt, &omega, &numPts)) { return api.argsError();
      return api.argsError();
  }

  // Get an array of points from the pointsArgs list.
  int dim = 2;
  auto terms = GetPointsFromList(api, termsArg, dim, "terms");
  if (terms == nullptr) {
      return nullptr;
  }
  int nlistterms = PyList_Size(termsArg);

  // Perform inverse fft operation.
  //
  auto mathObj = cvMath();
  double **pts = NULL;
  if (mathObj.inverseFFT(terms, nlistterms, t0, dt, omega, numPts, &pts) == SV_ERROR) {
       mathObj.deleteArray(terms,nlistterms,dim);
       api.error("Error calculating the inverse fft.");
       return nullptr;
  }

  // Create result string
  //
  // [TODO:DaveP] This converts results to a string, not
  //    a list list the fft function above.
  //
  char r[2048];
  PyObject *pylist = PyList_New(numPts);
  for (int i = 0; i < numPts; i++) {
      r[0] = '\0';
      sprintf(r,"%.6le %.6le",pts[i][0],pts[i][1]);
      PyObject *rr = PyBytes_FromString(r);
      PyList_SET_ITEM(pylist, i, rr);
  }

  // [TODO:DaveP] this is hideous!
  mathObj.deleteArray(terms,nlistterms,dim);
  mathObj.deleteArray(pts,numPts,dim);

  return pylist;
}

//--------------------------
// Math_compute_womersley
//--------------------------
//
// [TODO:DaveP] need keyword args here.
//
PyDoc_STRVAR(Math_compute_womersley_doc,
  "compute_womersley(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Math_compute_womersley(PyObject *self, PyObject *args)
{
  auto api = PyUtilApiFunction("Odddddd", PyRunTimeErr, __func__);
  PyObject *termsArg;
  double time = 0;
  double viscosity = 0;
  double omega = 0;
  double density = 0;
  double radmax = 0;
  double radius = 0;

  if (!PyArg_ParseTuple(args, api.format, &termsArg, &time, &viscosity, &omega, &density, &radmax, &radius)) {
      return api.argsError();
  }

  // TODO:DaveP] check other args.

  // Get an array of points from the pointsArgs list.
  int dim = 2;
  auto terms = GetPointsFromList(api, termsArg, dim, "terms");
  if (terms == nullptr) {
      return nullptr;
  }
  int nlistterms = PyList_Size(termsArg);

  // Perform the womersley operation.
  //
  auto mathObj = cvMath();
  double velocity = 0;
  if (mathObj.compute_v_womersley(terms, nlistterms, viscosity, density, omega, radmax, radius, time, &velocity) == SV_ERROR) {
       mathObj.deleteArray(terms,nlistterms,dim);
       api.error("Error calculating the womersley velocity.");
       return nullptr;
  }

  mathObj.deleteArray(terms,nlistterms,dim);

  return Py_BuildValue("d",velocity);
}

//---------------------------
// Math_linear_interpolate
//---------------------------
//
PyDoc_STRVAR(Math_linear_interpolate_doc,
  "linear_interpolate(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Math_linear_interpolate(PyObject *self, PyObject *args)
{
  auto api = PyUtilApiFunction("Oi", PyRunTimeErr, __func__);
  PyObject *pointsArg;
  int numInterpPoints = 0;

  if (!PyArg_ParseTuple(args, api.format, &pointsArg, &numInterpPoints)) {
      return api.argsError();
  }

  // Get an array of points from the pointsArgs list.
  int dim = 2;
  auto pts = GetPointsFromList(api, pointsArg, dim, "points");
  if (pts == nullptr) {
      return nullptr;
  }
  int nlistpts = PyList_Size(pointsArg);

  // Calculate dt so that our time series will go from 0 to T.
  double t0 = pts[0][0];
  double dt = (pts[nlistpts-1][0]-t0)/(numInterpPoints-1);
  double **outPts = NULL;

  // Perform the linear interpolation.
  auto mathObj = cvMath();
  if (mathObj.linearInterpolate(pts, nlistpts, t0, dt, numInterpPoints, &outPts) == SV_ERROR) {
       mathObj.deleteArray(pts,nlistpts,dim);
       api.error("Error linear interplating points.");
       return nullptr;
  }

  // create result string
  char r[2048];
  PyObject *pylist = PyList_New(numInterpPoints);
  for (int i = 0; i < numInterpPoints; i++) {
    r[0] = '\0';
    sprintf(r,"%.6le %.6le",outPts[i][0],outPts[i][1]);
    PyObject *rr = PyBytes_FromString(r);
    PyList_SET_ITEM(pylist, i, rr);
  }

  mathObj.deleteArray(pts,nlistpts,dim);
  mathObj.deleteArray(outPts,numInterpPoints,dim);

  return pylist;
}

//---------------------
// Math_curve_length
//---------------------
//
PyDoc_STRVAR(Math_curve_length_doc,
  "curve_length(points, closed=False) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Math_curve_length(PyObject *self, PyObject *args, PyObject* kwargs)
{
  std::cout << "========== Math_curve_length ==========" << std::endl;
  auto api = PyUtilApiFunction("O!|O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"points", "closed", NULL};
  PyObject *pointsArg;
  PyObject *closedArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &pointsArg, &PyBool_Type, &closedArg)) {
      return api.argsError();
  }

  // Set closed parameter.
  int closed = 0;
  if ((closedArg != nullptr) && PyObject_IsTrue(closedArg)) {
      closed = 1;
  }

  // Get an array of points from the pointsArgs list.
  int dim = 3;
  auto pts = GetPointsFromList(api, pointsArg, dim, "points");
  if (pts == nullptr) {
      return nullptr;
  }
  int nlistpts = PyList_Size(pointsArg);

  // Calculate the curve length.
  //
  auto mathObj = cvMath();
  double length = 0;
  if (mathObj.curveLength(pts, nlistpts, closed, &length) == SV_ERROR) {
       mathObj.deleteArray(pts,nlistpts,dim);
       api.error("Error calculating the curve length.");
       return nullptr;
  }

  mathObj.deleteArray(pts,nlistpts,dim);

  return Py_BuildValue("d",length);
}

//---------------------------------
// Math_linear_interpolate_curve
//---------------------------------
//
PyDoc_STRVAR(Math_linear_interpolate_curve_doc,
  "inear_interpolate_curve(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Math_linear_interpolate_curve(PyObject *self, PyObject *args)
{
  auto api = PyUtilApiFunction("Oii", PyRunTimeErr, __func__);
  PyObject *pointsArg;
  int numInterpPoints = 0;
  int closed = 0;

  if (!PyArg_ParseTuple(args, api.format, &pointsArg, &closed, &numInterpPoints)) {
      return api.argsError();
  }

  // Get an array of points from the pointsArgs list.
  int dim = 3;
  auto pts = GetPointsFromList(api, pointsArg, dim, "points");
  if (pts == nullptr) {
      return nullptr;
  }
  int nlistpts = PyList_Size(pointsArg);

  // Interpolate the curve.
  //
  auto mathObj = cvMath();
  double **outPts = NULL;
  if (mathObj.linearInterpolateCurve(pts, nlistpts, closed, numInterpPoints, &outPts) == SV_ERROR) {
      mathObj.deleteArray(pts,nlistpts,dim);
      api.error("Error interpolating the curve length.");
      return nullptr;
  }

  // create result string
  char r[2048];
  PyObject *pylist=PyList_New(numInterpPoints);
  for (int i = 0; i < numInterpPoints; i++) {
     r[0] = '\0';
     sprintf(r,"%.6le %.6le %.6le",outPts[i][0],outPts[i][1],outPts[i][2]);
     PyObject *rr=PyBytes_FromString(r);
     PyList_SET_ITEM(pylist, i, rr);
  }

  mathObj.deleteArray(pts,nlistpts,dim);
  mathObj.deleteArray(outPts,numInterpPoints,dim);

  return pylist;
}

//---------------------------
// Math_fit_least_squares
//---------------------------
//
PyDoc_STRVAR(Math_fit_least_squares_doc,
  "fit_least_squares(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Math_fit_least_squares(PyObject *self, PyObject *args)
{
  auto api = PyUtilApiFunction("OOii", PyRunTimeErr, __func__);
  int xOrder = 0;
  int yOrder = 0;

  PyObject *xtermsArg;
  PyObject *ytermsArg;

  if (!PyArg_ParseTuple(args, api.format,  &xtermsArg, &ytermsArg, &xOrder,&yOrder)) {
      return api.argsError();
  }

  int numberOfSamples = PyList_Size(xtermsArg);
  int numberOfSamplesY = PyList_Size(ytermsArg);
  if (numberOfSamples!= numberOfSamplesY) {
      api.error("The number of x terms ("+std::to_string(numberOfSamples)+") != the number of y terms ("+std::to_string(numberOfSamplesY)+".");
      return nullptr;
  }

  // Get an array of points from the xtermsArg list.
  int dim = 3;
  auto xt = GetPointsFromList(api, xtermsArg, dim, "x terms");
  if (xt == nullptr) {
      return nullptr;
  }

  // Get an array of points from the xtermsArg list.
  auto yt = GetPointsFromList(api, ytermsArg, dim, "y terms");
  if (yt == nullptr) {
      return nullptr;
  }

  // Perform the least squares fit.
  //
  auto mathObj = cvMath();
  double **mt = mathObj.createArray(xOrder,yOrder);
  if (mathObj.fitLeastSquares(numberOfSamples,xt,xOrder,yt,yOrder,mt) == SV_ERROR) {
     PyErr_SetString( PyRunTimeErr, "error in least squares fit" );
      mathObj.deleteArray(xt,numberOfSamples,xOrder);
      mathObj.deleteArray(yt,numberOfSamples,yOrder);
      api.error("Error performing the least squares fit.");
      return nullptr;
  }

  // create result string
  char r[2048];
  PyObject *pylist=PyList_New(xOrder);
  for (int i = 0; i < xOrder; i++) {
      r[0] = '\0';
      for (int j = 0; j < yOrder; j++) {
          sprintf(r,"%.6le ",mt[i][j]);
          PyObject *rr=PyBytes_FromString(r);
          PyList_SET_ITEM(pylist, i, rr);
     }
  }

  mathObj.deleteArray(xt,numberOfSamples,xOrder);
  mathObj.deleteArray(yt,numberOfSamples,yOrder);
  mathObj.deleteArray(mt,xOrder,yOrder);

  return pylist;
}


//---------------------
// Math_smooth_curve
//---------------------
//
PyDoc_STRVAR(Math_smooth_curve_doc,
  "smooth_curve(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Math_smooth_curve(PyObject *self, PyObject *args)
{
  auto api = PyUtilApiFunction("Oiii", PyRunTimeErr, __func__);
  int numInterpPoints = 0;
  int closed = 0;
  int numModes = 0;

  PyObject *pointsArg;
  if (!PyArg_ParseTuple(args, api.format,  &pointsArg, &closed, &numModes, &numInterpPoints)) {
      return api.argsError();
  }

  // Get an array of points from the pointsArgs list.
  int dim = 3;
  auto pts = GetPointsFromList(api, pointsArg, dim, "points");
  if (pts == nullptr) {
      return nullptr;
  }
  int nlistpts = PyList_Size(pointsArg);

  // Smooth the curve.
  //
  auto mathObj = cvMath();
  double **outPts = NULL;
  if (mathObj.smoothCurve(pts, nlistpts, closed, numModes, numInterpPoints, &outPts) == SV_ERROR) {
      mathObj.deleteArray(pts,nlistpts,dim);
      api.error("Error soothing the curve.");
      return nullptr;
  }

  // create result string
  char r[2048];
  PyObject *pylist=PyList_New(numInterpPoints);
  for (int i = 0; i < numInterpPoints; i++) {
      r[0] = '\0';
      sprintf(r,"%.6le %.6le %.6le",outPts[i][0],outPts[i][1],outPts[i][2]);
      PyObject *rr=PyBytes_FromString(r);
      PyList_SET_ITEM(pylist, i, rr);
  }

  mathObj.deleteArray(pts,nlistpts,dim);
  mathObj.deleteArray(outPts,numInterpPoints,dim);

  return pylist;
}


////////////////////////////////////////////////////////
//          M o d u l e  D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* MATH_MODULE = "math";
static char* MATH_MODULE_EXCEPTION = "math.Error";
static char* MATH_MODULE_EXCEPTION_OBJECT = "Error";

PyDoc_STRVAR(MathModule_doc, "math module functions");

//---------------
// PyMathMethods
//---------------
// math module methods.
//
static PyMethodDef PyMathMethods[] = {

   // [TODO:DaveP] Is this really useful?
   {"curve_length", (PyCFunction)Math_curve_length, METH_VARARGS|METH_KEYWORDS, Math_curve_length_doc},


/*
   {"compute_womersley", Math_compute_womersley, METH_VARARGS, Math_compute_womersley_doc},

   {"fft", Math_fft, METH_VARARGS, Math_fft_doc},

   {"fit_least_squares", Math_fit_least_squares, METH_VARARGS, Math_fit_least_squares_doc},

   {"inverse_fft", Math_inverse_fft, METH_VARARGS, Math_inverse_fft_doc},

   {"linear_interpolate", Math_linear_interpolate, METH_VARARGS, Math_linear_interpolate_doc},

   {"linear_interpolate_curve", Math_linear_interpolate_curve, METH_VARARGS, Math_linear_interpolate_curve_doc},

   {"smooth_curve", Math_smooth_curve, METH_VARARGS, Math_smooth_curve_doc},
*/

  {NULL,       NULL},
  };


//-----------------------
// Initialize the module
//-----------------------
// Define the initialization function called by the Python
// interpreter when the module is loaded.

//---------------------------------------------------------------------------
//                           PYTHON_MAJOR_VERSION 3
//---------------------------------------------------------------------------

#if PYTHON_MAJOR_VERSION == 3

// Size of per-interpreter state of the module.
// Set to -1 if the module keeps state in global variables.
static int perInterpreterStateSize = -1;

// Always initialize this to PyModuleDef_HEAD_INIT.
static PyModuleDef_Base m_base = PyModuleDef_HEAD_INIT;

// Define the module definition struct which holds all information
// needed to create a module object.
static struct PyModuleDef PyMathModule = {
   m_base,
   MATH_MODULE,
   MathModule_doc,
   perInterpreterStateSize,
   PyMathMethods
};

//---------------
// PyInit_PyMath
//---------------
// The initialization function called by the Python interpreter when the module is loaded.
//
PyMODINIT_FUNC
PyInit_PyMath(void)
{
  std::cout << "========== load math module ==========" << std::endl;
  auto module = PyModule_Create(&PyMathModule);
  if (module == NULL) {
    printf("Error creating Python math module!\n");
    return NULL;
  }

  // Add math.MathException exception.
  //
  PyRunTimeErr = PyErr_NewException(MATH_MODULE_EXCEPTION, NULL, NULL);
  Py_INCREF(PyRunTimeErr);
  PyModule_AddObject(module, MATH_MODULE_EXCEPTION_OBJECT, PyRunTimeErr);

  return module;
}

#endif

//---------------------------------------------------------------------------
//                           PYTHON_MAJOR_VERSION 2
//---------------------------------------------------------------------------

#if PYTHON_MAJOR_VERSION == 2

//-------------
// initpyMath
//-------------
PyMODINIT_FUNC
initpyMath(void)
{
PyObject *pyMth;
pyMth = Py_InitModule("pyMath",Math_methods);


PyRunTimeErr = PyErr_NewException("pyMath.error",NULL,NULL);
Py_INCREF(PyRunTimeErr );
PyModule_AddObject(pyMth, "error", PyRunTimeErr);

}
#endif

