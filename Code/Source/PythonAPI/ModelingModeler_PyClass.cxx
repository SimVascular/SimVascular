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

// Define the Python 'modeling.Modeler' class. This class defines modeling operations
// that create new Python 'modeling.Model' objects.
//
#ifndef PYAPI_MODELING_MODELER_H
#define PYAPI_MODELING_MODELER_H

#include <iostream>
#include <map>
#include <math.h>
#include <string>
#include <structmember.h>

extern bool ModelingCheckFileFormat(PyUtilApiFunction& api, SolidModel_KernelT kernel, std::string fileName);
extern bool ModelingCheckModelsKernels(PyUtilApiFunction& api, cvSolidModel* model1, cvSolidModel* model2);

//-------------------
// PyModelingModeler
//-------------------
// Define the data members for the modeling.Modeler class.
//
// Data members exposed as Python attributes:
//
//   kernelName
//
typedef struct {
  PyObject_HEAD
  PyObject* kernelName;      // Put data members at the front.
  int id;
  SolidModel_KernelT kernel;
} PyModelingModeler;

//////////////////////////////////////////////////////
//          U t i l i t y   F u n c t i o n s       //
//////////////////////////////////////////////////////

//-------------------
// GetModelFromPyObj
//-------------------
// Get the cvSolidModel object from a Python PyModelingModel object.
//
static cvSolidModel *
GetModelFromPyObj(PyObject* obj)
{
  // Check that the Python object is an SV Python Model object.
  if (!PyObject_TypeCheck(obj, &PyModelingModelType)) {
      return nullptr;
  }
  return ((PyModelingModel*)obj)->solidModel;
}

////////////////////////////////////////////////////////
//          C l a s s    M e t h o d s                //
////////////////////////////////////////////////////////
//
// Python 'Modeler' class methods.
//
//---------------------
// ModelingModeler_box
//---------------------
//
PyDoc_STRVAR(ModelingModeler_box_doc,
  "box(center, width, height, length)  \n\
   \n\
   Create a 3D solid box. \n\
   \n\
   Args:\n\
     center ([float,float,float]): The box center. \n\
     width (float): The box width in the X coordinate direction. \n\
     height (float): The box height in the Y coordinate direction. \n\
     length (float): The box length in the Z coordinate direction. \n\
   \n\
   Returns (Model): The box solid model. \n\
");

static PyObject *
ModelingModeler_box(PyModelingModeler* self, PyObject* args, PyObject* kwargs)
{
  #ifdef dbg_ModelingModeler_box
  std::cout << "[ModelingModeler_box] ========== ModelingModeler_box ==========" << std::endl;
  std::cout << "[ModelingModel_box] Self: " << self << std::endl;
  std::cout << "[ModelingModel_box] Kernel: " << self->kernel << std::endl;
  std::cout << "[ModelingModeler_box] self->id: " << self->id << std::endl;
  #endif
  auto api = PyUtilApiFunction("O!ddd", PyRunTimeErr, __func__);
  static char *keywords[] = {"center", "width", "height", "length", NULL};
  double width = 1.0;
  double height = 1.0;
  double length = 1.0;
  PyObject* centerArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &centerArg, &width, &height, &length)) {
      return api.argsError();
  }

  // Check that the center argument is valid.
  std::string emsg;
  if (!PyUtilCheckPointData(centerArg, emsg)) {
      api.error("The box center argument " + emsg);
      return nullptr;
  }

  // Get the center argument data.
  double center[3];
  for (int i = 0; i < PyList_Size(centerArg); i++) {
    center[i] = PyFloat_AsDouble(PyList_GetItem(centerArg,i));
  }

  if (width <= 0.0) {
      api.error("The box width argument is <= 0.0.");
      return nullptr;
  }

  if (height <= 0.0) {
      api.error("The box height argument is <= 0.0.");
      return nullptr;
  }

  if (length <= 0.0) {
      api.error("The box length argument is <= 0.0.");
      return nullptr;
  }

  #ifdef dbg_ModelingModeler_box
  std::cout << "[ModelingModel_box] Center: " << center[0] << "  " << center[1] << "  " << center[2] << std::endl;
  std::cout << "[ModelingModel_box] Width: " << width << std::endl;
  std::cout << "[ModelingModel_box] Height: " << height << std::endl;
  std::cout << "[ModelingModel_box] Length: " << length << std::endl;
  #endif

  // Create the new solid object.
  auto pyModelingModelObj = CreatePyModelingModelObject(self->kernel);
  auto model = ((PyModelingModel*)pyModelingModelObj)->solidModel;
  if (model == NULL) {
      api.error("Error creating a 3D box solid model.");
      return nullptr;
  }

  // Create the box solid.
  //
  double dims[3] = {width, height, length};
  if (model->MakeBox3d(dims, center) != SV_OK) {
      Py_DECREF(pyModelingModelObj);
      delete model;
      api.error("Error creating a 3D box solid model.");
      return nullptr;
  }

  return pyModelingModelObj;
}

//--------------------------
// ModelingModeler_cylinder
//--------------------------
//
PyDoc_STRVAR(ModelingModeler_cylinder_doc,
  "cylinder(center, axis, radius, length)  \n\
   \n\
   Create a 3D solid cylinder aligned with an axis. \n\
   \n\
   Args:\n\
     center ([float,float,float]): The cylinder center. \n\
     axis ([float,float,float]): The cylinder axis. \n\
     radius (float): The cylinder radius. \n\
     length (float): The cylinder length. \n\
   \n\
   Returns (Model): The cylinder solid model. \n\
");

static PyObject *
ModelingModeler_cylinder(PyModelingModeler* self, PyObject* args, PyObject* kwargs)
{
  #ifdef dbg_ModelingModeler_cylinder
  std::cout << "[ModelingModeler_cylinder] ========== ModelingModeler_cylinder ==========" << std::endl;
  std::cout << "[ModelingModel_cylinder] Kernel: " << self->kernel << std::endl;
  #endif
  auto api = PyUtilApiFunction("O!O!dd", PyRunTimeErr, __func__);
  static char *keywords[] = {"center", "axis", "radius", "length", NULL};
  double radius;
  double length;
  PyObject* centerArg;
  PyObject* axisArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &centerArg, &PyList_Type, &axisArg, &radius, &length)) {
      return api.argsError();
  }

  // Check argument values.
  //
  std::string emsg;
  if (!PyUtilCheckPointData(centerArg, emsg)) {
      api.error("The cylinder center argument " + emsg);
      return nullptr;
  }


  if (radius <= 0.0) {
      api.error("The radius argument is <= 0.0.");
      return nullptr;
  }

  if (length <= 0.0) {
      api.error("The length argument is <= 0.0.");
      return nullptr;
  }

  if ((axisArg != nullptr) && !PyUtilCheckPointData(axisArg, emsg)) {
      api.error("The cylinder axis argument " + emsg);
      return nullptr;
  }

  // Get argument data.
  //
  double center[3];
  double axis[3];
  for (int i = 0; i < 3; i++) {
      axis[i] = PyFloat_AsDouble(PyList_GetItem(axisArg,i));
      center[i] = PyFloat_AsDouble(PyList_GetItem(centerArg,i));
  }

  // Create the new solid.
  auto pyModelingModelObj = CreatePyModelingModelObject(self->kernel);
  if (pyModelingModelObj == nullptr) {
      api.error("Error creating a Python solid model object.");
      return nullptr;
  }
  auto model = ((PyModelingModel*)pyModelingModelObj)->solidModel;
  if (model == NULL) {
      api.error("Error creating a cylinder solid model.");
      return nullptr;
  }

  if (model->MakeCylinder(radius, length, center, axis) != SV_OK) {
      Py_DECREF(pyModelingModelObj);
      delete model;
      api.error("Error creating a cylinder solid model.");
      return nullptr;
  }

  return pyModelingModelObj;
}

//---------------------------
// ModelingModeler_ellipsoid
//---------------------------
//
// [TODO:DaveP] The cvModelingModel MakeEllipsoid method is not implemented.
//
PyDoc_STRVAR(ModelingModeler_ellipsoid_doc,
  "ellipsoid(center, radii)  \n\
   \n\
   Create a 3D solid ellipsoid. \n\
   \n\
   Args:\n\
     center ([float,float,float]): The ellipsoid center. \n\
     radii ([float,float,float]): The ellipsoid axes radii. \n\
");

static PyObject *
ModelingModeler_ellipsoid(PyModelingModeler* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("O!O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"center", "radii", NULL};
  PyObject* centerArg;
  PyObject* radiiArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &centerArg, PyList_Type, &radiiArg)) {
      return api.argsError();
  }

  std::string emsg;
  if (!PyUtilCheckPointData(centerArg, emsg)) {
      api.error("The ellipsoid center argument " + emsg);
      return nullptr;
  }

  if (!PyUtilCheckPointData(radiiArg, emsg)) {
      api.error("The ellipsoid 'radii' argument " + emsg);
      return nullptr;
  }

  double center[3];
  double r[3];

  for (int i = 0; i < 3; i++) {
      r[i] = PyFloat_AsDouble(PyList_GetItem(radiiArg, i));
      center[i] = PyFloat_AsDouble(PyList_GetItem(centerArg, i));
  }

  // Create the new solid.
  auto pyModelingModelObj = CreatePyModelingModelObject(self->kernel);
  if (pyModelingModelObj == nullptr) {
      api.error("Error creating a Python solid model object.");
      return nullptr;
  }
  auto model = ((PyModelingModel*)pyModelingModelObj)->solidModel;
  if (model == NULL) {
      api.error("Error creating a solid model object.");
      return nullptr;
  }

  if (model->MakeEllipsoid(r, center) != SV_OK) {
      delete model;
      api.error("Error creating an ellipsoid solid model.");
      return nullptr;
  }

  return pyModelingModelObj;
}

//---------------------------
// ModelingModeler_intersect
//---------------------------
//
// [TODO:DaveP] The SolidModel_SimplifyT argument is not used by SV. Why is it given?

PyDoc_STRVAR(ModelingModeler_intersect_doc,
  "intersect(model1, model2)  \n\
   \n\
   Create a solid from the Booealn intersect operation on two solids. \n\
   \n\
   Args:\n\
     model1 (Model): A solid model created by a modeler. \n\
     model2 (Model): A solid model created by a modeler. \n\
   \n\
   Returns (Model): The solid model of the intersected models. \n\
");

static PyObject *
ModelingModeler_intersect(PyModelingModeler* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("OO", PyRunTimeErr, __func__);
  static char *keywords[] = {"model1", "model2", NULL};
  PyObject* model1Arg;
  PyObject* model2Arg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &model1Arg, &model2Arg)) {
      return api.argsError();
  }

  // Check that the model1 argument is a SV Python Model object.
  auto model1 = GetModelFromPyObj(model1Arg);
  if (model1 == nullptr) {
      api.error("The first model argument is not a Model object.");
      return nullptr;
  }

  // Check that the model2 argument is a SV Python Model object.
  auto model2 = GetModelFromPyObj(model2Arg);
  if (model2 == nullptr) {
      api.error("The second model argument is not a Model object.");
      return nullptr;
  }

  // Check that the models were created using the same kernel.
  if (!ModelingCheckModelsKernels(api, model1, model2)) {
      return nullptr;
  }

  // Create the new solid.
  auto pyModelingModelObj = CreatePyModelingModelObject(self->kernel);
  if (pyModelingModelObj == nullptr) {
      api.error("Error creating a Python solid model object.");
      return nullptr;
  }
  auto model = ((PyModelingModel*)pyModelingModelObj)->solidModel;
  if (model == NULL) {
      api.error("Error creating a solid model.");
      return nullptr;
  }

  // Compute the intersection.
  //
  // The 'simplification' argument is not used.
  //
  SolidModel_SimplifyT simplification;
  if (model->Intersect(model1, model2, simplification) != SV_OK ) {
      delete model;
      api.error("Error performing a Boolean intersection.");
      return nullptr;
  }

  return pyModelingModelObj;
}

//----------------------
// ModelingModeler_read
//----------------------
//
PyDoc_STRVAR(ModelingModeler_read_doc,
  "read(file_name)  \n\
   \n\
   Read a solid model from a native format file. \n\
   \n\
   The native formats supported for each modeling kernel are: \n\
   \n\
         OpenCascade: brep \n\
   \n\
         Parasolid: xmt_txt \n\
   \n\
         PolyData:  ply, stl, vtk and vtp  \n\
   \n\
   Args:\n\
     file_name (str): The name of the file contining the solid model. \n\
   \n\
   Returns (Model): The solid model read from the file. \n\
");

static PyObject *
ModelingModeler_read(PyModelingModeler* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  static char *keywords[] = {"file_name", NULL};
  char *fileName;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileName)) {
      return api.argsError();
  }

  // Check that the file extension is valid for the modeler kernel.
  if (!ModelingCheckFileFormat(api, self->kernel, std::string(fileName))) {
      return nullptr;
  }

  // Create the new solid.
  auto pyModelingModelObj = CreatePyModelingModelObject(self->kernel);
  if (pyModelingModelObj == nullptr) {
      api.error("Error creating a Python solid model object.");
      return nullptr;
  }
  auto model = ((PyModelingModel*)pyModelingModelObj)->solidModel;
  if (model == NULL) {
      api.error("Error creating a solid model.");
      return nullptr;
  }

  if (model->ReadNative(fileName) != SV_OK) {
      delete model;
      api.error("Error reading a solid model from the file '" + std::string(fileName) + "'.");
      return nullptr;
  }

  return pyModelingModelObj;
}

//------------------------
// ModelingModeler_sphere
//------------------------
//
PyDoc_STRVAR(ModelingModeler_sphere_doc,
  "sphere(center, radius)  \n\
   \n\
   Create a 3D solid sphere. \n\
   \n\
   Args:\n\
     center ([float,float,float]): The sphere center. \n\
     radius (float): The sphere radius. \n\
   \n\
   Returns (Model): The sphere solid model. \n\
");

static PyObject *
ModelingModeler_sphere(PyModelingModeler* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("O!d", PyRunTimeErr, __func__);
  static char *keywords[] = {"center", "radius", NULL};
  PyObject* centerArg;
  double radius;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &centerArg, &radius)) {
      return api.argsError();
  }

  // Check for valid center data.
  std::string emsg;
  if (!PyUtilCheckPointData(centerArg, emsg)) {
      api.error("The sphere center argument " + emsg);
      return nullptr;
  }

  if (radius <= 0.0) {
      api.error("The 'radius' argument must be > 0.0.");
      return nullptr;
  }

  // Get sphere center.
  double center[3];
  for (int i = 0; i < PyList_Size(centerArg); i++) {
      center[i] = PyFloat_AsDouble(PyList_GetItem(centerArg, i));
  }

  // Create the new solid.
  auto pyModelingModelObj = CreatePyModelingModelObject(self->kernel);
  if (pyModelingModelObj == nullptr) {
      api.error("Error creating a Python solid model object.");
      return nullptr;
  }
  auto model = ((PyModelingModel*)pyModelingModelObj)->solidModel;
  if (model == NULL) {
      api.error("Error creating a cylinder solid model.");
      return nullptr;
  }

  if (model->MakeSphere(radius, center) != SV_OK) {
      delete model;
      api.error("Error creating a sphere solid model.");
      return nullptr;
  }

  return pyModelingModelObj;
}

//--------------------------
// ModelingModeler_subtract
//--------------------------
//
PyDoc_STRVAR(ModelingModeler_subtract_doc,
  "subtract(main, subtract)  \n\
   \n\
   Creates a solid from the result of a Boolean subtract operation on two solids. \n\
   \n\
   Args:\n\
     main (Model): The solid model to subtract from. \n\
     subtract (Model): The solid model to subtract from main. \n\
   \n\
   Returns (Model): The solid model of the subtracted models. \n\
");

static PyObject *
ModelingModeler_subtract(PyModelingModeler* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("OO", PyRunTimeErr, __func__);
  static char *keywords[] = {"main", "subtract", NULL};
  PyObject* mainModelArg;
  PyObject* subtractModelArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &mainModelArg, &subtractModelArg)) {
      return api.argsError();
  }

  // Check that the mainModel argument is a SV Python Model object.
  auto mainModel = GetModelFromPyObj(mainModelArg);
  if (mainModel == nullptr) {
      api.error("The main model argument is not a Model object.");
      return nullptr;
  }

  // Check that the subtractModel argument is a SV Python Model object.
  auto subtractModel = GetModelFromPyObj(subtractModelArg);
  if (subtractModel == nullptr) {
      api.error("The subtract model argument is not a Model object.");
      return nullptr;
  }

  // Check that the models were created using the same kernel.
  if (!ModelingCheckModelsKernels(api, mainModel, subtractModel)) {
      return nullptr;
  }

  // Create the new solid.
  auto pyModelingModelObj = CreatePyModelingModelObject(self->kernel);
  if (pyModelingModelObj == nullptr) {
      api.error("Error creating a Python solid model object.");
      return nullptr;
  }
  auto model = ((PyModelingModel*)pyModelingModelObj)->solidModel;
  if (model == NULL) {
      api.error("Error creating a solid model.");
      return nullptr;
  }

  // Compute the subtraction.
  //
  // The 'simplification' argument is not used.
  //
  SolidModel_SimplifyT simplification;
  if (model->Subtract(mainModel, subtractModel, simplification) != SV_OK) {
      delete model;
      api.error("Error performing the Boolean subtract.");
      return nullptr;
  }

  return pyModelingModelObj;
}

//-----------------------
// ModelingModeler_union
//-----------------------
//
PyDoc_STRVAR(ModelingModeler_union_doc,
  "union(model1, model2)  \n\
   \n\
   Create a solid from the Boolean union operation on two solids. \n\
   \n\
   Args:\n\
     model1 (Model): A solid model created by a modeler. \n\
     model2 (Model): A solid model created by a modeler. \n\
   \n\
   Returns (Model): The solid model of the unioned models. \n\
");

static PyObject *
ModelingModeler_union(PyModelingModeler* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("OO", PyRunTimeErr, __func__);
  static char *keywords[] = {"model1", "model2", NULL};
  PyObject* model1Arg;
  PyObject* model2Arg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &model1Arg, &model2Arg)) {
      return api.argsError();
  }

  // Check that the model1 argument is a SV Python Model object.
  auto model1 = GetModelFromPyObj(model1Arg);
  if (model1 == nullptr) {
      api.error("The first model argument is not a Model object.");
      return nullptr;
  }

  // Check that the model2 argument is a SV Python Model object.
  auto model2 = GetModelFromPyObj(model2Arg);
  if (model2 == nullptr) {
      api.error("The second model argument is not a Model object.");
      return nullptr;
  }

  // Check that the models were created using the same kernel.
  if (!ModelingCheckModelsKernels(api, model1, model2)) {
      return nullptr;
  }

  // Create the new solid.
  auto pyModelingModelObj = CreatePyModelingModelObject(self->kernel);
  if (pyModelingModelObj == nullptr) {
      api.error("Error creating a Python solid model object.");
      return nullptr;
  }
  auto model = ((PyModelingModel*)pyModelingModelObj)->solidModel;
  if (model == NULL) {
      api.error("Error creating a solid model.");
      return nullptr;
  }

  // Compute the union.
  //
  // The 'simplification' argument is not used.
  //
  SolidModel_SimplifyT simplification;
  if (model->Union(model1, model2, simplification) != SV_OK) {
      delete model;
      api.error("Error performing the Boolean union.");
      return nullptr;
  }

  return pyModelingModelObj;
}

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* MODELING_MODELER_CLASS = "Modeler";
static char* MODELING_MODELER_MODULE_CLASS = "modeling.Modeler";
// The name of the Modeler class veriable that contains all of the kernel types.
static char* MODELING_MODELER_CLASS_VARIBLE_NAMES = "names";

//--------------------------
// ModelingModelerClass_doc
//--------------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(ModelingModelerClass_doc,
   "Modeler(kernel) \n\
   \n\
   The Modeler class provides an interface for creating solid models and   \n\
   performing intersect, subtract and union Boolean operations on them.    \n\
   \n\
   A Modeler object is created for a specific kernel by passing a kernel   \n\
   name as an argument to the Modeler() constructor. The kernel name is    \n\
   specified using the modeling.Kernel class.                              \n\
   \n\
   Example: Creating a modeler for a PolyData kernel                       \n\
   \n\
       >>> modeler = sv.modeling.Modeler(sv.modeling.Kernel.POLYDATA)      \n\
   \n\
   A Modeler object creates solid models for the kernel it was created     \n\
   with. Modeler Boolean operations require models created with the same   \n\
   kernel.                                                                 \n\
   \n\
   Args:\n\
     kernel (str): The name of the kernel used to create the modeler.      \n\
   \n\
");

//--------------------------
// PyModelingModelerMembers
//--------------------------
// Set the class data attributes.
//
PyMemberDef PyModelingModelerMembers[] = {
  {"kernel",  offsetof(PyModelingModeler, kernelName),  T_OBJECT_EX, READONLY,  "The kernel name for this modeler."},
  {NULL}
};

//------------------------
// ModelingModelerMethods
//------------------------
//
static PyMethodDef PyModelingModelerMethods[] = {

  { "box", (PyCFunction)ModelingModeler_box, METH_VARARGS | METH_KEYWORDS, ModelingModeler_box_doc },

  { "cylinder", (PyCFunction)ModelingModeler_cylinder, METH_VARARGS | METH_KEYWORDS, ModelingModeler_cylinder_doc },

  { "intersect", (PyCFunction)ModelingModeler_intersect, METH_VARARGS | METH_KEYWORDS, ModelingModeler_intersect_doc },

  // [TODO:DaveP] The cvModelingModel MakeEllipsoid method is not implemented.
  //{ "ellipsoid", (PyCFunction)ModelingModeler_ellipsoid, METH_VARARGS | METH_KEYWORDS, ModelingModeler_ellipsoid_doc},

  { "read", (PyCFunction)ModelingModeler_read, METH_VARARGS|METH_KEYWORDS, ModelingModeler_read_doc },

  { "sphere", (PyCFunction)ModelingModeler_sphere, METH_VARARGS | METH_KEYWORDS, ModelingModeler_sphere_doc },

  { "subtract", (PyCFunction)ModelingModeler_subtract, METH_VARARGS | METH_KEYWORDS, ModelingModeler_subtract_doc },

  { "union", (PyCFunction)ModelingModeler_union, METH_VARARGS | METH_KEYWORDS, ModelingModeler_union_doc},

  {NULL, NULL}
};

//-----------------------
// PyModelingModelerInit
//-----------------------
// This is the __init__() method for the solid.Modeler class.
//
// This function is used to initialize an object after it is created.
//
static int
PyModelingModelerInit(PyModelingModeler* self, PyObject* args, PyObject *kwds)
{
  static int numObjs = 1;

  #ifdef dbg_PyModelingModelerInit
  std::cout << std::endl;
  std::cout << "[PyModelingModelerInit] ========== PyModelingModelerInit ==========" << std::endl;
  std::cout << "[PyModelingModelerInit] New PyModelingModeler object: " << numObjs << std::endl;
  #endif
  auto api = PyUtilApiFunction("", PyRunTimeErr, "ModelingModeler");
  char* kernelName = nullptr;
  if (!PyArg_ParseTuple(args, "s", &kernelName)) {
      return -1;
  }
  auto kernel = kernelNameEnumMap.at(std::string(kernelName));
  self->id = numObjs;
  self->kernel = kernel;
  self->kernelName = Py_BuildValue("s", kernelName);
  numObjs += 1;

  #ifdef dbg_PyModelingModelerInit
  std::cout << "[PyModelingModelerInit] self->id: " << self->id << std::endl;
  std::cout << "[PyModelingModelerInit] Kernel: " << kernel << std::endl;
  std::cout << "[PyModelingModelerInit] Kernel name: " << kernelName << std::endl;
  #endif

  return 0;
}

//-----------------------
// PyModelingModelerType
//-----------------------
// Define the Python type object that stores contour.kernel types.
//
PyTypeObject PyModelingModelerType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  MODELING_MODELER_MODULE_CLASS,
  sizeof(PyModelingModeler)
};

//----------------------
// PyModelingModelerNew
//----------------------
//
static PyObject *
PyModelingModelerNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  #ifdef dbg_PyModelingModelerNew
  std::cout << std::endl;
  std::cout << "[PyModelingModelerNew] ========== PyModelingModelerNew ==========" << std::endl;
  std::cout << "[PyModelingModelerNew] New ModelingModeler" << std::endl;
  #endif

  auto api = PyUtilApiFunction("s", PyRunTimeErr, "Modeler");
  char* kernelName = nullptr;
  if (!PyArg_ParseTuple(args, api.format, &kernelName)) {
      return api.argsError();
  }

  SolidModel_KernelT kernel;

  try {
      kernel = kernelNameEnumMap.at(std::string(kernelName));
  } catch (const std::out_of_range& except) {
      auto msg = "Unknown kernel name '" + std::string(kernelName) + "'." +
          " Valid names are: " + kernelValidNames + ".";
      api.error(msg);
      return nullptr;
  }

  #ifdef dbg_PyModelingModelerNew
  std::cout << "[PyModelingModelerNew] Kernel: " << kernel << std::endl;
  std::cout << "[PyModelingModelerNew] Kernel name: " << kernelName << std::endl;
  #endif

  try {
    auto ctore = CvSolidModelCtorMap.at(kernel);
  } catch (const std::out_of_range& except) {
      api.error("No modeler is defined for the kernel name '" + std::string(kernelName) + "'.");
      return nullptr;
  }

  auto self = (PyModelingModeler*)type->tp_alloc(type, 0);
  if (self != NULL) {
      self->id = 0;
  }

  return (PyObject *) self;
}

//--------------------------
// PyModelingModelerDealloc
//--------------------------
//
static void
PyModelingModelerDealloc(PyModelingModeler* self)
{
  #ifdef dbg_PyModelingModelerDealloc
  std::cout << "[PyModelingModelerDealloc] **** PyModelingModelerDealloc ****" << std::endl;
  #endif
  //delete self->solidModel;
  Py_TYPE(self)->tp_free(self);
}

//------------------------------
// SetModelingModelerTypeFields
//------------------------------
// Set the Python type object fields that stores ModelingModeler data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetModelingModelerTypeFields(PyTypeObject& solidModelType)
{
  // Doc string for this type.
  solidModelType.tp_doc = ModelingModelerClass_doc;
  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  solidModelType.tp_new = PyModelingModelerNew;
  //solidModelType.tp_new = PyType_GenericNew,
  solidModelType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  solidModelType.tp_init = (initproc)PyModelingModelerInit;
  solidModelType.tp_dealloc = (destructor)PyModelingModelerDealloc;
  solidModelType.tp_methods = PyModelingModelerMethods;
  solidModelType.tp_members = PyModelingModelerMembers;
};

//---------------------------
// CreateModelingModelerType
//---------------------------
static PyModelingModeler *
CreateModelingModelerType()
{
  return PyObject_New(PyModelingModeler, &PyModelingModelerType);
}

#endif


